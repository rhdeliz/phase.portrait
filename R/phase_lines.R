#' @name phase_lines
#' @title Generate Phase Lines for Visualization (Binned, Averaged, or all)
#' This function generates a data frame for visualizing phase lines by creating either binned, averaged, or all types of phase line summaries, using `x` and `y` variables or their fractional representations. The result can be used to create phase line plots with separate facets for `dx` or `dy`, with flexibility to filter based on a minimum number of observations per bin.
#' @param df A data frame containing columns: `x`, `y`, `dx`, `dy`, `x_variable`, `y_variable`, `x_label`, `y_label`, `dx_label`, `dy_label`
#' @param n_bins Integer specifying the number of bins to discretize `x` and `y`. Default is 4.
#' @param min_bin_n Integer specifying the minimum number of observations required in each bin.
#' @param input Character string specifying "standard" for direct values or "fraction" for fractional representation.
#' @param output Character string specifying output type: "binned", "average", or "all".
#' @return A data table with phase line summaries, including a column `output_type` indicating "binned" or "average".
#'
#' @examples
#' # Example usage
#' phase_lines(df, n_bins = 5, min_bin_n = 2, input = "standard", output = "all")
#' @export
library(data.table)
library(dplyr)
library(tidyr)

phase_lines <- function(df, n_bins = 4, min_bin_n = 1, input = "standard", output = "all", save = FALSE) {

  # Validate `input` and `output` parameters to ensure they contain valid values
  if (!input %in% c("standard", "fraction")) {
    stop("Invalid input: Choose 'standard' or 'fraction'")
  }
  if (!output %in% c("binned", "average", "all")) {
    stop("Invalid output type: Choose 'binned', 'average', or 'all'")
  }

  # Adjust binning parameter by reducing `n_bins` by 1 to use zero-based rounding
  n_bins <- n_bins - 1

  # Generate binned data if "binned" or "all" output is selected
  if (output %in% c("binned", "all")) {
    binned_df <- bind_rows(
      # Each mutate statement prepares a set of phase lines with specific x/y and dx/dy combinations
      df %>% mutate(bin = x, delta = dx, facet = paste(x_label, dx_label), group_var = y),
      df %>% mutate(bin = x, delta = dy, facet = paste(x_label, dy_label), group_var = y),
      df %>% mutate(bin = y, delta = dx, facet = paste(y_label, dx_label), group_var = x),
      df %>% mutate(bin = y, delta = dy, facet = paste(y_label, dy_label), group_var = x)
    ) %>%
      # Remove original columns that are no longer needed
      select(-c(x, y, dx, dy, dx_label, dy_label)) %>%
      # Mark the output type as "binned"
      mutate(output_type = "binned") %>%
      # Remove rows with any missing values
      drop_na() %>%
      rename(
        x = bin,
        y = delta
      ) %>%
      as.data.table() # Convert to data.table format
  }

  # Generate averaged data if "average" or "all" output is selected
  if (output %in% c("average", "all")) {
    # Calculate median dx and dy within groups for `x`
    phase_x <- df %>%
      drop_na() %>%
      group_by(input, condition, x_variable, y_variable, x, x_label, y_label, dx_label, dy_label) %>%
      summarize(dx = median(dx), dy = median(dy), n = n(), .groups = "drop") %>%
      filter(n >= min_bin_n) # Filter groups with fewer than min_bin_n observations

    # Calculate median dx and dy within groups for `y`
    phase_y <- df %>%
      drop_na() %>%
      group_by(input, condition, x_variable, y_variable, y, x_label, y_label, dx_label, dy_label) %>%
      summarize(dx = median(dx), dy = median(dy), n = n(), .groups = "drop") %>%
      filter(n >= min_bin_n)

    # Bind the different phase lines into one data frame for averaged output
    avg_df <- bind_rows(
      phase_x %>% mutate(facet = paste(x_label, dy_label), bin = x, delta = dy, output_type = "average"),
      phase_x %>% mutate(facet = paste(x_label, dx_label), bin = x, delta = dx, output_type = "average"),
      phase_y %>% mutate(facet = paste(y_label, dx_label), bin = y, delta = dx, output_type = "average"),
      phase_y %>% mutate(facet = paste(y_label, dy_label), bin = y, delta = dy, output_type = "average")
    ) %>%
      # Remove original columns that are no longer needed
      select(-c(x, y, dx, dy, dx_label, dy_label)) %>%
      # Mark the output type as "average"
      mutate(output_type = "average") %>%
      rename(
        x = bin,
        y = delta
      ) %>%
      drop_na() %>%
      as.data.table() # Convert to data.table format
  }

  # Combine binned and average data frames if all are selected
  phase_lines_dt <- if (output == "all") {
    rbind(binned_df, avg_df, fill = TRUE)
  } else if (output == "binned") {
    binned_df
  } else {
    avg_df
  }

  if(save == TRUE){
    fwrite(
      phase_lines_dt,
      paste0("phase_lines ",
             ifelse(input=="standard", "", input),
             " n_bins", n_bins,
             ".csv")
    )
  }
  return(phase_lines_dt)
}
