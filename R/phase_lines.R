#' @name phase_lines
#' @title Generate Phase Lines for Visualization (Binned, Averaged, or All)
#' @description This function generates a data frame for visualizing phase lines by creating either binned, averaged, or all types of phase line summaries, using `x` and `y` variables or their fractional representations. The result can be used to create phase line plots with separate facets for `dx` or `dy`, with flexibility to filter based on a minimum number of observations per bin.
#' @param df A data frame containing columns: `x`, `y`, `dx`, `dy`, `x_variable`, `y_variable`, `x_label`, `y_label`, `dx_label`, `dy_label`.
#' @param bin_width Integer specifying the number of bins to discretize `x` and `y`. Default is 4.
#' @param min_bin_n Integer specifying the minimum number of observations required in each bin.
#' @param input Character string specifying "standard" for direct values or "fraction" for fractional representation.
#' @param output Character string specifying output type: "binned", "average", or "all".
#' @param save Logical, if TRUE, saves the resulting data frame as a compressed CSV file. Default is FALSE.
#' @return A data table with phase line summaries, including a column `output_type` indicating "binned" or "average".
#'
#' @examples
#' # Example usage
#' phase_lines(df, bin_width = 5, min_bin_n = 2, input = "standard", output = "all")
#' @export
#' @import data.table
#' @import dplyr
phase_lines <- function(df, bin_width = 0.25, min_bin_n = 1, input = "standard",
                        output = "all", save = FALSE) {

  # Validate input and output parameters
  if (!input %in% c("standard", "fraction")) {
    stop("Invalid input: Choose 'standard' or 'fraction'")
  }
  if (!output %in% c("binned", "average", "all")) {
    stop("Invalid output type: Choose 'binned', 'average', or 'all'")
  }

  # Adjust binning parameter
  bin_width <- 1 / bin_width
  setDT(df)

  # Generate binned data if "binned" or "all" output is selected
  if (output %in% c("binned", "all")) {
    binned_df <- rbindlist(list(
      df[, .(bin = x, delta = dx, facet = paste("x:",x_label, dx_label), group_var = -y,
             x_variable, y_variable, x_label, y_label, dx_label, dy_label, condition, n = .N), by = .(x)],
      df[, .(bin = x, delta = dy, facet = paste("x:",x_label, dy_label), group_var = -y,
             x_variable, y_variable, x_label, y_label, dx_label, dy_label, condition, n = .N), by = .(x)],
      df[, .(bin = y, delta = dx, facet = paste("y:",y_label, dx_label), group_var = x,
             x_variable, y_variable, x_label, y_label, dx_label, dy_label, condition, n = .N), by = .(y)],
      df[, .(bin = y, delta = dy, facet = paste("y:",y_label, dy_label), group_var = x,
             x_variable, y_variable, x_label, y_label, dx_label, dy_label, condition, n = .N), by = .(y)]
    ), fill = TRUE)

    # Remove original x and y columns to avoid duplicates during renaming
    binned_df$x <- NULL
    binned_df$y <- NULL

    binned_df <- binned_df %>% rename(x = bin, y = delta)
    binned_df[, output_type := "binned"]
  }

  # Generate averaged data if "average" or "all" output is selected
  if (output %in% c("average", "all")) {
    phase_x <- df[complete.cases(df), .(
      dx = median(dx, na.rm = TRUE),
      dy = median(dy, na.rm = TRUE),
      n = .N
    ), by = .(input, condition, x_variable, y_variable, x, x_label, y_label, dx_label, dy_label)][n >= min_bin_n]

    phase_y <- df[complete.cases(df), .(
      dx = median(dx, na.rm = TRUE),
      dy = median(dy, na.rm = TRUE),
      n = .N
    ), by = .(input, condition, x_variable, y_variable, y, x_label, y_label, dx_label, dy_label)][n >= min_bin_n]

    avg_df <- rbindlist(list(
      phase_x[, .(facet = paste("x:", x_label, dy_label), bin = x, delta = dy, output_type = "average",
                  condition, n, x_variable, y_variable, x_label, y_label, dx_label, dy_label)],
      phase_x[, .(facet = paste("x:", x_label, dx_label), bin = x, delta = dx, output_type = "average",
                  condition, n, x_variable, y_variable, x_label, y_label, dx_label, dy_label)],
      phase_y[, .(facet = paste("y:", y_label, dx_label), bin = y, delta = dx, output_type = "average",
                  condition, n, x_variable, y_variable, x_label, y_label, dx_label, dy_label)],
      phase_y[, .(facet = paste("y:", y_label, dy_label), bin = y, delta = dy, output_type = "average",
                  condition, n, x_variable, y_variable, x_label, y_label, dx_label, dy_label)]
    ), fill = TRUE) %>% rename(x = bin, y = delta)
  }

  # Combine binned and average data frames if all are selected
  phase_lines_dt <- if (output == "all") {
    rbind(binned_df, avg_df, fill = TRUE)
  } else if (output == "binned") {
    binned_df
  } else {
    avg_df
  }
  phase_lines_dt$input <- input
  if (save) {
    fwrite(
      phase_lines_dt,
      paste0("phase_lines", ifelse(input == "standard", "", paste0("_", input)),
             # "_bin_width_", bin_width,
             ".csv.gz")
    )
  }
  return(phase_lines_dt)
}
