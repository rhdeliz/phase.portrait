#' @name phase_portrait
#' @title Generate Phase Portrait Summary (Standard or Fraction-Based)
#' @description This function generates a phase portrait by binning `x` and `y` variables into discrete intervals and calculating the median rate of change (`dx` and `dy`) within each bin. It supports both standard and fraction-based portraits, where `x` values can be shown as fractions of the total `(x + y)`, to visualize dynamics across binned states.
#' @param df A data frame containing the following columns:
#'   - `condition`: Factor or character column indicating the experimental or observational condition.
#'   - `x`: Numeric column representing values for the x-axis variable.
#'   - `y`: Numeric column representing values for the y-axis variable.
#'   - `dx`: Numeric column representing the rate of change of `x`.
#'   - `dy`: Numeric column representing the rate of change of `y`.
#'   - `x_variable`, `y_variable`: Character or factor columns identifying the variables.
#' @param bin_width Integer specifying the number of bins to discretize `x` and `y` variables. Default is 4.
#' @param min_bin_n Integer specifying the minimum number of observations required in each bin for it to be included in the output. Default is 1.
#' @param input Character string specifying the input of data, either "standard" for standard binning or "fraction" for fraction-based binning.
#' @param save Logical, if TRUE, saves the resulting data frame as a compressed CSV file. Default is FALSE.
#' @return A data frame with summarized phase portrait data containing:
#'   - `condition`, `x_variable`, `y_variable`: Grouping columns from the input.
#'   - `x`, `y`: Discretized values of `x` and `y` or fractional representation of `x` and total size as `y`.
#'   - `dx`, `dy`: Median values of `dx` and `dy` within each bin.
#'   - `n`: Number of observations in each bin.
#'   - `x_label`, `y_label`: Labels for x and y variables, adapted based on the input.
#'   - `dx_label`, `dy_label`: Labels for rate of change for x and y variables.
#'   - `input`: The input of the data ("standard" or "fraction").
#' @examples
#' # Example usage
#' phase_portrait(df, bin_width = 5, min_bin_n = 2, input = "standard")
#' phase_portrait(df, bin_width = 5, min_bin_n = 2, input = "fraction")
#' @export
#' @import data.table
#' @import dplyr
phase_portrait <- function(df, bin_width = 0.25, min_bin_n = 1, input = "standard", save = FALSE) {

  # Validate input
  if (!input %in% c("standard", "fraction")) {
    stop("Invalid input: Choose 'standard' or 'fraction'")
  }

  # Adjust binning parameter for zero-based rounding
  bin_width <- 1/bin_width

  setDT(df)
  # Define labels for x and y based on input
  if (input == "fraction") {
    df[, `:=`(
      y = x + y,                                   # Set `y` as the total size
      x = x / (x + y),                             # Calculate `x` as a fraction of the size
      x_label = paste0(x_variable, " pct"),        # Label for fractional `x`
      y_label = paste0(x_variable, " + ", y_variable)  # Label for size-based `y`
    )]
    df[, y := y/2]
  } else {
    df <- df %>%
      mutate(
        x_label = x_variable,           # Standard label for `x`
        y_label = y_variable            # Standard label for `y`
      )

    df[, `:=`(
      x_label = x_variable,        # Label for `x`
      y_label = y_variable      # Label for `y`
    )]
  }

  # Bin `x` and `y`, then calculate summaries
  phase_portrait_dt <- df %>%
    mutate(
      x = round(x * bin_width) / bin_width,    # Discretize `x`
      y = round(y * bin_width) / bin_width     # Discretize `y`
    ) %>%
    group_by(
      condition, x_variable, y_variable, x, y, x_label, y_label
    ) %>%  # Include `x_label` and `y_label` in the grouping
    summarize(
      dx = median(dx, na.rm = TRUE),                   # Median rate of change for `x`
      dy = median(dy, na.rm = TRUE),                   # Median rate of change for `y`
      n = n(),                           # Count of observations in each bin
      .groups = "drop"
    ) %>%
    filter(n >= min_bin_n) %>%           # Filter bins with fewer than min_bin_n observations
    mutate(
      dx_label = paste("dx:", x_label),
      dy_label = paste("dy:", y_label),
      input = input
    ) %>%
    as.data.table()

  if(save == TRUE){
    fwrite(
      phase_portrait_dt,
      paste0("phase_portrait",
             ifelse(input=="standard", "", paste0("_", input)),
             "_bin_width_", bin_width,
             ".csv.gz")
    )
  }
  return(phase_portrait_dt)
}
