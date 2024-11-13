#' @name pseudotime_trajectory
#' @title Calculate Pseudotime Trajectory Summary Statistics
#' This function calculates summary statistics (median and quantiles) for `x` and `y` variables along a pseudotime trajectory within each condition. It groups data by `condition` and `pseudotime`, then computes the 25th and 75th percentiles, as well as the median, for each group. This can be used to analyze trends or patterns in `x` and `y` over pseudotime.
#' @param df A data frame containing the following columns:
#'   - `condition`: Factor or character column indicating the experimental or observational condition.
#'   - `pseudotime`: Numeric column indicating pseudotime or a sequential progression.
#'   - `x`: Numeric column representing values for the x-axis variable.
#'   - `y`: Numeric column representing values for the y-axis variable.
#'   - `x_variable`: Character or factor column identifying the name of the `x` variable.
#'   - `y_variable`: Character or factor column identifying the name of the `y` variable.
#' @param input Either "standard" or "fraction" to specify the type of analysis.
#' @param save Logical, if TRUE saves the resulting data frame as a CSV file. Default is FALSE.
#' @return A data frame containing the calculated summary statistics for each combination
#'   of `condition` and `pseudotime`. Columns in the returned data frame include:
#'   - `x1`: 25th percentile of `x` values.
#'   - `y1`: 25th percentile of `y` values.
#'   - `x3`: 75th percentile of `x` values.
#'   - `y3`: 75th percentile of `y` values.
#'   - `x`: Median of `x` values.
#'   - `y`: Median of `y` values.
#'   - `n`: Number of observations in each group.
#' @examples
#' # Example usage:
#' pseudotime_trajectory(df)
#' @import data.table
#' @import dplyr
#' @export
pseudotime_trajectory <- function(df, input = "standard", save = FALSE){

  # Validate input parameters
  if (!input %in% c("standard", "fraction")) {
    stop("Invalid input: Choose 'standard' or 'fraction'")
  }

  # Validate df columns
  required_columns <- c("condition", "pseudotime", "x", "y", "x_variable", "y_variable")
  missing_columns <- setdiff(required_columns, names(df))
  if (length(missing_columns) > 0) {
    stop("The following required columns are missing in df: ", paste(missing_columns, collapse = ", "))
  }

  setDT(df)  # Ensure data.table format

  # Define labels for x and y based on input type
  if (input == "standard") {
    # Group by necessary variables
    pseudotime_trajectory <- df[, .(
      x1 = quantile(x, 0.25),             # 25th percentile of x
      y1 = quantile(y, 0.25),             # 25th percentile of y
      x3 = quantile(x, 0.75),             # 75th percentile of x
      y3 = quantile(y, 0.75),             # 75th percentile of y
      x = median(x),                      # Median of x
      y = median(y),                      # Median of y
      n = .N                              # Count of rows per group
    ), by = .(condition, pseudotime, x_variable, y_variable, x_label, y_label)]  # Include all relevant labels in grouping

  } else {
    pseudotime_trajectory <-
      df %>%
      mutate(
        # Labels for fractional x and cumulative y
        x_label = paste0(x_variable, " pct"),
        y_label = paste0(x_variable, " + ", y_variable)
      ) %>%
      rowwise() %>%
      mutate(
        y = x + y,                     # Set `y` as the total size
        x = x / y                      # Calculate `x` as a fraction of the total size
      ) %>%
      ungroup() %>%
      mutate(
        y = y/2
      ) %>%
      group_by(
        condition, pseudotime, x_variable, y_variable, x_label, y_label
      ) %>%
      summarize(
        x1 = quantile(x, 0.25),    # 25th percentile of transformed x
        y1 = quantile(y, 0.25),    # 25th percentile of transformed y
        x3 = quantile(x, 0.75),    # 75th percentile of transformed x
        y3 = quantile(y, 0.75),    # 75th percentile of transformed y
        x = median(x),      # Median of transformed x
        y = median(y),      # Median of transformed y
        n = n()                                # Count of rows per group
      ) %>%
      as.data.table()
  }

  # Save the output as CSV if `save` is TRUE
  if (save) {
    fwrite(
      pseudotime_trajectory,
      paste0("pseudotime_trajectory", ifelse(input == "standard", "", paste0("_", input)), ".csv.gz")
    )
  }

  return(pseudotime_trajectory)
}
