#' @name calculate_derivatives
#' @title Calculate Rate of Change (Delta) in Values Over Time
#' This function calculates the rate of change (delta) in the `value` column over `pseudotime`. The calculation method can be specified as lead, lag, or lead-lag.
#' @param df A data frame with `value`, `pseudotime`, `sample`, `condition`, and `variable` columns.
#' @param derivative A character string specifying the derivative method to calculate the rate of change.
#'   Options are:
#'   - `"lead"` (default): Uses the next timepoint to calculate the change.
#'   - `"lag"`: Uses the previous timepoint to calculate the change.
#'   - `"lead-lag"`: Calculates the change over two surrounding timepoints, useful for datasets with more than two timepoints.
#' @param delta_name A character string specifying the name of the new column for the rate of change. Default is `"delta"`.
#' @param save Logical, if TRUE, saves the resulting data frame to a file.
#' @return A data frame with an added `delta` column (or specified name) representing the rate of change in `value` over `pseudotime`.
#' @examples
#' df <- data.frame(
#'   value = c(1, 2, 3, 4, 5),
#'   pseudotime = c(1, 2, 3, 4, 5),
#'   sample = "sample1",
#'   condition = "condition1",
#'   variable = "variable1"
#' )
#' calculate_derivatives(df, derivative = "lead-lag", delta_name = "rate_change")
#'
#' @export
#' @import data.table
library(data.table)
calculate_derivatives <- function(df, derivative = "lead", delta_name = "delta", save = FALSE) {
  # Check for valid derivative method
  if (!derivative %in% c("lead", "lag", "lead-lag")) {
    stop("Derivative method does not exist. Choose 'lead', 'lag', or 'lead-lag'.")
  }

  # Arrange and group data by specified columns
  setDT(df)
  df <- df[order(pseudotime), .SD, by = .(condition, sample, variable)]

  # Calculate delta based on the chosen derivative method
  if (derivative == "lead") {
    df[, (delta_name) := (shift(value, type = "lead") - value) / (shift(pseudotime, type = "lead") - pseudotime)]
  } else if (derivative == "lag") {
    df[, (delta_name) := (shift(value, type = "lag") - value) / (shift(pseudotime, type = "lag") - pseudotime)]
  } else if (derivative == "lead-lag") {
    df[, (delta_name) := (shift(value, type = "lead") - shift(value, type = "lag")) /
         (shift(pseudotime, type = "lead") - shift(pseudotime, type = "lag"))]
  }

  # Optionally save the data
  if (save) {
    fwrite(df, paste0("delta_", delta_name, ".csv.gz"))
  }
  return(df)
}
