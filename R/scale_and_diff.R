#' @name scale_and_diff
#' @title Normalize Values and Calculate Derivatives
#' @description This function normalizes the `value` column within each group in a data frame and calculates the derivative (difference) between consecutive values. It first shifts values within each group to compute differences, then performs min-max normalization for consistency across groups. Optionally, results can be saved as a compressed CSV file.
#' @param df A data frame with a `value` column to be normalized and a `variable` column for grouping.
#' @param save Logical, if TRUE, saves the output as a compressed CSV file named "delta_lead.csv.gz". Default is FALSE.
#' @return A data frame with normalized `value` and calculated `delta` columns.
#' @examples
#' # Create an example data frame for normalization and differentiation
#' df <- data.frame(
#'   value = c(1, 2, 3, 4, 5),
#'   variable = "var1",
#'   pseudotime = 1:5
#' )
#'
#' # Apply normalization and differentiation
#' normalized_df <- scale_and_diff(df)
#'
#' # Apply and save the output
#' normalized_df <- scale_and_diff(df, save = TRUE)
#' @export
#' @import data.table
scale_and_diff <- function(df, save = FALSE) {

  setDT(df)  # Convert to data.table if not already

  # Group by 'variable' and perform transformations
  df[, `:=`(
    delta = shift(value, type = "lead")  - value # Calculate delta as the difference from next value
  ), by = variable]

  df[, `:=`(
    min = min(value),             # Minimum of 'value' in each group
    value = value - min(value)    # Subtract min from 'value' for min-max normalization
  ), by = variable]

  df[, `:=`(
    max = max(value),             # Maximum of 'value' in each group
    value = value / max(value),   # Normalize 'value' by max within each group
    delta = delta / max(value)    # Normalize 'delta' by max within each group
  ), by = variable]

  # Remove temporary columns used for min-max normalization
  df$min <- NULL
  df$max <- NULL

  # Optionally save the data frame as a compressed CSV file
  if (save) {
    fwrite(
      df,
      "delta_lead.csv.gz"
    )
  }

  return(df)
}
