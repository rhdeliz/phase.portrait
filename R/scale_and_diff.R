#' @name scale_and_diff
#' @title Normalize Values and calculate derivatives
#' This function normalizes a value column in a data frame using either max or min-max normalization.
#' @param df A data frame with a `value` column to be normalized.
#' @param normalization A character string specifying the normalization method.
#'   Options are:
#'   - `"max"` (default): Scales `value` by dividing by the maximum absolute value.
#'   - `"min-max"`: Scales `value` to a 0-1 range by subtracting the minimum and dividing by the maximum.
#' @return A data frame with the normalized `value` column.
#' @examples
#' df <- data.frame(value = c(1, 2, 3, 4, 5), variable = "var1")
#' normalize_values(df, normalization = "min-max")
#' @export
#' @import data.table
scale_and_diff <- function(df, save = FALSE) {

  setDT(df)  # Convert to data.table if not already

  # Group by 'variable' and perform the transformations
  df[, `:=`(
    delta = value - shift(value)  # Calculate delta as difference from previous value
  ), by = variable]

  df[, `:=`(
    min = min(value),          # Find min of 'value' in each group
    value = value - min(value)  # Subtract min from 'value'
  ), by = variable]

  df[, `:=`(
    max = max(value),            # Find max of 'value' in each group
    value = value / max(value),  # Normalize 'value' by max
    delta = delta / max(value)   # Normalize 'delta' by max
  ), by = variable]

  df$min <- NULL
  df$max <- NULL

  if (save) {
    fwrite(
      df,
      paste0("delta_lead.csv.gz")
    )
  }
  return(df)
}
