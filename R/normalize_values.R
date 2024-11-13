#' @name normalize_values
#' @title Normalize Values
#' @description This function normalizes the `value` column in a data frame using either max or min-max normalization.
#' It scales the values based on the specified method and optionally saves the output as a compressed CSV file.
#' @param df A data frame with a `value` column to be normalized.
#' @param normalization A character string specifying the normalization method.
#'   Options are:
#'   - `"max"` (default): Scales `value` by dividing by the maximum absolute value.
#'   - `"min-max"`: Scales `value` to a 0-1 range by subtracting the minimum and dividing by the range.
#' @param save Logical, if TRUE, saves the output as a compressed CSV file. Default is FALSE.
#' @return A data frame with the normalized `value` column.
#' @examples
#' # Create an example data frame for normalization
#' df <- data.frame(
#'   value = c(1, 2, 3, 4, 5),
#'   variable = "var1",
#'   pseudotime = 1:5
#' )
#'
#' # Apply min-max normalization
#' normalized_df <- normalize_values(df, normalization = "min-max")
#'
#' # Apply max normalization and save the output as a CSV file
#' normalized_df <- normalize_values(df, normalization = "max", save = TRUE)
#' @export
#' @import data.table
normalize_values <- function(df, normalization = "max", save = FALSE) {

  # Check for valid normalization method
  if (!normalization %in% c("max", "min-max")) {
    stop("Normalization method does not exist. Choose either 'max' or 'min-max'.")
  }

  setDT(df) # Convert to data.table for efficiency

  # Normalize values based on the chosen method
  setorder(df, pseudotime)
  if (normalization == "max") {
    df[, value := value / max(abs(value), na.rm = TRUE)]
  } else if (normalization == "min-max") {
    df[, value := (value - min(value, na.rm = TRUE)) / (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))]
  }

  # Optionally save the normalized data frame as a compressed CSV file
  if (save) {
    fwrite(
      df,
      paste0("normalized_", normalization, ".csv.gz")
    )
  }

  return(df)
}
