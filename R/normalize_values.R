#' @name normalize_values
#' @title Normalize Values
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
normalize_values <- function(df, normalization = "max", save = FALSE) {

  # Check for valid normalization method
  if (!normalization %in% c("max", "min-max")) {
    stop("Normalization method does not exist. Choose either 'max' or 'min-max'.")
  }
  setDT(df)
  # Normalize values based on the chosen method
  setorder(df, pseudotime)
  if (normalization == "max") {
    df[, value := value / max(abs(value), na.rm = TRUE)]
  } else if (normalization == "min-max") {
    df[, value := (value - min(value, na.rm = TRUE)) / (max(value, na.rm = TRUE) - min(value, na.rm = TRUE))]
  }

  if(save){
    frwite(
      df_phase_space,
      paste0("normalized_", normalization, ".csv.gz")
    )
  }
  return(df)
}
