#' @name phase_space
#' @title Generate Phase Space for Pairwise Variable Analysis with Parallel Processing
#' This function calculates the phase space for all pairwise combinations of variables within a data frame. It computes phase portraits by pairing each variable's values and their derivatives, then merges them into a combined phase space data frame. Parallel processing is supported for improved performance.
#' @param df A data frame containing the following columns:
#'   - `value`: Numeric values representing measurements or observations of interest.
#'   - `delta`: Numeric values representing the derivative (rate of change) of `value`.
#'   - `variable`: Factor or character column representing the variable or feature name.
#'   - `condition`: Factor or character column identifying different experimental or observational conditions.
#'   - `sample`: Factor or character column identifying different samples.
#'   - `pseudotime`: Numeric values representing temporal or sequential progression.
#' @param n_cores Integer specifying the number of cores to use for parallel processing.
#'   If NULL, defaults to the total available cores minus two (if running on Unix). On Windows, `n_cores` is set to 1.
#' @return A data frame containing phase space information with columns:
#'   - `x`: Values of the first variable in each pairwise combination.
#'   - `dx`: Derivative of the first variable.
#'   - `x_variable`: Name of the first variable.
#'   - `y`: Values of the second variable in each pairwise combination.
#'   - `dy`: Derivative of the second variable.
#'   - `y_variable`: Name of the second variable.
#'   - Additional columns: `condition`, `sample`, and `pseudotime`.
#' @examples
#' # Example data frame
#' df <- data.frame(
#'   value = runif(100),
#'   delta = rnorm(100),
#'   variable = rep(c("var1", "var2", "var3", "var4"), each = 25),
#'   condition = rep("cond1", 100),
#'   sample = rep(1:5, each = 20),
#'   pseudotime = rep(1:20, 5)
#' )
#'
#' # Generate phase space
#' phase_space(df)
#'
#' @export
#' @import data.table
#' @import dplyr
#' @import parallel
phase_space <- function(df, n_cores = NULL, save = FALSE) {

  # Set the number of cores to use
  if (is.null(n_cores)) {
    n_cores <- max(1, detectCores() - 2)
  }

  if (.Platform$OS.type == "windows") {
    # Suppress warnings just for makeCluster
    cl <- tryCatch({
      suppressWarnings(makeCluster(n_cores))
    }, warning = function(w) {
      invokeRestart("muffleWarning") # Ignore the warning and continue
    }, error = function(e) {
      stop("Error in creating cluster: ", e$message) # Handle errors normally
    })

    on.exit(stopCluster(cl)) # Ensure the cluster stops after the function completes

    # Load required packages on each cluster worker
    clusterEvalQ(cl, {
      library(dplyr)
      library(data.table)
    })
  }

  # Split data by variable to prepare for pairwise combinations
  split_table <- df %>%
    ungroup() %>%
    group_split(variable)

  # Number of unique variables
  n_variables <- NROW(split_table)

  # Generate all possible pairs of variables, excluding self-pairs
  combinations <- expand.grid(1:n_variables, 1:n_variables)
  names(combinations) <- c("x", "y")
  combinations <- combinations %>% filter(x != y)

  if (.Platform$OS.type == "windows") {
    # Export necessary objects to the cluster
    clusterExport(cl, varlist = c("split_table", "combinations"), envir = environment())
  }

  # Define function to calculate phase space for a given pair of variables
  phase_portrait_analysis <- function(i) {
    x_table <- combinations$x[i]
    y_table <- combinations$y[i]

    ref_table <- split_table[[x_table]] %>%
      rename(x = value, dx = delta, x_variable = variable)
    setDT(ref_table)

    qry_table <- split_table[[y_table]] %>%
      rename(y = value, dy = delta, y_variable = variable)
    setDT(qry_table)

    phase_space <- merge.data.table(ref_table, qry_table, by = c("condition", "sample", "pseudotime"))

    return(phase_space)
  }

  # Apply phase portrait analysis in parallel
  if (.Platform$OS.type == "windows") {
    df_phase_space <- parLapply(cl, 1:NROW(combinations), phase_portrait_analysis)
  }
  if(n_cores == 1){
    df_phase_space <- lapply(1:NROW(combinations), phase_portrait_analysis)
  } else{
    df_phase_space <- mclapply(1:NROW(combinations), phase_portrait_analysis, mc.cores = n_cores)
  }

  df_phase_space <- rbindlist(df_phase_space)
  df_phase_space[, `:=`(x_label = x_variable, y_label = y_variable)]

  if(save == TRUE){
    fwrite(
      df_phase_space,
      "phase_space.csv.gz"
    )
  }

  return(df_phase_space)
}
