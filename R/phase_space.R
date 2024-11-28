#' @name phase_space
#' @title Generate Phase Space for Pairwise Variable Analysis with Parallel Processing
#' @description This function calculates the phase space for all pairwise combinations of variables within a data frame. It computes phase portraits by pairing each variable's values and their derivatives, then merges them into a combined phase space data frame. Parallel processing is supported for improved performance.
#' @param df A data frame containing the following columns:
#'   - `value`: Numeric values representing measurements or observations of interest.
#'   - `delta`: Numeric values representing the derivative (rate of change) of `value`.
#'   - `variable`: Factor or character column representing the variable or feature name.
#'   - `condition`: Factor or character column identifying different experimental or observational conditions.
#'   - `sample`: Factor or character column identifying different samples.
#'   - `pseudotime`: Numeric values representing temporal or sequential progression.
#' @param n_cores Integer specifying the number of cores to use for parallel processing.
#'   If NULL, defaults to the total available cores minus two (if running on Unix). On Windows, `n_cores` is set to 1.
#' @param save Logical, if TRUE, saves the output as a compressed CSV file. Default is FALSE.
#' @param focus_variables Character representing the x-axis variable. Default is NULL
#' @return A data frame containing phase space information with columns:
#'   - `x`: Values of the first variable in each pairwise combination.
#'   - `dx`: Derivative of the first variable.
#'   - `x_variable`: Name of the first variable.
#'   - `y`: Values of the second variable in each pairwise combination.
#'   - `dy`: Derivative of the second variable.
#'   - `y_variable`: Name of the second variable.
#'   - Additional columns: `condition`, `sample`, and `pseudotime`.
#' @examples
#' # Generate an example dataset for phase space analysis
#' df <- data.frame(
#'   value = c(runif(50, min = 0, max = 10), runif(50, min = 0, max = 10)),
#'   delta = c(rnorm(50, mean = 0.5), rnorm(50, mean = -0.5)),
#'   variable = rep(c("var1", "var2"), each = 50),
#'   condition = rep("cond1", 100),
#'   sample = rep(1:10, each = 10),
#'   pseudotime = rep(seq(1, 10, length.out = 10), 10)
#' )
#'
#' # Generate phase space with default parameters
#' results <- phase_space(df)
#'
#' # Generate phase space with parallel processing on 4 cores
#' results <- phase_space(df, n_cores = 4)
#' @export
#' @import data.table
#' @import dplyr
#' @import parallel
phase_space <- function(df, n_cores = NULL, save = FALSE, focus_variables = NULL) {

  # Set the number of cores for parallel processing
  if (is.null(n_cores)) {
    n_cores <- max(1, detectCores() - 2)
  }

  if (.Platform$OS.type == "windows") {
    # Set up a cluster on Windows for parallel processing, handling warnings and errors
    cl <- tryCatch({
      suppressWarnings(makeCluster(n_cores))
    }, warning = function(w) {
      invokeRestart("muffleWarning") # Ignore warnings
    }, error = function(e) {
      stop("Error in creating cluster: ", e$message) # Stop on error
    })

    on.exit(stopCluster(cl)) # Ensure cluster stops after completion

    # Load required libraries on each cluster worker
    clusterEvalQ(cl, {
      library(dplyr)
      library(data.table)
    })
  }

  # Split data by variable for pairwise combinations
  split_table <- df %>%
    ungroup() %>%
    arrange(variable) %>%
    group_split(variable)

  # Generate all possible pairs of variables, excluding self-pairs
  n_variables <- NROW(split_table)

  combinations <- t(combn(n_variables, 2))
  combinations <- as.data.table(combinations)
  names(combinations) <- c("x", "y")
  combinations <- combinations %>% filter(x != y)

  if(!is.null(focus_variables)){
    x_variable_index <-
      df %>%
      ungroup() %>%
      select(variable) %>%
      distinct() %>%
      arrange(variable) %>%
      mutate(id = 1:n()) %>%
      filter(variable %in% focus_variables) %>%
      pull(id)

    setDT(combinations)
    combinations <- combinations[x %in% x_variable_index | y %in% x_variable_index]

    # Add explicit comparisons within x_variable_index if it contains multiple values
    if (length(x_variable_index) > 1) {
      additional_combinations <- as.data.table(t(combn(x_variable_index, 2)))
      names(additional_combinations) <- c("x", "y")
      combinations <- rbind(combinations, additional_combinations)
    }

    # Ensure x_variable_index values are always in the x column
    combinations <- combinations[, .(
      x = ifelse(x %in% x_variable_index, x, y),
      y = ifelse(x %in% x_variable_index, y, x)
    )]
    combinations <- combinations %>% arrange(x, y)

  }

  if (.Platform$OS.type == "windows") {
    # Export required objects to the cluster
    clusterExport(cl, varlist = c("split_table", "combinations"), envir = environment())
  }

  # Function to calculate phase space for a given pair of variables
  phase_portrait_analysis <- function(i) {
    x_table <- combinations$x[i]
    y_table <- combinations$y[i]

    ref_table <- split_table[[x_table]] %>%
      rename(x = value, dx = delta, x_variable = variable)
    setDT(ref_table)

    qry_table <- split_table[[y_table]] %>%
      rename(y = value, dy = delta, y_variable = variable)
    setDT(qry_table)

    # Merge data for the pair to form phase space
    phase_space <- merge.data.table(ref_table, qry_table, by = c("condition", "sample", "pseudotime"))

    return(phase_space)
  }

  # Apply phase portrait analysis in parallel
  if (.Platform$OS.type == "windows") {
    df_phase_space <- parLapply(cl, 1:NROW(combinations), phase_portrait_analysis)
  } else if(n_cores == 1) {
    df_phase_space <- lapply(1:NROW(combinations), phase_portrait_analysis)
  } else {
    df_phase_space <- mclapply(1:NROW(combinations), phase_portrait_analysis, mc.cores = n_cores)
  }

  # Combine results into a single data frame
  df_phase_space <- rbindlist(df_phase_space)
  df_phase_space[, `:=`(x_label = x_variable, y_label = y_variable)]

  if(is.null(focus_variables)){
    df_phase_space_flipped <-
      df_phase_space %>%
      rename(
        x = y,
        y = x,
        x_variable = y_variable,
        y_variable = x_variable,
        x_label = y_label,
        y_label = x_label,
        dx = dy,
        dy = dx
      )

    df_phase_space <- rbind(df_phase_space, df_phase_space_flipped)
    df_phase_space <- as.data.table(df_phase_space)
  }

  # Optionally save as a compressed CSV file
  if(save) {
    fwrite(df_phase_space, "phase_space.csv.gz")
  }

  return(df_phase_space)
}
