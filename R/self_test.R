#' @name self_test
#' @title Run Self-Test for Phase Portrait Analysis Package
#' @description This function performs a self-test of the phase portrait analysis workflow. It simulates example data, processes it, and runs a comprehensive analysis with specified parameters, saving the resulting plots and data summaries.
#' @details The function uses the Lotka-Volterra model to generate an example dataset, normalizes and computes derivatives on the data, and then performs a complete analysis pipeline including phase portrait and trajectory plotting. The outputs are saved automatically as files for verification.
#' @return No return value. Saves analysis results and plots to the working directory.
#' @examples
#' # Run the self-test to verify package functionality:
#' self_test()
#' @export
self_test <- function(){
  # Generate example dataset by simulating Lotka-Volterra model trajectories
  example_dataset <- rbind(lotka_volterra(), lotka_volterra(alpha = 0.75))

  # Normalize values and calculate derivatives
  d_table <- scale_and_diff(example_dataset)

  # Run the full analysis, saving output and individual plots
  run_analysis(d_table, option = "all", save = TRUE, plot_individual = TRUE, bin_width = 0.1)
}
