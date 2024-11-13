self_test <- function(){
  example_dataset <- rbind(lotka_volterra(), lotka_volterra(alpha = 0.75))
  d_table <- scale_and_diff(example_dataset)
  run_analysis(d_table, option = "all", save = TRUE, plot_individual = T, bin_width = 0.1)
}