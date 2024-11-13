#' @name plot_phase_line
#' @title Plot Phase Lines in Phase Space
#' @description This function generates a plot of phase lines in phase space, showing changes in `x` and `y` values (change rate) along with the grouping variable. It supports both standard and fraction-based input types and provides options to adjust axis limits, customize the plot's color scale, and save the plot as a PDF.
#' @param df A data frame containing columns `x`, `y`, `n`, `group_var`, `condition`, `facet`, `x_variable`, and `y_variable`.
#' @param input Character string specifying the input type, either "standard" or "fraction". Default is "standard".
#' @param min_x, max_x Optional numeric values to set limits for the x-axis.
#' @param min_y, max_y Optional numeric values to set limits for the y-axis.
#' @param save Logical, if TRUE, saves the plot as a PDF file. Default is FALSE.
#' @return A ggplot object representing phase lines in phase space.
#' @examples
#' plot_phase_line(df, input = "standard", min_x = -10, max_x = 10, min_y = -5, max_y = 5, save = TRUE)
#' @export
#' @import data.table
#' @import dplyr
#' @import ggplot2
#' @import viridis
plot_phase_line <- function(df, input = "standard", min_x = NULL, max_x = NULL,
                            min_y = NULL, max_y = NULL, save = FALSE, fix_coord = FALSE) {

  # Override 'input' if 'input' column exists in df
  if("input" %in% names(df)) {
    input <- df$input[1]
  }

  # Validate the `input` parameter
  if (!input %in% c("standard", "fraction")) {
    stop("Invalid input: Choose 'standard' or 'fraction'")
  }

  # Initialize plot with horizontal line at y = 0 for reference
  plot <-
    df %>%
    arrange(
      x, y
    ) %>%
    mutate(
      n = log(n + 1)
    ) %>%
    ggplot() +
    geom_hline(yintercept = 0) +
    geom_point(aes(x = x, y = y, size = n, color = group_var, group = group_var)) + # Add points
    geom_path(aes(x = x, y = y, color = group_var, group = group_var)) + # Add paths
    scale_size(range = c(0.01, 5)) +
    scale_color_distiller(palette = "RdBu", na.value = "black") + # Use RdBu color scale
    labs(
      title = paste("Phase lines,", input),  # Title with input type
      x = "Bin",  # x-axis label
      y = "Change Rate",  # y-axis label
      color = "Cut",  # Legend label for color
      size = "log(N+1)"
    ) +
    facet_grid(condition ~ facet, scales = "free") + # Separate panels by condition and facet
    theme_classic(base_size = 12) + # Classic theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) # Rotate x-axis text

  # Add optional x-axis limits
  if (!is.null(min_x) && !is.null(max_x) && !any(is.na(c(min_x, max_x)))) {
    plot <- plot + scale_x_continuous(limits = c(min_x, max_x))
  }

  # Add optional y-axis limits
  if (!is.null(min_y) && !is.null(max_y) && !any(is.na(c(min_y, max_y)))) {
    plot <- plot + scale_y_continuous(limits = c(min_y, max_y))
  }

  # Optionally save plot as PDF
  if (save) {
    ggsave(
      filename = paste0("phase_line_",
                        df$x_variable[1], "_", df$y_variable[1], "_", input,  ".pdf"),
      plot = plot,
      height = 4, width = 6
    )
  }

  return(plot)
}
