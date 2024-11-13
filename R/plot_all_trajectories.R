#' @name plot_all_trajectories
#' @title Plot All Trajectories in Phase Space
#' @description This function creates a phase space plot of all trajectories by visualizing `x` and `y` values across pseudotime. The plot can handle both standard and fraction-based input types, and provides options to customize axis limits, fix aspect ratio, and save the plot to a file.
#' @param df A data frame containing `x`, `y`, `pseudotime`, `sample`, `condition`, `x_variable`, and `y_variable`.
#' @param input A character string specifying the input type: either `"standard"` or `"fraction"`. Default is `"standard"`.
#' @param min_x Numeric. Minimum x-axis limit for the plot. Default is NULL.
#' @param max_x Numeric. Maximum x-axis limit for the plot. Default is NULL.
#' @param min_y Numeric. Minimum y-axis limit for the plot. Default is NULL.
#' @param max_y Numeric. Maximum y-axis limit for the plot. Default is NULL.
#' @param save Logical, if TRUE, saves the plot as a PDF file. Default is FALSE.
#' @param fix_coord Logical, if TRUE enforces a fixed aspect ratio. Default is FALSE.
#' @return A ggplot object of the phase space trajectories.
#' @examples
#' plot_all_trajectories(df, input = "standard", min_x = -10, max_x = 10, min_y = -5, max_y = 5, save = TRUE)
#' @export
#' @import data.table
#' @import dplyr
#' @import ggplot2
#' @import viridis
library(ggplot2)
library(viridis)
library(dplyr)
library(data.table)

plot_all_trajectories <- function(df, input = "standard", min_x = NULL, max_x = NULL,
                                  min_y = NULL, max_y = NULL, save = FALSE, fix_coord = FALSE){

  # Override 'input' with df$input[1] if 'input' column exists in df
  if("input" %in% names(df)) {
    input <- df$input[1]
  }

  # Validate `input` parameter to ensure it contains valid values
  if (!input %in% c("standard", "fraction")) {
    stop("Invalid input: Choose 'standard' or 'fraction'")
  }

  # Set axis labels based on input type
  if (input == "standard") {
    x_label <- df$x_variable[1]  # Use standard label for `x`
    y_label <- df$y_variable[1]  # Use standard label for `y`
  } else {  # Fractional representation
    df <- df %>%
      group_by(x_variable, y_variable, condition, pseudotime) %>%
      mutate(
        y = x + y,                     # Set `y` as the total size
        x = x / y                      # Calculate `x` as a fraction of the total size
      ) %>%
      ungroup() %>%
      mutate(
        y = y / 2
      ) %>%
      as.data.table()

    # Labels for fractional x and cumulative y
    x_label <- paste0(df$x_variable[1], " pct")
    y_label <- paste0(df$x_variable[1], " + ", df$y_variable[1])
  }

  # Initialize the plot with point and path layers, using pseudotime as color scale
  plot <- ggplot(df, aes(x = x, y = y, color = pseudotime, group = sample)) +
    geom_path(linewidth = 0.1) +                             # Connect points with lines
    scale_color_viridis(option = "plasma") +             # Color scale for pseudotime
    facet_grid(~condition) +                             # Separate plot panels by condition
    labs(
      title = paste("Pseudotime in phase space, all", input),  # Title with input type
      x = x_label,                                       # x-axis label
      y = y_label,                                       # y-axis label
      color = "Pseudotime"                               # Legend label for color
    ) +
    theme_classic(base_size = 12) +                      # Classic theme
    theme(
      legend.position = "bottom",                        # Position legend at bottom
      legend.key.width = unit(1, "cm"),                  # Width of legend key
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)  # Rotate x-axis text for readability
    )

  if(input == "standard" || fix_coord){
    plot <- plot + coord_fixed()
  }
  # Apply optional x-axis limits
  if(!is.null(min_x) && !is.null(max_x)) {
    plot <- plot + scale_x_continuous(limits = c(min_x, max_x))
  }

  # Apply optional y-axis limits
  if(!is.null(min_y) && !is.null(max_y)) {
    plot <- plot + scale_y_continuous(limits = c(min_y, max_y))
  }

  # Optionally save the plot as a PDF file with a filename based on the input type and variable names
  if (save) {
    ggsave(
      filename = paste0("all_trajectories_",
                        df$x_variable[1], "_", df$y_variable[1], "_", input,  ".pdf"),
      plot = plot,
      height = 4, width = 6
    )
  }

  return(plot)
}
