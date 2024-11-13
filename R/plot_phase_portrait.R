# Required Libraries
library(dplyr)
library(ggplot2)
library(viridis)
library(metR)
#' @name plot_phase_portrait
#' @title Plot Phase Portrait
#' This function generates a phase portrait plot, visualizing the dynamics of two variables (`x` and `y`) with arrows representing the rate of change (`dx` and `dy`). The plot uses color to indicate the magnitude of change and can optionally enforce axis limits, aspect ratios, and save the plot to a file.
#' @param df A data frame containing `x`, `y`, `dx`, `dy`, `condition`, `x_variable`, and `y_variable`.
#' @param input A character string indicating the type of input. Defaults to `"standard"`.
#' @param min_x Numeric. Minimum x-axis limit for the plot. Default is NULL.
#' @param max_x Numeric. Maximum x-axis limit for the plot. Default is NULL.
#' @param min_y Numeric. Minimum y-axis limit for the plot. Default is NULL.
#' @param max_y Numeric. Maximum y-axis limit for the plot. Default is NULL.
#' @param save Logical, if TRUE saves the plot to a PDF file. Default is FALSE.
#' @return A ggplot object showing the phase portrait.
#' @examples
#' plot_phase_portrait(df, input = "standard", min_x = -10, max_x = 10, min_y = -5, max_y = 5, save = TRUE)
#' @export
#' @import data.table
#' @import dplyr
#' @import ggplot2
#' @import viridis
#' @import metR
plot_phase_portrait <- function(df, input = "standard", bin_width = 0.25, min_x = NULL,
                                max_x = NULL, min_y = NULL, max_y = NULL,
                                save = FALSE, fix_coord = FALSE){

  # Override 'input' with df$input[1] if 'input' column exists in df
  if("input" %in% names(df)) {
    input <- df$input[1]
  }

  # Validate input parameters to ensure they contain valid values
  if (!input %in% c("standard", "fraction")) {
    stop("Invalid input: Choose 'standard' or 'fraction'")
  }

  # Calculate normalized color and unit direction for arrows
  df <- df %>%
    mutate(
      color = sqrt(dx^2 + dy^2),   # Calculate the magnitude of change for color scaling
      dx_unit = dx / color * 1,    # Normalize dx for unit arrow length
      dy_unit = dy / color * 1     # Normalize dy for unit arrow length
    )

  arrow_length <- bin_width*5

  # Initialize ggplot with arrows representing phase dynamics
  plot <- ggplot() +
    geom_arrow(                     # Custom arrow geometry for phase portrait
      data = df,
      aes(
        x, y,
        dx = dx_unit, dy = dy_unit, # Use normalized dx and dy
        color = color,
        mag = .5                    # Control arrow size (customize as needed)
      ),
      arrow.length = arrow_length              # Length of arrows (customize as needed)
    ) +
    scale_mag(                      # Scale arrowhead size
      max = 1/(arrow_length/1.5),                    # Big arrow head
      guide = 'none'                # Hide guide for arrow magnitude
    ) +
    scale_color_viridis() +         # Color scale for the magnitude
    facet_grid(~condition) +        # Facet plot by condition
    labs(
      title = paste("Phase portrait,", input),       # Title with input label
      x = df$x_label[1],                          # Label for x-axis
      y = df$y_label[1],                          # Label for y-axis
      color = "Magnitude"                            # Color legend title
    ) +
    theme_classic(base_size = 12) +      # Classic theme with base font size
    theme(
      legend.position = "bottom",                    # Place legend at bottom
      legend.key.width = unit(1, "cm"),              # Width of legend key
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)  # Tilt x-axis text
    )

  if(input == "standard" || fix_coord){
    plot <- plot + coord_fixed()
  }
  # Add optional x and y axis limits
  if(!is.null(min_x) && !is.null(max_x)) {
    plot <- plot + scale_x_continuous(limits = c(min_x, max_x))
  }
  if(!is.null(min_y) && !is.null(max_y)) {
    plot <- plot + scale_y_continuous(limits = c(min_y, max_y))
  }

  # Optionally save the plot as a PDF file with filename based on input type and variables
  if (save) {
    ggsave(
      filename = paste0("phase_portrait_",
                        df$x_variable[1], "_", df$y_variable[1], "_", input,  ".pdf"),
      plot = plot,
      height = 4, width = 6
    )
  }

  return(plot)
}
