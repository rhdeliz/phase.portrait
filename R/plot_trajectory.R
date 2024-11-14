#' @name plot_trajectory
#' @title Plot Trajectory in Phase Space
#' @description This function creates a phase space trajectory plot, visualizing `x` and `y` values along pseudotime. It connects `x1` to `x` and `x` to `x3` as well as `y1` to `y` and `y` to `y3`, representing directional flows with arrows. This enables a clear depiction of how each point in phase space progresses over time.
#' @param df A dataframe containing columns for coordinates (`x`, `y`), pseudotime, and directional endpoints (`x1`, `x3`, `y1`, `y3`). Optionally includes `input` (plot label), `condition` (facet label), `x_variable`, and `y_variable`.
#' @param input Character. Input type for the plot label. Default is "standard". If `df$input` exists, it will override this parameter.
#' @param min_x Numeric. Minimum x-axis limit for the plot. Default is NULL.
#' @param max_x Numeric. Maximum x-axis limit for the plot. Default is NULL.
#' @param min_y Numeric. Minimum y-axis limit for the plot. Default is NULL.
#' @param max_y Numeric. Maximum y-axis limit for the plot. Default is NULL.
#' @param save Logical. If TRUE, saves the plot as a PDF file. Default is FALSE.
#' @details This function creates a phase space trajectory plot where points and paths are visualized with arrows to indicate directionality. It uses pseudotime as a color gradient to show progression through phase space. Faceting by the `condition` column allows for separate plots by specified condition.
#' @return A ggplot object of the trajectory plot, with customizable color and axis limits.
#' @examples
#' # Example usage:
#' df <- data.frame(
#'   x = c(1, 2, 3), y = c(3, 2, 1),
#'   x1 = c(1.1, 2.1, 3.1), x3 = c(0.9, 1.9, 2.9),
#'   y1 = c(3.1, 2.1, 1.1), y3 = c(2.9, 1.9, 0.9),
#'   pseudotime = c(0.1, 0.5, 0.9),
#'   condition = c("A", "B", "A"),
#'   x_variable = "X Axis", y_variable = "Y Axis",
#'   input = "custom_label"
#' )
#' plot_trajectory(df, min_x = 0, max_x = 5, min_y = 0, max_y = 5)
#' @export
#' @import data.table
#' @import ggplot2
#' @import viridis
plot_trajectory <- function(df, input = "standard", min_x = NULL, max_x = NULL,
                            min_y = NULL, max_y = NULL, save = FALSE, fix_coord = FALSE){

  # Override 'input' with df$input[1] if 'input' column exists in df
  if("input" %in% names(df)) {
    input <- df$input[1]
  }

  # Validate input parameters to ensure they contain valid values
  if (!input %in% c("standard", "fraction")) {
    stop("Invalid input: Choose 'standard' or 'fraction'")
  }

  # Build the base plot with pseudotime as color and basic aesthetic mappings
  plot <- ggplot(
    df,
    aes(x = x, y = y, color = pseudotime)
  ) +

    # Segment from x1 to x, and from x to x3 to represent directional flow in x
    geom_segment(aes(x = x1, xend = x3, y = y), linewidth = 0.1) +

    # Segment from y1 to y, and from y to y3 to represent directional flow in y
    geom_segment(aes(x = x, y = y1, yend = y3), linewidth = 0.1) +

    # Draw path and point layers to trace and emphasize trajectory points
    geom_path(linewidth = 0.25) +
    geom_point(size = 0.1) +

    # Use the 'plasma' color palette from viridis for pseudotime progression
    scale_color_viridis(option = "plasma") +

    # Separate plots by the 'condition' variable, if present in df
    facet_grid(~condition) +

    # Add plot title and axis labels based on df's columns
    labs(
      title = paste("Pseudotime in phase space,", input),       # Title with selected input label
      x = df$x_label[1],                                      # Dynamic x-axis label
      y = df$y_label[1],                                      # Dynamic y-axis label
      color = "Pseudotime"                                       # Legend title for pseudotime
    ) +
    # Apply a clean, classic theme
    theme_classic(base_size = 12) +
    theme(
      legend.position = "bottom",                                # Place legend below plot
      legend.key.width = unit(0.75, "cm"),                          # Adjust legend key width
      axis.text.x = element_text(angle = 45, vjust = 1, hjust=1) # Rotate x-axis labels
    )

  if(input == "standard" || fix_coord){
    plot <- plot + coord_fixed()
  }

  # Add x and y axis limits if min/max values are provided
  if(!is.null(min_x) && !is.null(max_x)) {
    plot <- plot + scale_x_continuous(limits = c(min_x, max_x))
  }
  if(!is.null(min_y) && !is.null(max_y)) {
    plot <- plot + scale_y_continuous(limits = c(min_y, max_y))
  }

  # Optionally save plot as a PDF file with dynamic filename based on df's labels
  if(save == TRUE){
    ggsave(
      filename = paste0("trajectory_",
                        df$x_variable[1], "_", df$y_variable[1], "_", input,  ".pdf"),
      plot = plot,
      height = 4, width = 6
    )
  }

  # Return the final plot object
  return(plot)
}
