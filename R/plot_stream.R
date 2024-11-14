#' @name plot_stream
#' @title Plot Phase Streamline
#' This function creates a phase streamline plot, visualizing the dynamics of two variables (`x` and `y`) across conditions. It supports both standard and fraction-based input types, with options for binning, custom axis limits, fixed aspect ratio, and saving the plot.
#' @param df A data frame containing columns `x`, `y`, `dx`, `dy`, `condition`, `x_variable`, and `y_variable`.
#' @param input A character string indicating the type of input, either `"standard"` or `"fraction"`. Default is `"standard"`.
#' @param bin_width Integer specifying the number of bins for discretizing `x` and `y`. Default is 0.25.
#' @param min_x Numeric. Minimum x-axis limit for the plot. Default is NULL.
#' @param max_x Numeric. Maximum x-axis limit for the plot. Default is NULL.
#' @param min_y Numeric. Minimum y-axis limit for the plot. Default is NULL.
#' @param max_y Numeric. Maximum y-axis limit for the plot. Default is NULL.
#' @param save Logical, if TRUE, saves the plot as a PDF file. Default is FALSE.
#' @return A ggplot object showing the phase streamline.
#' @examples
#' plot_stream(df, input = "standard", bin_width = 4, min_x = 0, max_x = 1, min_y = 0, max_y = 1, save = TRUE)
#' @export
#' @import data.table
#' @import dplyr
#' @import ggplot2
#' @import viridis
#' @import metR

plot_stream <- function(df, input = "standard", bin_width = 0.25,
                        min_x = NULL, max_x = NULL, min_y = NULL, max_y = NULL,
                        save = FALSE, fix_coord = FALSE, res = 25) {

  # Override 'input' with the first value in df$input if 'input' column exists in df
  if("input" %in% names(df)) {
    input <- df$input[1]
  }

  # Validate input type to ensure it is either "standard" or "fraction"
  if (!input %in% c("standard", "fraction")) {
    stop("Invalid input: Choose 'standard' or 'fraction'")
  }

  # Set default axis limits based on data range if not provided
  if(is.null(min_x)){min_x <- min(df$x)}
  if(is.null(max_x)){max_x <- max(df$x)}
  if(is.null(min_y)){min_y <- min(df$y)}
  if(is.null(max_y)){max_y <- max(df$y)}

  # Define integer boundaries for binning scale, using floor/ceiling functions
  scaled_min_x <- as.integer(floor(min_x * bin_width))
  scaled_max_x <- as.integer(ceiling(max_x * bin_width))
  scaled_min_y <- as.integer(floor(min_y * bin_width))
  scaled_max_y <- as.integer(ceiling(max_y * bin_width))

  # Create a grid of coordinates within the scaled min and max boundaries
  grid <- expand.grid(
    x = seq(scaled_min_x, scaled_max_x, by = bin_width),
    y = seq(scaled_min_y, scaled_max_y, by = bin_width),
    input = unique(df$input),
    condition = unique(df$condition),
    x_variable = unique(df$x_variable),
    y_variable = unique(df$y_variable),
    x_label = unique(df$x_label),
    y_label = unique(df$y_label),
    dx_label = unique(df$dx_label),
    dy_label = unique(df$dy_label)
  )

  # Round data points in df to nearest bin center for x and y values
  df <- df %>%
    mutate(
      x = round(x / bin_width) * bin_width,
      y = round(y / bin_width) * bin_width
    )

  # Merge data with grid and calculate magnitude of the vector (color)
  result <- grid %>%
    left_join(df, by = c("x", "y", "input", "condition",
                         "x_variable", "y_variable",
                         "x_label", "y_label",
                         "dx_label", "dy_label")) %>%
    mutate(color = sqrt(dx^2 + dy^2)) # Calculate vector magnitude for coloring

  # Organize data for plotting, and calculate medians for dx and dy
  df <- result %>%
    group_by(input, condition, x_variable, y_variable, x_label, y_label, x, y, dx_label, dy_label) %>%
    summarize(
      dx = median(dx, na.rm = TRUE),
      dy = median(dy, na.rm = TRUE)
    )

  # Replace any NA values with 0
  df[is.na(df)] <- 0

  # Initialize ggplot with streamline layers
  plot <-
    ggplot(
      df, aes(x, y, dx = dx, dy = dy)
    ) +
    # geom_raster(
    #   aes(fill = sqrt(dx^2 + dy^2))
    # ) +
    geom_streamline(
      n = 10,              # Number of streamlines
      L = bin_width * 5,   # Length of streamlines
      res = res,
      linewidth = 0.1,
      color = "lightgrey"
    ) +
    geom_streamline(
      n = 10,
      L = bin_width,
      res = res,
      linewidth = 0.1
    ) +
    scale_fill_viridis() +
    scale_color_viridis() +
    facet_grid(~condition) +
    labs(
      title = paste("Phase portrait stream,", df$input[1]),  # Add title with input label
      x = paste0(df$x_label[1], "\n\n\n\n"),                  # Add x-axis label
      y = df$y_label[1],                                      # Add y-axis label
      color = "Magnitude"                                     # Color legend title
    ) +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "bottom",                             # Place legend at bottom
      legend.key.width = unit(0.75, "cm"),                       # Legend key width
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)  # Rotate x-axis labels
    )

  # Add a fixed coordinate ratio if requested
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

  # Optionally save plot as a PDF
  if(save) {
    ggsave(
      filename = paste0("stream_", df$x_variable[1], "_", df$y_variable[1], "_", input, ".pdf"),
      plot = plot,
      height = 4, width = 6
    )
  }

  return(plot)
}
