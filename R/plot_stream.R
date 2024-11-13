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
                        save = FALSE, fix_coord = FALSE, res = 25){

  # Override 'input' with df$input[1] if 'input' column exists in df
  if("input" %in% names(df)) {
    input <- df$input[1]
  }

  # Validate input to ensure it contains valid values
  if (!input %in% c("standard", "fraction")) {
    stop("Invalid input: Choose 'standard' or 'fraction'")
  }

  if(is.null(min_x)){min_x <- min(df$x)}
  if(is.null(max_x)){max_x <- max(df$x)}
  if(is.null(min_y)){min_y <- min(df$y)}
  if(is.null(max_y)){max_y <- max(df$y)}

  # Define the binning scale for the grid with integer boundaries
  scaled_min_x <- floor(min_x * bin_width)   # Rounds down to the nearest integer
  scaled_min_x <- as.integer(scaled_min_x)

  scaled_max_x <- ceiling(max_x * bin_width) # Rounds up to the nearest integer
  scaled_max_x <- as.integer(scaled_max_x)

  scaled_min_y <- floor(min_y * bin_width)   # Rounds down to the nearest integer
  scaled_min_y <- as.integer(scaled_min_y)

  scaled_max_y <- ceiling(max_y * bin_width) # Rounds up to the nearest integer
  scaled_max_y <- as.integer(scaled_max_y)

  # Create the grid using the actual bin size
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

  # Round coordinates in df to the nearest bin center based on calculated bin size
  df <- df %>%
    mutate(
      x = round(x / bin_width) * bin_width,
      y = round(y / bin_width) * bin_width
    )

  # Merge the data with the grid, and calculate the color based on vector length
  result <- grid %>%
    left_join(df, by = c("x", "y", "input", "condition",
                         "x_variable", "y_variable",
                         "x_label", "y_label",
                         "dx_label", "dy_label")) %>%
    mutate(color = sqrt(dx^2 + dy^2))

  # Arrange and finalize result
  df <- result %>%
    group_by(input, condition) %>%
    arrange(x, y)

  df <-
    df %>%
    group_by(input, condition,
             x_variable, y_variable,
             x_label, y_label,
             x, y,
             dx_label, dy_label) %>%
    summarize(
      dx = median(dx, na.rm = TRUE),
      dy = median(dy, na.rm = TRUE)
    )

  # Replace NA values with 0 for better plotting
  df[is.na(df)] <- 0

  # Initialize ggplot with streamline layer to show phase dynamics
  plot <-
    ggplot() +
    geom_streamline(
      data = df,
      aes(
        x, y,
        dx = dx, dy = dy
      ),
      n = 10,              # Number of streamlines
      L = bin_width*5,               # Length of streamlines
      res = res,
      linewidth = 0.1,
      color = "grey"
    ) +
    geom_streamline(
      data = df,
      aes(
        x, y,
        dx = dx, dy = dy
      ),
      n = 10,              # Number of streamlines
      L = bin_width*2,               # Length of streamlines
      res = res,
      linewidth = 0.1
    ) +
    scale_color_viridis() +
    facet_grid(~condition) +
    labs(
      title = paste("Phase portrait stream,", df$input[1]),  # Title with input label
      x = paste0(df$x_label[1], "\n\n\n\n"),                  # X-axis label
      y = df$y_label[1],                                      # Y-axis label
      color = "Magnitude"                                     # Legend title for color
    ) +
    theme_classic(base_size = 12) +
    theme(
      legend.position = "bottom",                             # Position legend at bottom
      legend.key.width = unit(1, "cm"),                       # Width of legend key
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)  # Rotate x-axis text
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

  # Optionally save plot as a PDF with dynamic filename
  if(save) {
    ggsave(
      filename = paste0("stream_",
                        df$x_variable[1], "_", df$y_variable[1], "_", input,  ".pdf"),
      plot = plot,
      height = 4, width = 6
    )
  }

  return(plot)
}
