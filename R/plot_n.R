#' @name plot_n
#' @title Plot Cell Count (N) in Phase Space
#' This function creates a phase space plot displaying the count (`n`) of observations in each bin. The plot can be generated for either standard or fraction-based input types, and provides options to customize axis limits, enforce a fixed aspect ratio, and save the plot to a file.
#' @param df A data frame containing `x`, `y`, `n`, `condition`, `x_variable`, and `y_variable`.
#' @param bin_width Integer specifying the number of bins to use for x and y axes. Default is 4.
#' @param input A character string specifying the input type, either "standard" or "fraction". Default is "standard".
#' @param min_x, max_x Optional numeric values to set limits for the x-axis.
#' @param min_y, max_y Optional numeric values to set limits for the y-axis.
#' @param save Logical, if TRUE, saves the plot as a PDF file. Default is FALSE.
#' @return A ggplot object showing the cell count (`n`) in phase space.
#' @examples
#' plot_n(df, bin_width = 4, input = "standard", min_x = -10, max_x = 10, min_y = -5, max_y = 5, save = TRUE)
#' @export
#' @import data.table
#' @import dplyr
#' @import ggplot2
#' @import viridis

library(ggplot2)
library(viridis)
library(dplyr)
library(data.table)

plot_n <- function(df, bin_width = 0.25, input = "standard", min_x = NULL, max_x = NULL,
                   min_y = NULL, max_y = NULL, save = FALSE, fix_coord = FALSE) {

  # Override 'input' with df$input[1] if 'input' column exists in df
  if ("input" %in% names(df)) {
    input <- df$input[1]
  }

  # Validate `input` parameter to ensure it contains valid values
  if (!input %in% c("standard", "fraction")) {
    stop("Invalid input: Choose 'standard' or 'fraction'")
  }

  bin_width <- 1/bin_width

  # Initialize the plot with tile and text layers to represent the count `n`
  plot <- df %>%
    ggplot(aes(x, y, color = n, fill = n)) +
    geom_tile() +                     # Tile fill for each bin based on count `n`
    scale_color_viridis(               # Set fill scale for `n`
      option = "rocket",
      direction = -1,
      trans = "log1p",
      labels = scales::label_log(base = 10),
      breaks = scales::trans_breaks("log1p", function(x) exp(x) - 1)  # Custom breaks
    ) +
    scale_fill_viridis(               # Set fill scale for `n`
      option = "rocket",
      direction = -1,
      trans = "log1p",
      labels = scales::label_log(base = 10),
      breaks = scales::trans_breaks("log1p", function(x) exp(x) - 1)  # Custom breaks
    ) +
    facet_grid(~condition) +          # Separate panels by `condition`
    labs(
      title = paste("N in phase space,", input),  # Dynamic title based on `input` type
      x = df$x_label[1],                        # x-axis label
      y = df$y_label[1],                        # y-axis label
      color = "N", fill = "N"                      # Legend labels for color and fill
    ) +
    theme_classic(base_size = 12) +               # Classic theme with base font size
    scale_size(guide = "none") +                  # Hide size legend
    theme(
      legend.position = "bottom",                 # Place legend at the bottom
      legend.key.width = unit(1, "cm"),           # Set legend key width
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)  # Rotate x-axis labels for readability
    )

  if(input == "standard" || fix_coord){
    plot <- plot + coord_fixed()
  }

  # Add optional x-axis limits if specified
  if (!is.null(min_x) && !is.null(max_x)) {
    plot <- plot + scale_x_continuous(limits = c(min_x - 1/(bin_width), max_x + 1/(bin_width)))
  }

  # Add optional y-axis limits if specified
  if (!is.null(min_y) && !is.null(max_y)) {
    plot <- plot + scale_y_continuous(limits = c(min_y - 1/(bin_width), max_y + 1/(bin_width)))
  }

  # Optionally save the plot as a PDF file with filename based on input type and variables
  if (save) {
    ggsave(
      filename = paste0("n_",
                        df$x_variable[1], "_", df$y_variable[1], "_", input,  ".pdf"),
      plot = plot,
      height = 4, width = 6
    )
  }

  return(plot)
}
