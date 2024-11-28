#' @name run_analysis
#' @title Run Analysis and Generate Phase Portrait Plots
#' This function performs an in-depth analysis on a dataset to produce various visualizations, including phase portraits, trajectory plots, and other dynamic plots in phase space. Users can specify different analysis options (standard or fraction-based), save individual or combined plot outputs, and control the granularity of binning and parallel processing.
#' @param df A data frame containing the dataset with columns `x`, `y`, `dx`, `dy`, `x_variable`, `y_variable`.
#' @param option A character string specifying the analysis type. Options include "all" (default), "standard", or "fraction".
#' @param bin_width Integer specifying the number of bins to discretize `x` and `y` in phase line calculations. Default is 4.
#' @param min_bin_n Integer specifying the minimum number of observations required in each bin. Default is 1.
#' @param save_tables Logical. If TRUE, saves intermediary data tables generated during analysis. Default is FALSE.
#' @param plot_individual Logical. If TRUE, saves each individual plot generated during the analysis. Default is FALSE.
#' @param plot_combined Logical. If TRUE, saves combined plots into one file for easy comparison. Default is TRUE.
#' @param n_cores Integer specifying the number of cores to use for parallel processing. Default is NULL (auto-detect).
#' @param phase_line_tables Character string specifying phase line table output format: "binned", "average", or "all". Default is "all".
#' @param focus_variables Character representing the x-axis variable. Default is NULL
#'
#' @details
#' The `run_analysis` function processes and visualizes phase space data comprehensively, offering flexibility for standard and fraction-based representations. Customizable options include binning, minimum observations per bin, and parallel processing. Users can save intermediary data tables, individual plots, or combined plots as needed. The main components of this function include:
#'
#' - **Phase Space Calculation**: Calls `phase_space()` to compute the core phase space dataset, using parallel processing if specified.
#' - **Data Preparation for Plotting**: Computes additional summary tables, including `pseudotime_trajectory()`, `phase_portrait()`, and `phase_lines()` based on the selected `option`. Standard and fraction-based versions are available, providing an in-depth view of phase space dynamics.
#' - **Plot Generation**: Generates various types of plots to visualize dynamics, including:
#'     - `trajectory_plots`: Shows pseudotime trajectories in phase space.
#'     - `all_trajectory_plots`: Displays all sample trajectories over pseudotime.
#'     - `stream_plots`: Shows dynamic phase portrait streams, illustrating variable changes over time.
#'     - `phase_portrait_plots`: Presents a static view of the phase portrait, summarizing variable interactions.
#'     - `phase_line_plots`: Visualizes phase lines across binned data, highlighting directional trends.
#'     - `n_plots`: Displays observation counts within each phase space bin.
#' - **Fraction-Based Plots**: Mirrors the standard plots but uses a fraction-based data representation, with plot types prefixed by `fract_`.
#' - **Plot Merging**: If `plot_combined` is TRUE, combines selected plots into a single layout for easier comparison, saved as a single PDF file.
#' - **Error Handling**: Each data processing and plotting step includes error handling, ensuring that issues are reported without stopping the entire workflow.
#' - **Parallel Processing**: Supports parallel processing with `n_cores` to optimize performance for large datasets.
#'
#' This function is ideal for exploratory data analysis and visual interpretation of phase space dynamics, particularly for examining complex systems over time. Enabling `save_tables` or `plot_individual` can produce multiple file outputs for detailed analysis, while combined plots are saved as a single PDF for convenience.
#'
#' @return This function saves the generated plots and does not return a value.
#'
#' @examples
#' # Example usage:
#' df <- data.frame(x = rnorm(100), y = rnorm(100), dx = rnorm(100), dy = rnorm(100),
#'                  x_variable = "X Axis", y_variable = "Y Axis")
#' run_analysis(df, option = "all", bin_width = 4, min_bin_n = 1, save_tables = TRUE, plot_individual = TRUE)
#' @export
#' @import data.table
#' @import dplyr
#' @import gridExtra
#' @import ggplot2
#' @import Cairo
#' @import grid
run_analysis <- function(df, bin_width = 0.25, min_bin_n = 1,
                         option = "all", phase_line_tables = "all",
                         save_tables = TRUE, plot_individual = TRUE,
                         plot_combined = TRUE, n_cores = NULL,
                         stream_resolution = 25, focus_variables = NULL) {

  original_option <- option  # Store the original option for later use

  # Set the number of cores for parallel processing
  if (is.null(n_cores) & .Platform$OS.type != "windows") {
    n_cores <- max(1, detectCores() - 2)
  } else{
    n_cores = 1
  }

  # Validate phase_line_tables argument
  if (!phase_line_tables %in% c("binned", "average", "all")) {
    stop("Invalid phase_line_tables type: Choose 'binned', 'average', or 'all'")
  }

  # Define lists for different types of plot options
  standard_list <- c("trajectory_plots", "all_trajectory_plots",
                     "stream_plots", "phase_portrait_plots",
                     "phase_line_plots", "n_plots")

  fraction_list <- paste0("fract_", standard_list)

  # Adjust the plot options based on the chosen option
  if (original_option == "all") {
    option <- c(standard_list, fraction_list)
  } else if (original_option == "standard") {
    option <- standard_list
  } else if (original_option == "fraction") {
    option <- fraction_list
  }

  # Generate the phase space table
  phase_space_dt <- tryCatch({
    result <- phase_space(df, n_cores = n_cores, save = save_tables,
                          focus_variables = focus_variables)
    print("Calculated phase_space")
    result  # Return the result
  }, error = function(e) {
    cat("Error in generating phase space table:", e$message, "\n")
    return(NULL)
  })

  pseudotime_trajectory_dt <-
    if ("trajectory_plots" %in% option || "fract_trajectory_plots" %in% option) {
      tryCatch({
        result <- pseudotime_trajectory(phase_space_dt, save = save_tables)
        print("Calculated pseudotime_trajectory_dt")
        result  # Return the result of pseudotime_trajectory
      }, error = function(e) {
        cat("Error in calculating pseudotime trajectory:", e$message, "\n")
        return(NULL)
      })
    } else NULL

  phase_portrait_dt <-
    if ("stream_plots" %in% option || "phase_portrait_plots" %in% option || "phase_line_plots" %in% option || "n_plots" %in% option) {
      tryCatch({
        result <- phase_portrait(phase_space_dt, bin_width = bin_width,
                                 min_bin_n = min_bin_n, save = save_tables)
        print("Calculated phase_portrait_dt")
        result  # Return the result
      }, error = function(e) {
        cat("Error in calculating phase portrait:", e$message, "\n")
        return(NULL)
      })
    } else {
      NULL
    }
  phase_lines_dt <- if ("phase_line_plots" %in% option) {
    tryCatch({
      result <- phase_lines(phase_portrait_dt, bin_width = bin_width,
                            min_bin_n = min_bin_n, output = phase_line_tables,
                            save = save_tables)
      print("Calculated phase_lines_dt")
      result
    }, error = function(e) {
      cat("Error in calculating phase lines:", e$message, "\n")
      return(NULL)
    })
  } else NULL

  fract_pseudotime_trajectory_dt <- if ("fract_trajectory_plots" %in% option) {
    tryCatch({
      result <- pseudotime_trajectory(phase_space_dt, save = save_tables, input = "fraction")
      print("Calculated fract_pseudotime_trajectory_dt")
      result
    }, error = function(e) {
      cat("Error in calculating fractional pseudotime trajectory:", e$message, "\n")
      return(NULL)
    })
  } else NULL

  fract_phase_portrait_dt <-
    if ("fract_stream_plots" %in% option || "fract_phase_portrait_plots" %in% option || "fract_phase_line_plots" %in% option || "fract_n_plots" %in% option) {
      tryCatch({
        result <- phase_portrait(phase_space_dt, bin_width = bin_width,
                                 min_bin_n = min_bin_n, save = save_tables,
                                 input = "fraction")
        print("Calculated fract_phase_portrait_dt")
        result
      }, error = function(e) {
        cat("Error in calculating fractional phase portrait:", e$message, "\n")
        return(NULL)
      })
    } else NULL

  fract_lines_dt <- if ("fract_phase_line_plots" %in% option) {
    tryCatch({
      result <- phase_lines(fract_phase_portrait_dt, bin_width = bin_width,
                            min_bin_n = min_bin_n, input = "fraction",
                            output = phase_line_tables, save = save_tables)
      print("Calculated fract_lines_dt")
      result
    }, error = function(e) {
      cat("Error in calculating fractional phase lines:", e$message, "\n")
      return(NULL)
    })
  } else NULL

  delta_limits <- if (!is.null(phase_lines_dt) || !is.null(fract_lines_dt)) {
    tryCatch({
      all_phase_lines <- rbind(phase_lines_dt, fract_lines_dt, fill = TRUE)
      range <- range(all_phase_lines$dy, na.rm = TRUE)
      print("Calculated delta_limits")
      range
    }, error = function(e) {
      cat("Error in calculating delta limits:", e$message, "\n")
      return(NULL)
    })
  } else NULL

  phase_space_dt <-
    phase_space_dt %>%
    arrange(
      x_variable, y_variable
    )

  variable_list <-
    phase_space_dt %>%
    ungroup() %>%
    select(
      x_variable, y_variable
    ) %>%
    distinct() %>%
    rename(
      x = x_variable,
      y = y_variable
    )

  # Decrease paralellization if too many plots
  n_plots <- NROW(variable_list)
  if(n_plots > 500){
    n_cores <- max(1, round(n_plots/150/n_cores))
  }

  # Plot generation code blocks follow the same pattern
  trajectory_plots <- if ("trajectory_plots" %in% option) {
    tryCatch({
      result <- iterate_plotting(pseudotime_trajectory_dt, plot_trajectory,
                                 n_cores = n_cores,
                                 save = plot_individual)
      print("Calculated trajectory_plots")
      result
    }, error = function(e) {
      cat("Error in generating trajectory plots:", e$message, "\n")
      return(NULL)
    })
  } else NULL

  all_trajectory_plots <- if ("all_trajectory_plots" %in% option) {
    tryCatch({
      result <- iterate_plotting(phase_space_dt, plot_all_trajectories,
                                 n_cores = n_cores,
                                 save = plot_individual)
      print("Calculated all_trajectory_plots")
      result  # Return result
    }, error = function(e) {
      cat("Error in generating all trajectory plots:", e$message, "\n")
      return(NULL)
    })
  } else NULL

  stream_plots <- if ("stream_plots" %in% option) {
    tryCatch({
      result <- iterate_plotting(phase_portrait_dt, plot_stream,
                                 bin_width = bin_width, save = plot_individual,
                                 n_cores = n_cores,
                                 res = stream_resolution)
      print("Calculated stream_plots")
      result  # Return result
    }, error = function(e) {
      cat("Error in generating stream plots:", e$message, "\n")
      return(NULL)
    })
  } else NULL

  phase_portrait_plots <- if ("phase_portrait_plots" %in% option) {
    tryCatch({
      result <- iterate_plotting(phase_portrait_dt, plot_phase_portrait,
                                 bin_width = bin_width,
                                 n_cores = n_cores,
                                 save = plot_individual)
      print("Calculated phase_portrait_plots")
      result
    }, error = function(e) {
      cat("Error in generating phase portrait plots:", e$message, "\n")
    })
  }

  phase_line_plots <- if ("phase_line_plots" %in% option && !is.null(delta_limits)) {
    tryCatch({
      result <- iterate_plotting(phase_lines_dt, plot_phase_line,
                                 min_y = delta_limits[1], max_y = delta_limits[2],
                                 n_cores = n_cores,
                                 save = plot_individual)
      print("Calculated phase_line_plots")
      result
    }, error = function(e) {
      cat("Error in generating phase line plots:", e$message, "\n")
    })
  }

  n_plots <- if ("n_plots" %in% option) {
    tryCatch({
      result <- iterate_plotting(phase_portrait_dt, plot_n, bin_width = bin_width,
                                 n_cores = n_cores,
                                 save = plot_individual)
      print("Calculated n_plots")
      result
    }, error = function(e) {
      cat("Error in generating n plots:", e$message, "\n")
    })
  }

  # Fraction-based options
  fract_trajectory_plots <- if ("fract_trajectory_plots" %in% option) {
    tryCatch({
      result <- iterate_plotting(fract_pseudotime_trajectory_dt, plot_trajectory,
                                 input = "fraction", save = plot_individual,
                                 n_cores = n_cores,
                                 fixed_axes = TRUE, fix_coord = TRUE)
      print("Calculated fract_trajectory_plots")
      result
    }, error = function(e) {
      cat("Error in generating fractional trajectory plots:", e$message, "\n")
    })
  }

  fract_all_trajectory_plots <- if ("fract_all_trajectory_plots" %in% option) {
    tryCatch({
      result <- iterate_plotting(phase_space_dt, plot_all_trajectories,
                                 input = "fraction",
                                 fixed_axes = TRUE, fix_coord = TRUE,
                                 n_cores = n_cores,
                                 save = plot_individual)
      print("Calculated fract_all_trajectory_plots")
      result
    }, error = function(e) {
      cat("Error in generating fractional all trajectory plots:", e$message, "\n")
    })
  }

  fract_stream_plots <- if ("fract_stream_plots" %in% option) {
    tryCatch({
      result <- iterate_plotting(fract_phase_portrait_dt, plot_stream,
                                 fixed_axes = TRUE, fix_coord = TRUE,
                                 save = plot_individual,
                                 n_cores = n_cores,
                                 res = stream_resolution)
      print("Calculated fract_stream_plots")
      result
    }, error = function(e) {
      cat("Error in generating fractional stream plots:", e$message, "\n")
    })
  }

  fract_phase_portrait_plots <- if ("fract_phase_portrait_plots" %in% option) {
    tryCatch({
      result <- iterate_plotting(fract_phase_portrait_dt, plot_phase_portrait,
                                 bin_width = bin_width,
                                 fixed_axes = TRUE, fix_coord = TRUE,
                                 n_cores = n_cores,
                                 save = plot_individual)
      print("Calculated fract_phase_portrait_plots")
      result
    }, error = function(e) {
      cat("Error in generating fractional phase portrait plots:", e$message, "\n")
    })
  }

  fract_phase_line_plots <- if ("fract_phase_line_plots" %in% option && !is.null(delta_limits)) {
    tryCatch({
      result <- iterate_plotting(fract_lines_dt, plot_phase_line,
                                 min_y = delta_limits[1], max_y = delta_limits[2],
                                 fixed_axes = TRUE, fix_coord = TRUE,
                                 n_cores = n_cores,
                                 save = plot_individual)
      print("Calculated fract_phase_line_plots")
      result
    }, error = function(e) {
      cat("Error in generating fractional phase line plots:", e$message, "\n")
    })
  }

  fract_n_plots <- if ("fract_n_plots" %in% option) {
    tryCatch({
      result <- iterate_plotting(fract_phase_portrait_dt, plot_n,
                                 bin_width = bin_width, save = plot_individual,
                                 fixed_axes = TRUE, fix_coord = TRUE,
                                 n_cores = n_cores,
                                 input = "fraction")
      print("Calculated fract_n_plots")
      result
    }, error = function(e) {
      cat("Error in generating fractional n plots:", e$message, "\n")
    })
  }

  # Define save_combined_plot globally to avoid redundant function creation
  save_combined_plot <- function(plot_grob, filename, height, width) {
    # Use Cairo PDF device for better compatibility with parallel processing
    CairoPDF(file = filename, height = height, width = width)

    # Draw the combined grob directly onto the device
    grid::grid.draw(plot_grob)

    # Close the PDF device
    dev.off()
  }

  # Define merge_plots function
  merge_plots <- function(i, option) {
    # Define plot elements based on option
    plot_elements <- switch(option,
                            "all" = list(
                              all_trajectory_plots[[i]], trajectory_plots[[i]], n_plots[[i]], phase_portrait_plots[[i]],
                              stream_plots[[i]], phase_line_plots[[i]],
                              fract_all_trajectory_plots[[i]], fract_trajectory_plots[[i]],
                              fract_n_plots[[i]], fract_phase_portrait_plots[[i]], fract_stream_plots[[i]], fract_phase_line_plots[[i]]
                            ),
                            "standard" = list(
                              all_trajectory_plots[[i]], trajectory_plots[[i]], n_plots[[i]], phase_portrait_plots[[i]],
                              stream_plots[[i]], phase_line_plots[[i]]
                            ),
                            "fraction" = list(
                              fract_all_trajectory_plots[[i]], fract_trajectory_plots[[i]], fract_n_plots[[i]],
                              fract_phase_portrait_plots[[i]], fract_stream_plots[[i]], fract_phase_line_plots[[i]]
                            ),
                            stop("Invalid option")
    )

    # Create your specific layout matrix
    if(option == "all"){
      layout_matrix <- rbind(
        c(1, 2, 3, 4, 5),
        c(6, 6, 6, 6, 6),
        c(7, 8, 9, 10, 11),
        c(12, 12, 12, 12, 12)
      )
    } else{
      layout_matrix <- rbind(
        c(1, 2, 3, 4, 5),
        c(6, 6, 6, 6, 6)
      )
    }


    # Create the arranged grob without plotting
    combined_grob <- arrangeGrob(
      grobs = plot_elements,
      layout_matrix = layout_matrix
    )

    # Save the plot to PDF without front-end rendering
    filename <- paste0(variable_list$x[i], " ", variable_list$y[i], "_combined_plot.pdf")
    height <- if (option == "all") 9 * 2 else 9
    width <- 16  # Adjust as needed

    save_combined_plot(combined_grob, filename, height, width)

    # Clean up to free memory
    rm(plot_elements, combined_grob)
    dev.off()
    gc()

    return(NULL)
  }

  # Set cores to one for plot merging
  n_cores = 1

  # Apply merge function if combined plotting is requested
  # Pending fix
  if (plot_combined & n_cores > 1 & original_option %in% c("all", "standard", "fraction")) {
    tryCatch({
      mclapply(1:NROW(variable_list), function(i) merge_plots(i, original_option), mc.cores = n_cores)
    }, error = function(e) {
      cat("Error in merging and saving combined plots:", e$message, "\n")
    })
  }

  if(plot_combined & n_cores == 1 & original_option %in% c("all", "standard", "fraction")){
    tryCatch({
      lapply(1:NROW(variable_list), function(i) merge_plots(i, original_option))
    }, error = function(e) {
      cat("Error in merging and saving combined plots:", e$message, "\n")
    })
  }
  gc()
}

