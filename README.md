```markdown
# phase.portrait

<!-- badges: start -->
<!-- badges: end -->

**phase.portrait** is a comprehensive toolkit designed to facilitate the analysis and visualization of phase space dynamics. This package is ideal for exploring complex systems through phase portraits, trajectories, and streamlines, and it provides flexible options for both standard and fraction-based representations of data. With **phase.portrait**, you can create detailed phase plots, analyze variable relationships over pseudotime, and produce insightful visual summaries through customizable options for binning, parallel processing, and saving.

## Installation

To install the development version of **phase.portrait** from GitHub, use the following command:

```r
# Install from GitHub
# Uncomment and run the following line if using devtools:
# devtools::install_github("rhdeliz/phase.portrait")
```

## Overview

The `run_analysis` function is the primary function in **phase.portrait**. It allows users to perform a complete analysis on a dataset, generating phase portraits, trajectory plots, streamlines, and other visualizations based on user-defined parameters.

Main features include:
- **Phase Space Calculation**: Calculate and visualize phase spaces with support for parallel processing.
- **Flexible Plotting Options**: Create standard and fraction-based plots with options for binning, axis limits, and combined or individual outputs.
- **Customizable Visuals**: Choose from various plot types, including trajectory plots, stream plots, phase portraits, and n-count plots.
- **Error Handling and Parallel Processing**: Support for parallel processing and comprehensive error handling to streamline large dataset analyses.

## Example Usage

Here's a basic example demonstrating how to use **phase.portrait** to generate phase portraits and visualize data over pseudotime.

```r
save_path <- "~/Documents/phase_portrait_analysis"
bin_width = 0.1
setwd(dirname(save_path))
dir.create(save_path)
setwd(save_path)

save_path <- file.path(save_path, paste("bin_width", bin_width))
dir.create(save_path)# Define the base path where analysis results will be saved
save_path <- "~/Documents/phase_portrait_analysis"

# Set the working directory to the parent directory of the save path
setwd(dirname(save_path))

# Create the base directory if it does not exist
dir.create(save_path)

# Change the working directory to the newly created save path
setwd(save_path)

# Create a subdirectory within the save path for storing results based on bin width
save_path <- file.path(save_path, paste("bin_width", bin_width))
dir.create(save_path)

# Change the working directory to the bin-width-specific save path
setwd(save_path)

# Generate an example dataset by combining two sets of data with different parameters
# `lotka_volterra()` generates synthetic data for phase portrait analysis
example_dataset <- rbind(lotka_volterra(), lotka_volterra(alpha = 0.75))

# Scale and compute the difference of the dataset to prepare for analysis
d_table <- scale_and_diff(example_dataset)

# Run the analysis with specified parameters:
# - `option = "all"` specifies to perform a comprehensive analysis
# - `bin_width` defines the bin size for the analysis
# - `min_bin_n = 1` sets the minimum number of data points per bin
setwd(save_path)

example_dataset <- rbind(lotka_volterra(), lotka_volterra(alpha = 0.75))
d_table <- scale_and_diff(example_dataset)
run_analysis(d_table, option = "all", bin_width = bin_width, min_bin_n = 1)
```

The `run_analysis` function allows for high-level control over data analysis, enabling users to generate various types of plots and tables based on input parameters. The `option` parameter lets you choose between "all", "standard", or "fraction" analysis types.

## Plot Examples

### Trajectory Plot

The trajectory plot visualizes pseudotime dynamics in phase space, showing the relationship between `x` and `y` variables over time.

```r
plot_trajectory(df, min_x = 0, max_x = 5, min_y = 0, max_y = 5)
```

### Phase Line Plot

The phase line plot shows phase line summaries across binned data, helpful for analyzing dynamic changes in specific variables.

```r
plot_phase_line(df, input = "standard")
```

### Phase Portrait Stream

Stream plots display phase portrait streams to illustrate the direction and magnitude of change for variables across pseudotime.

```r
plot_stream(df, n_bins = 5)
```

**phase.portrait** is well-suited for exploratory data analysis and visual interpretation of complex system dynamics, providing the tools you need to analyze trajectories, streams, and phase portraits efficiently and flexibly.
```
