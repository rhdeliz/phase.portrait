% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_n.R
\name{plot_n}
\alias{plot_n}
\title{Plot Cell Count (N) in Phase Space}
\arguments{
\item{df}{A data frame containing \code{x}, \code{y}, \code{n}, \code{condition}, \code{x_variable}, and \code{y_variable}.}

\item{bin_width}{Integer specifying the number of bins to use for x and y axes. Default is 4.}

\item{input}{A character string specifying the input type, either "standard" or "fraction". Default is "standard".}

\item{min_x, }{max_x Optional numeric values to set limits for the x-axis.}

\item{min_y, }{max_y Optional numeric values to set limits for the y-axis.}

\item{save}{Logical, if TRUE, saves the plot as a PDF file. Default is FALSE.}

\item{fix_coord}{Logical, if TRUE enforces a fixed aspect ratio. Default is FALSE.}
}
\value{
A ggplot object showing the cell count (\code{n}) in phase space.
}
\description{
This function creates a phase space plot displaying the count (\code{n}) of observations in each bin. The plot can be generated for either standard or fraction-based input types, and provides options to customize axis limits, enforce a fixed aspect ratio, and save the plot to a file.
}
\examples{
plot_n(df, bin_width = 4, input = "standard", min_x = -10, max_x = 10, min_y = -5, max_y = 5, save = TRUE)
}
