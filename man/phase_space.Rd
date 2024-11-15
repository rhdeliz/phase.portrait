% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/phase_space.R
\name{phase_space}
\alias{phase_space}
\title{Generate Phase Space for Pairwise Variable Analysis with Parallel Processing}
\usage{
phase_space(df, n_cores = NULL, save = FALSE)
}
\arguments{
\item{df}{A data frame containing the following columns:
\itemize{
\item \code{value}: Numeric values representing measurements or observations of interest.
\item \code{delta}: Numeric values representing the derivative (rate of change) of \code{value}.
\item \code{variable}: Factor or character column representing the variable or feature name.
\item \code{condition}: Factor or character column identifying different experimental or observational conditions.
\item \code{sample}: Factor or character column identifying different samples.
\item \code{pseudotime}: Numeric values representing temporal or sequential progression.
}}

\item{n_cores}{Integer specifying the number of cores to use for parallel processing.
If NULL, defaults to the total available cores minus two (if running on Unix). On Windows, \code{n_cores} is set to 1.}

\item{save}{Logical, if TRUE, saves the output as a compressed CSV file. Default is FALSE.}
}
\value{
A data frame containing phase space information with columns:
\itemize{
\item \code{x}: Values of the first variable in each pairwise combination.
\item \code{dx}: Derivative of the first variable.
\item \code{x_variable}: Name of the first variable.
\item \code{y}: Values of the second variable in each pairwise combination.
\item \code{dy}: Derivative of the second variable.
\item \code{y_variable}: Name of the second variable.
\item Additional columns: \code{condition}, \code{sample}, and \code{pseudotime}.
}
}
\description{
This function calculates the phase space for all pairwise combinations of variables within a data frame. It computes phase portraits by pairing each variable's values and their derivatives, then merges them into a combined phase space data frame. Parallel processing is supported for improved performance.
}
\examples{
# Generate an example dataset for phase space analysis
df <- data.frame(
  value = c(runif(50, min = 0, max = 10), runif(50, min = 0, max = 10)),
  delta = c(rnorm(50, mean = 0.5), rnorm(50, mean = -0.5)),
  variable = rep(c("var1", "var2"), each = 50),
  condition = rep("cond1", 100),
  sample = rep(1:10, each = 10),
  pseudotime = rep(seq(1, 10, length.out = 10), 10)
)

# Generate phase space with default parameters
results <- phase_space(df)

# Generate phase space with parallel processing on 4 cores
results <- phase_space(df, n_cores = 4)
}
