% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_derivatives.R
\name{calculate_derivatives}
\alias{calculate_derivatives}
\title{Calculate Rate of Change (Delta) in Values Over Time}
\arguments{
\item{df}{A data frame with \code{value}, \code{pseudotime}, \code{sample}, \code{condition}, and \code{variable} columns.}

\item{derivative}{A character string specifying the derivative method to calculate the rate of change.
Options are:
\itemize{
\item \code{"lead"} (default): Uses the next timepoint to calculate the change.
\item \code{"lag"}: Uses the previous timepoint to calculate the change.
\item \code{"lead-lag"}: Calculates the change over two surrounding timepoints, useful for datasets with more than two timepoints.
}}

\item{delta_name}{A character string specifying the name of the new column for the rate of change. Default is \code{"delta"}.}

\item{save}{Logical, if TRUE, saves the resulting data frame to a file.}
}
\value{
A data frame with an added \code{delta} column (or specified name) representing the rate of change in \code{value} over \code{pseudotime}.
}
\description{
This function calculates the rate of change (delta) in the \code{value} column over \code{pseudotime}. The calculation method can be specified as lead, lag, or lead-lag, allowing flexibility for datasets with varying temporal structures.
}
\examples{
df <- data.frame(
  value = c(1, 2, 3, 4, 5),
  pseudotime = c(1, 2, 3, 4, 5),
  sample = "sample1",
  condition = "condition1",
  variable = "variable1"
)
calculate_derivatives(df, derivative = "lead-lag", delta_name = "rate_change")

}
