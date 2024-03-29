% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Hedges2007.R
\name{hedges2007}
\alias{hedges2007}
\title{Estimate standardized mean differences for clustered data}
\usage{
hedges2007(
  outcome,
  groups,
  pooled = TRUE,
  comp.name = NULL,
  cluster = NULL,
  subset = NULL
)
}
\arguments{
\item{outcome}{numeric vector}

\item{groups}{vector of \code{length(outcome)} indicating the two groups to compare}

\item{pooled}{If \code{TRUE} (default), the pooled standard deviation is calculated to standardize the mean difference.
If \code{FALSE}, the standard deviation of \code{comp.name} will be used.}

\item{comp.name}{If \code{pooled = FALSE}, the name of the group in \code{groups} whose standard deviation will be used to
standardize the mean difference.}

\item{cluster}{optional vector of \code{length(outcome)} indicating the primary sampling unit in a multi-stage / clustered sampling design}

\item{subset}{optional logical vector of \code{length(outcome)} indicating a subset of the data to use in the calculations}
}
\value{
A named vector containing the standardized mean difference and its standard error.
}
\description{
Computes mean difference standardized for clustered data by the "total variance", and their standard errors, using the method described by Hedges (2007).
}
\details{
The standardized mean difference and its standard error are computed using Equations 15 and 16 of Hedges (2007).
When cluster sample sizes are unequal, the average cluster size is used to obtain a close approximation of the
standardized mean difference estimate and standard error (p. 351).
}
\references{
Hedges, L. V. (2007). Effect sizes in cluster-randomized designs. \emph{Journal of Educational and Behavioral Statistics, 32}(4), 341–370. \url{https://doi.org/10.3102/1076998606298043}.
}
