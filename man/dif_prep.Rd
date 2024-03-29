% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DIF-Prep.R
\name{dif_prep}
\alias{dif_prep}
\title{Data prep for \code{DIFreport} functions.}
\usage{
dif_prep(
  data,
  dif.groups,
  ref.name = NULL,
  items = names(data),
  anchors = NULL,
  poly.items = NULL,
  max.values = NULL,
  cond.groups = NULL,
  cluster = NULL,
  na.to.0 = FALSE
)
}
\arguments{
\item{data}{a \code{data.frame} containing the item responses with subjects in rows and items in columns. Items can be dichotomous or polytomous, but the columns must be numeric. All reverse coding should be completed before using this function.}

\item{dif.groups}{column position or name in \code{data} indicating the two (and only two) groups to compare for DIF.}

\item{ref.name}{character; specifying the reference group in \code{dif.groups}. This primarily for organizing the report output.
If examining standardized mean differences, the standard deviation of this group can be used rather than the pooled standard deviation
by setting \code{pooled = FALSE} in \code{\link[DIFreport]{dif_report}} or \code{\link[DIFreport]{summary_report}}}

\item{items}{vector of column positions or names in \code{data} indicating the items to analyze for DIF. Default is to use all columns.}

\item{anchors}{vector of column positions or names \code{data} indicating the anchor items. If specified, must be a subset of \code{items}.}

\item{poly.items}{vector of column positions or names \code{data} indicating polytomous items. If \code{NULL} (default), polytomous items will be auto-detected as those with > 2 responses options.}

\item{max.values}{the maximum value each item in \code{poly.items} can take, declared by an integer vector of the same length as \code{poly.items} or a single value to be used for all \code{poly.items}. If \code{NULL} (default), values will be auto-detected. This value is used for unit-scoring polytomous items for loess, MH, and logistic methods.}

\item{cond.groups}{column name or number in \code{data} indicating the two (and only two) groups on which to condition the mean difference between \code{dif.groups}.}

\item{cluster}{column name or number in \code{data} indicating the primary sampling unit in a multi-stage or clustered sampling design -- used to adjust effect sizes and their standard errors.}

\item{na.to.0}{After removing empty rows, should remaining NAs in \code{items} columns be converted to 0? Default is FALSE.}
}
\value{
A named \code{list} containing the pre-processed inputs and item flags.
}
\description{
Data pre-processing to gather information and prepare data for use in other \code{DIFreport} functions.
}
\details{
This function saves the input data in a format used by other \code{DIFreport} functions and also runs a number of pre-processing steps:
\itemize{
  \item Drops rows in \code{data} with \code{NA} for all \code{items} columns, \code{dif.groups}, \code{cond.groups} (if specified), or \code{cluster} (if specified).
  \item Converts \code{dif.groups} and to a \code{factor} and confirms there are only two groups. Same for \code{cond.groups} if specified.
  \item Flag items with no variance.
  \item Flag items with different number of response categories across the \code{dif.groups}.
  \item Identify items with more than two response categories (i.e., polytomous items).
}
It is recommended, but not required, that responses for each item are coded as consecutive integers with a minimum value of 0 (e.g., 0, 1, 2).
}
\examples{
data("mdat")

dif.data <- dif_prep(data = mdat,
                             dif.groups = "treated",
                             ref.name = "Control",
                             items = 5:ncol(mdat)
                             cond.groups ="gender",
                             cluster = "clusterid",
                             na.to.0 = TRUE)

}
