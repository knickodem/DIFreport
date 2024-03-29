% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Report-Functions.R
\name{dif_report}
\alias{dif_report}
\alias{effect_report}
\alias{dif_effect_report}
\title{Generate report}
\usage{
dif_report(
  dif.analysis,
  file.name,
  report.format = "html_document",
  report.title = file.name,
  measure.name = "measure",
  dataset.name = "dataset",
  biased.items = "IRT"
)

effect_report(
  dif.models,
  effect.robustness,
  file.name,
  report.format = "html_document",
  report.title = file.name,
  measure.name = "measure",
  dataset.name = "dataset"
)

dif_effect_report(
  dif.analysis,
  dif.models,
  effect.robustness,
  file.name,
  report.format = "html_document",
  report.title = file.name,
  measure.name = "measure",
  dataset.name = "dataset",
  biased.items = "IRT"
)
}
\arguments{
\item{dif.analysis}{An object returned from \code{\link[DIFreport]{dif_analysis}}}

\item{file.name}{File name to create on disk. The file path can also be specified here. If the path is omitted, the file is saved to the working directory.}

\item{report.format}{File format of the report. Default is HTML ("html_document"). See \code{\link[rmarkdown]{render}} for other options.}

\item{report.title}{An optional character string indicating the report title, which is printed in the report.}

\item{measure.name}{An optional character string naming the measure being evaluated, which is printed in the report.}

\item{dataset.name}{An optional character string naming the dataset used, which is printed in the report.}

\item{biased.items}{Οne of \code{c("MH", "logistic", "IRT")}. Determines which DIF method should be used to identify biased items. Default is "IRT".}

\item{dif.models}{An object returned from \code{\link[DIFreport]{dif_models}}}

\item{effect.robustness}{An object returned from \code{\link[DIFreport]{effect_robustness}}}
}
\value{
a summary report of:
\itemize{
  \item \code{dif_report} - DIF analysis results conducted in \code{dif.analysis}
  \item \code{effect_report} - robustness of treatment effects to DIF as estimated from \code{dif_models} and \code{effect_robustness}
  \item \code{dif_effect_report} - DIF analysis results and treatment effect robustness checks
 }
}
\description{
Produces a report summarizing DIF analysis for a given measure and grouping variable, the robustness of treatment effect estimates to DIF, or both
}
\details{
If uniform DIF is detected in \code{dif.analysis} with \code{biased.items = "IRT"}, the direction of uniform DIF is determined using
the b parameter for dichotomous items and the location of the first threshold for polytomous items.
}
\examples{
data("mdat")

# prep data
dif.data <- dif_data_prep(item.data = mdat`[`5:ncol(mdat)],
                             tx.group.id = mdat$treated,
                             dif.group.id = mdat$gender,
                             cluster.id = mdat$clusterid,
                             na.to.0 = TRUE)

# DIF analysis by dif.group.id
# using rest scores and binning match.scores by deciles to avoid empty cells in MH analysis
dif.analysis <- dif_analysis(dif.data = dif.data,
                           methods =  c("MH", "IRT"),
                           match.type = "Rest",
                           match.bins = seq(0, 1, by = .1))

# Report only DIF analysis results
dif_report(dif.analysis = dif.analysis,
           file.name = "DIF-Gender-MDAT-Language",
           report.format = "html_document",
           report.title = "MDAT Language: Gender DIF",
           measure.name = "MDAT Language",
           biased.items = "IRT")

# Report DIF analysis and treatment effect robustness check
dif.models <- dif_models(dif.analysis = dif.analysis, biased.items = "MH")
effect.robustness <- effect_robustness(dif.models = dif.models, irt.scoring = "WLE")
dif_report(dif.analysis = dif.analysis,
           dif.models = dif.models,
           effect.robustness = effect.robustness,
           file.name = "DIF-Effect-Gender-MDAT-Language",
           biased.items = "IRT",
           report.format = "html_document",
           report.title = "MDAT Language: Gender DIF and Tx Effects",
           measure.name = "MDAT Language")

}
