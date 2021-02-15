#' Summarize DIF Analysis Results
#'
#' Conduct a DIF analysis and produce report summarizing results
#'
#' @param dif.data The output of \code{\link[WBdif]{dif_data_prep}}.
#' @param report.type Produce report including both the DIF analysis results and treatment effect robustness checks ("dif.effects"; default), only DIF analysis ("dif.only"), or only treatment effect robustness checks ("effects.only")?
#' @param report.title Title of report
#' @param measure.name Name of the measure being evaluated for DIF, which is used in the report.
#' @param file.name File name to create on disk. Defaults to \code{measure.name}. The file path can also be specified here. The file is saved to the working directory by default.
#' @param dataset.name (optional) name of the dataset where the measure and
#' item responses came from, which is printed in the report. For World Bank data, this will typically be a country.
#' @param bias.method From which method in \code{methods} should the biased items, if detected, be extracted? The options are "MH", "logistic", or "IRT" (default).
#' @param match.type Οne of \code{c("Total", "Rest")}. Determines whether the total score or rest score should be used as the stratifying variable for loess, MH, and logistic regression methods.
#' @param match.bins (optional) vector of bin sizes for stratifying the matching variable in
#' the MH method. This is passed to the \code{probs} argument of \code{stas::quantile}.
#' @param irt.scoring Factor score estimation method, which is passed to
#' \code{\link[mirt]{fscores}}. Default is "WLE". See \code{\link[mirt]{fscores}} documentation for other options.
#'
#' @details
#' This function is a wrapper around \code{\link[WBdif]{dif_analysis}}, \code{\link[WBdif]{dif_models}}, \code{\link[WBdif]{effect_robustness}}, and \code{\link[WBdif]{dif_report}} in order to simplify report production when desired.
#'
#' @examples
#' data("mdatlang")
#'
#' # prep data
#' dif.data <- dif_data_prep(item.data = mdatlang`[`5:ncol(mdatlang)],
#'                           tx.group.id = mdatlang$treated,
#'                           dif.group.id = mdatlang$gender,
#'                           cluster.id = mdatlang$clusterid,
#'                           na.to.0 = TRUE)
#'
#' summary_report(dif.data = dif.data, report.type = "dif.effects",
#'               report.title = "Gender DIF on MDAT Language",
#'               measure.name = "MDAT Language",
#'               file.name = "DIF-Gender-MDAT-Language",
#'               methods = c("loess", "MH", "logistic", "IRT"),
#'               bias.method = "IRT",
#'               match.type  = "Total")
#' @export

summary_report <- function(dif.data, report.type = "dif.effects",
                          report.title = NULL, measure.name = NULL,
                          file.name = measure.name, dataset.name = NULL,
                          methods = c("loess", "MH", "logistic", "IRT"), bias.method = "IRT",
                          match.type  = "Total", match.bins = NULL,
                          irt.scoring = "WLE"){

  ## Quick report output checks
  if(is.null(report.title)){
    stop("What would you like as the title of the report? Specify in report.title argument.")
  }
  if(is.null(measure.name)){
    stop("What is the name of your measure/instrument/scale/test? Specify in measure.name argument.")
  }
  if(!(bias.method %in% methods)){
    stop("bias.method must be one of the DIF methods specified in the methods argument.")
  }

  if(report.type %in% c("dif.only", "dif.effects")){

    ## run dif_analysis
    dif.analysis <- dif_analysis(dif.data = dif.data, methods = methods,
                                 match.type = match.type, match.bins = match.bins)

    nodif <- is.character(dif.analysis[[bias.method]]$biased.items) # currently indicates the return is "No DIF was detected"
  }


  if(report.type == "dif.only" | nodif == TRUE){


    dif_report(dif.analysis = dif.analysis, report.type = "dif.only",
               report.title = report.title, measure.name = measure.name,
               file.name = file.name, dataset.name = dataset.name,
               bias.method = bias.method)

  } else {

    dif.models <- dif_models(dif.analysis = dif.analysis, biased.items = bias.method)
    effect.robustness <- effect_robustness(dif.models = dif.models, std.group = dif.data$std.group, irt.scoring = irt.scoring)

    if(report.type == "effects.only"){
      dif_report(dif.analysis = NULL, dif.models = dif.models, effect.robustness = effect.robustness,
                 report.type = "effects.only", report.title = report.title, measure.name = measure.name,
                 file.name = file.name, dataset.name = dataset.name,
                 bias.method = bias.method)
    } else {

      dif_report(dif.analysis = dif.analysis, dif.models = dif.models, effect.robustness = effect.robustness,
                 report.type = "dif.effects", report.title = report.title, measure.name = measure.name,
                 file.name = file.name, dataset.name = dataset.name,
                 bias.method = bias.method)

    }

  }

}