#' Generate report
#'
#' Produces a report summarizing DIF analysis for a given measure and grouping variable, the robustness of treatment effect estimates to DIF, or both
#'
#' @param dif.analysis An object returned from \code{\link[WBdif]{dif_analysis}}
#' @param dif.models An object returned from \code{\link[WBdif]{dif_models}}
#' @param effect.robustness An object returned from \code{\link[WBdif]{effect_robustness}}
#' @param file.name File name to create on disk. The file path can also be specified here. If the path is omitted, the file is saved to the working directory.
#' @param biased.items Οne of \code{c("MH", "logistic", "IRT")}. Determines which DIF method should be used to identify biased items. Default is "IRT".
#' @param report.format File format of the report. Default is HTML ("html_document"). See \code{link[rmarkdown]{render}} for other options.
#' @param report.title An optional character string indicating the report title, which is printed in the report.
#' @param measure.name An optional character string naming the measure being evaluated, which is printed in the report.
#' @param dataset.name An optional character string naming the dataset used, which is printed in the report.
#'
#' @details
#' If uniform DIF is detected in \code{dif.analysis} with \code{biased.items = "IRT"}, the direction of uniform DIF is determined using
#' the b parameter for dichotomous items and the location of the first threshold for polytomous items.
#'
#' @return a summary report of:
#' \itemize{
#'   \item \code{dif_report} - DIF analysis results conducted in \code{dif.analysis}
#'   \item \code{effect_report} - robustness of treatment effects to DIF as estimated from \code{dif_models} and \code{effect_robustness}
#'   \item \code{dif_effect_report} - DIF analysis results and treatment effect robustness checks
#'  }
#'
#' @examples
#' data("mdat")
#'
#' # prep data
#' dif.data <- dif_data_prep(item.data = mdat`[`5:ncol(mdat)],
#'                              tx.group.id = mdat$treated,
#'                              dif.group.id = mdat$gender,
#'                              cluster.id = mdat$clusterid,
#'                              na.to.0 = TRUE)
#'
#' # DIF analysis by dif.group.id
#' # using rest scores and binning match.scores by deciles to avoid empty cells in MH analysis
#' dif.analysis <- dif_analysis(dif.data = dif.data,
#'                            methods =  c("MH", "IRT"),
#'                            match.type = "Rest",
#'                            match.bins = seq(0, 1, by = .1))
#'
#' # Report only DIF analysis results
#' dif_report(dif.analysis = dif.analysis,
#'            file.name = "DIF-Gender-MDAT-Language",
#'            report.format = "html_document",
#'            report.title = "MDAT Language: Gender DIF",
#'            measure.name = "MDAT Language",
#'            biased.items = "IRT")
#'
#' # Report DIF analysis and treatment effect robustness check
#' dif.models <- dif_models(dif.analysis = dif.analysis, biased.items = "MH")
#' effect.robustness <- effect_robustness(dif.models = dif.models, irt.scoring = "WLE")
#' dif_report(dif.analysis = dif.analysis,
#'            dif.models = dif.models,
#'            effect.robustness = effect.robustness,
#'            file.name = "DIF-Effect-Gender-MDAT-Language",
#'            biased.items = "IRT",
#'            report.format = "html_document",
#'            report.title = "MDAT Language: Gender DIF and Tx Effects",
#'            measure.name = "MDAT Language")
#'
#' @export

dif_report <- function(dif.analysis,
                       file.name,
                       report.format = "html_document",
                       report.title = file.name,
                       measure.name = "measure",
                       dataset.name = "dataset",
                       biased.items = "IRT"){

  ## Input checks
  if(!(biased.items %in% c("MH", "logistic", "IRT"))) {
    stop("biased.items must be one of c(\'MH\', \'logistic\', \'IRT\').")
  }

  inputs <- dif.analysis$inputs
  outputs <- inputs2report(inputs = inputs)

  ## DIF info
  if(is.null(dif.analysis[[biased.items]])){
    stop(paste("The", biased.items, "method was not found in dif.analysis."))
  }

  match.type <- inputs$match.type
  dif.type <- dif.analysis[[biased.items]]$dif.type

  # biased items by DIF method table
  biased.items.table <- biased_items_table(dif.analysis)

  # If no biased items by selected method
  if(is.character(dif.analysis[[biased.items]]$biased.items)) { # expected to be "No DIF was detected", but using the general is.character in case we want to alter the message

    n.biased <- 0

  } else { # If biased items detected

    ## Gathering directionality if uniform dif
    if(dif.type == "uniform"){
      toward <- which_direction(dif.analysis = dif.analysis, biased.items = biased.items, dif.models = NULL)
    }

    # biased items for selected detection method
    n.biased <- length(dif.analysis[[biased.items]]$biased.items)
  }

  ## running report
  template <- system.file("rmd", "DIF-Only-Report.Rmd", package = "WBdif")
  dir <- getwd()
  rmarkdown::render(input = template,
                    output_format = report.format,
                    output_file = file.name,
                    output_dir = dir,
                    output_options = list(toc = TRUE)) #is always_allow_html = TRUE actually needed?
}

#' @rdname dif_report
#' @export

effect_report <- function(dif.models,
                          effect.robustness,
                          file.name,
                          report.format = "html_document",
                          report.title = file.name,
                          measure.name = "measure",
                          dataset.name = "dataset"){

  inputs <- dif.models$inputs
  outputs <- inputs2report(inputs = inputs)

  ## Effects Info
  alphas.list <- coeff_alpha(dif.models, std.group = inputs$std.group)
  effects.tables <- mapply(effects_table, effect.robustness, alphas.list, SIMPLIFY = FALSE)
  effects.plots <- lapply(effect.robustness, effects_plot)
  bias.plots <- bias_plots(dif.models)

  ## running report
  template <- system.file("rmd", "Effects-Only-Report.Rmd", package = "WBdif")
  dir <- getwd()
  rmarkdown::render(input = template,
                    output_format = report.format,
                    output_file = file.name,
                    output_dir = dir,
                    output_options = list(toc = TRUE))

}

#' @rdname dif_report
#' @export

dif_effect_report <- function(dif.analysis,
                              dif.models,
                              effect.robustness,
                              file.name,
                              report.format = "html_document",
                              report.title = file.name,
                              measure.name = "measure",
                              dataset.name = "dataset",
                              biased.items = "IRT"){
  ## Input checks
  if(!(biased.items %in% c("MH", "logistic", "IRT"))) {
    stop("biased.items must be one of c(\'MH\', \'logistic\', \'IRT\').")
  }

  inputs <- dif.analysis$inputs
  outputs <- inputs2report(inputs = inputs)

  ## DIF info
  if(is.null(dif.analysis[[biased.items]])){
    stop(paste("The", biased.items, "method was not found in dif.analysis."))
  }

  match.type <- inputs$match.type
  dif.type <- dif.analysis[[biased.items]]$dif.type

  # biased items by DIF method table
  biased.items.table <- biased_items_table(dif.analysis)

  ## If no biased items by selected method
  if(is.character(dif.analysis[[biased.items]]$biased.items)) { # expected to be "No DIF was detected", but using the general is.character in case we want to alter the message

    n.biased <- 0

  } else { # If biased items detected

    ## Gathering directionality if uniform dif
    if(dif.type == "uniform"){
      toward <- which_direction(dif.analysis = dif.analysis, biased.items = biased.items, dif.models = dif.models)
    }

    # biased items for selected detection method
    n.biased <- length(dif.analysis[[biased.items]]$biased.items)
  }

  ## Effects Info
  alphas.list <- coeff_alpha(dif.models, std.group = inputs$std.group)
  effects.tables <- mapply(effects_table, effect.robustness, alphas.list, SIMPLIFY = FALSE)
  effects.plots <- lapply(effect.robustness, effects_plot)
  bias.plots <- bias_plots(dif.models)

  ## running report
  template <- system.file("rmd", "DIF-Effects-Report.Rmd", package = "WBdif")
  dir <- getwd()
  rmarkdown::render(input = template,
                    output_format = report.format,
                    output_file = file.name,
                    output_dir = dir,
                    output_options = list(toc = TRUE))
}


## Which inputs gathered from dif.analysis or dif.models need to be reported?
inputs2report <- function(inputs){

  # Item info
  n.items <- ncol(inputs$item.data)
  item.name.range <- paste(names(inputs$item.data)[[1]], "-",
                           names(inputs$item.data)[[n.items]])

  # Names of items with no variance
  no.var.items <- ifelse(length(inputs$no.var.items) !=0,
                         paste("^a^", paste(names(inputs$no.var.items), collapse = ", ")),
                         "none")

  # names of items with no within group variance
  no.var.by.group.items <- ifelse(length(inputs$no.var.by.group.items) !=0,
                                  paste("^b^", paste(names(inputs$no.var.by.group.items), collapse = ", ")),
                                  "none")

  # levels of the dif.group variable
  dif.group1 <- levels(inputs$dif.group.id)[[1]]
  dif.group2 <- levels(inputs$dif.group.id)[[2]]

  return(list(n.items = n.items,
              item.name.range = item.name.range,
              no.var.items = no.var.items,
              no.var.by.group.items = no.var.by.group.items,
              dif.group1 = dif.group1,
              dif.group2 = dif.group2))

}

## If uniform DIF is detected, which direction is the DIF?
which_direction <- function(dif.analysis, biased.items, dif.models = NULL){

  if(biased.items %in% c("MH", "logistic")){

    item.level <- dif.analysis[[biased.items]]$item.level

    toward1 <- nrow(item.level[item.level$refined.bias == TRUE & item.level$refined.OR < 1, ])
    toward2 <- nrow(item.level[item.level$refined.bias == TRUE & item.level$refined.OR > 1, ])

  } else if(biased.items == "IRT"){

    if(!is.null(dif.models)){
      dif.mod <- dif.models$dif.mod  # use dif.mod with only freed items when possible
      biased.items <- dif.models$biased.items
    } else {
      dif.mod <- dif.analysis$IRT$dif.mod # rather than the omnibus dif.mod
      biased.items <- dif.analysis$IRT$biased.items
    }

    ## extract biased item parameter estimates for each dif.group
    b1 <- mirt::coef(dif.mod, IRTpars = TRUE)[[1]][c(biased.items)]
    b1 <- lapply(b1, function(x) x[, 2]) # extracts b parameter (or first threshold)
    b1.df <- as.data.frame(Reduce(rbind, b1)) # single column

    b2 <- mirt::coef(dif.mod, IRTpars = TRUE)[[2]][c(biased.items)]
    b2 <- lapply(b2, function(x) x[, 2]) # extracts b parameter (or first threshold)
    b2.df <- as.data.frame(Reduce(rbind, b2))

    # alternative parameter extraction, especially for using 2nd threshold
    # g1 <- lapply(g1, function(x) x[,grepl("b$|b2", dimnames(x)[[2]])])

    # calculate difference in b (difficulty) parameter
    b.diff <- b1.df[[1]] - b2.df[[1]]

    toward1 <- length(b.diff[b.diff < 0])
    toward2 <- length(b.diff[b.diff > 0])

  }
  return(c(toward1, toward2))
}