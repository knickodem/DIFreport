#' Generate DIF analysis report
#'
#' Produces a report summarizing an analysis of measurement bias for a given
#' measure and grouping variable
#'
#' @param dif.analysis An object returned from \code{\link[WBdif]{dif_analysis}}
#' @param dif.models An object returned from \code{\link[WBdif]{dif_models}}
#' @param effect.robustness An object returned from \code{\link[WBdif]{effect_robustness}}
#' @param report.type Produce report including both the DIF analysis results and treatment effect robustness checks ("dif.effects"; default), only DIF analysis ("dif.only"), or only treatment effect robustness checks ("effects.only")?
#' @param report.title Title of the report
#' @param measure.name Name of the measure being evaluated for DIF, which is used in the report.
#' @param file.name File name to create on disk. Defaults to \code{measure.name}. The file path can also be specified here. The file is saved to the working directory by default.
#' @param dataset.name (optional) name of the dataset where the measure and
#' item responses came from, which is printed in the report. For World Bank data, this will typically be a country.
#' @param bias.method From which method in \code{dif.analysis} should the biased items, if detected, be extracted? The options are "MH", "logistic", or "IRT" (default).
#'
#' @details
#' With \code{bias.method = "IRT"}, the direction of uniform DIF is determined using
#' the b parameter for dichotomous items and the location of the first threshold for
#' polytomous items.
#'
#' @return produces a report summarizing a) whether any items on \code{measure.name} are biased with
#' respect to the groups defined in \code{dif.analysis}, and, if so, b) the extent to which the bias affects treatment contrasts.
#'
#' @examples
#' data("mdatlang")
#'
#' # prep data
#' dif.data <- dif_data_prep(item.data = mdatlang`[`5:ncol(mdatlang)],
#'                              tx.group.id = mdatlang$treated,
#'                              dif.group.id = mdatlang$gender,
#'                              cluster.id = mdatlang$clusterid,
#'                              na.to.0 = TRUE)
#'
#' # DIF analysis by dif.group.id
#' # using rest scores and binning match.scores by deciles to avoid empty cells in MH analysis
#' dif.analysis <- dif_analysis(dif.data = dif.data,
#'                            methods =  c("MH", "IRT")
#'                            match.type = "Rest",
#'                            match.bins = seq(0, 1, by = .1))
#' @export

dif_report <- function(dif.analysis = NULL, dif.models = NULL, effect.robustness = NULL,
                       report.type = "dif.effects",
                       report.title = NULL, measure.name = NULL,
                       file.name = measure.name, dataset.name = NULL,
                       bias.method = "IRT"){

  ## Quick report output checks
  if(is.null(report.title)){
    stop("What would you like as the title of the report? Specify in report.title argument.")
  }
  if(is.null(measure.name)){
    stop("What is the name of your measure/instrument/scale/test? Specify in measure.name argument.")
  }
  if(report.type %in% c("dif.only", "dif.effects") & is.null(dif.analysis)){
    stop("dif.analysis must be provided to produce 'dif.only' and 'dif.effects' reports.")
  }
  if(report.type %in% c("effects.only", "dif.effects")){
    if(is.null(dif.models)){
      stop("dif.models must be provided to produce 'effects.only' and 'dif.effects' reports.")
    }
    if(is.null(effect.robustness)){
      stop("effect.robustness must be provided to produce 'dif.only' and 'dif.effects' reports.")
    }
  }

  if(!is.null(dif.analysis)){
    inputs <- dif.analysis$inputs
  } else if(!is.null(dif.models)){
    inputs <- dif.models$inputs
  } else {
    stop("dif.analysis and/or dif.models must be provided.")
  }


  ## Measure info
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

  ## DIF info
  if(report.type %in% c("dif.only", "dif.effects")){
    if(is.null(dif.analysis[[bias.method]])){
      stop(paste("The", bias.method, "method was not found in dif.analysis."))
    }

    match.type <- inputs$match.type
    dif.type <- dif.analysis[[bias.method]]$dif.type

    # biased items by DIF method table
    bi.table <- bi_table(dif.analysis)


    # If no biased items by selected method
    if(is.character(dif.analysis[[bias.method]]$biased.items)) { # expected to be "No DIF was detected", but using the general is.character in case we want to alter the message

      n.biased <- 0
      nodifmessage <- paste0("No DIF was detected by ", bias.method, ".")

    } else {

      # biased items for selected detection method
      biased.items <- dif.analysis[[bias.method]]$biased.items   # integer vector of column locations
      n.biased <- length(biased.items)

      ## Gathering directionality if uniform dif (and double-checking for biased items)
      if(dif.type == "uniform" & is.integer(biased.items)){
        toward <- which_direction(dif.analysis = dif.analysis, dif.models = dif.models,
                                  bias.method = bias.method, biased.items = biased.items)

      }
    }
  }

  if(report.type %in% c("effects.only", "dif.effects")){

    ## Effects Info
    alphas.list <- coeff_alpha(dif.models, std.group = inputs$std.group)
    effects.tables <- mapply(effects_table, effect.robustness, alphas.list, SIMPLIFY = FALSE)
    effects.plots <- lapply(effect.robustness, effects_plot)
    bias.plots <- bias_plots(dif.models)
    biased.items <- names(inputs$item.data)[dif.models$biased.items]

  }


  ## running report
  if(report.type == "dif.only"){
    template <- system.file("rmd", "DIF-Only-Report.Rmd", package = "WBdif")
  } else if(report.type == "effects.only"){
    template <- system.file("rmd", "Effects-Only-Report.Rmd", package = "WBdif")
  } else if(report.type == "dif.effects"){
    template <- system.file("rmd", "DIF-Effects-Report.Rmd", package = "WBdif")
  } else {stop("invalid report.type")}


  if(!grepl("\\.html", file.name)){
    file.name <- paste0(file.name, ".html")
  }

  dir <- getwd()
  rmarkdown::render(input = template,
                    output_file = file.name,
                    output_dir = dir)
}




bi_table <- function(dif.analysis){

  item.data <- dif.analysis$inputs$item.data

  # extracting biased items from each method
  bi.list <- extract_bi(dif.analysis)
  bi.list <- bi.list[lengths(bi.list) != 0] # removes NULL elements

  # creating table of biased items by method table for report
  item.df <- data.frame(biased.items = names(item.data))
  for(i in names(bi.list)){
    if(is.character(bi.list[[i]])){
      temp <- data.frame(biased.items = names(item.data),
                         x = NA)
      names(temp) <- c("biased.items", i)
      item.df <- merge(item.df, temp, by = "biased.items", all.x = TRUE)
    } else {
      temp <- data.frame(biased.item = names(item.data[bi.list[[i]]]),
                         IRT = "X")
      names(temp) <- c("biased.items", i)
      item.df <- merge(item.df, temp, by = "biased.items", all.x = TRUE)
    }
  }
  # remove unbiased items
  bi.df <- item.df[rowSums(is.na(item.df)) != ncol(item.df) - 1,]

  return(bi.df)

}

which_direction <- function(dif.analysis, dif.models = NULL, bias.method, biased.items){
  if(bias.method %in% c("MH", "logistic")){

    item.level <- dif.analysis[[bias.method]]$item.level

    toward1 <- nrow(item.level[item.level$refined.bias == TRUE & item.level$refined.OR < 1, ])
    toward2 <- nrow(item.level[item.level$refined.bias == TRUE & item.level$refined.OR > 1, ])

  } else if(bias.method == "IRT"){

    if(!is.null(dif.models)){
      dif.mod <- dif.models$dif.mod  # use model with only freed items when possible
      # biased.items <- dif.models$biased.items
    } else {
      dif.mod <- dif.analysis$IRT$dif.mod
      # biased.items <- dif.analysis$IRT$biased.items
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





