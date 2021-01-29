#' Generate DIF analysis report
#'
#' Produces a report summarizing an analysis of measurement bias for a given
#' measure and grouping variable
#'
#' @param dif.analysis an object returned from \code{dif_analysis}
#' @param file.name file name to create on disk.
#' @param measure.name name of the measure being evaluated for DIF, which is
#' used in the report.
#' @param dif.group.name name of the group used in the DIF analysis, which is
#' used in the report.
#' @param dataset.name (optional) name of the dataset where the measure and
#' item responses came from, which is printed in the report.
#' For World Bank data, this will typically be a country.
#' @param report.title title of the report
#' @param bias.method options are "MH", "logistic", or "IRT" (default).
#' Which method in the \code{dif_analysis} object should the biased items, if detected, be
#' extracted?
#' @param irt.scoring factor score estimation method, which is passed to
#' \code{\link[mirt]{fscores}}. Default is "WLE". See \code{\link[mirt]{fscores}}
#' documentation for other options.
#' @param tx.group When `NULL` (default) unconditional treatment effects are assessed
#' for robustness with the assumption that the treatment indicator was used as the
#' grouping variable in \code{dif_analysis}. If treatment indicator was not the grouping
#' variable in \code{dif_analysis}, the factor vector for treatment indicator needs to be
#' supplied here. Robustness of the conditional treatment effects is then assessed.
#'
#' @details
#' With \code{bias.method = "IRT"}, the direction of uniform DIF is determined using
#' the b parameter for dichotomous items and the location of the first threshold for
#' polytomous items.
#'
#' @return Uses the template "Bias_Correction_Report.Rmd" to produce a
#' report summarizing whether any items on \code{measure.name} are biased with
#' respect to \code{dif.group.name}, and, if so, to what extent this affects
#' treatment contrasts on \code{measure.name}.
#'
#' @examples
#' wb.measure <- data.frame(tx = rep(c("tx", "control"), times = 10),
#'                          gender = rep(c("male", "female"), each = 10),
#'                          item1 = sample(c(0,1), 20, replace = TRUE),
#'                          item2 = sample(c(0,1), 20, replace = TRUE),
#'                          item3 = sample(c(0,1), 20, replace = TRUE),
#'                          item4 = sample(c(0,1), 20, replace = TRUE),
#'                          item5 = sample(c(0,1), 20, replace = TRUE))
#'
#' ## Unconditional Treatment Effects
#' # DIF analysis by treatment condition
#' dif.by.tx <- dif_analysis(measure.data = wb.measure[3:7],
#'                           dif.group = wb.measure$tx,
#'                           score.type = "Rest",
#'                           methods = c("loess", "logistic"))
#'
#' dif_report(dif.analysis = dif.by.tx,
#'            file.name = "WB-LR.html",
#'            measure.name = "Letter Recognition",
#'            dif.group.name = "Treatment Condition",
#'            bias.method = "logistic",
#'            irt.scoring = "WLE")
#'
#' ## Conditional Treatment Effects
#' # DIF analysis by gender
#' dif.by.gender <- dif_analysis(measure.data = wb.measure`[`3:7`]`,
#'                               dif.group = wb.measure$gender,
#'                               score.type = "Rest",
#'                               methods = c("loess", "logistic"))
#'
#' dif_report(dif.analysis = dif.by.gender,
#'            file.name = "WB-LR.html",
#'            measure.name = "Letter Recognition",
#'            dif.group.name = "Gender",
#'            bias.method = "logistic",
#'            irt.scoring = "WLE",
#'            tx.group = wb.measure$tx)
#'
#' @export

dif_report <- function(dif.analysis,
                       file.name,
                       measure.name,
                       dif.group.name,
                       dataset.name = NULL,
                       report.title = NULL,
                       bias.method = "IRT",
                       irt.scoring = "WLE",
                       tx.group = NULL,
                       clusters = NULL){

  ## Determining whether the bias.method is was used for the DIF analysis
  if(is.null(dif.analysis[[bias.method]])){

    stop(paste(bias.method, "DIF analysis was not found. Use the dif_analysis function
               with", bias.method, "in the methods argument"))

  }

  # shortcut to make code easier to read
  inputs <- dif.analysis$inputs

  ## Gathering biased item information for summary report
  # extracting biased items from each method
  bi.list <- extract_bi(dif.analysis)
  bi.list <- bi.list[lengths(bi.list) != 0] # removes NULL elements

  # obtaining item names and putting in a table for report
  # Note: returns NA if no biased items
  # bi.df <- lapply(bi.list,
  #                 function(x){paste(names(inputs$data)[x], collapse = ", ")})
  # bi.df <- data.frame(method = c("MH", "logistic", "IRT"),
  #                     biased.items = stack(bi.df)[[1]])

  # creating table of biased items by method table for report
  item.df <- data.frame(biased.items = names(inputs$data))
  for(i in names(bi.list)){
    if(is.character(bi.list[[i]])){
      temp <- data.frame(biased.items = names(inputs$data),
                         x = NA)
      names(temp) <- c("biased.items", i)
      item.df <- merge(item.df, temp, by = "biased.items", all.x = TRUE)
    } else {
      temp <- data.frame(biased.item = names(inputs$data[bi.list[[i]]]),
                         IRT = "X")
      names(temp) <- c("biased.items", i)
      item.df <- merge(item.df, temp, by = "biased.items", all.x = TRUE)
    }
  }
  # remove unbiased items
  bi.df <- item.df[rowSums(is.na(item.df)) != ncol(item.df) - 1,]

  # If no biased items
  if(is.character(bi.list[[bias.method]])) { # bi.list[[bias.method]][1] == "No DIF was detected"

    biased.items <- NULL   # need to define biased.items for robustness check
    n.biased <- 0
    nodifmessage <- paste0("No DIF was detected by ", bias.method, ".")

  } else {

    # biased items for selected detection method
    biased.items <- bi.list[[bias.method]]    # integer vector of column locations
    n.biased <- length(bi.list[[bias.method]])

  }

  ## Gathering input information for summary report
  # item info to print
  n.items <- ncol(inputs$data)
  item.name.range <- paste(names(inputs$data)[[1]], "-",
                           names(inputs$data)[[n.items]])

  # Names of items with no variance
  nvi <- ifelse(length(inputs$no.var.items) !=0,
                paste("^a^", paste(names(inputs$no.var.items), collapse = ", ")),
                "none")

  # names of items with no within group variance
  nvgi <- ifelse(length(inputs$no.var.by.group.items) !=0,
                 paste("^b^", paste(names(inputs$no.var.by.group.items),
                                    collapse = ", ")),
                 "none")

  # levels of the dif.group variable
  dif.group1 <- levels(inputs$dif.group)[[1]]
  dif.group2 <- levels(inputs$dif.group)[[2]]

  # type of dif
  dif.type <- dif.analysis[[bias.method]]$dif.type

  ## Gathering directionality if uniform dif (and double-checking for biased items)
  if (dif.type == "uniform" & !is.null(biased.items)){
    if(bias.method %in% c("MH", "logistic")){

      uni.temp <- dif.analysis[[bias.method]]$item.level

      toward1 <- nrow(uni.temp[uni.temp$refined.bias == TRUE & uni.temp$refined.OR < 1, ])
      toward2 <- nrow(uni.temp[uni.temp$refined.bias == TRUE & uni.temp$refined.OR > 1, ])

    } else if(bias.method == "IRT"){

      ## extract biased item parameter estimates for each dif.group
      g1 <- mirt::coef(dif.analysis$IRT$dif.mod, IRTpars = TRUE)[[1]][c(biased.items)]
      g1 <- lapply(g1, function(x) x[, 2]) # extracts b parameter (or first threshold)
      g1.df <- as.data.frame(Reduce(rbind, g1)) # single column

      g2 <- mirt::coef(dif.analysis$IRT$dif.mod, IRTpars = TRUE)[[2]][c(biased.items)]
      g2 <- lapply(g2, function(x) x[, 2]) # extracts b parameter (or first threshold)
      g2.df <- as.data.frame(Reduce(rbind, g2))

      # alternative parameter extraction, especially for using 2nd threshold
      # g1 <- lapply(g1, function(x) x[,grepl("b$|b2", dimnames(x)[[2]])])

      # calculate difference in b (difficulty) parameter
      b.diff <- g1.df[[1]] - g2.df[[1]]

      toward1 <- length(b.diff[b.diff < 0])
      toward2 <- length(b.diff[b.diff > 0])

    }
  }

  # For the unconditional effects
  if(is.null(tx.group)){ # tx indicator is pulled from dif.analysis rather than provided

    uncond.effects <- effect_robustness(scale.data = inputs$data,
                                     dif.group = inputs$dif.group,
                                     biased.items = biased.items,
                                     no.var.items = inputs$no.var.items,
                                     no.var.by.group.items = inputs$no.var.by.group.items,
                                     poly = inputs$poly.items,
                                     no.dif.mod = dif.analysis$IRT$no.dif.mod,
                                     irt.scoring = irt.scoring,
                                     clusters = clusters)

  } else { # For conditional effects

    ## Need tx.group to be a factor with only 2 levels
    if(is.factor(tx.group) == FALSE){
      tx.group <- factor(tx.group)
    }

    if(length(levels(tx.group)) != 2){
      stop("tx.group must contain only two levels")
    }

    # levels of the condition variable (currently intended to be treatment condition)
    tx.group1 <- levels(tx.group)[[1]]
    tx.group2 <- levels(tx.group)[[2]]
    tx.group.levs <- paste(tx.group1, tx.group2, sep = ", ") # for printing in report


    ## Treatment effect subset by dif.group1
    cond.effects1 <- effect_robustness(scale.data = inputs$data[inputs$dif.group == dif.group1, ],
                                    dif.group = tx.group[inputs$dif.group == dif.group1],
                                    biased.items = biased.items,
                                    no.var.items = inputs$no.var.items,
                                    no.var.by.group.items = inputs$no.var.by.group.items,
                                    poly = inputs$poly.items,
                                    no.dif.mod = NULL,
                                    irt.scoring = irt.scoring,
                                    clusters = clusters[inputs$dif.group == dif.group1])

    ## Treatment effect subset by dif.group2
    cond.effects2 <- effect_robustness(scale.data = inputs$data[inputs$dif.group == dif.group2, ],
                                    dif.group = tx.group[inputs$dif.group == dif.group2],
                                    biased.items = biased.items,
                                    no.var.items = inputs$no.var.items,
                                    no.var.by.group.items = inputs$no.var.by.group.items,
                                    poly = inputs$poly.items,
                                    no.dif.mod = NULL,
                                    irt.scoring = irt.scoring,
                                    clusters = clusters[inputs$dif.group == dif.group2])

    ## Interaction effects
    interaction.effects <- effect_robustness(scale.data = inputs$data,
                                          dif.group = inputs$dif.group,
                                          biased.items = biased.items,
                                          no.var.items = inputs$no.var.items,
                                          no.var.by.group.items = inputs$no.var.by.group.items,
                                          poly = inputs$poly.items,
                                          no.dif.mod = dif.analysis$IRT$no.dif.mod,
                                          irt.scoring = irt.scoring,
                                          tx.group = tx.group,
                                          clusters = clusters)

  }

  ## Creating test score plots
  # should test.plot be created when no biased items are present?
  if(bias.method == "IRT"& !is.null(biased.items)){
    score.plot <- bias_plot(dif.analysis = dif.analysis)
  } else {
    score.plot <- NULL
  }
# score.plot <- NULL

  ## Naming report and output file
  if(is.null(report.title)){

    report.title <- paste0("Measurement Bias Analysis of ", measure.name, " by ",
                           dif.group.name)

  }
  template <- system.file("rmd", "Bias-Correction-Report.Rmd", package = "WBdif")
  dir <- getwd()

  if(!grepl("\\.html", file.name)){
    file.name <- paste0(file.name, ".html")
  }


  ## running report
  rmarkdown::render(input = template,
                    output_file = file.name,
                    output_dir = dir)
}