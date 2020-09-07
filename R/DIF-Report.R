# Options for bias.method are "MH", "logistic", and "IRT"
# irt.method is passed to mirt::fscores
# To request conditional treatment effects, provide DIF_analysis results using the conditional variable to dif.results argument,
# and the vector of treatment condition to conditional argument

#' Produces a report (via .Rmd) that summarizes. measurement bias analysis for a given
#' scale and given grouping variable
#'
#' @param dif.results desc
#' @param dataset.name desc
#' @param measure.name desc
#' @param comparison.name desc
#' @param bias.method desc
#' @param irt.method desc
#' @param conditional desc
#'
#' @return Uses the template file "Bias_Correction_Report.Rmd" to produce a report summarizing whether any items on `measure.name` are biased with respect to `comparison.name`, and, if so, to what extent this affects treatment contrasts on `measure.name`.
#'
#' @note If `conditional = NULL` the grouping variable given in `comparison.name` is taken to be the treatment constrast. If `conditional = some.variable` ... actually this is kind of complicated.
#' @export

dif_report <- function(dif.results,
                       dataset.name,
                       measure.name,
                       comparison.name = "Treatment Condition",
                       bias.method = "IRT", irt.method = "WLE",
                       conditional = NULL){

  ## Determining whether the bias.method is was used for the DIF analysis
  if(is.null(dif.results[[bias.method]])){

    stop(paste(bias.method, "DIF analysis was not found. Use the DIF_analysis function with", bias.method, "in the methods argument"))

  } else {

    bi <- extract_biased_items(dif.results)[[bias.method]]
    n.biased <- length(bi)

    # Kludge to run even without bi
    if(bi[1] == "No DIF was detected") {
      bi <- NULL
      n.biased <- 0
      nodifmessage <- paste0("No DIF was detected by ", bias.method, ". Therefore, robustness checks of treatment effects were not performed.")
    }
  }

  ## Gathering information for summary report
  all.inputs <- dif.results$inputs   # shortcut to make code easier to read

  n.items <- ncol(all.inputs$data)
  item_name_range <- paste(names(all.inputs$data)[[1]], "-",
                           names(all.inputs$data)[[n.items]])

  # Names of items with no variance
  nvi <- ifelse(length(all.inputs$no.var.items) !=0,
                paste("^a^", paste(names(all.inputs$no.var.items), collapse = ", ")),
                "none")

  # names of items with no within group variance
  nvgi <- ifelse(length(all.inputs$No_Var_by_Group_Items) !=0,
                 paste("^b^", paste(names(all.inputs$no.var.by.group.items), collapse = ", ")),
                 "none")

  # levels of the comparison variable
  comp1 <- levels(all.inputs$group)[[1]]
  comp2 <- levels(all.inputs$group)[[2]]
  comparison_levs <- paste(comp1, comp2, sep = ", ")

  # # Determining whether any biased items were identified. If not, run report without robustness check
  # if(is.character(bi)){
  #
  #  NoDIFmessage <- paste0(bi, " by ", bias.method, ". Therefore, a treatment effect robustness check was not performed.")
  #
  #   n.biased <- 0
  #   bias_type <- "none"
  #
  #   ## Naming output file and running report
  #   filename <- paste0(Dataset_Name, "-", Measure_Name, " by ", Comparison_Name, ".html")
  #   rmarkdown::render("Bias_Correction_Report.Rmd",
  #                     output_file = filename)
  #   } else {

  ## Gather info uniquely for each DIF method

  # Kludge to run even without bi
  if (is.null(bi)) {

    bias.type <- "NA"

  } else if(bias.method == "MH"){

    bias.type <- "uniform"

    mhtemp <- dif.results$mh$item.results

    toward1 <- nrow(mhtemp[mhtemp$Refined_bias == TRUE & mhtemp$Refined_OR < 1, ])
    toward2 <- nrow(mhtemp[mhtemp$Refined_bias == TRUE & mhtemp$Refined_OR > 1, ])

  } else if(bias.method == "logistic"){

    bias.type <- ifelse(dif.results$logistic$item.results[1, 3] == 1,
                        "uniform", "non-uniform")

  } else if(bias.method == "IRT"){

    bias.type <- ifelse(dif.results$IRT$item.results[1, 3] == 1,
                        "uniform", "non-uniform")
  }


  if(is.null(conditional)){ # For the unconditional effects

    uncond.effects <- get_robustness(scale.data = all.inputs$data,
                                     groupvec = all.inputs$group,
                                     biased.items = bi,
                                     no.var.items = c(all.inputs$no.var.items,
                                                      all.inputs$no.var.by.group.items),
                                     nodif.mod = dif.results$irt$nodif.mod,  # Automatically pulls from dif.results, but could make this an option
                                     irt.method = irt.method)

  } else if(length(levels(conditional)) == 2){

    # levels of the condition variable (currently intended to be treatment condition)
    cond1 <- levels(conditional)[[1]]
    cond2 <- levels(conditional)[[2]]
    cond.levs <- paste(cond1, cond2, sep = ", ") # combining levels for printing in report


    ## Treatment effect subset by comp1
    cond.effects1 <- get_robustness(scale.data = all.inputs$data[all.inputs$group == comp1, ],
                                    group = conditional[all.inputs$group == comp1],
                                    biased.items = bi,
                                    no.var.items = c(all.inputs$no.var.items,
                                                     all.inputs$no.var.by.group.items),
                                    nodif.mod = NULL,
                                    irt.method = irt.method)

    ## Treatment effect subset by comp2
    cond.effects2 <- get_robustness(scale.data = all.inputs$data[all.inputs$group == comp2, ],
                                    group = conditional[all.inputs$group == comp2],
                                    biased.items = bi,
                                    no.var.items = c(all.inputs$no.var.items,
                                                     all.inputs$no.var.by.group.items),
                                    nodif.mod = NULL,
                                    irt.method = irt.method)

    ## Calculating interaction
    cond1mat <- as.matrix(cond.effects1$effects.table[,c(2,3,4)]) # Converting numeric columns in dataframe to matrix
    cond2mat <- as.matrix(cond.effects2$effects.table[,c(2,3,4)])
    cediff <- as.data.frame(cond1mat - cond2mat)                   # Calculating the difference
    InteractionEffects <- cbind(data.frame(Item = cond.effects1$effects.table$Items),
                                cediff) # converting back to dataframe


  } else {
    stop("conditional must be a factor with two levels")
  }

  ## Naming output file and running report
  filename <- paste0(dataset.name, "-", measure.name, " by ", comparison.name, ".html")
  rmarkdown::render("Bias_Correction_Report.Rmd",
                    output_file = filename)
  #}
}