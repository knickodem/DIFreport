#' Generate report of DIF analysis
#'
#' Produces a report (via .Rmd) that summarizes measurement bias analysis for a given
#' scale and grouping variable
#'
#' @param dif.analysis an object returned from `dif_analysis`
#' @param dataset.name character; name of the dataset where the measure and
#' item responses came from. This is printed in the report title and summary.
#' For World Bank data, this will typically be a country.
#' @param measure.name character; name of the measure being evaluated for DIF.
#' This is printed in the report title and summary.
#' @param dif.group.name character; name of the group used in the DIF analysis.
#' @param bias.method options are "MH", "logistic", or "IRT" (default).
#' Which method in the `dif.analysis` object should the biased items, if detected, be
#' extracted?
#' @param irt.scoring factor score estimation method, which is passed to [mirt::fscores()].
#' Default is "WLE". See [mirt::fscores()] documentation for other options.
#' @param tx.group When `NULL` (default) unconditional treatment effects are assessed
#' for robustness with the assumption that the treatment indicator was used as the
#' grouping variable in `dif_analysis`. If treatment indicator was not the grouping
#' variable in `dif_analysis`, the factor vector for treatment indicator needs to be
#' supplied here. Robustness of the conditional treatment effects is then assessed.
#'
#' @return Uses the template "Bias_Correction_Report.Rmd" to produce a
#' report summarizing whether any items on `measure.name` are biased with
#' respect to `dif.group.name`, and, if so, to what extent this affects
#' treatment contrasts on `measure.name`.
#'
#' @export

dif_report <- function(dif.analysis,
                       dataset.name,
                       measure.name,
                       dif.group.name,
                       bias.method = "IRT",
                       irt.scoring = "WLE",
                       tx.group = NULL){

  ## Determining whether the bias.method is was used for the DIF analysis
  if(is.null(dif.analysis[[bias.method]])){

    stop(paste(bias.method, "DIF analysis was not found. Use the dif_analysis function
               with", bias.method, "in the methods argument"))

  }

  # shortcut to make code easier to read
  all.inputs <- dif.analysis$inputs

  ## Gathering biased item information for summary report
  # extracting biased items from each method
  bi.list <- extract_bi(dif.analysis)

  # obtaining item names and putting in a table for report
  bi.df <- lapply(bi.list,
                  function(x){names(all.inputs$data)[x]})
  bi.df <- stack(bi.df)[c(2,1)]
  names(bi.df) <- c("method", "biased.items")

  # If no biased items
  if(bi.list[[bias.method]][1] == "No DIF was detected") {

    n.biased <- 0
    nodifmessage <- paste0("No DIF was detected by ", bias.method, ". Therefore,
                             robustness checks of treatment effects were not performed.")
  } else {

    # biased items for selected detection method
    bi <- bi.list[[bias.method]]
    n.biased <- length(bi.list[[bias.method]])

  }

  ## Gathering input information for summary report
  # item info to print
  n.items <- ncol(all.inputs$data)
  item.name.range <- paste(names(all.inputs$data)[[1]], "-",
                           names(all.inputs$data)[[n.items]])

  # Names of items with no variance
  nvi <- ifelse(length(all.inputs$no.var.items) !=0,
                paste("^a^", paste(names(all.inputs$no.var.items), collapse = ", ")),
                "none")

  # names of items with no within group variance
  nvgi <- ifelse(length(all.inputs$no.var.by.group.items) !=0,
                 paste("^b^", paste(names(all.inputs$no.var.by.group.items),
                                    collapse = ", ")),
                 "none")

  # levels of the dif.group variable
  dif.group1 <- levels(all.inputs$dif.group)[[1]]
  dif.group2 <- levels(all.inputs$dif.group)[[2]]
  dif.group.levs <- paste(dif.group1, dif.group2, sep = ", ")

  # type of dif
  dif.type <- dif.analysis[[bias.method]]$dif.type

  ## Gathering directionality If uniform dif
  if (dif.type == "uniform"){
    if(bias.method %in% c("MH", "logistic")){

      uni.temp <- dif.analysis[[bias.method]]$item.level

      toward1 <- nrow(uni.temp[uni.temp$refined.bias == TRUE & uni.temp$refined.OR < 1, ])
      toward2 <- nrow(uni.temp[uni.temp$refined.bias == TRUE & uni.temp$refined.OR > 1, ])

    } else if(bias.method == "IRT"){

      ## extract biased item parameter estimates for each dif.group
      ## from global.irt uniform.mod
      g1 <- coef(dif.analysis$irt$uniform.mod)[[1]][bi]
      g1.df <- as.data.frame(Reduce(rbind, g1))

      g2 <- coef(dif.analysis$irt$uniform.mod)[[2]][bi]
      g2.df <- as.data.frame(Reduce(rbind, g2))

      # calculate difference in d parameter
      irt.direct <- data.frame(item = names(g1),
                               diff = g2.df$d - g1.df$d)

      toward1 <- nrow(irt.direct$diff > 0)
      toward1 <- nrow(irt.direct$diff < 0)

    }
  }

  ## Should robustness checks be run?
  if(n.biased > 0){
    # For the unconditional effects
    if(is.null(tx.group)){ # tx indicator is pulled from dif.analysis rather than provided

      uncond.effects <- get_robustness(scale.data = all.inputs$data,
                                       dif.group = all.inputs$dif.group,
                                       biased.items = bi,
                                       no.var.items = c(all.inputs$no.var.items,
                                                        all.inputs$no.var.by.group.items),
                                       no.dif.mod = dif.analysis$irt$no.dif.mod,
                                       irt.scoring = irt.scoring)

    } else if(length(levels(tx.group)) == 2){

      # levels of the condition variable (currently intended to be treatment condition)
      tx.group1 <- levels(tx.group)[[1]]
      tx.group2 <- levels(tx.group)[[2]]
      tx.group.levs <- paste(tx.group1, tx.group2, sep = ", ") # for printing in report


      ## Treatment effect subset by tx.group1
      cond.effects1 <- get_robustness(scale.data = all.inputs$data[all.inputs$dif.group == tx.group1, ],
                                      dif.group = all.inputs$dif.group[all.inputs$dif.group == tx.group1],
                                      biased.items = bi,
                                      no.var.items = c(all.inputs$no.var.items,
                                                       all.inputs$no.var.by.group.items),
                                      no.dif.mod = NULL,
                                      irt.scoring = irt.scoring)

      ## Treatment effect subset by comp2
      cond.effects2 <- get_robustness(scale.data = all.inputs$data[all.inputs$dif.group == tx.group2, ],
                                      dif.group = all.inputs$dif.group[all.inputs$dif.group == tx.group2],
                                      biased.items = bi,
                                      no.var.items = c(all.inputs$no.var.items,
                                                       all.inputs$no.var.by.group.items),
                                      no.dif.mod = NULL,
                                      irt.scoring = irt.scoring)

      ## Calculating interaction - STILL NEEDS UPDATING
      cond1mat <- as.matrix(cond.effects1$effects.table[,c(2,3,4)]) # Converting numeric columns in dataframe to matrix
      cond2mat <- as.matrix(cond.effects2$effects.table[,c(2,3,4)])
      cediff <- as.data.frame(cond1mat - cond2mat)                   # Calculating the difference
      interaction.effects <- cbind(data.frame(Item = cond.effects1$effects.table$Items),
                                  cediff) # converting back to dataframe


    } else {
      stop("conditional must be a factor with two levels")
    }
  }

  ## Naming output file and running report
  template <- system.file("Bias-Correction-Report.Rmd", package = "WB")
  dir <- getwd()
  filename <- paste0(dataset.name, "-", measure.name, " by ", dif.group.name, ".html")
  rmarkdown::render(template,
                    output_file = filename,
                    output_dir = dir)
}
