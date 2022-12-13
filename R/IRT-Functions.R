#' Item response theory DIF method
#'
#' Conducts DIF analysis using an item response theory approach
#'
#' @param item.data data frame of item responses with subjects in rows and items in columns
#' @param dif.group.id factor vector of group membership for which DIF is evaluated.
#'
#' @details
#' First conducts an omnibus test of DIF by comparing the fit of no DIF, uniform DIF, and non-uniform DIF 2PL IRT models.
#' The models are run \code{\link[mirt]{multipleGroup}} by constraining slopes
#' and intercepts, slopes only, and nothing, respectively, to be equal
#' between the levels of \code{dif.group.id}. Model fit is compared with a
#' likelihood ratio test. If DIF is detected through the model comparisons, the
#' specific item(s) with DIF are identified in a two-stage process (initial and refinement) using \code{\link[mirt]{DIF}}.
#'
#' @return A list containing
#' \itemize{
#' \item DIF model comparisons
#' \item item-level DIF tests
#' \item integer vector of the items showing DIF (i.e., biased items)
#' \item type of DIF
#' \item IRT models needed for treatment effect robustness check
#' }
#'
#' @export

dif_irt <- function(item.data, dif.group.id){

  #### Comparing no dif, uniform dif, and nonuniform dif models ####
  global.irt <- tryCatch(expr = {
    run_global_irt(item.data = item.data, dif.group.id = dif.group.id)
  },
  error = function(e){
    message("Did not run IRT method. Possible empty cell(s) in the item by group
            frequency tables. If so, remove the item from measure.data")
    warning(paste("IRT", e))
    return(NULL)}
  )

  if(is.null(global.irt)){

    irt <- NULL

  } else {

    nitems <- 1:ncol(item.data)

    ## If DIF was detected in the global tests
    if(length(global.irt$dif.params) > 0){

      ## Stage 1 - Initial DIF
      stage1 <- lapply(nitems, run_item_irt, global.irt = global.irt, which.model = "no.dif.mod")
      # if we want more flexibility in the code, might need to use a for loop instead

      # convert list to df
      stage1.df <- Reduce(rbind, stage1)

      # Benjamini–Hochberg procedure for false discovery rate < 5%
      stage1.df$bias <- stats::p.adjust(stage1.df$p, method = "BH") < .05

      # The items to free in stage 2
      irt.free <- which(stage1.df$bias == 1)

      # Extracting X2 test results from initial run for output
      initial.df <- cbind(rownames(stage1.df),
                          data.frame(stage1.df[,6:9], row.names = NULL))
      names(initial.df) <- c("item", "chisq", "df", "p", "bias")


      if(length(irt.free) > 0){
        names(initial.df)[2:5] <- paste0("initial.", names(initial.df)[2:5])

        ## Stage 2 - Refine/Purify
        # Re-estimate no dif model while freeing IRT_free items
        global.irt$no.dif.mod2 <- mirt::multipleGroup(item.data, model = 1,
                                                      group = dif.group.id,
                                                      invariance = c('free_var','free_means',
                                                                     names(item.data)[-irt.free]))


        stage2 <- lapply(nitems[-irt.free], run_item_irt,
                         global.irt = global.irt,
                         which.model = "no.dif.mod2")
        # if we want more flexiblity in the code, might need to use a for loop instead

        # convert list to df
        stage2.df <- Reduce(rbind, stage2)

        # Benjamini–Hochberg procedure for false discovery rate < 5%
        stage2.df$bias <- stats::p.adjust(stage2.df$p, method = "BH") < .05

        ## Extracting X2 test results from refined run for output
        refined.df <- cbind(rownames(stage2.df),
                            data.frame(stage2.df[,6:9], row.names = NULL))
        names(refined.df) <- c("item", "refined.chisq",
                               "refined.df", "refined.p", "refined.bias")

        ## Combining initial and refined output
        item.irt <- merge(x = initial.df,
                          y = refined.df,
                          by = "item",
                          all.x = TRUE,
                          all.y = FALSE)

        ## Ordering items
        item.irt$item <- factor(item.irt$item, levels = names(item.data))
        item.irt <- item.irt[order(item.irt$item), ]
        row.names(item.irt) <- NULL
        biased.items <- which(item.irt$refined.bias == TRUE | is.na(item.irt$refined.bias))

        # returning the uniform or non-uniform dif mod for creating test score bias plots
        if(global.irt$dif.type == "uniform"){
          dif.mod <- global.irt$uniform.mod
        } else if (global.irt$dif.type == "non-uniform"){
          dif.mod <- global.irt$nonuniform.mod
        }

        ## Merging initial and refined stage results
        irt <- list(global.level = global.irt$model.comparison,
                    item.level = item.irt,
                    biased.items = biased.items,
                    dif.type = global.irt$dif.type,
                    no.dif.mod = global.irt$no.dif.mod,
                    dif.mod = dif.mod)

      } else {

        initial.df$item <- factor(initial.df$item, levels = names(item.data))
        initial.df <- initial.df[order(initial.df$item), ]
        row.names(initial.df) <- NULL

        irt <- list(global.level = global.irt$model.comparison,
                    item.level = initial.df,
                    biased.items = "No DIF was detected",
                    dif.type = global.irt$dif.type,
                    no.dif.mod = global.irt$no.dif.mod)
        message("Global IRT model comparisons suggested DIF, but none was found
                through individual item comparisons.")
      }

    }  else {

      irt <- list(global.level = global.irt$model.comparison,
                  item.level = "No item DIF was detected through model comparisons",
                  biased.items = "No DIF was detected",
                  dif.type = global.irt$dif.type,
                  no.dif.mod = global.irt$no.dif.mod)
    }
  }

  return(irt)

}

#' IRT Model Comparisons for DIF
#'
#' Conduct an omnibus test for DIF by comparing no DIF, uniform DIF, and non-uniform DIF IRT models
#'
#' @param item.data data frame of item responses with subjects in rows
#' and items in columns
#' @param dif.group.id factor vector of group membership for which DIF is evaluated.
#' @return A list containing model comparison results table, the type of DIF, and the model objects

run_global_irt <- function(item.data, dif.group.id){

  ## Fitting nested models - Fit 2PL models with varying constraints
  nonuniform.mod <- mirt::multipleGroup(item.data, model = 1, group = dif.group.id, SE = F)

  uniform.mod <- mirt::multipleGroup(item.data, model = 1, group = dif.group.id,
                                     invariance = c('slopes', 'free_var'), SE = F)

  no.dif.mod <- mirt::multipleGroup(item.data, model = 1, group = dif.group.id,
                                    invariance = c('slopes', 'intercepts',
                                                   'free_var','free_means'), SE = F)

  ## Model comparisons
  uniform.tab <- mirt::anova(no.dif.mod, uniform.mod, verbose = FALSE)
  nonunif.tab <- mirt::anova(uniform.mod, nonuniform.mod, verbose = FALSE)

  ## Extracting comparison results
  tab <- rbind(uniform.tab, nonunif.tab[2,])
  tab$model <- c("No DIF", "Uniform", "Non-uniform")
  tab <- tab[,c("model", "logLik", "X2", "df", "p")]


  ## Do we need to test for DIF at the item-level, and if so, which parameter?
  if(tab$p[[3]] < .05) {

    dif.params <- c("a1", "d")
    dif.type <- "non-uniform"

  } else if(tab$p[[2]] < .05) {

    dif.params <- c("d")
    dif.type <- "uniform"

  } else {

    dif.params <- character()
    dif.type <- "No DIF detected"

  }

  ## Compiling results to output
  global.irt <- list(model.comparison = tab,
                     dif.params = dif.params,
                     dif.type = dif.type,
                     no.dif.mod = no.dif.mod,
                     uniform.mod = uniform.mod,
                     nonuniform.mod = nonuniform.mod)

  return(global.irt)

}


#' IRT item tests for DIF
#'
#' Test each item for DIF using IRT models
#'
#' @param global.irt an object returned from \code{\link[WBdif]{run_global_irt}}
#' @param which.model the model in \code{global.irt} to use for testing DIF. Options
#' are "no.dif.mod" (default), "uniform.mod" or "nonuniform.mod".
#' @param items2test numeric; the item to test for DIF, which is passed to the
#' items2test argument of \code{\link[mirt]{DIF}}.
#' @return an object returned from \code{\link[mirt]{DIF}}

run_item_irt <- function(global.irt,
                         which.model = "no.dif.mod",
                         items2test){ # need to loop each item rather than 1:nitems in
  # order to get all the output we need

  # identify if item is dichotomous or polytomous
  itemtype <- extract.mirt(global.irt[[which.model]], "itemtype")[[items2test]]

  if(itemtype == "graded" & ("d" %in% global.irt$dif.params)){

    # from coefs of group1 (arbitrary), isolate item, then extract dim2 names except a1
    thresh.params <- dimnames(coef(global.irt[[which.model]])[[1]][[items2test]])[[2]][-1]

    # add threshold parameters, then drop initial d parameter
    global.irt$dif.params <- c(global.irt$dif.params, thresh.params)
    global.irt$dif.params <- global.irt$dif.params[global.irt$dif.params != "d"]

  }

  ## Identifying items with DIF
  item.irt <- mirt::DIF(global.irt[[which.model]],
                        which.par = global.irt$dif.params,
                        items2test = items2test,
                        scheme = "drop", #seq_stat = .05, max_run = 2,
                        Wald = FALSE,
                        return_models = FALSE) # , p.adjust = "BH", ...

  return(item.irt)
}