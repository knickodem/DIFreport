#' Item response theory DIF method
#'
#' Conducts DIF analysis in an item response theory framework using the iterative Wald procedure (Cao et al., 2017; Tay et al., 2015; Woods et al., 2013)
#'
#' @param item.data data frame of item responses with subjects in rows and items in columns
#' @param dif.groups factor vector of group membership for which DIF is evaluated.
#' @inheritParams dif_analysis
#'
#' @details
#' The process begins by conducting an omnibus test of DIF by comparing the fit of no DIF, uniform DIF,
#' and non-uniform DIF models. All models are run with \code{\link[mirt]{multipleGroup}}.
#' Specifically, in step 1, the no DIF model is run with parameters estimated freely between the \code{dif.groups}.
#' The latent trait mean and variance for the focal group are saved. In step 2, the uniform DIF model is estimated
#' while constraining the latent trait parameters for the focal group to those in step 1 and constraining
#' the item slopes to be equal across \code{dif.groups}. The non-uniform DIF model is then estimated by
#' adding equality constraints to the item intercepts or thresholds. Model fit is compared between the
#' No DIF and uniform DIF, and between the uniform DIF and non-uniform DIF models with a likelihood ratio test.
#' If no DIF is detected through the model comparisons, the process concludes. Otherwise, the specific item(s)
#' with DIF are identified in steps 3 and 4 using \code{\link[mirt]{DIF}}. This two-step process is sometimes
#' referred to as the initial and refinement steps for identifying anchor items and items with DIF.
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
#' @references
#' Cao, M., Tay, L., & Liu, Y. (2017). A Monte Carlo study of an iterative Wald test procedure for DIF analysis. *Educational and Psychological Measurement, 77*(1), 104-118. doi:10.1177/0013164416637104
#' Tay, L., Meade, A. W., & Cao, M. (2015). An overview and practical guide to IRT measurement equivalence analysis. *Organizational Research Methods, 18*(1), 3-46. doi:10.1177/1094428114553062
#' Woods, C. M., Cai, L., & Wang, M. (2013). The Langer-improved Wald test for DIF testing with multiple groups: Evaluation and comparison to two-group IRT. *Educational and Psychological Measurement, 73*(3), 532-547. doi:10.1177/0013164412464875
#'
#' @import mirt
#' @export

dif_irt <- function(item.data, dif.groups, item.type, method, Wald){

  #### Comparing no dif, uniform dif, and nonuniform dif models ####
  global.irt <- tryCatch(expr = {
    run_global_irt(item.data = item.data, dif.groups = dif.groups, item.type = item.type)
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

    ## If DIF was detected in the global tests
    if(length(global.irt$dif.params) > 0){

      ## Number of items in the measure
      nitems <- ncol(item.data)

      ## Stage 1 - Initial DIF
      stage1 <- lapply(1:nitems, run_item_irt,
                       global.irt = global.irt,
                       which.model = "nonuniform.mod")
      # if we want more flexibility in the code, might need to use a for loop instead

      # convert list to df
      stage1.df <- Reduce(rbind, stage1)

      # Benjamini–Hochberg procedure for false discovery rate < 5%
      stage1.df$adj.p <- stats::p.adjust(stage1.df$p, method = "BH")
      stage1.df$bias <- stage1.df$adj.p < .05

      # The items to free in stage 2
      irt.free <- which(stage1.df$bias == 1)

      # Extracting X2 test results from initial run for output
      initial.df <- cbind(rownames(stage1.df),
                          data.frame(stage1.df[,names(stage1.df) %in% c("X2", "df", "p", "adj.p", "bias")], row.names = NULL))
      names(initial.df) <- c("item", "chisq", "df", "p", "adj.p", "bias")


      if(length(irt.free) > 0){
        names(initial.df)[2:6] <- paste0("initial.", names(initial.df)[2:6])

        ## Stage 2 - Refine/Purify
        # Re-estimate no dif model while freeing IRT_free items
        global.irt$no.dif.mod2 <- mirt::multipleGroup(item.data, model = 1,
                                                      group = dif.groups,
                                                      itemtype = item.type,
                                                      invariance = c('free_var','free_means',
                                                                     names(item.data)[-irt.free]))


        stage2 <- lapply(c(1:nitems)[-irt.free], run_item_irt,
                         global.irt = global.irt,
                         which.model = "no.dif.mod2")
        # if we want more flexiblity in the code, might need to use a for loop instead

        # convert list to df
        stage2.df <- Reduce(rbind, stage2)

        # Benjamini–Hochberg procedure for false discovery rate < 5%
        stage2.df$adj.p <- stats::p.adjust(stage2.df$p, method = "BH")
        stage2.df$bias <- stage2.df$adj.p < .05

        ## Extracting X2 test results from refined run for output
        refined.df <- cbind(rownames(stage2.df),
                            data.frame(stage2.df[,names(stage1.df) %in% c("X2", "df", "p", "adj.p", "bias")], row.names = NULL))
        names(refined.df) <- c("item", "refined.chisq",
                               "refined.df", "refined.p", "refined.adj.p", "refined.bias")

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
#' @inheritParams dif_irt
#'
#' @return A list containing model comparison results table, the type of DIF, and the model objects
#'
#' @noRd

run_global_irt <- function(item.data, dif.groups, item.type, method = "MHRM"){

  ## Step 1 syntax and model
  s1.syn <- paste0("F1 = 1-", length(item.data))
  no.dif.mod <- mirt::multipleGroup(item.data, model = s1.syn, itemtype = item.type, group = dif.groups,
                                    method = method, SE = T,
                                    invariance = c('slopes', 'intercepts',
                                                   'free_var','free_means'))

  ## extracting parameter values and generating syntax for step 2
  # save latent trait estimates for comparison group
  comp.pars <- coef(no.dif.mod)[[2]] # assumes only 2 groups

  # get starting values for step 2 model
  start.pars <- mirt::multipleGroup(item.data, model = s1.syn, itemtype = item.type,
                                    group = dif.groups, pars="values")
  npars <- nrow(start.pars) # number of parameters

  # replacing mean and variance values for comparison group with Step 1 values
  start.pars[c(npars-1, npars), "value"] <- c(comp.pars$GroupPars[1,]) # assumes parameters are the last two in the table

  # Step 2 model - fixes latent trait parameters
  s2.syn  <- paste0(s1.syn, "\nFIXED = (GROUP, MEAN_1), (GROUP, COV_11)")

  ## Running step 2 models
  # checking for uniform DIF
  uniform.mod <- mirt::multipleGroup(item.data, model = s2.syn, itemtype = item.type,
                                     group = dif.groups, invariance = c('slopes'), # 'free_var'
                                     method = method, SE = T, pars = start.pars)

  # checking for any DIF
  nonuniform.mod <- mirt::multipleGroup(item.data, model = s2.syn, itemtype = item.type,
                                        group = dif.groups, method = method, SE = T, pars = start.pars)

  ## Model comparisons
  uniform.tab <- mirt::anova(no.dif.mod, uniform.mod, verbose = FALSE)
  nonunif.tab <- mirt::anova(no.dif.mod, nonuniform.mod, verbose = FALSE)

  ## Extracting comparison results
  tab <- rbind(uniform.tab, nonunif.tab[2,])
  tab$Model <- c("No DIF", "Uniform", "Non-uniform")
  tab <- tab[,c("Model", "AIC", "SABIC", "logLik", "X2", "df", "p")]
  row.names(tab) <- NULL


  ## Do we need to test for DIF at the item-level, and if so, which parameter?
  if(tab$p[[2]] < .05) { # may need to change this if nonunif.tab uses no.dif.mod as comparison model

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
#' @param global.irt an object returned from \code{\link[DIFreport]{run_global_irt}}
#' @param which.model the model in \code{global.irt} to use for testing DIF. Options
#' are \code{"no.dif.mod"} (default), \code{"uniform.mod"} or \code{"nonuniform.mod"}.
#' @param items2test numeric; the item to test for DIF, which is passed to the
#' items2test argument of \code{\link[mirt]{DIF}}.
#'
#' @return an object returned from \code{\link[mirt]{DIF}}

run_item_irt <- function(global.irt,
                         which.model = "nonuniform.mod",
                         items2test,    # need to loop each item rather than 1:nitems in
                         scheme = "add",
                         Wald = TRUE){
  # order to get all the output we need

  # identify if item is dichotomous or polytomous
  itemtype <- mirt::extract.mirt(global.irt[[which.model]], "itemtype")[[items2test]]

  # need to expand itemtype to all models with multiple thresholds
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
                        scheme = scheme, #seq_stat = .05, max_run = 2,
                        Wald = Wald, # can only be TRUE if scheme = "add"
                        return_models = FALSE) # , p.adjust = "BH", ...

  return(item.irt)
}