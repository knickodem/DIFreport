#' Logistic regression DIF method
#'
#' Conducts DIF analysis with logistic regression
#'
#' @param scale.data data frame of dichotomous item responses with subjects in rows
#' and items in columns
#' @param dif.group factor vector of group membership for which DIF is evaluated.
#' @param score.type character indicating whether \code{match} is a
#' total summed score ("Total") or the summed score excluding the item under
#' investigation ("Rest").
#' @param match list of \code{ncol(scale.data)} elements. Each element is a numeric vector
#' of match scores used as a predictor in the logistic regression.
#' In \code{dif_logistic}, if \code{score.type} = "Total", \code{match} will be
#' a single numeric vector.
#' @param item.data dataframe created in \code{run_global_logistic}
#' @param dif.type character indicating "No DIF", "uniform" DIF or "non-uniform" DIF as
#' determined in \code{run_global_logistic}.
#'
#' @details
#' \code{run_global_logistic} compares no DIF, uniform DIF, and non-uniform DIF logistic
#' regression models by regressing dichotomous item responses from \code{scale.data}
#' on item, \code{match}, and \code{dif.group}, along with interactions. If DIF is
#' detected through the model comparisons, the specific item(s) with DIF are
#' identified via \code{run_item_logistic}. \code{dif_logistic} is a wrapper
#' around the other functions that organizes the initial and refinement phases of
#' the DIF analysis and compiles the results.
#'
#' @return a four-element list containing 1) DIF model comparisons,
#' 2) item-level DIF tests, 3) integer vector of the items showing DIF
#' (i.e., biased items), and 4) type of DIF
#'
#' @export

dif_logistic <- function(scale.data, dif.group, score.type, match){

  ## Number of items in the measure
  nitems <- ncol(scale.data)

  ## match scores need to be a list for use in run_global_logistic
  if(score.type == "Rest"){

    match <- match

  } else if(score.type == "Total"){

    match <- rep(list(match), nitems)
  }

  #### Testing for uniform and non-uniform DIF across all items ####
  global.log <- run_global_logistic(scale.data = scale.data,
                                    dif.group = dif.group,
                                    match = match)


  if(global.log$dif.type %in% c("uniform", "non-uniform")){

    #### Testing each item for DIF ####

    ## Stage 1 - Initial DIF
    stage1 <- list()
    for(i in names(scale.data)){

      stage1[[i]] <- run_item_logistic(
        item.data = global.log$long.data[global.log$long.data$item == i, ],
        dif.type = global.log$dif.type
      )
    }

    # dataframe which will be output in report
    stage1.df <- Reduce(rbind, stage1)

    # Benjamini–Hochberg procedure for false discovery rate < 5%
    names(stage1.df)[names(stage1.df == "pvalue")] <- "p"
    stage1.df$bias <- stats::p.adjust(stage1.df$p, method = "BH") < .05

    #### Stage 2 - Refinement/purification of match_on criterion ####
    # The items to exclude based on initial Logistic DIF analysis
    log.drops <- which(stage1.df$bias == 1)

    if(length(log.drops) > 0){

      # Storage
      stage2 <- list()

      if(score.type == "Rest"){

        for(i in 1:nitems){

          # Recalculate rest score while also removing biased items identified in stage 1
          match2 <- sum_score(scale.data = scale.data, drops = c(i, log.drops))

          # filtering based on the item and adding re-calculated match score
          temp.df <- global.log$long.data[global.log$long.data$item == names(scale.data)[[i]], ]
          temp.df$match <- match2

          stage2[[i]] <- run_item_logistic(item.data = temp.df,
                                           dif.type = global.log$dif.type)

        }

      } else if(score.type == "Total"){

        # Recalculate total score while removing biased items identified in stage 1
        match2 <- sum_score(scale.data = scale.data, drops = c(log.drops))

        for(i in 1:nitems){

          # filtering based on the item and adding re-calculated total score
          temp.df <- global.log$long.data[global.log$long.data$item == names(scale.data)[[i]], ]
          temp.df$match <- match2

          stage2[[i]] <- run_item_logistic(item.data = temp.df,
                                           dif.type = global.log$dif.type)
        }
      }

      # dataframe which will be output in report
      stage2.df <- Reduce(rbind, stage2)

      # Benjamini–Hochberg procedure for false discovery rate < 5%
      names(stage2.df)[names(stage2.df == "pvalue")] <- "p"
      stage2.df$bias <- stats::p.adjust(stage2.df$p, method = "BH") < .05


      #### Output dataframe combining stage 1 and stage 2 ####
      names(stage1.df)[-c(1)] <- paste0("initial.", names(stage1.df)[-c(1)])
      names(stage2.df)[-c(1)] <- paste0("refined.", names(stage2.df)[-c(1)])
      item.log <- cbind(stage1.df, stage2.df[,-c(1:2)])

      logistic <- list(global.level = global.log$model.comparison,
                       item.level = item.log,
                       biased.items = which(stage2.df$refined.bias == 1),
                       dif.type = global.log$dif.type)
    } else {

      logistic <- list(global.level = global.log$model.comparison,
                       item.level = stage1.df,
                       biased.items = "No DIF was detected",
                       dif.type = global.log$dif.type)
    }

  } else {

    logistic <- list(global.level = global.log$model.comparison,
                     item.level = "No DIF was detected through model comparisons",
                     biased.items = "No DIF was detected",
                     dif.type = global.log$dif.type)
  }

  return(logistic)

}


#' @rdname dif_logistic
#' @export

run_global_logistic <- function(scale.data, dif.group, match){

  #### Omnibus test for DIF  ####
  long.data <- data.frame(response = NA, match = NA, item = NA, dif.group = NA)

  ## Gather item information into a long format dataframe
  for (i in 1:ncol(scale.data)) {

    long.temp <- data.frame(response = scale.data[,i],
                            match = match[[i]],
                            item = rep(names(scale.data)[i], nrow(scale.data)),
                            dif.group = as.numeric(dif.group)-1)

    long.data <- rbind(long.data, long.temp)
  }


  long.data <- long.data[-1, ]  # removes first row which is all NA

  ## No DIF model, then adding grouping variable
  mod0 <- stats::glm(response ~ -1 + item + match:item,
              data = long.data, family = binomial)    # No DIF
  mod1 <- stats::glm(response ~ -1 + item + match:item + dif.group:item,
              data = long.data, family = binomial)    # uniform DIF
  mod2 <- stats::glm(response ~ -1 + item + match:item + dif.group:item + dif.group:match:item,
              data = long.data, family = binomial)    # nonuniform DIF

  ## Omnibus test for any DIF
  model.comparison <- stats::anova(mod0, mod1, mod2, test = "LRT")
  model.comparison$model <- c("No DIF", "Uniform", "Non-uniform")
  names(model.comparison) <- c("resid.df", "resid.dev", "df", "chisq", "p", "model")
  model.comparison <- model.comparison[,c(6, 2:5)]


  ## Do we need to test for DIF at the item-level?
  if(model.comparison$p[[3]] < .05) {

    dif.type <- c("non-uniform")

  } else if(model.comparison$p[[2]] < .05) {

    dif.type <- c("uniform")

  } else {

    dif.type <- c("No DIF detected")

  }

  global.log <- list(model.comparison = model.comparison,
                     dif.type = dif.type,
                     no.dif.mod = mod0,
                     uniform.mod = mod1,
                     nonuniform.mod = mod2,
                     long.data = long.data)

  return(global.log)


}

#' @rdname dif_logistic
#' @export

run_item_logistic <- function(item.data, dif.type){


  item.nodif <- stats::glm(response ~ 1 + match,
                    data = item.data, family = binomial)

  if(dif.type == "uniform"){

    item.dif <- stats::glm(response ~ 1 + match + dif.group,
                    data = item.data, family = binomial)

    OR <- coef(item.dif)[grepl("dif.group", names(coef(item.dif)))]
    OR <- exp(OR) # indicates direction of uniform bias
  }

  if(dif.type == "non-uniform"){

    item.dif <- stats::glm(response ~ 1 + match + dif.group + dif.group:match,
                    data = item.data, family = binomial)
  }

  modcomp <- stats::anova(item.nodif, item.dif, test = "LRT")

  if(dif.type == "uniform"){

    item.log <- data.frame(item = item.data$item[[1]],
                           OR = OR,
                           deviance = modcomp$Deviance[-1],
                           df = modcomp$Df[-1],
                           p = modcomp$`Pr(>Chi)`[-1])

  } else {

    item.log <- data.frame(item = item.data$item[[1]],
                           deviance = modcomp$Deviance[-1],
                           df = modcomp$Df[-1],
                           p = modcomp$`Pr(>Chi)`[-1])
  }

  return(item.log)

}