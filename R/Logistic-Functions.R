#' Logistic regression DIF method
#'
#' Conducts DIF analysis with logistic regression
#'
#' @inheritParams dif_mh
#'
#' @details
#' First conducts an omnibus test of DIF by comparing fit of no DIF, uniform DIF, and non-uniform DIF logistic
#' regression models. The no DIF model regresses the dichotomous item responses from \code{item.data}
#' on item, \code{match.scores}, and the two-way interaction. The uniform DIF model adds \code{dif.groups}
#' and the interaction with item while the non-uniform model adds the three-way interaction. If DIF is
#' detected through the model comparisons, the specific item(s) with DIF are
#' identified in a two-stage process - initial detection stage and refinement stage.
#'
#' @return list containing
#' \itemize{
#'   \item DIF model comparisons
#'   \item item-level DIF tests
#'   \item integer vector of the items showing DIF (i.e., biased items)
#'   \item type of DIF
#'   }
#'
#' @export

dif_logistic <- function(item.data, dif.groups, match.type, match.scores){

  ## Number of items in the measure
  nitems <- ncol(item.data)

  ## match scores need to be a list for use in run_global_logistic
  if(match.type == "Rest"){

    match.scores <- match.scores

  } else if(match.type == "Total"){

    match.scores <- rep(list(match.scores), nitems)
  }

  #### Testing for uniform and non-uniform DIF across all items ####
  global.log <- run_global_logistic(item.data = item.data,
                                    dif.groups = dif.groups,
                                    match.scores = match.scores)


  if(global.log$dif.type %in% c("uniform", "non-uniform")){

    #### Testing each item for DIF ####

    ## Stage 1 - Initial DIF
    stage1 <- list()
    for(i in names(item.data)){

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

    #### Stage 2 - Refinement/purification of match.scores criterion ####
    # The items to exclude based on initial Logistic DIF analysis
    log.drops <- which(stage1.df$bias == 1)

    if(length(log.drops) > 0){

      # Storage
      stage2 <- list()

      if(match.type == "Rest"){

        for(i in 1:nitems){

          # Recalculate rest score while also removing biased items identified in stage 1
          match.scores2 <- sum_score(item.data = item.data, drops = c(i, log.drops))

          # filtering based on the item and adding re-calculated match score
          temp.df <- global.log$long.data[global.log$long.data$item == names(item.data)[[i]], ]
          temp.df$match.scores <- match.scores2

          stage2[[i]] <- run_item_logistic(item.data = temp.df,
                                           dif.type = global.log$dif.type)

        }

      } else if(match.type == "Total"){

        # Recalculate total score while removing biased items identified in stage 1
        match.scores2 <- sum_score(item.data = item.data, drops = c(log.drops))

        for(i in 1:nitems){

          # filtering based on the item and adding re-calculated total score
          temp.df <- global.log$long.data[global.log$long.data$item == names(item.data)[[i]], ]
          temp.df$match.scores <- match.scores2

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

#' Logistic Model Comparisons for DIF
#'
#' Conduct an omnibus test for DIF by comparing no DIF, uniform DIF, and non-uniform DIF logistic regression models
#'
#' @inheritParams dif_logistic
#' @return A list containing model comparison results table, the type of DIF, the model objects, and \code{data.frame} of item response and match.score information

run_global_logistic <- function(item.data, dif.groups, match.scores){

  #### Omnibus test for DIF  ####
  long.data <- data.frame(response = NA, match.scores = NA, item = NA, dif.groups = NA)

  ## Gather item information into a long format dataframe
  for (i in 1:ncol(item.data)) {

    long.temp <- data.frame(response = item.data[,i],
                            match.scores = match.scores[[i]],
                            item = rep(names(item.data)[i], nrow(item.data)),
                            dif.groups = as.numeric(dif.groups)-1)

    long.data <- rbind(long.data, long.temp)
  }


  long.data <- long.data[-1, ]  # removes first row which is all NA

  ## No DIF model, then adding grouping variable
  mod0 <- stats::glm(response ~ -1 + item*match.scores,
              data = long.data, family = binomial)    # No DIF
  mod1 <- stats::glm(response ~ -1 + item*match.scores + item*dif.groups,
              data = long.data, family = binomial)    # uniform DIF
  mod2 <- stats::glm(response ~ -1 + item*match.scores*dif.groups,
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

#' Single Item Logistic Models for DIF
#'
#' Internal function for dif_logistic
#'
#' @param item.data subset of long format data returned from \code{\link[DIFreport]{run_global_logistic}} for a single item
#' @param dif.type extracted from object returned from \code{\link[DIFreport]{run_global_logistic}}
#' @return a \code{data.frame} containing results of a likelihood ratio test

run_item_logistic <- function(item.data, dif.type){


  item.nodif <- stats::glm(response ~ 1 + match.scores,
                    data = item.data, family = binomial)

  if(dif.type == "uniform"){

    item.dif <- stats::glm(response ~ 1 + match.scores + dif.groups,
                    data = item.data, family = binomial)

    OR <- coef(item.dif)[grepl("dif.groups", names(coef(item.dif)))]
    OR <- exp(OR) # indicates direction of uniform bias
  }

  if(dif.type == "non-uniform"){

    item.dif <- stats::glm(response ~ 1 + match.scores + dif.groups + dif.groups:match.scores,
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