#' Robustness of treatment effects
#'
#' Evaluates the robustness of treatment effects when DIF is detected
#'
#' @param scale.data data frame of dichotomous item responses with subjects in rows
#' and items in columns
#' @param dif.group factor vector of group membership for which DIF is evaluated
#' @param biased.items numeric vector; location of biased items in \code{scale.data}
#' @param no.var.items numeric vector; location of items in \code{scale.data} with 0 variance
#' @param no.dif.mod \code{MultipleGroupClass} object from \code{mirt} where parameters are
#' constrained to be equal between levels of \code{dif.group}
#' @param irt.scoring factor score estimation method, which is passed to the
#' \code{method} argument of \code{\link[mirt]{fscores}}
#' @param tx.group factor vector for treatment indicator
#' @param m1 mean for dif.group 1
#' @param m2 mean for dif.group 2
#' @param sd1 sd for dif.group 1
#' @param sd2 sd for dif.group 2
#' @param n1 sample size for dif.group 1
#' @param n2 sample size for dif.group 2
#' @param score vector of scale scores
#'
#' @details
#'
#' @return A two-item list containing 1) data.frame of treatment effect estimates
#' and 2) character string of the groups being compared
#'
#' @export

get_robustness <- function(scale.data,
                           dif.group,
                           biased.items = NULL,
                           no.var.items = numeric(),
                           no.dif.mod = NULL,
                           irt.scoring = "WLE",
                           tx.group = NULL){


  #### Using Total Score ####

  total <- sum_score(scale.data)
  delta.total <- smd_wrapper(score = total,
                             dif.group = dif.group,
                             tx.group = tx.group)
  r.total <- psy::cronbach(scale.data)$alpha

  # Kludge to run without biased items
  # (KN Note: In get_report, this function is only run when there are biased items.
  # So the statement currently always evaluates to FALSE)
  # if (is.null(biased.items)) {
  #   star <- total
  #   delta.star <- delta.total
  #   r.star <- r.total
  # } else {
    star <- sum_score(scale.data, drops = biased.items)
    delta.star <- smd_wrapper(score = star,
                              dif.group = dif.group,
                              tx.group = tx.group)
    r.star <- psy::cronbach(scale.data[,-c(biased.items)])$alpha
  # }


  #### Using IRT Score ####

  # Removing items with no variance, if they exist
  if(length(no.var.items) > 0){
    scale.data <- scale.data[-c(no.var.items)]
  }

  ## IRT model with parameters constrained to be equal between groups
  if(is.null(no.dif.mod)){

    no.dif.mod <- mirt::multipleGroup(scale.data, model = 1, group = dif.group,
                                     invariance = c('slopes', 'intercepts',
                                                    'free_var','free_means'))

  }
  ## IRT model with parameter constraints freed for biased.items

  if(is.null(biased.items)) {

    mod.bo <- no.dif.mod

  } else {

    mod.bo <- mirt::multipleGroup(scale.data, model = 1, group = dif.group,
                                  invariance = c(names(scale.data)[-biased.items],
                                                 'free_var','free_means'))
  }

  ## Calculating IRT scores
  theta.scalar <- mirt::fscores(no.dif.mod, method = irt.scoring)
  theta.bo <- mirt::fscores(mod.bo, method = irt.scoring)

  ## Calculating standardized mean differences
  delta.scalar <- smd_wrapper(score = theta.scalar,
                              dif.group = dif.group,
                              tx.group = tx.group)
  delta.bo <- smd_wrapper(score = theta.bo,
                          dif.group = dif.group,
                          tx.group = tx.group)


  effects.table <- data.frame(Items = c("All Items", "Bias Omitted"),
                              `IRT Delta` = c(delta.scalar, delta.bo),
                              `Scale Delta` = c(delta.total, delta.star),
                              `Adj. Delta` = c((delta.total / sqrt(r.total)),
                                               (delta.star / sqrt(r.star))),
                              Reliability = c(r.total, r.star),
                              check.names = FALSE)

  effects <- list(effects.table = effects.table,
                  comparison = paste(levels(dif.group)[[2]], "-", levels(dif.group)[[1]]))


  return(effects)

}


#### Calculate standardized mean difference from two independent or paired groups ####
## SD can be pooled by providing sd1 and sd2
calc_smd <- function(m1, m2, sd1, sd2 = NULL, n1, n2, sample = "ind"){


  # raw mean difference
  md <- (m2 - m1)

  # Use only SD from group 1 or an overall SD
  if(is.null(sd2)){

    sigma <- sd1

  } else {

    # sigma for independent groups
    if(sample == "ind"){

      sigmanum <- (n1 - 1) * (sd1^2) + (n2 - 1) * (sd2^2)
      sigmadenom <- (n1 + n2 - 2)
      sigma <- sqrt(sigmanum / sigmadenom)

    } else{

      # sigma for paired groups
      sigma <- sqrt((sd1^2 + sd2^2) / 2)

    }
  }

  # Calculating d
  d <- md / sigma

  return(d)
}

smd_wrapper <- function(score, dif.group, tx.group = NULL){

  # ## levels of the grouping variable
  # dif.group1 <- levels(dif.group)[1]
  # dif.group2 <- levels(dif.group)[2]
  #
  # if(!is.null(tx.group)){
  #
  #   ## levels of the treatment indicator
  #   tx.group1 <- levels(tx.group)[1]
  #   tx.group2 <- levels(tx.group)[2]
  #
  # }

  if(is.null(tx.group)){

  means <- tapply(score, dif.group, mean, na.rm = T)
  sds <- tapply(score, dif.group, sd, na.rm = T)


  delta <- calc_smd(m1 = means[[1]],
                  m2 = means[[2]],
                  sd1 = sds[[1]])
  } else {

    ## creates 2x2 tables (assuming tx.group and dif.group each have 2 levels)
    means <- tapply(score, list(tx.group, dif.group), mean, na.rm = T)
    sds <- tapply(score, list(tx.group, dif.group), sd, na.rm = T)
    ns <- tapply(score, list(tx.group, dif.group), length)

    # m = (M_{dif.groupTX} - M_{dif.groupControl}) * SD_{dif.groupControl}
    delta <- calc_smd(m1 = (means[[2]] - means[[1]]),# * sds[[1]],
                     m2 = (means[[4]] - means[[3]]),# * sds[[3]],
                     sd1 = sds[[1]],
                     sd2 = sds[[3]],
                     n1 = ns[[1]],
                     n2 = ns[[3]])
  }

  return(delta)
}