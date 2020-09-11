#### Use information produced from `DIF_analysis` to compare treatment effect estimates ####
# biased.items - vector of items in `scale.data` to exclude from score calculation; currently only accepts locations, not names
# IRT based deltas will be NaNs if any IRT score is +/- Inf

#' Robustness of treatment effects
#'
#' Evaluates the robustness of treatment effects when DIF is detected
#'
#' @param scale.data data frame of dichotomous item responses with subjects in rows
#' and items in columns
#' @param dif.group factor vector of group membership for which DIF is evaluated
#' @param biased.items numeric vector; location of biased items in `scale.data`
#' @param no.var.items numeric vector; location of items in `scale.data` with 0 variance
#' @param no.dif.mod `MultipleGroupClass` object from `mirt` where parameters are
#' constrained to be equal between levels of `dif.group`
#' @param irt.scoring factor score estimation method, which is passed to the
#' `method` argument of [mirt::fscores()]
#' @param m1 mean for dif.group 1
#' @param m2 mean for dif.group 2
#' @param sd1 sd for dif.group 1
#' @param sd2 sd for dif.group 2
#' @param n1 sample size for dif.group 1
#' @param n2 sample size for dif.group 2
#' @param sample currently set to "ind"; `dif.groups` treated as independent when
#' calculating the pooled variance
#' @param score vector of scale scores
#' @param interaction if `NULL` (default), the sd of the control (reference) group is
#' used in the denominator. Otherwise, must supply vector of deltas to calculate
#' interaction effect.
#'
#' @details
#'
#' @return
#'
#' @export

get_robustness <- function(scale.data,
                           dif.group,
                           biased.items = NULL,
                           no.var.items = numeric(),
                           no.dif.mod = NULL,
                           irt.scoring = "WLE"){


  #### Using Total Score ####

  total <- sum_score(scale.data)
  delta.total <- smd_wrapper(score = total, dif.group = dif.group)
  r.total <- psy::cronbach(scale.data)$alpha

  # Kludge to run without biased items
  # (KN Note: In get_report, this function is only run when there are biased items.
  # So the statement currently always evaluates to FALSE)
  if (is.null(biased.items)) {
    star <- total
    delta.star <- delta.total
    r.star <- r.total
  } else {
    star <- sum_score(scale.data, drops = biased.items)
    delta.star <- smd_wrapper(score = star, dif.group = dif.group)
    r.star <- psy::cronbach(scale.data[,-c(biased.items)])$alpha
  }


  #### Using IRT Score ####

  # Removing items with no variance, if they exist
  if(length(no.var.items) > 0){
    scale.data <- scale.data[-c(no.var.items)]
  }

  ## IRT model with parameters constrained to be equal between groups
  if(is.null(nodif.mod)){

    nodif.mod <- mirt::multipleGroup(scale.data, model = 1, group = dif.group,
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
  theta.scalar <- mirt::fscores(nodif.mod, method = irt.scoring)
  theta.bo <- mirt::fscores(mod.bo, method = irt.scoring)

  ## Calculating standardized mean differences
  delta.scalar <- smd_wrapper(score = theta.scalar, dif.group = dif.group)
  delta.bo <- smd_wrapper(score = theta.bo, dif.group = dif.group)


  effects.table <- data.frame(Items = c("All Items", "Bias Omitted"),
                          `IRT Delta` = c(delta.scalar$delta, delta.bo$delta),
                          `Scale Delta` = c(delta.total$delta, delta.star$delta),
                          `Adj. Delta` = c((delta.total$delta / sqrt(r.total)), (delta.star$delta / sqrt(r.star))),
                          Reliability = c(r.total, r.star),
                          check.names = FALSE)

  effects <- list(effects.table = effects.table,
                  comparison = paste(levels(group)[[2]], "-", levels(group)[[1]]),
                  deltas = list(
                    delta.total = delta.total,
                    delta.star = delta.star,
                    delta.scalar = delta.scalar,
                    delta.bo = delta.bo))


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

smd_wrapper <- function(score, dif.group){

  ## levels of the grouping variable
  dif.group1 <- levels(dif.group)[1]
  dif.group2 <- levels(dif.group)[2]

  means <- tapply(score, dif.group, mean, na.rm = T)
  sds <- tapply(score, dif.group, sd, na.rm = T)
  ns <- tapply(score, dif.group, length)

  smd <- list(delta = calc_smd(m1 = means[[dif.group1]],
                               m2 = means[[dif.group2]],
                               sd1 = sds[[dif.group1]]),
              sd = sds[[dif.group1]],
              n = ns[[dif.group1]])


  # delta <- calc_smd(m1 = interaction[[1]] * sds[[dif.group1]],
  #                  m2 = interaction[[2]] * sds[[dif.group2]],
  #                  sd1 = sds[[dif.group1]],
  #                  sd2 = sds[[dif.group2]],
  #                  n1 = ns[[dif.group1]],
  #                  n2 = ns[[dif.group2]])



  return(smd)
}