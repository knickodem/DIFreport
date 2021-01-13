#' Robustness of treatment effects
#'
#' Calculates treatment effect and robustness in the presence of DIF
#'
#' @param scale.data data frame of dichotomous item responses with subjects in rows
#' and items in columns
#' @param dif.group factor vector of group membership for which DIF is evaluated
#' @param biased.items integer vector; location of biased items in \code{scale.data}
#' @param no.var.items integer vector; location of items in \code{scale.data} with 0 variance
#' @param no.var.by.group.items integer vector; location of items in \code{scale.data}
#' with 0 variance within a \code{dif.group}
#' @param poly integer vector; location of polytomous items in \code{scale.data}
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
#' Treatment effects are calculated as the standardized mean difference.
#' If \code{tx.group} is specified, the treatment effect is calculated for each group in
#' \code{dif.group} as well as the \code{tx.group} by \code{dif.group} interaction.
#' The interaction is calculated as difference in within-dif.group treatment effects
#' divided by the pooled SD of the dif.group1 and dif.group2 control groups.
#'
#' The reported raw scale reliability is \alpha calculated via
#' \code{\link[psy]{cronbach}}.
#'
#' @return
#' \code{get_robustness} - a two-item list containing 1) data.frame of treatment effect
#' estimates and 2) character string of the groups being compared \cr
#' \code{calc_smd} - numeric value
#' \cod{smd_wrapper} - numeric value
#'
#' @references
#' Cohen, J. (1988). \emph{Statistical power analysis for the behavioral sciences}
#' (2nd ed.). Hillsdale, NJ: Lawrence Erlbaum.
#'
#' @export

get_robustness <- function(scale.data,
                           dif.group,
                           biased.items = NULL,
                           no.var.items = integer(),
                           no.var.by.group.items = integer(),
                           poly = integer(),
                           no.dif.mod = NULL,
                           irt.scoring = "WLE",
                           tx.group = NULL){

  # Removing items with no variance, if they exist
  # Need to do this step b/c biased.item values were determined after removing no.var.items
  if(length(no.var.items) > 0){
    scale.data <- scale.data[-c(no.var.items)]
  }

  #### Using Total Score ####
  total <- sum_score(scale.data, poly = poly)
  delta.total <- smd_wrapper(score = total,
                             dif.group = dif.group,
                             tx.group = tx.group)
  r.total <- psy::cronbach(scale.data)$alpha

  if(!is.null(biased.items)) {

    star <- sum_score(scale.data, drops = biased.items, poly = poly)
    delta.star <- smd_wrapper(score = star,
                              dif.group = dif.group,
                              tx.group = tx.group)
    r.star <- psy::cronbach(scale.data[,-c(biased.items)])$alpha
  }


  #### Using IRT Score ####

  # Removing items with no variance, if they exist
  if(length(no.var.by.group.items) > 0){
    scale.data <- scale.data[-c(no.var.by.group.items)]
  }

  ## IRT model with parameters constrained to be equal between groups
  if(is.null(no.dif.mod)){

    no.dif.mod <- mirt::multipleGroup(scale.data, model = 1, group = dif.group,
                                     invariance = c('slopes', 'intercepts',
                                                    'free_var','free_means'))
  }

  ## Calculating IRT scores
  theta.scalar <- mirt::fscores(no.dif.mod, method = irt.scoring)

  ## Calculating standardized mean differences
  delta.scalar <- smd_wrapper(score = theta.scalar,
                              dif.group = dif.group,
                              tx.group = tx.group)

  ## IRT model with parameter constraints freed for biased.items
  if(!is.null(biased.items)) {

    mod.bo <- mirt::multipleGroup(scale.data, model = 1, group = dif.group,
                                  invariance = c(names(scale.data)[-biased.items],
                                                 'free_var','free_means'))
    ## Calculating IRT scores
    theta.bo <- mirt::fscores(mod.bo, method = irt.scoring)

    ## Calculating standardized mean differences
    delta.bo <- smd_wrapper(score = theta.bo,
                            dif.group = dif.group,
                            tx.group = tx.group)
  }

  if(!is.null(biased.items)) {

  effects.table <- data.frame(Items = c("All Items", "Bias Omitted"),
                              `IRT Delta` = c(delta.scalar, delta.bo),
                              `Scale Delta` = c(delta.total, delta.star),
                              `Adj. Delta` = c((delta.total / sqrt(r.total)),
                                               (delta.star / sqrt(r.star))),
                              Reliability = c(r.total, r.star),
                              check.names = FALSE)
  } else {

    effects.table <- data.frame(Items = c("All Items"),
                                `IRT Delta` = c(delta.scalar),
                                `Scale Delta` = c(delta.total),
                                `Adj. Delta` = c((delta.total / sqrt(r.total))),
                                Reliability = c(r.total),
                                check.names = FALSE)
  }

  effects <- list(effects.table = effects.table,
                  comparison = paste(levels(dif.group)[[2]], "-", levels(dif.group)[[1]]))


  return(effects)

}

#' @rdname get_robustness

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

#' @rdname get_robustness

smd_wrapper <- function(score, dif.group, tx.group = NULL){

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