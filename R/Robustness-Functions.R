#' Robustness of treatment effects
#'
#' Estimates treatment effects and robustness in the presence of DIF
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
#' @param clusters vector indicating cluster membership
#' @param score vector of total scores
#'
#' @details
#' Treatment effects are estimated as the standardized mean difference.
#' If \code{tx.group} is specified, the treatment effect is estimated for each group in
#' \code{dif.group} as well as the \code{tx.group} by \code{dif.group} interaction.
#' The interaction is estimated as difference in within-dif.group treatment effects
#' divided by the pooled SD of the dif.group1 and dif.group2 control groups.
#'
#' The reported total score reliability is \eqn{\alpha}.
#'
#' @return
#' \code{effect_robustness} - a two-item list containing 1) data.frame of treatment effect
#' estimates and 2) character string of the groups being compared \cr
#' \code{smd_wrapper} - numeric value
#'
#'
#' @export

effect_robustness <- function(scale.data,
                              dif.group,
                              biased.items = NULL,
                              no.var.items = integer(),
                              no.var.by.group.items = integer(),
                              poly = integer(),
                              no.dif.mod = NULL,
                              irt.scoring = "WLE",
                              tx.group = NULL,
                              clusters = NULL){

  # Removing items with no variance, if they exist
  # Need to do this step b/c biased.item values were determined after removing no.var.items
  if(length(no.var.items) > 0){
    scale.data <- scale.data[-c(no.var.items)]
  }

  #### Using Total Score ####
  total <- sum_score(scale.data, poly = poly)
  delta.total <- smd_wrapper(score = total,
                             dif.group = dif.group,
                             tx.group = tx.group,
                             clusters = clusters)
  r.total <- est_alpha(scale.data)

  if(!is.null(biased.items)) {

    star <- sum_score(scale.data, drops = biased.items, poly = poly)
    delta.star <- smd_wrapper(score = star,
                              dif.group = dif.group,
                              tx.group = tx.group,
                              clusters = clusters)
    r.star <- est_alpha(scale.data[,-c(biased.items)])
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

  ## Estimating IRT scores
  theta.scalar <- mirt::fscores(no.dif.mod, method = irt.scoring)

  ## Estimating standardized mean differences
  delta.scalar <- smd_wrapper(score = theta.scalar,
                              dif.group = dif.group,
                              tx.group = tx.group,
                              clusters = clusters)

  ## IRT model with parameter constraints freed for biased.items
  if(!is.null(biased.items)) {

    mod.bo <- mirt::multipleGroup(scale.data, model = 1, group = dif.group,
                                  invariance = c(names(scale.data)[-biased.items],
                                                 'free_var','free_means'))
    ## Estimating IRT scores
    theta.bo <- mirt::fscores(mod.bo, method = irt.scoring)

    ## Estimating standardized mean differences
    delta.bo <- smd_wrapper(score = theta.bo,
                            dif.group = dif.group,
                            tx.group = tx.group,
                            clusters = clusters)
  }

  if(!is.null(biased.items)) {

    effects.table <- data.frame(Items = c("All Items", "Bias Omitted"),
                                `IRT Delta` = c(delta.scalar[[1]], delta.bo[[1]]),
                                `Total Delta` = c(delta.total[[1]], delta.star[[1]]),
                                `Adj. Delta` = c((delta.total[[1]] / sqrt(r.total)),
                                                 (delta.star[[1]] / sqrt(r.star))),
                                Reliability = c(r.total, r.star),
                                check.names = FALSE)

    # what is the standard error of Adj. Delta? se(Delta)/ sqrt(reliability)?
    effects.data <- rbind(delta.scalar,
                          delta.bo,
                          delta.total,
                          delta.star)
    effects.data$delta <- factor(c("IRT - All Items", "IRT - Bias Omitted",
                                   "Total - All Items", "Total - Bias Omitted"),
                                 levels = c("IRT - All Items", "IRT - Bias Omitted",
                                            "Total - All Items", "Total - Bias Omitted"))


  } else {

    effects.table <- data.frame(Items = c("All Items"),
                                `IRT Delta` = c(delta.scalar[[1]]),
                                `Total Delta` = c(delta.total[[1]]),
                                `Adj. Delta` = c((delta.total[[1]] / sqrt(r.total))),
                                Reliability = c(r.total),
                                check.names = FALSE)

    # what is the standard error of Adj. Delta? se(Delta)/ sqrt(reliability)?
    effects.data <- rbind(delta.scalar,
                          delta.total)
    effects.data$delta <- factor(c("IRT - All Items","Total - All Items"),
                                 levels = c("IRT - All Items","Total - All Items"))
  }


  effects.plot <- ggplot(data = effects.data, aes(x = smd, y = delta)) +
    geom_vline(xintercept = 0, colour = "black" , size = 1 , linetype = 2) +
    geom_pointrange(aes(xmin = smd - 1.96 * smd.se,
                        xmax = smd + 1.96 * smd.se), size = 1) +
    xlab("Treatment Effect [95% CI]") +
    ylab("Scoring Method") +
    theme(strip.placement = "outside",
          strip.text.y = element_text(angle = 180,vjust=1, face = "bold"),
          strip.background = element_blank(),
          panel.spacing = unit(0,"cm"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor=element_blank(),
          axis.line = element_line(colour = "black"))

  effects <- list(effects.table = effects.table,
                  effects.plot = effects.plot,
                  comparison = paste(levels(dif.group)[[2]], "-", levels(dif.group)[[1]]))


  return(effects)
}

# PH edits 01/28/2021
get_icc <- function(score, clusters) {
  if(!is.null(clusters)){
    variances <- lme4::VarCorr(lme4::lmer(score ~ 1 + (1|clusters)))
    variances <- c(as.numeric(variances), attr(variances, "sc")^2)
    icc <- variances[[1]] / sum(variances)
  } else {
    icc <- NULL
  }
 return(icc)
}


get_avg_cluster_n <- function(group, clusters) {
  if(!is.null(clusters)){
    ns  <- table(group)
    cs1 <- table(clusters[group == levels(group)[[1]]])
    nu1 <- (ns[[1]]^2 - sum(cs1^2)) / (ns[[1]] * (length(cs1) - 1))
    cs2 <- table(clusters[group == levels(group)[[2]]])
    nu2 <- (ns[[2]]^2 - sum(cs2^2)) / (ns[[2]] * (length(cs2) - 1))
    cluster.n <- mean(c(nu1, nu2))
   } else {
    cluster.n <- NULL
   }
  return(cluster.n)
}

smd_wrapper <- function(score, dif.group, tx.group = NULL, clusters = NULL){

  if(is.null(tx.group)){

    means <- tapply(score, dif.group, mean, na.rm = T)
    sds <- tapply(score, dif.group, sd, na.rm = T)
    ns <- tapply(score, dif.group, length)

    icc <- get_icc(score, clusters)
    cluster.n <- get_avg_cluster_n(dif.group, clusters)

    delta <- est_smd(m1 = means[[1]],
                     m2 = means[[2]],
                     sdp = sds[[1]], # using sd of control (reference) group
                     n1 = ns[[1]],
                     n2 = ns[[2]],
                     hedges.g = TRUE,
                     cluster.n = cluster.n,
                     icc = icc)
  } else {

    # This isn't quite right...
    icc <- get_icc(score, clusters)
    cluster.n <- get_avg_cluster_n(dif.group, clusters)

    ## creates 2x2 tables (assuming tx.group and dif.group each have 2 levels)
    means <- tapply(score, list(tx.group, dif.group), mean, na.rm = T)
    sds <- tapply(score, list(tx.group, dif.group), sd, na.rm = T)
    ns <- tapply(score, list(tx.group, dif.group), length)


    # m = (M_{dif.groupTX} - M_{dif.groupControl}) * SD_{dif.groupControl}
    delta <- est_smd(m1 = (means[[2]] - means[[1]]),# * sds[[1]],
                     m2 = (means[[4]] - means[[3]]),# * sds[[3]],
                     sd1 = sds[[1]],
                     sd2 = sds[[3]],
                     n1 = ns[[1]],
                     n2 = ns[[3]],
                     hedges.g = TRUE,
                     cluster.n = cluster.n,
                     icc = icc)
  }

  return(delta)
}