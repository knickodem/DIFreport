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


effect_robustness <- function(dif.models, std.group = NULL, irt.scoring = "WLE") {

  # Setup inputs
  inputs <- dif.models$inputs
  biased.items <- dif.models$biased.items
  dif.group.id <- inputs$dif.group.id
  dif.groups <- unique(dif.group.id)
  tx.group.id <- inputs$tx.group.id
  tx.groups <- get_tx.groups(tx.group.id, std.group)
  cluster.id <- inputs$cluster.group.id
  poly.items <- inputs$poly.items

  # Get outcome variables
  total.score <- sum_score(inputs$item.data, poly = inputs$poly.items)
  adj.total.score <- sum_score(inputs$item.data, drops = biased.items, poly = poly.items)
  irt.score <- mirt::fscores(dif.models$no.dif.mod, method = irt.scoring)
  adj.irt.score <- mirt::fscores(dif.models$dif.mod, method = irt.scoring)
  scores <- data.frame(irt.score, adj.irt.score, total.score, adj.total.score)
  score.type <- c("IRT: All Items", "IRT: Bias Omitted","Total: All Items", "Total: Bias Omitted")

  # Wrappers for lapply
  effect_sizes <- function(x, subset = NULL){
    hedges2007(x, tx.group.id, std.group, cluster.id, subset)
  }

  # Compute effect sizes and alphas: Unconditional
  if(sum(tx.groups == dif.groups) == 1) {
    effects <- data.frame(Reduce(rbind, lapply(scores, effect_sizes)), row.names = NULL)
    effects.list <- list(effects)
    names(effects.list) <- paste(tx.groups[2], "-", tx.groups[1])

  } else { #Conditional
    group1 <-  dif.group.id == dif.groups[1]
    effects1 <- data.frame(Reduce(rbind, lapply(scores, effect_sizes, group1)), row.names = NULL)

    group2 <- dif.group.id == dif.groups[2]
    effects2 <- data.frame(Reduce(rbind, lapply(scores, effect_sizes, group2)), row.names = NULL)

    interactions <- effects2
    interactions$effect.size <- effects2$effect.size  - effects1$effect.size
    interactions$effect.size.se <- sqrt(effects2$effect.size.se^2 + effects1$effect.size.se^2)

    effects.list <- list(effects1, effects2, interactions)
    names(effects.list) <- c(paste0(tx.groups[2], " - ", tx.groups[1],": ",
                                    c(dif.groups)), "interaction")
  }
  out <- lapply(effects.list, function(x) cbind(x, score.type))
  return(out)
}


effects_table <- function(effects, alphas = NULL) {
  row.names(effects) <- NULL # sigh
  dat <- data.frame(Items = c("All Items", "Bias Omitted"),
            `IRT effect size` = effects[1:2,1],
            `IRT SE` = effects[1:2,2],
            `Total score effect size` = effects[3:4,1],
            `Total score SE` = effects[3:4,2],
             check.names = FALSE)
   if(!is.null(alphas)) {
   dat$`Total score alpha` <- alphas
   }
  dat
}

coeff_alpha <-function(dif.models, std.group = NULL) {
  inputs <- dif.models$inputs
  item.data <- inputs$item.data
  biased.items <- dif.models$biased.items
  dif.group.id <- inputs$dif.group.id
  dif.groups <- unique(dif.group.id)
  tx.group.id <- inputs$tx.group.id
  tx.groups <- get_tx.groups(tx.group.id, std.group)

  if(sum(tx.groups == dif.groups) == 1) {
     alphas <- c(est_alpha(item.data), est_alpha(item.data[-c(biased.items)]))
     alphas.list <- list(alphas)
     names(alphas.list) <- paste(tx.groups[2], "-", tx.groups[1])
  } else {
    group1 <-  dif.group.id == dif.groups[1]
    alphas1 <- c(est_alpha(item.data[group1, ]), est_alpha(item.data[group1, -c(biased.items)]))

    group2 <- dif.group.id == dif.groups[2]
    alphas2 <- c(est_alpha(item.data[group2, ]), est_alpha(item.data[group2, -c(biased.items)]))

    alphas.list <- list(alphas1, alphas2, NULL)
    names(alphas.list) <- c(paste0(tx.groups[2], " - ", tx.groups[1],": ",
                                    c(dif.groups)), "interaction")
  }
  return(alphas.list)
}
