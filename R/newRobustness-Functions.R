#' Robustness of treatment effects
#'
#' Compares standardized treatment effects estimated with and without adjustments for DIF.
#'
#' @param dif.models Output from \code{dif_models}
#' @param std.group A value of \code{tx.group.id} that identifies the group whose standard deviation will be used to standardize the effect size. If \code{NULL} (default), the pooled standard devaition is used. Note that \code{tx.group.id} is defined in the function \code{dif_data_prep} and passed to \code{effect_robustness} via \code{dif.models}.
#' @param irt.scoring What type of IRT scoring procedure should be used? Is passed to the \code{method} argument of \code{mirt::fscores}. See \code{help(fscores, mirt)}.
#'
#' @details
#' Data and models are passed to \code{effect_robustness} via the \code{dif.models} argument. If \code{tx.group.id == dif.group.id}, then the unconditional standardized treatment effect is computed. If \code{tx.group.id != dif.group.id}, the standardized treatment effect is computed conditional on \code{dif.group.id} (e.g., conditional on gender), and the difference in treatment effects is also reported. The treatment effects and their standard errors are computed using the method described by Hedges (2007).
#'
#'   Treatment effects and their standard errors are reported for four different outcome variables.
#'   \itemize{
#'   \item The unit-weighted total score computed with all items.
#'   \item The unit-weighted total score computed with DIFfy items omitted (i.e. the items identified in \code{dif.models$biased.items}).
#'   \item  IRT scores computed using a model that constrains all items to have equal parameters over levels of \code{dif.group.id} (ie., \code{dif.models$no.dif.mod}) .
#'   \item IRT scores computed using a model that  allows parameters of DIFfy items to vary over levels of \code{dif.group.id} (ie., \code{dif.models$dif.mod}).
#' }
#'  IRT scores are computed using the method given by \code{irt.scoring}.
#'
#'
#' @return
#' A \code{list} of data.frames, with each \code{data.frame} summarizing the standardized effect size and standard error for the four different outcomes. The length of the list depends on whether the conditional or unconditional effect sizes were requested.
#'
#' @references
#' Hedges, L. V. (2007). Effect Sizes in Cluster-Randomized Designs. Journal of Educational and Behavioral Statistics, 32, 341–370. \url{https://doi.org/10.3102/1076998606298043}.
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
