#' Estimate standardized mean differences for clustered data
#'
#' Computes mean difference standardized for clustered data by the "total variance", and their standard errors, using the method described by Hedges (2007).
#'
#' @param outcome numeric vector
#' @param groups vector of \code{length(outcome)} indicating the two groups to compare
#' @param pooled If \code{TRUE} (default), the pooled standard deviation is calculated to standardize the mean difference.
#' If \code{FALSE}, the standard deviation of \code{comp.name} will be used.
#' @param comp.name If \code{pooled = FALSE}, the name of the group in \code{groups} whose standard deviation will be used to
#' standardize the mean difference.
#' @param cluster optional vector of \code{length(outcome)} indicating the primary sampling unit in a multi-stage / clustered sampling design
#' @param subset optional logical vector of \code{length(outcome)} indicating a subset of the data to use in the calculations
#'
#' @details
#' The standardized mean difference and its standard error are computed using Equations 15 and 16 of Hedges (2007).
#' When cluster sample sizes are unequal, the average cluster size is used to obtain a close approximation of the
#' standardized mean difference estimate and standard error (p. 351).
#'
#' @return
#' A named vector containing the standardized mean difference and its standard error.
#'
#' @references
#' Hedges, L. V. (2007). Effect sizes in cluster-randomized designs. \emph{Journal of Educational and Behavioral Statistics, 32}(4), 341–370. \url{https://doi.org/10.3102/1076998606298043}.
#'
#' @export

hedges2007 <- function(outcome, groups, pooled = TRUE, comp.name = NULL, cluster = NULL, subset = NULL) {

  ## subset, if requested
  if (!is.null(subset)) {
    if (length(subset) != length(outcome)) {
      warnings("subset is not the same length as outcome.")
    }
    outcome <- outcome[subset]
    groups <- groups[subset]

    if(!is.null(cluster)){
    cluster <- cluster[subset]
    }
  }

  if(!is.factor(groups)){
    groups <- factor(groups) # must be factor, otherwise output orders will be inconsistent
  }

  if(!is.null(comp.name)){
    comp.name <- as.character(comp.name) # in case numeric is supplied
    group.lvls <- get_groups(groups, comp.name)
    groups <- factor(groups, levels = group.lvls)
  } else{
    group.lvls <- levels(groups)
  }

  # means, sds, and sample sizes
  M <- tapply(outcome, groups, mean, na.rm = T) # in order of factor levels
  SD <- get_sd(outcome, groups, pooled, comp.name)
  nTC <- table(groups)
  N <- sum(nTC)

  # clustering
  if(is.null(cluster)){
    icc <- cluster.n <- 0
    # when icc = 0, the value of cluster.n does not matter
  } else {
  icc <- get_icc(outcome, cluster)
  cluster.n <- get_avg_cluster_n(groups, cluster)
  # Use of cluster.n is based on Hedges note on p. 351
  }

  # Effect size (Hedges 2007 Eq 15)
  coeff <-  1 - (2 * (cluster.n - 1) * icc) / (N - 2)
  effect.size <- ((M[group.lvls[2]] - M[group.lvls[1]]) / SD)* sqrt(coeff)

  # Effect size SE (Hedges 2007 Eq 16)
  part1 <- N / (nTC[1]* nTC[2]) * (1 + (cluster.n - 1) * icc)
  part2.num <- (N - 2) * (1 - icc)^2 + cluster.n * (N - 2 * cluster.n) * icc^2 +
    2 * (N - 2 * cluster.n ) * icc * (1 - icc)
  part2.denom <- 2 * (N - 2) * ((N - 2) - 2 * (cluster.n - 1) * icc)
  effect.size.se <- sqrt(part1 + effect.size^2 * (part2.num / part2.denom))

  names(effect.size) <- names(effect.size.se) <- NULL
  return(c(effect.size = effect.size, effect.size.se = effect.size.se))
}

#' Estimate the intraclass correlation (ICC)
#'
#' Helper function for \code{hedges2007} that requires \code{lme4}.
#'
#' @inheritParams hedges2007
#' @importFrom lme4 VarCorr lmer
#'
#' @noRd

get_icc <- function(outcome, cluster) {
    variances <- lme4::VarCorr(lme4::lmer(outcome ~ 1 + (1|cluster)))
    variances <- c(as.numeric(variances), attr(variances, "sc")^2)
    icc <- variances[[1]] / sum(variances)
 return(icc)
}


#' Compute average cluster size
#'
#' Helper function for \code{hedges2007} that computes Equation 19 of Hedges (2007).
#'
#' @inheritParams hedges2007
#'
#' @return
#' The average cluster size
#'
#' @noRd

get_avg_cluster_n <- function(groups, cluster) {
  # Hedges 2007 under Equation 19
    nTC  <- table(groups) # total group sample size
    cluster.n1 <- table(cluster[groups == names(nTC)[1]]) # cluster sample sizes for group 1
    if(length(cluster.n1) == 1){ # if group only has 1 cluster
      n1 <- nTC[[1]]
    } else {
      n1 <- (nTC[1]^2 - sum(cluster.n1^2)) / (nTC[1] * (length(cluster.n1) - 1)) # avg cluster n for group 1
    }

    cluster.n2 <- table(cluster[groups == names(nTC)[2]]) # cluster sample sizes for group 2
    if(length(cluster.n2) == 1){ # if group only has 1 cluster
      n2 <- nTC[[2]]
    } else {
      n2 <- (nTC[2]^2 - sum(cluster.n2^2)) / (nTC[2] * (length(cluster.n2) - 1)) # avg cluster n for group 2
    }

    avg.cluster.n <- mean(c(n1, n2))

  return(avg.cluster.n)
}

#' Computes the pooled standard deviation
#'
#' Helper function for \code{hedges2007}.
#'
#' @inheritParams hedges2007
#'
#' @return If \code{is.null(comp.name)}, returns the pooled standard deviation; else returns the standard deviation of \code{comp.name}.
#'
#' @noRd

get_sd <- function(outcome, groups, pooled = TRUE, comp.name = NULL){
  nTC <- table(groups) # output in order of groups levels
  Var <- tapply(outcome, groups, var, na.rm = T) # output in order of groups levels

  if(pooled){
    # assumes unequal group sample sizes and variances, but also works if equal
    SD <- sqrt(((nTC[1] - 1) * Var[1] + (nTC[2] - 1) * Var[2]) / (sum(nTC) - 2))
  } else if (is.null(comp.name)) {
    stop("comp.name must be specified when pooled = FALSE")
  } else {
    SD <- sqrt(Var[comp.name])
  }
  names(SD) <- NULL
  return(SD)
}

#' Orders group levels and checks for input errors
#'
#' Helper function for \code{hedges2007}.
#'
#' @inheritParams hedges2007
#'
#' @return A vector containing the two unique values of \code{groups}, with the name of the comparison group in the first position, and the name of the focal group in the second position.
#'
#' @noRd

get_groups <- function(groups, comp.name = NULL){

  group.lvls <- levels(groups)
  if (length(group.lvls) != 2) {
    stop("groups must have exactly 2 unique values")
  }

  if(!is.null(comp.name)){
    if(!comp.name %in% group.lvls) {
      stop("comp.name is not in groups")
    }
    group.lvls <- c(group.lvls[comp.name == group.lvls], group.lvls[comp.name != group.lvls])
  }

  return(group.lvls)
}