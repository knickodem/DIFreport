hedges2007 <- function(outcome, tx.group.id, std.group = NULL, cluster.id = NULL, subset = NULL) {
# std.group is the value of tx.group.id. If provided, effectsize is standardized by the standard deviation of std.group. If NULL, the pooled standard deviation is used.

#Subset is logical vector where F denotes cases that we will omitted.
  tx.groups <- get_tx.groups(tx.group.id, std.group)

  if (!is.null(subset)) {
    if (length(subset) != length(outcome)) {
      warnings("subset is not the same length as outcome.")
    }
    outcome <- outcome[subset]
    tx.group.id <- tx.group.id[subset]
    cluster.id <- cluster.id[subset]
  }

  icc <- get_icc(outcome, cluster.id)
  cluster.n <- get_avg_cluster_n(group.id, cluster.id)
  SD <- get_sd(outcome, tx.group.id, std.group)
  M <- tapply(outcome, tx.group.id, mean, na.rm = T)
  N <- table(tx.group.id)
  NT <- sum(N)

  #Effect size (Hedges 2007 Eq 15)
  coeff <-  1 - (2 * (cluster.n - 1) * icc) / (NT - 2)
  effect.size <- (M[tx.groups[2]] - M[tx.groups[1]]) / SD * coeff

  # Effect size SE (Hedges 2007 Eq 16)
  part1 <- NT / N[1] / N[2] * (1 + (cluster.n - 1) * icc)
  part2.num <- (NT - 2) * (1 - icc)^2 + cluster.n * (NT - 2 * cluster.n) * icc^2 +
    2 * (NT - 2 * cluster.n ) * icc * (1 - icc)
  part2.denom <- 2 * (NT - 2) * ((NT - 2) - 2 * (cluster.n - 1) * icc)
  effect.size.se <- sqrt(part1 + effect.size * part2.num / part2.denom)

  names(effect.size) <- names(effect.size.se) <- NULL # sigh
  return(c(effect.size = effect.size, effect.size.se = effect.size.se))
}



get_icc <- function(outcome, cluster.id) {
  if (!is.null(cluster.id)) {
    variances <- lme4::VarCorr(lme4::lmer(outcome ~ 1 + (1|cluster.id)))
    variances <- c(as.numeric(variances), attr(variances, "sc")^2)
    icc <- variances[[1]] / sum(variances)
  } else {
    icc <- 0
  }
 return(icc)
}

get_avg_cluster_n <- function(group.id, cluster.id) {
  # Hedges 2007 Equation 19
  if (!is.null(cluster.id)) {
    N  <- table(group.id)
    cluster.n1 <- table(cluster.id[group.id == names(N)[1]])
    n1 <- (N[1]^2 - sum(cluster.n1^2)) / (N[1] * (length(cluster.n1) - 1))
    cluster.n2 <- table(cluster.id[group.id == names(N)[2]])
    n2 <- (N[2]^2 - sum(cluster.n2^2)) / (N[2] * (length(cluster.n2) - 1))
    avg.cluster.n <- mean(c(n1, n2))

    #avg.cluster.n = NaN if only 1 cluster per group...
    if(is.nan(avg.cluster.n)){
      avg.cluster.n <- 0
    }
  } else {
    avg.cluster.n <- 0
  }
  return(avg.cluster.n)
}

get_sd <- function(outcome, group.id, std.group = NULL){
  N <- table(group.id)
  Var <- tapply(outcome, group.id, var, na.rm = T)

  if (is.null(std.group)) {
    SD <- sqrt(((N[1] - 1) * Var[1] + (N[2] - 1) * Var[2]) / (sum(N) - 2))
  } else {
    SD <- sqrt(Var[std.group])
  }
  return(SD)
}

get_tx.groups <- function(tx.group.id, std.group){
# checks tx.group.id and std.group and orders tx.groups so that std.group is first. if std groups is not NULL, groups are ordered by `unique`

  tx.groups <- unique(tx.group.id)
  if (length(tx.groups) != 2) {
    stop("tx.group.id must have exactly 2 unique values")
  }

  if (!is.null(std.group)) {
    if(!std.group%in%tx.groups) {
      stop("std.group is not in tx.group.id")
      tx.groups <- c(tx.groups[std.group == tx.groups], tx.groups[std.group != tx.groups])
    }
  }
  tx.groups
}
