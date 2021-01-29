#' Standardized Mean Difference
#'
#' Estimates the standardized mean difference with or without adjusting for clustering
#'
#' @param outcome numeric vector of outcome values
#' @param groups factor or character vector with 2 levels indicating group membership
#' @param m1 mean for group 1
#' @param m2 mean for group 2
#' @param sdp user-specified divisor for estimating the smd
#' (e.g., population sd, pooled sd)
#' @param sd1 standard deviation for group 1
#' @param sd2 standard deviation for group 2
#' @param n1 sample size for group 1
#' @param n2 sample size for group 2
#' @param hedges.g unbiased estimation of standardized mean difference?
#' @param clusters vector indicating cluster membership
#' @param cluster.n average cluster size (assumed to be equal for both \code{groups})
#' @param icc intraclass correlation - proportion of total variance that is between
#' cluster variance
#'
#' @details
#' The standardized mean difference can be estimated from the raw data
#' (\code{outcome}, \code{groups}, \code{clusters}) or from the sample statistics.
#' If raw data is supplied the pooled standard deviation is used as the divisor
#' unless \code{sdp} is specified.
#'
#' When \code{clusters} is supplied, the average cluster size for each group is
#' calculated via Equation 19 in Hedges (2007). \code{cluster.n} is then defined as
#' the mean of the average cluster size for the two groups. \code{icc} is estimated
#' from an unconditional random effects model via \code{\link[lme4]{lmer}}.
#' The smd and standard error estimates are cluster-adjusted using
#' Equations 15 and 16 in Hedges (2007).
#'
#' @return
#' If sample sizes are supplied (raw data or both \code{n1} and \code{n2}),
#' the standardized mean difference and its standard error are returned in a data.frame.
#' Otherwise only the estimated standardized mean difference is returned as a numeric value.
#'
#' @references
#' Cohen, J. (1988). \emph{Statistical power analysis for the behavioral sciences}
#' (2nd ed.). Hillsdale, NJ: Lawrence Erlbaum.
#' Hedges, L. V. (1981). Distribution theory for Glass’s estimator of effect size
#' and related estimators. \emph{Journal of Educational Statistics, 6}(2), 107–128.
#' Hedges, L. V. (2007). Effect sizes in cluster-randomized designs.
#' \emph{Journal of Educational and Behavioral Statistics, 32}(4), 341-370.
#'
#' @export

est_smd <- function(outcome = NULL, groups = NULL, m1 = NULL, m2 = NULL,
                    sdp = NULL, sd1 = NULL, sd2 = NULL, n1 = NULL, n2 = NULL,
                   hedges.g = FALSE, clusters = NULL, cluster.n = NULL, icc = NULL){

  # calculate sample statistics from raw data
  if(!is.null(outcome) & !is.null(groups)){
    groups <- factor(groups)
    if(length(levels(groups)) > 2){
      stop("groups must have only 2 levels")
    }

    means <- tapply(outcome, groups, mean, na.rm = T)
    sds <- tapply(outcome, groups, sd, na.rm = T)
    ns <- tapply(outcome, groups, length)

    m1 <- means[[1]]
    m2 <- means[[2]]
    sd1 <- sds[[1]]
    sd2 <- sds[[2]]
    n1 <- ns[[1]]
    n2 <- ns[[2]]

    if(!is.null(clusters)){

      ## cluster sizes
      # ns = sum(cs)
      cs1 <- table(clusters[groups == levels(groups)[[1]]])
      nu1 <- (ns[[1]]^2 - sum(cs1^2)) / (ns[[1]] * (length(cs1) - 1))
      cs2 <- table(clusters[groups == levels(groups)[[2]]])
      nu2 <- (ns[[2]]^2 - sum(cs2^2)) / (ns[[2]] * (length(cs2) - 1))
      cluster.n <- mean(c(nu1, nu2))

      ## ICC
      variances <- lme4::VarCorr(lme4::lmer(outcome ~ 1 + (1|clusters)))
      variances <- c(as.numeric(variances), attr(variances, "sc")^2)
      icc <- variances[[1]] / sum(variances)

    }
  }


  # raw mean difference
  rmd <- (m2 - m1)

  # sigma
  if(!is.null(sdp)){

    sigma <- sdp

  } else {

    sigmanum <- (n1 - 1) * (sd1^2) + (n2 - 1) * (sd2^2)
    sigmadenom <- (n1 + n2 - 2)
    sigma <- sqrt(sigmanum / sigmadenom)

  }

  # d
  smd <- rmd / sigma

  # g
  if(hedges.g == TRUE){

    eta <- n1 + n2 - 2
    j <- 1 - 3 / (4 * eta - 1)
    smd <- j * smd

  }

  # standard error
  if(is.null(n1) | is.null(n2)){
    smd <- smd
  } else if(!is.null(n1) & !is.null(n2)){
    N <- n1 + n2
    ntilde <- N / (n1 * n2)

    if(is.null(cluster.n) & is.null(icc)){ # no clustering

      # Hedges and Olkin approximation
      right <- smd^2 / (2 * N)
      smd.se <- sqrt(ntilde + right)

      smd <- data.frame(smd = smd, smd.se = smd.se) # returns

    } else if(!is.null(cluster.n) & !is.null(icc)){ # cluster adjusted
      if(icc < 0 | icc > 1){
        stop("icc must be between 0 and 1")
      }

      wt <- (cluster.n - 1) * icc

      # cluster-adjusted smd (Hedges, 2007 eq 15)
      smd <- smd * sqrt(1 - ((2 * wt) / (N - 2)))

      # cluster-adjusted se (Hedges, 2007, eq 16)
      left <- ntilde * (1 + wt)
      nom <- (N - 2) * (1 - icc)^2 + cluster.n * (N - 2 * cluster.n) * icc^2 +
        2 * (N - (2 * cluster.n)) * icc * (1 - icc)
      denom <- 2 * (N - 2) * ((N - 2) - 2 * wt)
      smd.se <- sqrt(left + (smd^2 * (nom / denom)))

      smd <- data.frame(smd = smd, smd.se = smd.se) # returns

      } else{
      stop("icc and cluster.n (avg cluster size) must be provided to adjust for clustering")
    }
  } else {stop("huh")}

  return(smd)

}



est_smd <- function(outcome = NULL, groups = NULL, m1 = NULL, m2 = NULL,
                    sdp = NULL, sd1 = NULL, sd2 = NULL, n1 = NULL, n2 = NULL,
                   hedges.g = FALSE, clusters = NULL, cluster.n = NULL, icc = NULL){

  # calculate sample statistics from raw data
  if(!is.null(outcome) & !is.null(groups)){
    groups <- factor(groups)
    if(length(levels(groups)) > 2){
      stop("groups must have only 2 levels")
    }

    means <- tapply(outcome, groups, mean, na.rm = T)
    sds <- tapply(outcome, groups, sd, na.rm = T)
    ns <- tapply(outcome, groups, length)

    m1 <- means[[1]]
    m2 <- means[[2]]
    sd1 <- sds[[1]]
    sd2 <- sds[[2]]
    n1 <- ns[[1]]
    n2 <- ns[[2]]

    if(!is.null(clusters)){

      ## cluster sizes
      # ns = sum(cs)
      cs1 <- table(clusters[groups == levels(groups)[[1]]])
      nu1 <- (ns[[1]]^2 - sum(cs1^2)) / (ns[[1]] * (length(cs1) - 1))
      cs2 <- table(clusters[groups == levels(groups)[[2]]])
      nu2 <- (ns[[2]]^2 - sum(cs2^2)) / (ns[[2]] * (length(cs2) - 1))
      cluster.n <- mean(c(nu1, nu2))

      ## ICC
      variances <- lme4::VarCorr(lme4::lmer(outcome ~ 1 + (1|clusters)))
      variances <- c(as.numeric(variances), attr(variances, "sc")^2)
      icc <- variances[[1]] / sum(variances)

    }
  }


  # raw mean difference
  rmd <- (m2 - m1)

  # sigma
  if(!is.null(sdp)){

    sigma <- sdp

  } else {

    sigmanum <- (n1 - 1) * (sd1^2) + (n2 - 1) * (sd2^2)
    sigmadenom <- (n1 + n2 - 2)
    sigma <- sqrt(sigmanum / sigmadenom)

  }

  # d
  smd <- rmd / sigma

  # g
  if(hedges.g == TRUE){

    eta <- n1 + n2 - 2
    j <- 1 - 3 / (4 * eta - 1)
    smd <- j * smd

  }

  # standard error
  if(is.null(n1) | is.null(n2)){
    smd <- smd
  } else if(!is.null(n1) & !is.null(n2)){
    N <- n1 + n2
    ntilde <- N / (n1 * n2)

    if(is.null(cluster.n) & is.null(icc)){ # no clustering

      # Hedges and Olkin approximation
      right <- smd^2 / (2 * N)
      smd.se <- sqrt(ntilde + right)

      smd <- data.frame(smd = smd, smd.se = smd.se) # returns

    } else if(!is.null(cluster.n) & !is.null(icc)){ # cluster adjusted
      if(icc < 0 | icc > 1){
        stop("icc must be between 0 and 1")
      }

      wt <- (cluster.n - 1) * icc

      # cluster-adjusted smd (Hedges, 2007 eq 15)
      smd <- smd * sqrt(1 - ((2 * wt) / (N - 2)))

      # cluster-adjusted se (Hedges, 2007, eq 16)
      left <- ntilde * (1 + wt)
      nom <- (N - 2) * (1 - icc)^2 + cluster.n * (N - 2 * cluster.n) * icc^2 +
        2 * (N - (2 * cluster.n)) * icc * (1 - icc)
      denom <- 2 * (N - 2) * ((N - 2) - 2 * wt)
      smd.se <- sqrt(left + (smd^2 * (nom / denom)))

      smd <- data.frame(smd = smd, smd.se = smd.se) # returns

      } else{
      stop("icc and cluster.n (avg cluster size) must be provided to adjust for clustering")
    }
  } else {stop("huh")}

  return(smd)

}