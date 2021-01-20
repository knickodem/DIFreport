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
#' @param cluster.n average cluster size
#' @param icc intraclass correlation - proportion of total variance that is between
#' cluster variance
#'
#' @details
#' The standardized mean difference can be calculated from the raw data
#' (\code{outcome} and \code{groups}) or from the sample statistics.
#' If raw data is supplied the pooled standard deviation is used as the divisor
#' unless \code{sdp} is specified.
#'
#' Both \code{cluster.n} and \code{icc} must be supplied to adjust for clustering.
#' Otherwise, the unadjusted estimate and standard error are returned without a warning.
#' The cluster-adjustment assumes the average cluster size is equal in group 1
#' and group 2.
#'
#' @return
#' If both \code{n1} and \code{n2} are supplied the standardized mean difference
#' and its variance are returned in a data.frame. Otherwise only the estimated
#' standardized mean difference is returned as a numeric value.
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
                   hedges.g = FALSE, cluster.n = NULL, icc = NULL){

  if(!is.null(outcome) & !is.null(groups)){

    if(length(levels(factor(groups))) > 2){
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

      # cluster-adjusted smd
      smd <- smd * sqrt(1 - ((2 * wt) / (N - 2)))

      # cluster-adjusted se
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