#' Mantel-Haenszel DIF method
#'
#' Conducts DIF analysis with the Mantel-Haenszel method
#'
#' @inheritParams dif_analysis
#' @param item.data A \code{data.frame} of dichotomous item responses with subjects in rows and items in columns.
#' @param dif.groups factor vector of group membership for which DIF is evaluated.
#' @param match.scores A numeric vector (if \code{match.type = "Total"}) or \code{list} of \code{ncol(item.data)}
#' numeric vectors (if \code{match.type = "Rest"}) of match scores used as the stratifying variable in the MH procedure.
#' @param exclude character vector of column names in \code{item.data} to exclude from DIF analysis, such as the anchor items or (currently) polytomous items.
#'
#' @details
#' \code{run_mh} conducts a Mantel-Haenszel test via \code{\link[stats]{mantelhaen.test}} for each
#' item in \code{item.data}, except those specified in \code{exclude}, grouped by \code{dif.groups} and stratified by the scores supplied
#' to \code{match.scores}, which, if specified, are binned by \code{match.bins}. Biased items
#' are identified through a two-step process of initial and refinement phases.
#'
#' @return A list containing
#' \itemize{
#' \item a \code{data.frame} with MH results for each item in \code{item.data}
#' \item an integer vector of the items showing DIF (i.e., biased items)
#' }
#'
#' @export

dif_mh <- function(item.data, dif.groups,
                   match.type, match.scores, match.bins = NULL,
                   exclude = NULL, stop = TRUE){

  ## Items to include in the DIF analysis
  nitems <- ncol(item.data) # Number of items in the measure
  include <- 1:nitems
  include <- include[!include %in% exclude]

  #### MH testing stage 1 - initial DIF items ####

  ## Using rest scores as the matching criterion
  if(match.type == "Rest"){

    # Storage
    stage1 <- vector("list", length = length(include))

    # Loop over items
    for(i in include){

      stage1[[i]] <- run_mh(item.data = item.data,
                            item = i,
                            dif.groups = dif.groups,
                            match.scores = match.scores[[i]],
                            match.bins = match.bins,
                            stop = stop)
    }

  } else if(match.type == "Total"){

    stage1 <- lapply(include, run_mh,
                     item.data = item.data,
                     dif.groups = dif.groups,
                     match.scores = match.scores,
                     match.bins = match.bins,
                     stop = stop)
  }

  # Converting list elements into single dataframe
  mh1 <- Reduce(rbind, stage1)

  # Benjamini–Hochberg procedure for false discovery rate < 5%
  mh1$p.adj <- stats::p.adjust(mh1$p, method = "BH")
  mh1$bias <-  mh1$p.adj < .05

  #### mh testing stage 2 - Refinement/purification of match score criterion ####
  # The items to exclude based on initial mh DIF analysis
  mh.drops <- which(mh1$bias == 1)

  if(length(mh.drops) > 0){

    if(match.type == "Rest"){

      # Storage
      stage2 <- vector("list", length = length(include))

      # Loop over items
      for(i in include) {

        # Recalculate rest scores removing biased items identified in stage 1
        match.scores2 <- sum_score(item.data = item.data, drops = c(i, mh.drops))

        stage2[[i]] <- run_mh(item.data = item.data,
                              item = i,
                              dif.groups = dif.groups,
                              match.scores = match.scores2,
                              match.bins = match.bins,
                              stop = stop)

      }

    } else if(match.type == "Total") {

      # Recalculate total score removing biased items identified in stage 1
      match.scores2 <- sum_score(item.data = item.data, drops = mh.drops)

      stage2 <- lapply(include, run_mh,
                       item.data = item.data,
                       dif.groups = dif.groups,
                       match.scores = match.scores2,
                       match.bins = match.bins,
                       stop = stop)

    }

    # Converting list elements into single dataframe
    mh2 <- Reduce(rbind, stage2)

    # Benjamini–Hochberg procedure for false discovery rate = 5%
    mh2$p.adj <- stats::p.adjust(mh2$p, method = "BH")
    mh2$bias <-  mh2$p.adj < .05

    # Output dataframe combining stage 1 and stage 2
    names(mh1)[-1] <- paste0("initial.", names(mh1)[-1])
    names(mh2)[-1] <- paste0("refined.", names(mh2)[-1])

    mh <- list(item.level = cbind(mh1, mh2[,-1]),
               biased.items = which(mh2$refined.bias == 1),
               dif.type = "uniform")

  } else {

    mh <- list(item.level = mh1,
               biased.items = "No DIF was detected",
               dif.type = "No DIF detected")
  }

  return(mh)
}

#' MH test of DIF for single item
#'
#' Internal function for dif_mh
#'
#' @param item location of item in \code{item.data} to investigate for DIF
#' @inheritParams dif_analysis
#' @inheritParams dif_mh
#'
#' @return a \code{data.frame} of Mantel-Haenszel test results
#'
#' @noRd

run_mh <- function(item.data, dif.groups, item,
                   match.scores, match.bins = NULL, stop = TRUE){

  ## match.bins is to prevent error, "sample size in each stratum must be > 1"
  if(is.null(match.bins)){
    z <- match.scores
  } else {
    # categorizes match.scores into strata based on match.bins
    z <- cut(match.scores, unique(stats::quantile(match.scores, match.bins, type = 1)))
    # can't recall why we use type = 1
  }

    ## Runs mh test and catches any errors
    mh <- tryCatch(expr = {
      stats::mantelhaen.test(x = item.data[, item],
                      y = dif.groups,
                      z = z,
                      exact = T)
    },
    error = function(e){
      message(paste(e,
                    "Use the 'match.bins' argument to try different cut thresholds for 'match.scores'."))
      return(list(estimate = NA,
                  conf.int = c(NA, NA),
                  p.value = NA))
      }
    )

  # Note: OR > 1 if item favors group2, OR < 1 if item favors group1
  mh.out <- data.frame(item = names(item.data)[[item]],
                       OR = mh$estimate[[1]],
                       lower = mh$conf.int[[1]],
                       upper = mh$conf.int[[2]],
                       p = mh$p.value)

  ## If error, should analysis stop? Otherwise return mh.out with NAs for item
  if(stop == TRUE & is.na(mh.out$OR[[1]])){
    stop("Item = ", mh.out$item[[1]])
  }

  return(mh.out)

}