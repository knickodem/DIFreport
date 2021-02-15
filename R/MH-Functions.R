#' Mantel-Haenszel DIF method
#'
#' Conducts DIF analysis with Mantel-Haenszel method
#'
#' @param item.data data frame of dichotomous item responses with subjects in rows
#' and items in columns
#' @param dif.group.id factor vector of group membership for which DIF is evaluated.
#' @param match.type A character indicating whether \code{match.scores} is a
#' total summed score ("Total") or the summed score excluding the item under
#' investigation ("Rest").
#' @param match.scores A numeric vector (if \code{match.type = "Total"}) or
#' \code{list} of \code{ncol(item.data)} numeric vectors (if \code{match.type = "Rest"})
#' of match scores used as the stratifying variable in the MH procedure.
#' @param match.bins optional vector of bin sizes for stratifying \code{match.scores}.
#' This is passed to the \code{probs} argument of \code{\link[stats]{quantile}}.
#'
#' @details
#' \code{run_mh} conducts a Mantel-Haenszel test via \code{\link[stats]{mantelhaen.test}} for each
#' item in \code{item.data} grouped by \code{dif.group.id} and stratified by the scores supplied
#' to \code{match.scores}, which, if specified, are binned by \code{match.bins}.
#' \code{dif_mh} is a wrapper around \code{run_mh} that organizes the initial and refinement
#' phases of the DIF analysis and compiles the results.
#'
#' @return A list containing
#' \itemize{
#' \item a \code{data.frame} with MH results for each item in \code{item.data}
#' \item an integer vector of the items showing DIF (i.e., biased items)
#' }
#'
#' @export

dif_mh <- function(item.data, dif.group.id,
                   match.type, match.scores, match.bins = NULL){

  ## Number of items in the measure
  nitems <- ncol(item.data)

  #### MH testing stage 1 - initial DIF items ####

  ## Using rest scores as the matching criterion
  if(match.type == "Rest"){

    # Storage
    stage1 <- list()

    # Loop over items
    for(i in 1:nitems){

      stage1[[i]] <- run_mh(item.data = item.data,
                            item = i,
                            dif.group.id = dif.group.id,
                            match.scores = match.scores[[i]],
                            match.bins = match.bins)
    }

  } else if(match.type == "Total"){

    stage1 <- lapply(1:nitems, run_mh,
                     item.data = item.data,
                     dif.group.id = dif.group.id,
                     match.scores = match.scores,
                     match.bins = match.bins)
  }

  # Converting list elements into single dataframe
  mh1 <- Reduce(rbind, stage1)

  # Benjamini–Hochberg procedure for false discovery rate < 5%
  mh1$bias <- stats::p.adjust(mh1$p, method = "BH") < .05

  #### mh testing stage 2 - Refinement/purification of match score criterion ####
  # The items to exclude based on initial mh DIF analysis
  mh.drops <- which(mh1$bias == 1)

  if(length(mh.drops) > 0){

    if(match.type == "Rest"){

      # Storage
      stage2 <- list()

      # Loop over items
      for(i in 1:nitems) {

        # Recalculate rest score while also removing biased items identified in stage 1
        match.scores2 <- sum_score(item.data = item.data, drops = c(i, mh.drops))

        stage2[[i]] <- run_mh(item.data = item.data,
                              item = i,
                              dif.group.id = dif.group.id,
                              match.scores = match.scores2,
                              match.bins = match.bins)

      }

    } else if(match.type == "Total") {

      # Recalculate rest score while also removing biased items identified in stage 1
      match.scores2 <- sum_score(item.data = item.data, drops = c(mh.drops))

      stage2 <- lapply(1:nitems, run_mh,
                       item.data = item.data,
                       dif.group.id = dif.group.id,
                       match.scores = match.scores2,
                       match.bins = match.bins)

    }

    # Converting list elements into single dataframe
    mh2 <- Reduce(rbind, stage2)

    # Benjamini–Hochberg procedure for false discovery rate = 5%
    mh2$bias <- stats::p.adjust(mh2$p, method = "BH") < .05

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
#' @param item.data data frame of dichotomous item responses with subjects in rows
#' and items in columns
#' @param dif.group.id factor vector of group membership for which DIF is evaluated.
#' @param item location of item in \code{item.data} to investigate for DIF
#' @param match.scores A numeric vector of match scores used as the stratifying variable in the MH procedure.
#' @param match.bins optional vector of bin sizes for stratifying \code{match.scores}.
#' This is passed to the \code{probs} argument of \code{\link[stats]{quantile}}.
#' @return a \code{data.frame} of Mantel-Haenszel test results

run_mh <- function(item.data, dif.group.id, item,
                   match.scores, match.bins = NULL){


  if(is.null(match.bins)){ # stratifies on all scores with > 1 value

    # observations to drop if score frequency is too low
    m <- table(match.scores) - 1  # frequency (-1) of each score; WHY DO THIS STEP? COULD CHANGE m == 0 to m == 1?
    drop <- !match.scores %in% as.numeric(names(m[m == 0]))

    ## Runs mh test and catches any errors
    mh <- tryCatch(expr = {
      stats::mantelhaen.test(x = item.data[drop, item],
                      y = dif.group.id[drop],
                      z = match.scores[drop],
                      exact = T)
    },
    error = function(e){
      message(paste("Original error:", e))
      warning("Empty cell(s) in the two-way MH tables. Use the 'match.bins' argument to
      try different cut thresholds for 'match.scores'.")
      return(list(estimate = NA,
                  conf.int = c(NA, NA),
                  p.value = NA))}
    )
  } else{

    # categorizes match.scores into strata based on match.bins
    strata <- cut(match.scores, unique(stats::quantile(match.scores, match.bins, type = 1)))

    ## Runs mh test and catches any errors
    mh <- tryCatch(expr = {
      stats::mantelhaen.test(x = item.data[, item],
                      y = dif.group.id,
                      z = strata,
                      exact = T)
    },
    error = function(e){
      message(paste("Original error:", e))
      warning("Empty cell(s) in the two-way MH tables. Use the 'match.bins' argument to
      try different cut thresholds for 'match.scores'.")
      return(list(estimate = NA,
                  conf.int = c(NA, NA),
                  p.value = NA))}
    )
  }

  # Note: OR > 1 if item favors group2, OR < 1 if item favors group1
  mh.out <- data.frame(item = names(item.data)[[item]],
                       OR = mh$estimate[[1]],
                       lower = mh$conf.int[[1]],
                       upper = mh$conf.int[[2]],
                       p = mh$p.value)

  return(mh.out)

}