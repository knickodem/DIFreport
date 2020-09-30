#' Mantel-Haenszel DIF method
#'
#' Conducts DIF analysis with Mantel-Haenszel method
#'
#' @param scale.data data frame of dichotomous item responses with subjects in rows
#' and items in columns
#' @param dif.group factor vector of group membership for which DIF is evaluated.
#' @param score.type character indicating whether \code{match} is a
#' total summed score ("Total") or the summed score excluding the item under
#' investigation ("Rest").
#' @param match numeric vector used as the stratifying variable in the MH procedure.
#' In \code{get_mh}, if \code{score.type} = "Rest", \code{match} will be a list of vectors -
#' one for each item in \code{scale.data}.
#' @param match.bins optional vector of bin sizes for stratifying \code{match} in
#' \code{run_mh}. This is passed to the \code{probs} argument of \code{\link[stats]{quantile}}.
#' @param item integer; item in \code{scale.data} under investigation for DIF
#'
#' @details
#' \code{run_mh} conducts a Mantel-Haenszel test via \code{\link[stats]{mantelhaen.test}} for each
#' item in \code{scale.data} grouped by \code{dif.group} and stratified by the scores supplied
#' to \code{match}, which, if specified, are binned by \code{match.bins}.
#' \code{get_mh} is a wrapper around \code{run_mh} that organizes the initial and refinement
#' phases of the DIF analysis and compiles the results.
#'
#' @return A two-element list containing 1) a data frame with MH results for each item
#' in \code{scale.data} and 2) an integer vector of the items showing DIF (i.e., biased items).
#'
#' @export

get_mh <- function(scale.data,
                   dif.group,
                   score.type,
                   match,
                   match.bins = NULL){

  ## Number of items in the measure
  nitems <- ncol(scale.data)

  #### MH testing stage 1 - initial DIF items ####

  ## Using rest scores as the matching criterion
  if(score.type == "Rest"){

    # Storage
    stage1 <- list()

    # Loop over items
    for(i in 1:nitems){

      stage1[[i]] <- run_mh(scale.data = scale.data,
                            item = i,
                            dif.group = dif.group,
                            match = match[[i]],
                            match.bins = match.bins)
    }

  } else if(score.type == "Total"){

    stage1 <- lapply(1:nitems, run_mh,
                     scale.data = scale.data,
                     dif.group = dif.group,
                     match = match,
                     match.bins = match.bins)
  }

  # Converting list elements into single dataframe
  mh1 <- Reduce(rbind, stage1)

  # Benjamini–Hochberg procedure for false discovery rate < 5%
  mh1$bias <- p.adjust(mh1$p, method = "BH") < .05

  #### mh testing stage 2 - Refinement/purification of match_score criterion ####
  # The items to exclude based on initial mh DIF analysis
  mh.drops <- which(mh1$bias == 1)

  if(length(mh.drops) > 0){

    if(score.type == "Rest"){

      # Storage
      stage2 <- list()

      # Loop over items
      for(i in 1:nitems) {

        # Recalculate rest score while also removing biased items identified in stage 1
        match2 <- sum_score(scale.data = scale.data, drops = c(i, mh.drops))

        stage2[[i]] <- run_mh(scale.data = scale.data,
                              item = i,
                              dif.group = dif.group,
                              match = match2,
                              match.bins = match.bins)

      }

    } else if(score.type == "Total") {

      # Recalculate rest score while also removing biased items identified in stage 1
      match2 <- sum_score(scale.data = scale.data, drops = c(mh.drops))

      stage2 <- lapply(1:nitems, run_mh,
                       scale.data = scale.data,
                       dif.group = dif.group,
                       match = match2,
                       match.bins = match.bins)

    }

    # Converting list elements into single dataframe
    mh2 <- Reduce(rbind, stage2)

    # Benjamini–Hochberg procedure for false discovery rate = 5%
    mh2$bias <- p.adjust(mh2$p, method = "BH") < .05

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


#' @rdname get_mh
#' @export

run_mh <- function(scale.data,
                   dif.group,
                   item,
                   match,
                   match.bins = NULL){


  if(is.null(match.bins)){ # stratifies on all scores with > 1 value

    # observations to drop if score frequency is too low
    m <- table(match) - 1  # frequency (-1) of each score; WHY DO THIS STEP? COULD CHANGE m == 0 to m == 1?
    drop <- !match %in% as.numeric(names(m[m == 0]))

    ## Runs mh test and catches any errors
    mh <- tryCatch(expr = {
      mantelhaen.test(x = scale.data[drop, item],
                      y = dif.group[drop],
                      z = match[drop],
                      exact = T)
    },
    error = function(e){
      message(paste("Original error:", e))
      warning("Empty cell(s) in the two-way MH tables. Try different cut thresholds
              'match' using the 'match.bins' argument.")
      return(list(estimate = NA,
                  conf.int = c(NA, NA),
                  p.value = NA))}
    )
  } else{

    # categorizes match into strata based on match.bins
    strata <- cut(match, unique(quantile(match, match.bins, type = 1)))

    ## Runs mh test and catches any errors
    mh <- tryCatch(expr = {
      mantelhaen.test(x = scale.data[, item],
                      y = dif.group,
                      z = strata,
                      exact = T)
    },
    error = function(e){
      message(paste("Original error:", e))
      warning("Empty cell(s) in the two-way MH tables. Try different cut thresholds
              'match' using the 'match.bins' argument.")
      return(list(estimate = NA,
                  conf.int = c(NA, NA),
                  p.value = NA))}
    )
  }

  # Note: OR > 1 if item favors group2, OR < 1 if item favors group1
  mh.out <- data.frame(item = names(scale.data)[[item]],
                       OR = mh$estimate[[1]],
                       lower = mh$conf.int[[1]],
                       upper = mh$conf.int[[2]],
                       p = mh$p.value)

  return(mh.out)

}