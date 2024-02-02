#' Differential item functioning
#'
#' Evaluates differential item functioning (DIF) using loess, Mantel-Haenszel (MH),
#' logistic regression, and item response theory (IRT) approaches.
#'
#' @param dif.data an object returned from \code{\link[DIFreport]{dif_prep}}
#' @param dif.methods character vector with one or more methods of of investigating DIF:
#' locally estimated scatterplot smoothing (\code{"loess"}), Mantel-Haenszel test (\code{"MH"}),
#' logistic regression (\code{"logistic"}), and multi-group item response theory (\code{"IRT"}). The default is all four methods with dichotomous items and
#' \code{c("loess", "IRT")} when polytomous items are detected; MH and logistic can only accommodate dichotomous data.
#' @param match.type For the loess, MH, and logistic methods, a character indicating whether a
#' total summed score (\code{"Total"}; default) or the summed score excluding the item under
#' investigation (\code{"Rest"}) should be used as the stratifying variable. ADD IRT SCORING AS AN OPTION!!!!!
#' @param match.bins For MH, an optional vector of bin sizes for stratifying the \code{match.type} score.
#' This is passed to the \code{probs} argument of \code{stats::quantile}. See examples for usage.
#' @param item.type For IRT, the type of model to fit for each item. The default is \code{"2PL"} for dichotomous items and \code{"graded"} for polytomous items.
#' See \code{\link[mirt]{mirt}} for more options and details.
#' @param stop If \code{FALSE}, when possible, errors will return \code{NA} for the item under investigation rather than stop the analysis completely (default is \code{TRUE}.
#'
#' @details
#'  This is a wrapper for the functions \code{\link[DIFreport]{dif_loess}}, \code{\link[DIFreport]{dif_mh}},
#'  \code{\link[DIFreport]{dif_logistic}}, and \code{\link[DIFreport]{dif_irt}}, which run the requested DIF analyses
#'  on \code{dif.data$item.data} by \code{dif.data$dif.groups}.
#'
#' Usage notes:
#' \itemize{
#' \item Dichotomous items must be coded 0 = incorrect, 1 = correct.
#' \item The Mantel-Haenszel and logistic methods can only accommodate dichotomous items.
#' \item Polytomous item responses are expected to be sequential integers (e.g., 1, 2, 3) but the lowest code does not have to be 0. Polytomous items are unit scaled when calculating the total or rest score.
#' \item Items with different number of response categories across levels of \code{dif.data$dif.groups} are removed from \code{dif.data$item.data} for the "IRT" method.
#' }
#'
#' @return A \code{list} containing \code{dif.data} and the results from each selected method.
#' The list is not formatted in a directly usable manner; it is intended to be passed to
#' \code{\link[DIFreport]{dif_models}} for further processing or to \code{\link[DIFreport]{dif_report}} for user-friendly formatting.
#'
#' @examples
#' data("mdat")
#'
#' # prep data
#' dif.data <- dif_prep(item.data = mdat`[`5:ncol(mdat)],
#'                              dif.groups = mdat$gender,
#'                              na.to.0 = TRUE)
#'
#' # DIF analysis by dif.groups
#' # using rest scores and binning match scores by deciles to avoid empty cells in MH analysis
#' dif.analysis <- dif_analysis(dif.data = dif.data,
#'                            dif.methods =  c("MH", "IRT"),
#'                            match.type = "Rest",
#'                            match.bins = seq(0, 1, by = .1))
#' @export

dif_analysis <- function(dif.data,
                         dif.methods = "default",
                         match.type = "Total",
                         match.bins = NULL,
                         item.type = NULL,
                         stop = TRUE){

  if(inherits(dif.data, "DIFprep")){

    ## pulling constants from dif.data
    item.data <- dif.data$item.data
    dif.groups <- dif.data$dif.groups
    anchors <- dif.data$anchors
    gvar0.items <- dif.data$gvar0.items
    poly.items <- dif.data$poly.items
    max.values <- dif.data$max.values

    # Should we allow dif_x functions to be self-contained?
    # No, ppl should use dif_analysis b/c reporting functions will be drawn from DIFanalysis object
    # Additionally, we would then need to calculate match.scores within each dif_x, which adds time
    # dif_x functions will, however, still be exported to make it easier for ppl to see what is going on

  } else {
    stop("dif.data must be of class 'DIFprep'. Use the dif_prep function first.")
  }

  if(length(dif.methods) == 1){
  if(dif.methods == "default"){
    dif.methods <- if(length(poly.items) == 0) c("loess", "MH", "logistic", "IRT") else c("loess", "IRT")
  }
  }

  # ## Saving original data with all items for use in loess; WHY???
  # item.data.orig <- item.data

  ## Number of items in the measure for dif analysis
  n.items <- ncol(item.data)

  #### Creating match.scores object ####
  # calculating vector of total scores or list of rest scores

  if(any(c("loess", "MH", "logistic") %in% dif.methods)){

    not0 <- which(apply(item.data, 2, function(x) min(na.omit(x))) != 0)
    if(length(not0) > 0){
      stop("Lowest response option should be 0 for proper scoring with loess, MH, and logistic methods.\nWe recommend recoding these items to have 0 as the lowest response:\n", paste(names(not0), collapse = ", "))
    }

  if(match.type == "Rest"){ # Returns a list with n.items elements of length = nrow(item.data)

    match.scores <- lapply(X = c(1:n.items), FUN = sum_score,
                           item.data = item.data, poly.items = poly.items, max.values = max.values)

  } else if(match.type == "Total"){ # a single vector of length = nrow(item.data)

    match.scores <- sum_score(item.data = item.data, drops = NULL, poly.items = poly.items, max.values = max.values)

  }  else {
    stop("match.type argument must be 'Rest' or 'Total'")
  }
  }
  # Note. use of polytomous items addressed in dif_mh and dif_logistic, particularly for rest scores

  #### Mantel-Haenszel ####
  if("MH" %in% dif.methods){

    MH <- dif_mh(item.data = item.data,
                 dif.groups = dif.groups,
                 match.type = match.type,
                 match.scores = match.scores,
                 match.bins = match.bins,
                 exclude = c(poly.items, anchors),
                 stop = stop)

  } else {

    MH <- NULL

  }

  #### Logistic Regression ####
  if("logistic" %in% dif.methods){

    if(length(poly.items) > 0){
      stop("Remove polytomous items from item.data to use logistic method.")
    }

    ## check if dichotomous items are coded 1/0

    # if match.scores was previously calculated in loess & no items need to be dropped
    # OR match.scores was previously calculated in the MH method, use it.
    # Otherwise, (re-)calculate match.scores
    if(!is.null(match.scores) & (length(no.var.items) == 0 | ("MH" %in% dif.methods))){

      match.scores <- match.scores

    } else {

      ## Calculating vector of total scores or list of rest scores
      if(match.type == "Rest"){ # Returns a list with n_items elements of length = nrow(item.data)

        match.scores <- lapply(X = c(1:ncol(item.data.orig)), FUN = sum_score,
                               item.data = item.data.orig, poly.items = poly.items)

      } else if(match.type == "Total"){ # a single vector of length = nrow(item.data)

        match.scores <- sum_score(item.data = item.data.orig, drops = NULL, poly.items = poly.items)

      }  else {
        stop("match.type argument must be 'Rest' or 'Total'")
      }
    }

    logistic <- dif_logistic(item.data = item.data,
                             dif.groups = dif.groups,
                             match.type = match.type,
                             match.scores = match.scores)

  } else {

    logistic <- NULL

  }

  #### LOESS ####
  if("loess" %in% dif.methods){

    loess <- dif_loess(item.data = item.data,
                       dif.groups = dif.groups,
                       match.type = match.type,
                       match.scores = match.scores)

  } else {

    loess <- NULL

  }


  #### Item Response Theory ####
  if("IRT" %in% dif.methods){

    ## Removing items with no within group variance, if they exist
    if(length(gvar0.items) > 0){
      item.data <- item.data[-gvar0.items]
    }

    IRT <- dif_irt(item.data = item.data,
                   dif.groups = dif.groups,
                   item.type = item.type)

  } else {

    IRT <- NULL

  }

  dif.analysis <- list(loess = loess,
                       MH = MH,
                       logistic = logistic,
                       IRT = IRT,
                       inputs = c(dif.data = dif.data, match.type = match.type))

  class(dif.analysis) <- "DIFanalysis"


  return(dif.analysis)
}
