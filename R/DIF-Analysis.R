#' Differential item functioning
#'
#' Evaluates differential item functioning (DIF) using loess, Mantel-Haenszel (MH),
#' logistic regression, and item response theory (IRT) approaches.
#'
#' @param dif.data The output of \code{\link[WBdif]{data_data_prep}}.
#' @param methods A character \code{vector} with one or more of \code{c("loess", "MH", "logistic", "IRT")}. The default is all four methods.
#' @param match.type Οne of \code{c("Total", "Rest")}. Determines whether the total score or rest score should be used as the stratifying variable for loess, MH, and logistic regression methods.
#' @param match.bins (optional) vector of bin sizes for stratifying the matching variable in
#' the MH method. This is passed to the \code{probs} argument of \code{stas::quantile}.
#'
#' @details
#'  This is a wrapper for the functions \code{\link[WBdif]{dif_loess}}, \code{\link[WBdif]{dif_mh}}, \code{\link[WBdif]{dif_logistic}}, and \code{\link[WBdif]{dif_irt}}. Runs the requested DIF analyses on \code{dif.data$item.data} using \code{dif.data$dif.group.id} as the conditioning variable.
#'
#' Usage notes:
#' \itemize{
#' \item Dichotomous items must be coded 0 = incorrect, 1 = correct.
#' \item The Mantel-Haenszel and logistic methods can only accommodate dichotomous items.
#' \item Polytomous item responses are expected to be sequential integers (e.g., 1, 2, 3) but the lowest code does not have to be 0. Polytomous items are unit scaled when calculating the total or rest score.
#' \item Items with no variance are removed from \code{dif.data$item.data} when running the "MH", "logistic", and "IRT" methods.
#' \item Items with different number of response categories across levels of \code{dif.data$dif.group.id} are removed from \code{dif.data$item.data} for the "IRT" method.
#' }
#'
#' @return A \code{list} containing \code{dif.data} and the results from each selected method. The list is not formatted in a very readable way; it is intended to be passed to \code{\link[WBdif]{dif_models}} for further processing or to \code{\link[WBdif]{dif_report}} for user-friendly formatting.
#'
#' @examples
#' data("mdatlang")
#'
#' # prep data
#' dif.data <- dif_data_prep(item.data = mdatlang`[`5:ncol(mdatlang)],
#'                              tx.group.id = mdatlang$treated,
#'                              dif.group.id = mdatlang$gender,
#'                              cluster.id = mdatlang$clusterid,
#'                              na.to.0 = TRUE)
#'
#' # DIF analysis by dif.group.id
#' # using rest scores and binning match.scores by deciles to avoid empty cells in MH analysis
#' dif.analysis <- dif_analysis(dif.data = dif.data,
#'                            methods =  c("MH", "IRT")
#'                            match.type = "Rest",
#'                            match.bins = seq(0, 1, by = .1))
#' @export

dif_analysis <- function(dif.data, methods = c("loess", "MH", "logistic", "IRT"),
                         match.type = "Total", match.bins = NULL){

  # pulling constants from dif.data
  item.data <- dif.data$item.data
  dif.group.id <- dif.data$dif.group.id
  no.var.items <- dif.data$no.var.items
  no.var.by.group.items <- dif.data$no.var.by.group.items
  poly.items <- dif.data$poly.items

  ## Saving original data with all items for use in loess
  item.data.orig <- item.data

  if(length(no.var.items) > 0){
    item.data <- item.data[-c(no.var.items)]
  }

  ## Number of items in the measure for dif analysis
  n.items <- ncol(item.data)

  ## Creating match.scores object
  match.scores <- NULL

  #### LOESS ####
  if("loess" %in% methods){

      ## Calculating vector of total scores or list of rest scores
      if(match.type == "Rest"){ # Returns a list with n_items elements of length = nrow(item.data)

        match.scores <- lapply(X = c(1:ncol(item.data.orig)), FUN = sum_score,
                               item.data = item.data.orig, poly.items = poly.items)

      } else if(match.type == "Total"){ # a single vector of length = nrow(item.data)

        match.scores <- sum_score(item.data = item.data.orig, drops = NULL, poly.items = poly.items)

      }  else {
        stop("match.type argument must be 'Rest' or 'Total'")
      }

    loess <- dif_loess(item.data = item.data.orig,
                       dif.group.id = dif.group.id,
                       match.type = match.type,
                       match.scores = match.scores)

  } else {

    loess <- NULL

  }

  #### Mantel-Haenszel ####
  if("MH" %in% methods){

    if(length(poly.items) > 0){
      stop("Remove polytomous items from item.data to use MH method.")
    }

    # Need to recalculate match.scores if there were items with no variance
    if(is.null(match.scores) | length(no.var.items) > 0){

      ## Calculating vector of total scores or list of rest scores
      if(match.type == "Rest"){ # Returns a list with n_items elements of length = nrow(measure.data)

        match.scores <- lapply(X = c(1:ncol(item.data.orig)), FUN = sum_score,
                               item.data = item.data.orig)

      } else if(match.type == "Total"){ # a single vector of length = nrow(measure.data)

        match.scores <- sum_score(item.data = item.data.orig, drops = NULL)

      }  else {
        stop("match.type argument must be 'Rest' or 'Total'")
      }
    }

    MH <- dif_mh(item.data = item.data,
                 dif.group = dif.group.id,
                 match.type = match.type,
                 match.scores = match.scores,
                 match.bins = match.bins)

  } else {

    MH <- NULL

  }

  #### Logistic Regression ####
  if("logistic" %in% methods){

    if(length(poly.items) > 0){
      stop("Remove polytomous items from item.data to use logistic method.")
    }

    # if match.scores was previously calculated in loess & no items need to be dropped
    # OR match.scores was previously calculated in the MH method, use it.
    # Otherwise, (re-)calculate match.scores
    if(!is.null(match.scores) & (length(no.var.items) == 0 | ("MH" %in% methods))){

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
                             dif.group.id = dif.group.id,
                             match.type = match.type,
                             match.scores = match.scores)

  } else {

    logistic <- NULL

  }


  #### Item Response Theory ####
  if("IRT" %in% methods){

    ## Removing items with no within group variance, if they exist
    if(length(no.var.by.group.items) > 0){
      item.data <- item.data[-c(no.var.by.group.items)]
    }

    IRT <- dif_irt(item.data = item.data,
                   dif.group.id = dif.group.id)

  } else {

    IRT <- NULL

  }

  dif.analysis <- list(loess = loess,
                       MH = MH,
                       logistic = logistic,
                       IRT = IRT,
                       inputs = c(dif.data, match.type = match.type))


  return(dif.analysis)
}
