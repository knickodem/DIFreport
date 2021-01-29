#' Differential item functioning
#'
#' Evaluates differential item functioning (DIF) using loess, Mantel-Haenszel (MH),
#' logistic regression, and item response theory (IRT) approaches.
#'
#' @param measure.data data frame of item responses with subjects in rows
#' and items in columns. See Details for response formats.
#' @param dif.group factor or character vector with 2 levels; indicates group membership
#' for which DIF is evaluated. If a character vector, this will be transformed
#' to a \code{\link[base]{factor}}.
#' @param methods character vector with one or more of the four available methods:
#' "loess", "MH", "logistic", and "IRT".
#' The default is all four methods.
#' @param score.type character indicating whether the total summed score ("Total") or the
#' summed score excluding the item under investigation ("Rest") should be used for the
#' matching variable in the loess, MH, or logistic regression methods.
#' @param match.bins optional vector of bin sizes for stratifying the matching variable in
#' the MH method. This is passed to the \code{probs} argument of
#' \code{\link[stats]{quantile}}.
#'
#' @details
#' This function calls the specific DIF methods functions (loess, MH, logistic, IRT)
#' and compiles the results.
#'
#' Dichotomous items are expected to be coded 0 = incorrect, 1 = correct.
#' The Mantel-Haenszel and logistic methods can only accommodate dichotomous items.
#' Polytomous item responses are expected to be sequential integers (e.g., 1, 2, 3) but
#' the lowest code does not have to be 0. Polytomous items are unit scaled when
#' calculating the total or rest score.
#'
#' Response variance is a prerequisite for DIF, so the function identifies and
#' reports items with no variance or no variance within the levels of \code{dif.group}
#' (e.g., all cases have same response). The "loess" method produces
#' loess curves for every item in \code{measure.data}, but items with no variance
#' are removed from \code{measure.data} for the "MH", "logistic", and "IRT" methods.
#' Additionally, items with no variance within a \code{dif.group} level are removed from
#' the "IRT" method because these lead to under-identified models.
#'
#' See \code{link[WBdif]{dif_mh}}, \code{link[WBdif]{dif_logistic}}, and
#' \code{link[WBdif]{dif_irt}} for details about the specific DIF methods.
#'
#'
#' @return a list with DIF results from each selected method
#'
#' @examples
#' wb.measure <- data.frame(tx = rep(c("tx", "control"), times = 10),
#'                          gender = rep(c("male", "female"), each = 10),
#'                          item1 = sample(c(0,1), 20, replace = TRUE),
#'                          item2 = sample(c(0,1), 20, replace = TRUE),
#'                          item3 = sample(c(0,1), 20, replace = TRUE),
#'                          item4 = sample(c(0,1), 20, replace = TRUE),
#'                          item5 = sample(c(0,1), 20, replace = TRUE))
#'
#' ## Using deciles of rest (or total) score for match.bins to
#' ## avoid empty cells in the two-way MH tables
#' tenths <- seq(0, 1, by = .1)
#'
#' ## DIF analysis by treatment condition using rest scores and binning by deciles for MH
#' dif.by.tx <- dif_analysis(measure.data = wb.measure`[`3:7`]`,
#'                           dif.group = wb.measure$tx,
#'                           score.type = "Rest",
#'                           methods = c("loess", "MH", "logistic", "IRT"),
#'                           match.bins = tenths)
#'
#' ## DIF analysis by gender using total scores without binning for MH
#' dif.by.gender <- dif_analysis(measure.data = wb.measure`[`3:7`]`,
#'                               dif.group = wb.measure$gender,
#'                               score.type = "Total",
#'                               methods = c("loess", "MH", "logistic", "IRT"),
#'                               match.bins = NULL)
#'
#'
#' @export

dif_analysis <- function(measure.data,
                         dif.group,
                         methods = c("loess", "MH", "logistic", "IRT"),
                         score.type = c("Rest", "Total"),
                         match.bins = NULL){

  ## Need dif.group to be a factor
  if(is.factor(dif.group) == FALSE){
    dif.group <- factor(dif.group)
  }

  #### Identifying problematic items ####
  md.orig <- measure.data # saving original data with all items for use in loess

  ## Items with no variance
  var.check <- lapply(measure.data, var, na.rm = TRUE)
  nvi <- which(var.check == 0)

  ## Items with no variance for one group (different interpretation for polytomous item)
  # current function: does any cell in table == 0?
  # These items are only removed from IRT analysis
  var.check.group <- lapply(measure.data,
                            function(x) { sum(table(x, dif.group, useNA = "no") == 0) })
  nvgi <- which(var.check.group > 0)

  ## No variance = No DIF so removing no variance items from measure.data.
  if(length(nvi) > 0){
    measure.data <- measure.data[-c(nvi)]
  }

  ## Number of items in the measure for dif analysis
  n.items <- ncol(measure.data)

  ## identify polytomous items
  poly <- which(apply(measure.data, 2, max, na.rm = TRUE) > 1)
  # returns named integer vector

  ## creating match.scores object
  match.scores <- NULL

  #### LOESS ####
  if("loess" %in% methods){

      ## Calculating vector of total scores or list of rest scores
      if(score.type == "Rest"){ # Returns a list with n_items elements of length = nrow(measure.data)

        match.scores <- lapply(X = c(1:ncol(md.orig)), FUN = sum_score,
                               scale.data = md.orig, poly = poly)

      } else if(score.type == "Total"){ # a single vector of length = nrow(measure.data)

        match.scores <- sum_score(scale.data = md.orig, drops = NULL, poly = poly)

      }  else {
        stop("score.type argument must be 'Rest' or 'Total'")
      }


    loess <- dif_loess(scale.data = md.orig,
                       dif.group = dif.group,
                       score.type = score.type,
                       match = match.scores)

  } else{

    loess <- NULL

  }

  #### Mantel-Haenszel ####
  if("MH" %in% methods){

    if(length(poly) > 0){
      stop("Remove polytomous items from measure.data to use MH method.")
    }

    # Need to recalculate match.scores if there were items with no variance
    if(is.null(match.scores) | length(nvi) > 0){

      ## Calculating vector of total scores or list of rest scores
      if(score.type == "Rest"){ # Returns a list with n_items elements of length = nrow(measure.data)

        match.scores <- lapply(X = c(1:ncol(md.orig)), FUN = sum_score,
                               scale.data = md.orig)

      } else if(score.type == "Total"){ # a single vector of length = nrow(measure.data)

        match.scores <- sum_score(scale.data = md.orig, drops = NULL)

      }  else {
        stop("score.type argument must be 'Rest' or 'Total'")
      }
    }

    MH <- dif_mh(scale.data = measure.data,
                 dif.group = dif.group,
                 score.type = score.type,
                 match = match.scores,
                 match.bins = match.bins)

  } else {

    MH <- NULL

  }

  #### Logistic Regression ####
  if("logistic" %in% methods){

    if(length(poly) > 0){
      stop("Remove polytomous items from measure.data to use logistic method.")
    }

    # if match.scores was previously calculated in loess & no items need to be dropped
    # OR match.scores was previously calculated in the MH method, use it.
    # Otherwise, (re-)calculate match.scores
    if(!is.null(match.scores) & (length(nvi) == 0 | ("MH" %in% methods))){

      match.scores <- match.scores

    } else {

      ## Calculating vector of total scores or list of rest scores
      if(score.type == "Rest"){ # Returns a list with n_items elements of length = nrow(measure.data)

        match.scores <- lapply(X = c(1:ncol(md.orig)), FUN = sum_score,
                               scale.data = md.orig, poly = poly)

      } else if(score.type == "Total"){ # a single vector of length = nrow(measure.data)

        match.scores <- sum_score(scale.data = md.orig, drops = NULL, poly = poly)

      }  else {
        stop("score.type argument must be 'Rest' or 'Total'")
      }
    }


    logistic <- dif_logistic(scale.data = measure.data,
                             dif.group = dif.group,
                             score.type = score.type,
                             match = match.scores)


  } else{

    logistic <- NULL

  }


  #### Item Response Theory ####
  if("IRT" %in% methods){

    ## Removing items with no within group variance, if they exist
    if(length(nvgi) > 0){
      measure.data <- measure.data[-c(nvgi)]
    }

    IRT <- dif_irt(scale.data = measure.data,
                   dif.group = dif.group)

  } else{

    IRT <- NULL

  }

  dif.analysis <- list(loess = loess,
                       MH = MH,
                       logistic = logistic,
                       IRT = IRT,
                       inputs = list(data = md.orig,
                                     dif.group = dif.group,
                                     score.type = score.type,
                                     poly.items = poly,
                                     no.var.items = nvi,
                                     no.var.by.group.items = nvgi))

  return(dif.analysis)

}
