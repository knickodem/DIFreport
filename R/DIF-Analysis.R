#' Differential item functioning
#'
#' Evaluates differential item functioning (DIF) using loess, Mantel-Haenszel (MH),
#' logistic regression, and item response theory (IRT) approaches.
#'
#' @param measure.data data frame of dichotomous item responses with subjects in rows
#' and items in columns
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
#' This function primarily serves as a wrapper around the method specific functions, but
#' also identifies and reports items with no variance (e.g., all 0 responses) or no
#' variance within the levels of \code{dif.group}. The "loess" method produces loess curves
#' for every item in \code{measure.data}. Items with no variance are removed from
#' \code{measure.data} for the "MH", "logistic", and "IRT" methods because response
#' variance is a prerequisite for DIF. Additionally, items with no variance within
#' a \code{dif.group} level are removed from the "IRT" method because these lead to
#' the models being under-identified.
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
#' dif.by.tx <- dif_analysis(measure.data = wb.measure'\['3:7],
#'                           dif.group = wb.measure$tx,
#'                           score.type = "Rest",
#'                           methods = c("loess", "MH", "logistic", "IRT"),
#'                           match.bins = tenths)
#'
#' ## DIF analysis by gender using total scores without binning for MH
#' dif.by.gender <- dif_analysis(measure.data = wb.measure'\['3:7],
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
  var.check <- lapply(measure.data, var)
  nvi <- which(var.check == 0)

  ## Items with no variance for one group (different interpretation for polytomous item)
  # current function: does any cell in table == 0?
  # These items are only removed from IRT analysis
  var.check.group <- lapply(measure.data, function(x) { sum(table(x, dif.group) == 0) })
  nvgi <- which(var.check.group > 0)

  ## No variance = No DIF so removing no variance items from measure.data.
  if(length(nvi) > 0){
    measure.data <- measure.data[-c(nvi)]
  }

  ## Number of items in the measure for dif analysis
  n.items <- ncol(measure.data)

  ## Calculating vector of total scores or list of rest scores
  if(score.type == "Rest"){ # Returns a list with n.items elements of length = nrow(measure.data)

    match.scores <- lapply(c(1:n.items), sum_score, scale.data = measure.data)

  } else if(score.type == "Total"){ # returns a single vector of length = nrow(measure.data)

    match.scores <- sum_score(scale.data = measure.data, drops = NULL)

  } else {
    stop("score.type argument must be 'Rest' or 'Total'")
  }


  #### LOESS ####
  if("loess" %in% methods){

    # Need to recalculate match.scores if there were items with no variance
    if(length(nvi) > 0){

      ## Calculating vector of total scores or list of rest scores
      if(score.type == "Rest"){ # Returns a list with n_items elements of length = nrow(measure.data)

        loess.match <- lapply(c(1:ncol(md.orig)), sum_score, scale.data = md.orig)

      } else if(score.type == "Total"){ # a single vector of length = nrow(measure.data)

        loess.match <- sum_score(scale.data = md.orig, drops = NULL)

      }  else {
        stop("that's weird")
      }
    } else {
      loess.match <- match.scores
    }

    loess <- get_loess(scale.data = md.orig,
                       dif.group = dif.group,
                       score.type = score.type,
                       match = loess.match)

  } else{

    loess <- NULL

  }

  #### Mantel-Haenszel ####
  if("MH" %in% methods){

    MH <- get_mh(scale.data = measure.data,
                 dif.group = dif.group,
                 score.type = score.type,
                 match = match.scores,
                 match.bins = match.bins)

  } else {

    MH <- NULL

  }

  #### Logistic Regression ####
  if("logistic" %in% methods){



    logistic <- get_logistic(scale.data = measure.data,
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

    IRT <- get_irt(scale.data = measure.data,
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
                                     no.var.items = nvi,
                                     no.var.by.group.items = nvgi))

  return(dif.analysis)

}
