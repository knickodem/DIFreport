#' Differential item functioning
#'
#' Evaluates differential item functioning (DIF) using loess, Mantel-Haenszel (MH),
#' logistic regression, and item response theory (IRT) approaches.
#'
#' @param dif.data The output of \code{WBdif::data_prep}.
#' @param methods A character \code{vector} with one or more of \code{c("loess", "MH", "logistic", "IRT")}. The default is all four methods.
#' @param match.type Οne of \code{c("Total", "Rest")}. Determines whether the total score or rest score should be used as the stratifying variable for loess, MH, and logistic regression methods.
#' @param match.bins Optional vector of bin sizes for stratifying the matching variable in
#' the MH method. This is passed to the \code{probs} argument of \code{stas::quantile}.
#'
#' @details
#'  This is a wrapper for the functions \code{WBdif{ functions: \code{loess}, \code{MH}, \code{logistic}, and \code{IRT}. Runs the requested DIF analyses on \code{dif.data$item.data} using \code{dif.data$dif.group.id} as the conditioning variable.

#'
#' Usage notes:
#' \itemize{
#' \item Dichotomous items must be coded 0 = incorrect, 1 = correct.
#' \item The Mantel-Haenszel and logistic methods can only accommodate dichotomous items.
#' \item Polytomous item responses are expected to be sequential integers (e.g., 1, 2, 3) but the lowest code does not have to be 0. Polytomous items are unit scaled when calculating the total or rest score.
#' \item Items with no variance are removed from \code{dif.data$item.data} when running the "MH", "logistic", and "IRT" methods.
#' \item Items with different number of response categories accross levels of \code{dif.data$dif.group.id} are removed from \code{dif.data$item.data} for the "IRT" method.
#' }
#'
#' @return A \code{list} containing \code{dif.data} and the results from each selected method. The list is not formatted in a very readable way; it is intended to be passed to \code{WBdif::dif_models} for further processing or to \code{WBdif::dif_report} for user-friendly formatting.
#'
#' @examples
#' dat <- data.frame(tx = rep(c("tx", "control"), times = 10),
#'                          gender = rep(c("male", "female"), each = 10),
#'                          item1 = sample(c(0,1), 20, replace = TRUE),
#'                          item2 = sample(c(0,1), 20, replace = TRUE),
#'                          item3 = sample(c(0,1), 20, replace = TRUE),
#'                          item4 = sample(c(0,1), 20, replace = TRUE),
#'                          item5 = sample(c(0,1), 20, replace = TRUE))
#' ## Prep data
#' dif.data <-  dif_data_prep(dat`[`, -c(1, 2)`]`,
#'                            dif.group.id = dat$gender,
#'                            tx.group.id = dat$tx)
#'
#' ## Using deciles of rest (or total) score for match.bins to avoid empty cells
#' tenths <- seq(0, 1, by = .1)
#'
#' ## DIF analysis by dif.group.id using rest scores and binning by deciles for MH
#' dif.by.tx <- dif_analysis(dif.data = dif.data,
#'                            methods =  "MH",
#'                            match.type = "Rest",
#'                            match.bins = tenths)
#' @export

dif_analysis <- function(dif.data,
                         methods = c("loess", "MH", "logistic", "IRT"),
                         match.type = "Total",
                         match.bins = NULL){

  # PH: pulling constants now in dif.data
  item.data <- dif.data$item.data
  dif.group.id <- dif.data$dif.group.id
  no.var.items <- dif.data$no.var.items
  no.var.by.group.items <- dif.data$no.var.by.group.items
  poly <- dif.data$poly.items

    ## Need dif.group.id to be a factor
  if(is.factor(dif.group.id) == FALSE){
    dif.group.id <- factor(dif.group.id)
  }


  # Saving original data with all items for use in loess
  item.data.orig <- item.data

  #### PH: Moved the following stuff to data_prep:
    # no.var.items
    # no.var.by.group.items
    # poly.items


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
                               scale.data = item.data.orig, poly = poly)

      } else if(match.type == "Total"){ # a single vector of length = nrow(item.data)

        match.scores <- sum_score(scale.data = item.data.orig, drops = NULL, poly = poly)

      }  else {
        stop("match.type argument must be 'Rest' or 'Total'")
      }


    loess <- dif_loess(scale.data = item.data.orig,
                       dif.group = dif.group.id,
                       score.type = match.type,
                       match = match.scores)

  } else{

    loess <- NULL

  }

  #### Mantel-Haenszel ####
  if("MH" %in% methods){

    if(length(poly) > 0){
      stop("Remove polytomous items from item.data to use MH method.")
    }

    # Need to recalculate match.scores if there were items with no variance
    if(is.null(match.scores) | length(no.var.items) > 0){

      ## Calculating vector of total scores or list of rest scores
      if(match.type == "Rest"){ # Returns a list with n_items elements of length = nrow(measure.data)

        match.scores <- lapply(X = c(1:ncol(item.data.orig)), FUN = sum_score,
                               scale.data = item.data.orig)

      } else if(match.type == "Total"){ # a single vector of length = nrow(measure.data)

        match.scores <- sum_score(scale.data = item.data.orig, drops = NULL)

      }  else {
        stop("match.type argument must be 'Rest' or 'Total'")
      }
    }

    MH <- dif_mh(scale.data = item.data,
                 dif.group = dif.group.id,
                 score.type = match.type,
                 match = match.scores,
                 match.bins = match.bins)

  } else {

    MH <- NULL

  }

  #### Logistic Regression ####
  if("logistic" %in% methods){

    if(length(poly) > 0){
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
                               scale.data = item.data.orig, poly = poly)

      } else if(match.type == "Total"){ # a single vector of length = nrow(item.data)

        match.scores <- sum_score(scale.data = item.data.orig, drops = NULL, poly = poly)

      }  else {
        stop("match.type argument must be 'Rest' or 'Total'")
      }
    }


    logistic <- dif_logistic(scale.data = item.data,
                             dif.group = dif.group.id,
                             score.type = match.type,
                             match = match.scores)


  } else{

    logistic <- NULL

  }


  #### Item Response Theory ####
  if("IRT" %in% methods){

    ## Removing items with no within group variance, if they exist
    if(length(no.var.by.group.items) > 0){
      item.data <- item.data[-c(no.var.by.group.items)]
    }

    IRT <- dif_irt(scale.data = item.data,
                   dif.group = dif.group.id)

  } else{

    IRT <- NULL

  }

  dif.analysis <- list(loess = loess,
                       MH = MH,
                       logistic = logistic,
                       IRT = IRT,
                       inputs = c(dif.data, match.type = match.type))


  return(dif.analysis)

}




dif_models <- function(dif.analysis, biased.items = "IRT"){

stop.message <- "biased.items must be one of c(\'IRT\', \'logistic\', \'MH\') or column indices of item.data"

  if (is.character(biased.items)) {
    if (!biased.items%in%c("IRT", "Logistic", "MH")) {
      stop(stop.message)
    }
    if (biased.items == "IRT") {
      biased.items <- dif.analysis$IRT$biased.items
    } else if (biased.items == "MH") {
        biased.items <- dif.analysis$MH$biased.items
    } else if (biased.items == "logistic") {
        biased.items <- dif.analysis$logistic$biased.items
    }
    if(biased.items == "No DIF was detected") {
      stop(paste0("No biased items were reported by \'dif.analysis\' using the provided method."))
    }
  }


  if (is.double(biased.items) | is.integer(biased.items)) {
    if (mean(biased.items%in%1:ncol(dif.analysis$input$item.data)) == 1) {
    } else {stop(stop.message)}
  } else {stop(stop.message)}


  ### Run multigroup IRT models
  inputs <- dif.analysis$inputs
  # Remove items with no variance, if they exist

  if (length(inputs$no.var.by.group.items) > 0) {
    inputs$data <- inputs$data[-c(inputs$no.var.by.group.items)]
  }

  ## IRT model with parameters constrained to be equal between groups
  no.dif.mod <- dif.analysis$IRT$no.dif.mod

  if (is.null(no.dif.mod)) {
    no.dif.mod <-
      mirt::multipleGroup(data = inputs$item.data,
                          model = 1,
                          group = inputs$dif.group.id,
                          invariance = c('slopes', 'intercepts',
                                         'free_var','free_means'))
  }


  ## IRT model with biased items freed over groups
    dif.mod <-
      mirt::multipleGroup(data = inputs$item.data,
                          model = 1,
                          group = inputs$dif.group.id,
                          invariance = c(names(inputs$item.data)[-biased.items],
                                         'free_var','free_means'))

  return(list(no.dif.mod = no.dif.mod,
              dif.mod = dif.mod,
              biased.items = biased.items,
              inputs = inputs))
}

