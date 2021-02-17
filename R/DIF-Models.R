#' IRT models with biased items
#'
#' Primarily an interim function between dif_analysis and effect_robustness
#'
#' @param dif.analysis Output from \code{\link[WBdif]{dif_analysis}}
#' @param dif.data Output from \code{\link[WBdif]{dif_data_prep}}. Only required if \code{dif.analysis} is not provided.
#' @param biased.items If \code{dif.analysis} is specified, a character indicating the method from which to extract the biased items. Options are "IRT" (default), "logistic", "MH".
#' If \code{dif.data} is specified, the \code{vector} of column indices indicating the biased items in \code{dif.data$item.data}
#'
#' @details
#' Estimates a 2PL IRT model with the parameters on biased items feed over groups - the DIF model.
#' The No DIF model, which constrains all item parameters to be equal across the groups, is either extracted from \code{dif.analysis} or estimated as well.
#' The IRT models are estimated using \code{\link[mirt]{multipleGroup}}.
#'
#' @return
#' A named \code{list} containing the data and IRT models to estimate treatment effects.
#'
#' @examples
#' data("mdat")
#'
#' # prep data
#' dif.data <- dif_data_prep(item.data = mdat`[`5:ncol(mdat)],
#'                              dif.group.id = mdat$gender,
#'                              na.to.0 = TRUE)
#'
#' # With biased items identified via DIF analysis
#' dif.analysis <- dif_analysis(dif.data = dif.data,
#'                              methods =  c("MH", "IRT"),
#'                              match.type = "Rest",
#'                              match.bins = seq(0, 1, by = .1))
#' dif.models <- dif_models(dif.analysis = dif.analysis, biased.items = "MH")
#'
#' # With user-specified biased items
#' dif.models <- dif_models(dif.data = dif.data, biased.items = c(1, 5, 7))
#'
#' @export

dif_models <- function(dif.analysis = NULL, dif.data = NULL, biased.items = "IRT"){

  stop.message <- "biased.items must be one of c(\'IRT\', \'logistic\', \'MH\') or column indices of item.data"

  ## If dif.analysis is provided, extract inputs and no.dif.mod (if it exists)
  if(!is.null(dif.analysis)){

    inputs <- dif.analysis$inputs

    if(is.null(dif.analysis$IRT)){
      no.dif.mod <- NULL
    } else {
      no.dif.mod <- dif.analysis$IRT$no.dif.mod # IRT model with parameters constrained to be equal between groups
    }
  } else if(!is.null(dif.data)){

    inputs <- dif.data
    no.dif.mod <- NULL

    if(is.character(biased.items)){
      stop("When dif.analysis = NULL, biased.items must be a vector of column indices for dif.data$item.data")
    }
  } else {
    stop("One of dif.analysis or dif.data must be provided.")
  }

  ## biased item checks
  if(is.character(biased.items)){
    if (!biased.items %in% c("IRT", "logistic", "MH")) {
      stop(stop.message)
    } else {
      biased.items <- dif.analysis[[biased.items]]$biased.items
      if(is.character(biased.items)){
        stop(paste0("No biased items were reported by \'dif.analysis\' using the provided method."))
      }
    }
  }


  if (is.double(biased.items) | is.integer(biased.items)) {
    if (mean(biased.items %in% 1:ncol(inputs$item.data)) == 1) {
    } else {stop(stop.message)}
  } else {stop(stop.message)}


  # Remove items with no variance within a group, if they exist
  if (length(inputs$no.var.by.group.items) > 0) {
    inputs$item.data <- inputs$item.data[-c(inputs$no.var.by.group.items)]
  }


  # IRT model with parameters constrained to be equal between groups
  if(is.null(no.dif.mod)){
    no.dif.mod <- mirt::multipleGroup(data = inputs$item.data,
                                      model = 1,
                                      group = inputs$dif.group.id,
                                      invariance = c('slopes', 'intercepts',
                                                     'free_var','free_means'))
  }

  # IRT model with biased items freed over groups
  dif.mod <- mirt::multipleGroup(data = inputs$item.data,
                                 model = 1,
                                 group = inputs$dif.group.id,
                                 invariance = c(names(inputs$item.data)[-biased.items],
                                                'free_var','free_means'))

  return(list(no.dif.mod = no.dif.mod,
              dif.mod = dif.mod,
              biased.items = biased.items,
              inputs = inputs))
}