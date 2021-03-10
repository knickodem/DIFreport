#' Estimate a 2-group IRT model with biased items
#'
#'  This function is a wrapper for \code{mirt::multipleGroup} that lets the user extract biased items from the output of \code{dif.analysis} or specify a custom list of biased items. All parameters of items identified as biased items are allowed to vary over groups (i.e. non-uniform DIF is assumed).
#'
#' @param dif.analysis Output from \code{\link[WBdif]{dif_analysis}}
#' @param biased.items One of \code{"IRT" "logistic", "MH"}, indicating the method from \code{dif_analysis} used to identify the biased items, or a numeric vector indicating the column indices of \code{item.data} that identify the biased items.
#'
#' @details
#' Estimates a 2PL / GRM IRT model with the parameters of biased items freed over groups. A comparable model in which all item parameters are equal across the groups is either extracted from \code{dif.analysis} or estimated. The IRT models are estimated using \code{\link[mirt]{multipleGroup}}.
#'
#' @return
#' A named \code{list} containing the DIF and no-DIF IRT models, the column indices if \code{item.data} identifying the biased items, as well as the output of \code{dif_data_prep} (passed from \code{dif.analysis}).
#'
#' @examples
#' data("mdat")
#'
#' # prep data
#' dif.data <- dif_data_prep(item.data = mdat[5:ncol(mdat)],
#'                              dif.group.id = mdat$gender,
#'                              na.to.0 = TRUE)
#'
#' dif.analysis <- dif_analysis(dif.data = dif.data, dif.methods =  c("MH", "IRT"))
#'
#' # With biased items identified via DIF analysis
#' dif.models <- dif_models(dif.analysis = dif.analysis, biased.items = "MH")
#'
#' # With user-specified biased items
#' dif.models2 <- dif_models(dif.analysis = dif.analysis, biased.items = c(1, 5, 7))
#'
#' @export

dif_models <- function(dif.analysis, biased.items = "IRT"){

  stop.message <- "biased.items must be one of c(\'IRT\', \'logistic\', \'MH\') or column indices of item.data"

    inputs <- dif.analysis$inputs

    if(is.null(dif.analysis$IRT)){
      no.dif.mod <- NULL
    } else {
      no.dif.mod <- dif.analysis$IRT$no.dif.mod # IRT model with parameters constrained to be equal between groups
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
