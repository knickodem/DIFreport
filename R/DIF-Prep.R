#' Data prep for \code{DIFreport} functions.
#'
#' Data pre-processing to gather information and prepare data for use in other \code{DIFreport} functions.
#'
#' @param data a \code{data.frame} containing the item responses with subjects in rows and items in columns. Items can be dichotomous or polytomous, but the columns must be numeric. All reverse coding should be completed before using this function.
#' @param dif.groups column position or name in \code{data} indicating the two (and only two) groups to compare for DIF.
#' @param ref.name character; specifying the reference group in \code{dif.groups}. This primarily for organizing the report output.
#' If examining standardized mean differences, the standard deviation of this group can be used rather than the pooled standard deviation
#' by setting \code{pooled = FALSE} in \code{\link[DIFreport]{dif_report}} or \code{\link[DIFreport]{summary_report}}
#' @param items vector of column positions or names in \code{data} indicating the items to analyze for DIF. Default is to use all columns.
#' @param anchors vector of column positions or names \code{data} indicating the anchor items. If specified, must be a subset of \code{items}.
#' @param poly.items vector of column positions or names \code{data} indicating polytomous items. If \code{NULL} (default), polytomous items will be auto-detected as those with > 2 responses options.
#' @param max.values the maximum value each item in \code{poly.items} can take, declared by an integer vector of the same length as \code{poly.items} or a single value to be used for all \code{poly.items}. If \code{NULL} (default), values will be auto-detected. This value is used for unit-scoring polytomous items for loess, MH, and logistic methods.
#' @param cond.groups column name or number in \code{data} indicating the two (and only two) groups on which to condition the mean difference between \code{dif.groups}.
#' @param cluster column name or number in \code{data} indicating the primary sampling unit in a multi-stage or clustered sampling design -- used to adjust effect sizes and their standard errors.
#' @param na.to.0 After removing empty rows, should remaining NAs in \code{items} columns be converted to 0? Default is FALSE.
#'
#' @details
#' This function saves the input data in a format used by other \code{DIFreport} functions and also runs a number of pre-processing steps:
#' \itemize{
#'   \item Drops rows in \code{data} with \code{NA} for all \code{items} columns, \code{dif.groups}, \code{cond.groups} (if specified), or \code{cluster} (if specified).
#'   \item Converts \code{dif.groups} and to a \code{factor} and confirms there are only two groups. Same for \code{cond.groups} if specified.
#'   \item Flag items with no variance.
#'   \item Flag items with different number of response categories across the \code{dif.groups}.
#'   \item Identify items with more than two response categories (i.e., polytomous items).
#' }
#' It is recommended, but not required, that responses for each item are coded as consecutive integers with a minimum value of 0 (e.g., 0, 1, 2).
#' @return A named \code{list} containing the pre-processed inputs and item flags.
#'
#' @examples
#' data("mdat")
#'
#' dif.data <- dif_prep(data = mdat,
#'                              dif.groups = "treated",
#'                              ref.name = "Control",
#'                              items = 5:ncol(mdat)
#'                              cond.groups ="gender",
#'                              cluster = "clusterid",
#'                              na.to.0 = TRUE)
#'
#' @export

dif_prep <- function(data, dif.groups, ref.name = NULL, items = names(data),
                     anchors = NULL, poly.items = NULL, max.values = NULL,
                     cond.groups = NULL, cluster = NULL, na.to.0 = FALSE){

  data <- as.data.frame(data)
  item.data <- data[,items]

  ## Input checks
  # Are anchors and poly.items subsets of items? What are positions in item.data?
  if(!is.null(anchors)){
    # if position, grab names
    if(is.numeric(anchors)){
      anchors <- names(data)[anchors]
    }
    if(is.character(anchors)){
      # make sure names are subset of item.data
      if(!all(anchors %in% names(item.data))){
        stop("anchors (", paste(anchors, collapse = ", "), ") must be a subset of items (",
             paste(names(item.data), collapse = ", "), ")")
      }
      # convert names to positions in item.data for later use
      a.n <- anchors
      anchors <- which(names(item.data) %in% anchors)
      names(anchors) <- a.n

    } else {stop("Something went wrong with anchors. Submit issue to https://github.com/knickodem/DIFreport/issues")}
  }
  if(!is.null(poly.items)){
    # if position, grab names
    if(is.numeric(poly.items)){
      poly.items <- names(data)[poly.items]
    }
    if(is.character(poly.items)){
      # make sure names are subset of item.data
      if(!all(poly.items %in% names(item.data))){
        stop("poly.items (", paste(poly.items, collapse = ", "), ") must be a subset of items (",
             paste(names(item.data), collapse = ", "), ")")
      }
      # convert names to positions in item.data for later use
      p.n <- poly.items
      poly.items <- which(names(item.data) %in% poly.items)
      names(poly.items) <- p.n

    } else {stop("Something went wrong with poly.items Submit issue to https://github.com/knickodem/DIFreport/issues")}
  }

  # are items numeric?
  nn <- which(lapply(item.data, is.numeric) == 0)
  # apply(data[,items], 2, is.numeric) # sometimes identifies SPSS variables as character when they should be numeric
  if(length(nn) > 0){
    stop("items must be numeric. Non-numeric items: ", paste(names(nn), collapse = ", " ))
  }


  # Need dif.groups to be a 2-level factor (for now)
  dif.groups <- factor(data[[dif.groups]])
  if(length(levels(dif.groups)) > 2){
    stop("dif.groups must have only 2 levels")
  }

  if(!is.null(comp.name)){
    comp.name <- as.character(comp.name) # in case numeric is supplied
    if(!(comp.name %in% levels(dif.groups))) {
      stop("comp.name must be a value in dif.groups column")
    }
  }

  ## Identifying cases to drop based on missing data
  # Note: FALSE elements in drop.cases are the cases removed from analysis

  # Cases with NA for all items
  drop.cases <- apply(is.na(item.data), 1, mean) != 1

  # Cases missing on dif.groups
  drop.cases <- ifelse(is.na(dif.groups), FALSE, drop.cases)


  # prepping cond.groups and identifying missing
  if(!is.null(cond.groups)){
  # Need cond.groups to be a 2-level factor
  cond.groups <- factor(data[[cond.groups]])
  if(length(levels(cond.groups)) > 2){
    stop("cond.groups must have only 2 levels")
  }
  # Cases missing on cond.groups
  drop.cases <- ifelse(is.na(cond.groups), FALSE, drop.cases)
  }

   # prepping cluster and identifying any missing
  if(!is.null(cluster)) {
    cluster <- factor(data[[cluster]])
    drop.cases <- ifelse(is.na(cluster), FALSE, drop.cases)
    cluster <- cluster[drop.cases]
  }

  ## Dropping the missing cases
  item.data <- item.data[drop.cases, ]
  dif.groups <- dif.groups[drop.cases]
  if(!is.null(cond.groups)){
    cond.groups <- cond.groups[drop.cases]
  }

  ## Replacing remaining NAs with 0 (should we put before removing empty rows?)
  if(na.to.0 == TRUE) {
    item.data[is.na(item.data)] <- 0
  }

  ## Dropping items with no variance
  vc <- which(lapply(item.data, var, na.rm = TRUE) == 0)
  if(length(vc) > 0){
    item.data <- item.data[-c(vc)]
    warning("The following items had no variance and were dropped from the data: ",
            paste(names(vc), collapse = ", "))
  }

  ## Items with any empty cells within dif.groups
  # need to be removed from IRT analysis (done in dif_analysis), but could just remove here
  vcg <- lapply(item.data, function(x) { sum(table(x, dif.groups, useNA = "no") == 0) })
  gvar0.items <- which(vcg > 0)
  if(length(gvar0.items) > 0){
    warning("The following items had no variance within a group: ",
            paste(names(gvar0.items), collapse = ", "), ".\nWe recommend dropping these items as they can cause issues in the subsequent analysis.")
  }

  ## Polytomous items; returns named integer vector
  if(is.null(poly.items)){
  poly.items <- which(apply(item.data, 2, function(x) length(unique(na.omit(x)))) > 2)
  }

  # maximum value of polytomous items
  if(is.null(max.values)){
    if(length(poly.items) == 1){ # otherwise the apply function breaks
      max.values <- max(item.data[[poly.items]], na.rm = TRUE)
    } else{
      max.values <- apply(item.data[,poly.items], 2, function(x) max(x, na.rm = TRUE))
    }
  } else if(length(max.values) == 1){
    max.values <- rep(max.values, times = length(poly.items))
  }
  if(length(max.values) != length(poly.items)){
    stop("max.values and poly.items must be the same length, but were ", length(max.values), " and ", length(poly.items),", respectively")
  }

  ## Lowest response option should be 0 for proper scoring
  not0 <- which(apply(item.data, 2, function(x) min(x, na.rm = TRUE)) != 0)
  if(length(not0) > 0){
    warning("Lowest response option should be 0 for proper scoring with loess, MH, and logistic methods.\nWe recommend recoding these items to have 0 as the lowest response:\n", paste(names(not0), collapse = ", "),
            "\nIf these are polytomous items where 0 was an option but not observed, add them to the poly.items argument.")
  }

  output <- list(item.data = item.data,
                 dif.groups = dif.groups,
                 comp.name = comp.name,
                 cond.groups = cond.groups,
                 cluster = cluster,
                 anchors = anchors,
                 gvar0.items = gvar0.items,
                 poly.items = poly.items,
                 max.values = max.values)

  class(output) <- "DIFprep"

  return(output)
}