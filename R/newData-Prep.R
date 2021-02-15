#' Data prep for \code{WBdif} functions.
#'
#' Data pre-processing and formatting; needs to be called before using other \code{WBdif} functions.
#'
#' @param item.data A \code{data.frame} of item responses with subjects in rows
#' and items in columns. Data can be binary or ordered categorical.
#' @param tx.group.id A \code{vector} of length \code{nrow(item.data)} indicating the treatment groups. Must have exactly two unique value.
#' @param dif.group.id A \code{vector} of length \code{nrow(item.data)} indicating the DIF groups. Must have exactly two unique value. See details for discussion of default value.
#' @param cluster.id An optional \code{vector} of length \code{nrow(item.data)} indicating the primary sampling unit in a multi-stage / clustered sampling design -- used to adjust effect sizes and their standard errors.
#' @param std.group An optional value of \code{tx.group.id} that identifies the group whose standard deviation will be used to standardize the treatment effect.
#' @param na.to.0 After removing empty rows, should remaining NAs in \code{item.data} be converted to 0? Default is FALSE.
#'
#' @details
#' This function saves the input data in a format used by other \code{WBdif} functions. The relation between \code{tx.group.id} and \code{dif.group.id} is especially important. If only one is supplied (or these are equal), then \code{\link[WBdif]{effect_robustness}} evaluates the unconditional treatment effects; if both are specified and they are not equal, then \code{\link[WBdif]{effect_robustness}} evaluates treatment effects conditional on the DIF groups (e.g., conditional on gender).
#'
#' This function also runs a number of pre-processing steps:
#' \itemize{
#'   \item Drop cases with empty rows \code{item.data} (i.e., rows with all NAs) or with \code{NA} for \code{dif.group.id}, \code{tx.group.id}, or \code{cluster.id}.
#'   \item Flag items with no variance.
#'   \item Flag items with different number of response categories in the different DIF groups.
#'   \item Flag items with more than two response categories.
#' }
#' @return A named \code{list} containing the pre-processed inputs and item flags.
#'
#' @examples
#' data("mdatlang")
#'
#' dif.data <- dif_data_prep(item.data = mdatlang`[`5:ncol(mdatlang)`]`,
#'                              tx.group.id = mdatlang$treated,
#'                              dif.group.id = mdatlang$gender,
#'                              cluster.id = mdatlang$clusterid,
#'                              na.to.0 = TRUE)
#'
#' @export

dif_data_prep <- function(item.data, tx.group.id = NULL, dif.group.id = tx.group.id,
                     cluster.id = NULL, std.group = NULL, na.to.0 = FALSE){

  ## Input checks
  if(is.null(tx.group.id) & is.null(dif.group.id)){
    stop("tx.group.id and dif.group.id cannot both be NULL.")
  }
  if(is.null(tx.group.id) & !is.null(dif.group.id)){
    tx.group.id <- dif.group.id
  }

  # Need tx.group.id to be a 2-level factor
  if(!is.null(tx.group.id) & !is.factor(tx.group.id)){
    tx.group.id <- factor(tx.group.id)
    if(length(levels(tx.group.id)) > 2){
      stop("tx.group.id must have only 2 levels")
    }
  }

  # Need dif.group.id to be a 2-level factor
  if(!is.null(dif.group.id) & !is.factor(dif.group.id)){
    dif.group.id <- factor(dif.group.id)
    if(length(levels(dif.group.id)) > 2){
      stop("dif.group.id must have only 2 levels")
    }
  }

  if (!is.null(std.group)) {
    if(!(std.group %in% levels(tx.group.id))) {
      stop("std.group is not in tx.group.id")
    }}

  ## Identifying cases to drop based on missing data
  # Note: FALSE elements in drop.cases are the cases removed from analysis

  # Cases with NA for all items
  drop.cases <- apply(is.na(item.data), 1, mean) != 1

  # Cases missing on tx.group.id
  drop.cases <- ifelse(is.na(tx.group.id), FALSE, drop.cases)

  # Cases missing on dif.group.id
  drop.cases <- ifelse(is.na(dif.group.id), FALSE, drop.cases)

   # Cases missing on cluster.id + start drops
  if (!is.null(cluster.id)) {
    drop.cases <- ifelse(is.na(cluster.id), FALSE, drop.cases)
    cluster.id <- cluster.id[drop.cases]
  }

  ## Dropping the missing cases
  item.data <- item.data[drop.cases, ]
  tx.group.id <- tx.group.id[drop.cases]
  dif.group.id <- dif.group.id[drop.cases]

  ## Replacing remaining NAs with 0
  if (na.to.0 == TRUE) {
    item.data[is.na(item.data)] <- 0
  }

  ## Items with no variance
  var.check <- lapply(item.data, var, na.rm = TRUE)
  no.var.items <- which(var.check == 0)

  ## Items with any empty cells within dif.groups (need to be removed from IRT analysis)
  var.check.group <- lapply(item.data,
                            function(x) { sum(table(x, dif.group.id, useNA = "no") == 0) })
  no.var.by.group.items <- which(var.check.group > 0)

  ## Polytomous items; returns named integer vector
  poly.items <- which(apply(item.data, 2, max, na.rm = TRUE) > 1)

  return(list(item.data = item.data,
              tx.group.id = tx.group.id,
              dif.group.id = dif.group.id,
              cluster.id = cluster.id,
              std.group = std.group,
              no.var.items = no.var.items,
              no.var.by.group.items = no.var.by.group.items,
              poly.items = poly.items))
}