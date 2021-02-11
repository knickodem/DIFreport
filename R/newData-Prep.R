#' Formats data for use by \code{WBdif} functions
#'
#' Handle missing data to run a DIF analysis and report
#'
#' @param item.data A \code{data.frame} of item responses with subjects in rows
#' and items in columns. Data can be binary or ordered categorical.
#' @param dif.group.id A \code{vector} of length \code{nrow(item.data)} indicating the DIF groups. Must have exactly two unique value. See details for discussion of default value.
#' @param tx.group.id A \code{vector} of length \code{nrow(item.data)} indicating the treatment groups. Must have exactly two unique value.
#' @param cluster.id An optional \code{vector} of length \code{nrow(item.data)} indicating the primary sampling unit in multi-stage / clustered sampling design -- used to adjust standard errors when computing standardized treatment effects.
#' @param na.to.0 After removing empty rows, should remaining NAs be converted to 0?
#' Default is FALSE.
#'
#' @details
#' This function saves the input data in a format used by other \code{WBdif} functions. The relation between \code{tx.group.id} and \code{dif.group.id} is especially important. If these are equal, then \code{WBdif::effect_robustness} evalutes the unconditional treatment effects; if they are not equal, then \code{WBdif::effect_robustness} evaluates treatement effects conditional on the DIF groups (e.g., conditional on gender).
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
#' dat <- data.frame(tx = rep(c("tx", "control"), times = 10),
#'                       gender = rep(c("male", "female"), each = 10),
#'                       item1 = sample(c(0,1), 20, replace = TRUE),
#'                       item2 = sample(c(0,1), 20, replace = TRUE),
#'                       item3 = sample(c(0,1), 20, replace = TRUE),
#'                       item4 = sample(c(0,1), 20, replace = TRUE),
#'                       item5 = sample(c(0,1), 20, replace = TRUE))
#' ## add missing data
#' dat <- dat`[`c(1, 4), `]` <- NA
#' dat <- dat`[`c(2, 5), 1`]` <- NA
#'
#' dif_data_prep(dat`[`, -c(1, 2)`]`,
#'          dif.group.id = dat$gender,
#'          tx.group.id = dat$tx,
#'          na.to.0 = TRUE)
#'
#' @export

dif_data_prep <- function(item.data, tx.group.id, dif.group.id = tx.group.id,
                     cluster.id = NULL, na.to.0 = FALSE){

  # Add checks for inputs

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
              no.var.items = no.var.items,
              no.var.by.group.items = no.var.by.group.items,
              poly.items = poly.items))
}