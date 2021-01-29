#' Preparing data for DIF analysis
#'
#' Handle missing data to run a DIF analysis and report
#'
#' @param measure.data data frame of item responses with subjects in rows
#' and items in columns.
#' @param dif.group vector of group membership by which DIF is evaluated.
#' @param tx.group vector indicating treatment condition unless treatment indicator was
#' already supplied to \code{dif.group}.
#' @param clusters vector of cluster membership.
#' @param na0 after removing empty rows, should remaining NAs be converted to 0?
#' Default is FALSE.
#'
#' @details
#' Identifies empty rows in measure.data (i.e., rows with all NAs) and rows with NA for
#' the \code{dif.group} and, if supplied, \code{tx.group} and \code{clusters}.
#' These rows are removed. Thus, \code{tx.group} and \code{clusters} should only be
#' supplied when intending to use them in \code{effect_robustness} or \code{dif_report}.
#'
#' @return list containing \code{measure.data} and \code{dif.group} (if supplied, also
#' \code{tx.group} and \code{clusters}) after consistent handling of missing data.
#'
#' @examples
#' measure <- data.frame(tx = rep(c("tx", "control"), times = 10),
#'                       gender = rep(c("male", "female"), each = 10),
#'                       item1 = sample(c(0,1), 20, replace = TRUE),
#'                       item2 = sample(c(0,1), 20, replace = TRUE),
#'                       item3 = sample(c(0,1), 20, replace = TRUE),
#'                       item4 = sample(c(0,1), 20, replace = TRUE),
#'                       item5 = sample(c(0,1), 20, replace = TRUE))
#' ## add missing data
#' measure <- measure`[`c(1, 4), `]` <- NA
#' measure <- measure`[`c(2, 5), 1`]` <- NA
#'
#' dif_prep(measure`[`, -c(1, 2)`]`,
#'          dif.group = measure$gender,
#'          tx.group = measure$tx,
#'          na0 = TRUE)
#'
#' @export

dif_prep <- function(measure.data, dif.group, tx.group = dif.group,
                     clusters = NULL, na0 = FALSE){

  # Only examining unconditional effects (i.e., DIF by treatment condition)?
  unconditional <- identical(dif.group, tx.group)

  ## Identifying cases to drop based on missing data
  # Note: FALSE elements in drop.cases are the cases removed from analysis
  drop.cases <- apply(is.na(measure.data), 1, mean) != 1    # cases with NA for all items
  drop.cases <- ifelse(is.na(dif.group), FALSE, drop.cases)  # NA for dif.group

  # If examining conditional effects
  if(unconditional == FALSE){
    drop.cases <- ifelse(is.na(tx.group), FALSE, drop.cases)  # NA for dif.group
  }

  # If adjusting effects by cluster membership
  if(!is.null(clusters)){
    drop.cases <- ifelse(is.na(clusters), FALSE, drop.cases)  # NA for clusters
    clusters <- clusters[drop.cases] # dropping missing data cases from cluster vector
  }

  ## Dropping the identified missing data cases
  measure.data <- measure.data[drop.cases, ] # from the measure response dataframe
  dif.group <- dif.group[drop.cases]         # from the grouping variable vector

  ## Replacing remaining NAs with 0
  if(na0 == TRUE){
  measure.data[is.na(measure.data)] <- 0
}

  ## If examining conditional effects
  if(unconditional == FALSE){

    # Dropping missing data cases from the conditional variable vector
    tx.group <- tx.group[drop.cases]

    # Output dif variable vector (dif.group) and tx indicator vector (tx.group)
    output <- list(measure.data = measure.data,
                   dif.group = dif.group,
                   tx.group = tx.group,
                   clusters = clusters)

  } else {

    # Output dif variable vector (dif.group) which is the tx indicator
    output <- list(measure.data = measure.data,
                   dif.group = dif.group,
                   clusters = clusters)
  }

  return(output)

}