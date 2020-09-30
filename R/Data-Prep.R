#' Preparing World Bank Data
#'
#' Function specifically for cleaning World Bank Data to run a DIF analysis and report
#'
#' @param data `data.frame` containing item responses and group indicators.
#' @param wb.items regex character string identifying item response columns in `data`
#' @param tx.group.name character; variable name of treatment indicator in `data`.
#' @param dif.group.name optional character; variable name in `data` for which DIF
#' is evaluated. Supply only if different from `tx.group.name`
#' Default is `tx.group.name`
#'
#' @details
#' Currently, `tx.group.name` (and `dif.group.name` if supplied) must name a factor
#'  with 2 (and only 2) levels
#'
#' @return A named list where `measure.data` is the data frame of item responses
#' and `dif.group` is the vector of group membership for which DIF is evaluated.
#' If `dif.group` is not the treatment indicator, then the vector of treatment indicators
#' is returned in the element `tx.group`.
#'
#' @export

wb_data_prep <- function(data, wb.items, tx.group.name, dif.group.name = tx.group.name){

  ## Subsetting measure specific data and removing wave identifier from column names
  measure.data <- data[grep(wb.items, names(data))]
  names(measure.data) <- substr(names(measure.data), 1, nchar(names(measure.data)) - 2)

  ## Identifying cases to drop based on missing data
  # Note: FALSE elements in drop.cases are the cases removed from analysis
  drop.cases <- apply(is.na(measure.data), 1, mean) != 1    # cases with NA for all items
  drop.cases <- ifelse(is.na(data[[tx.group.name]]), FALSE, drop.cases)  # NA for tx.group

  # If examining conditional effects
  if(dif.group.name != tx.group.name){
    drop.cases <- ifelse(is.na(data[[dif.group.name]]), FALSE, drop.cases)  # NA for dif.group
  }


  ## Dropping the identified missing data cases
  measure.data <- measure.data[drop.cases, ]   # from the measure response dataframe
  tx.group <- data[drop.cases, ][[tx.group.name]]    # from the grouping variable vector

  ## Replacing remaining NAs with 0
  measure.data[is.na(measure.data)] <- 0

  ## If examining conditional effects
  if(dif.group.name != tx.group.name){

    # Dropping missing data cases from the conditional variable vector
    dif.group <- data[drop.cases, ][[dif.group.name]]

    # Output with tx indicator vector (tx.group) and dif variable vector (dif.group)
    output <- list(measure.data = measure.data,
                   tx.group = tx.group,
                   dif.group = dif.group)

  } else {

    # Output tx indicator vector AS the dif variable vector (dif.group)
    output <- list(measure.data = measure.data,
                   dif.group = tx.group)
  }

  return(output)

}