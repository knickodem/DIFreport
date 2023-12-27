#' Internal convenience functions
#'
#' Collection of convenience functions to improve automation and code readability
#'
#' @param item.data frame of item responses with subjects in rows and items in columns.
#' @param drops integer vector; item locations in \code{item.data} to exclude from score calculation
#' @param poly.items integer vector; location of polytomous items in \code{item.data}
#' @param max.values integer vector of equal length to \code{poly.items} indicating the maximum value each item in \code{poly.items} can take.
#'
#' @details
#' Sums item responses into a scale score. The \code{drops} argument is used for calculating the rest score.
#' Polytomous items are unit scaled so each item has a maximum value of 1.
#'
#' @return
#' numeric vector of summed scores
#'
#'
#' @noRd

sum_score <- function(item.data, drops = NULL, poly.items = integer(), max.values = integer()){

  # unit scale all polytomous items
  if(length(poly.items) > 0){
    item.data[,poly.items] <- mapply(function(x, y) x / y, item.data[,poly.items], max.values)
  }

  if(!is.null(drops)){
    # calculating rest score
    score <- apply(item.data[,-drops], 1, sum, na.rm = T)
  } else {
    # calculating total score
    score <- apply(item.data, 1, sum, na.rm = T)
  }

  return(score)
}



#'
#' @param df a data frame
#' @param bold.bias character indicating whether results tables should bold instances
#' of DIF or not ("no", the default). If so, must specify whether the table
#' displays model comparisons ("model") or item-level tests ("item").
#' @param digits integer; number decimal places to print in report tables
#'
#' @details
#' Converts the DIF results from a data frame to a \code{\link[flextable]{flextable}} and
#' formats the table for printing in the report.
#'
#' @return
#' a \code{\link[flextable]{flextable}} object
#'
#' @importFrom flextable flextable bold autofit colformat_double set_flextable_defaults
#'
#' @noRd

format_flex <- function(df, bold.bias = "no", digits = 3){

  ftab <- flextable::flextable(df)

  numericcols <- which(unlist(lapply(df, is.numeric)))
  if(length(numericcols) > 0){
  ftab <- flextable::colformat_double(ftab, j = numericcols, digits = digits)
  }

  if(bold.bias == "item"){

    lastcol <- names(df)[[ncol(df)]]
    ftab <- flextable::bold(ftab, i = as.formula(paste("~", lastcol,"== TRUE")), part =  "body")
    ftab <- flextable::bold(ftab, i = as.formula(paste("~ is.na(", lastcol,")")), part =  "body")

  } else if(bold.bias == "global"){

    ftab <- bold(ftab, i = ~ p < .05, part =  "body")
  }

  ftab <- flextable::autofit(ftab)

  return(ftab)
}

#' @param item.data frame of item responses with subjects in rows and items in columns.
#' @return numeric value
#' @noRd

est_alpha <- function(item.data){
  item.data <- na.omit(item.data)
  nitems <- ncol(item.data)
  alpha <- (nitems/(nitems - 1)) * (1 - sum(apply(item.data, 2, var)) /
                                      var(apply(item.data, 1, sum)))
  return(alpha)
}

#' @param dif.analysis an object returned from \code{\link[DIFreport]{dif_analysis}}
#' @noRd

coeff_alpha <-function(dif.models, std.group = NULL) {
  inputs <- dif.models$inputs
  item.data <- inputs$item.data
  biased.items <- dif.models$biased.items
  dif.group.id <- inputs$dif.group.id
  dif.groups <- levels(dif.group.id)
  tx.group.id <- inputs$tx.group.id
  tx.groups <- get_tx.groups(tx.group.id, std.group)

  if(identical(tx.groups, dif.groups)) { #sum(tx.groups == dif.groups) == 1
    alphas <- c(est_alpha(item.data), est_alpha(item.data[-c(biased.items)]))
    alphas.list <- list(alphas)
    names(alphas.list) <- paste(tx.groups[2], "-", tx.groups[1])
  } else {
    group1 <-  dif.group.id == dif.groups[1]
    alphas1 <- c(est_alpha(item.data[group1, ]), est_alpha(item.data[group1, -c(biased.items)]))

    group2 <- dif.group.id == dif.groups[2]
    alphas2 <- c(est_alpha(item.data[group2, ]), est_alpha(item.data[group2, -c(biased.items)]))

    alphas.list <- list(alphas1, alphas2, NULL)
    names(alphas.list) <- c(paste0(tx.groups[2], " - ", tx.groups[1],": ",
                                   c(dif.groups)), "interaction")
  }
  return(alphas.list)
}

