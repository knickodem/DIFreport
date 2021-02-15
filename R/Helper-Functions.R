#' Internal convenience functions
#'
#' Collection of convenience functions to improve automaticity and code readability
#'
#' @param data frame of item responses with subjects in rows
#' and items in columns.
#' @param drops integer vector; item locations in \code{item.data} to exclude from
#' score calculation
#' @param poly.items integer vector; location of polytomous items in \code{item.data}
#' @param dif.analysis an object returned from \code{dif_analysis}
#' @param df a data frame
#' @param bold.bias character indicating whether results tables should bold instances
#' of DIF or not ("no", the default). If so, must specify whether the table
#' displays model comparisons ("model") or item-level tests ("item").
#' @param digits integer; number decimal places to print in report tables
#'
#' @details
#' \code{sum_score} sums item responses into a scale score. Polytomous items are
#' unit scaled so each item has a maximum value of 1. \cr
#' \code{extract_bi} extracts the biased items identified from each \code{dif.analysis}
#' method. If no biased items were detected, a character string indicated so is extracted. \cr
#' \code{format_flex} converts the DIF results from a data frame to a
#' \code{\link[flextable]{flextable}} and formats the table for printing in the report. \cr
#'
#' @return
#' \code{sum_score} returns a numeric vector of summed scores \cr
#' \code{extract_bi} returns list of biased items from each \code{dif.analysis} method \cr
#' \code{format_flex} returns a \code{\link[flextable]{flextable}} object \cr
#' \code{est_alpha} returns a numeric value
#'

sum_score <- function(item.data, drops = NULL, poly.items = integer()){

  # unit scale all polytomous items
  if(length(poly.items) > 0){

    item.data[,poly.items] <- apply(item.data[,poly.items], 2, function(x) x / max(x, na.rm = TRUE))

  }

  if(!is.null(drops)){

    # calculating rest score
    score <- apply(item.data[,-c(drops)], 1, sum, na.rm = T)

  } else {

    # calculating total score
    score <- apply(item.data, 1, sum, na.rm = T)
  }

  return(score)
}


#' @rdname sum_score

extract_bi <- function(dif.analysis){

  da <- dif.analysis[c("MH", "logistic", "IRT")]
  # da <- da[!sapply(da,is.null)]

  all.bi <- lapply(da, "[[", "biased.items")

  return(all.bi)
}

#' @rdname sum_score

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

#' @rdname sum_score

est_alpha <- function(item.data){
  item.data <- na.omit(item.data)
  nitems <- ncol(item.data)
  alpha <- (nitems/(nitems - 1)) * (1 - sum(apply(item.data, 2, var)) /
                                      var(apply(item.data, 1, sum)))
  return(alpha)
}

