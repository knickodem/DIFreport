#' Internal convenience functions
#'
#' Collection of convenience functions to improve automaticity and code readability
#'
#' @param scale.data data frame of dichotomous item responses used to calculate the score
#' @param drops integer vector of item locations in \code{scale.data} to exclude from
#' score calculation
#' @param dif.analysis an object returned from \code{dif_analysis}
#' @param df a data frame
#' @param bold.bias character indicating whether results tables should bold instances
#' of DIF or not ("no", the default). If so, must specify whether the table
#' displays model comparisons ("model") or item-level tests ("item").
#' @param digits integer; number decimal places to print in report tables
#'
#' @details
#' \code{sum_score} sums dichotomous item responses into a scale score
#' \code{extract_bi} extracts the biased items identified from each \code{dif.analysis} method.
#' If no biased items were detected, a character string indicated so is extracted.
#' \code{format_flex} converts the DIF results from a data frame to a
#' \code{\link[flextable]{flextable}} and formats the table for printing in the report.
#'
#' @return
#' \code{sum_score} returns a numeric vector of summed scores
#' \code{extract_bi} returns list of biased items from each \code{dif.analysis} method
#' \code{format_flex} returns a \code{\link[flextable]{flextable}} object
#'

sum_score <- function(scale.data, drops = NULL){

  # calculating rest or total score
  if(!is.null(drops)){

    score <- apply(scale.data[,-c(drops)], 1, sum)

  } else {

    score <- apply(scale.data, 1, sum)
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

  numericcols <- which(unlist(lapply(df, is.numeric)))

  ftab <- flextable(df)
  ftab <- colformat_num(ftab, j = numericcols, digits = digits)

  if(bold.bias == "item"){

    lastcol <- names(df)[[ncol(df)]]
    ftab <- bold(ftab, i = as.formula(paste("~", lastcol,"== TRUE")), part =  "body")
    ftab <- bold(ftab, i = as.formula(paste("~ is.na(", lastcol,")")), part =  "body")

  } else if(bold.bias == "global"){

    ftab <- bold(ftab, i = ~ p < .05, part =  "body")
  }

  ftab <- autofit(ftab)

  return(ftab)
}