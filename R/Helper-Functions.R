#' Internal convenience functions
#'
#' Collection of convenience functions to improve automaticity and code readability
#'
#' @param scale.data data frame of dichotomous item responses used to calculate the score
#' @param drops integer vector; item locations in \code{scale.data} to exclude from
#' score calculation
#' @param poly integer vector; location of polytomous items in \code{scale.data}
#' @param dif.analysis an object returned from \code{dif_analysis}
#' @param df a data frame
#' @param bold.bias character indicating whether results tables should bold instances
#' of DIF or not ("no", the default). If so, must specify whether the table
#' displays model comparisons ("model") or item-level tests ("item").
#' @param digits integer; number decimal places to print in report tables
#'
#' @details
#' \code{sum_score} sums dichotomous item responses into a scale score \cr
#' \code{extract_bi} extracts the biased items identified from each \code{dif.analysis}
#' method. If no biased items were detected, a character string indicated so is extracted. \cr
#' \code{format_flex} converts the DIF results from a data frame to a
#' \code{\link[flextable]{flextable}} and formats the table for printing in the report. \cr
#'
#' @return
#' \code{sum_score} returns a numeric vector of summed scores \cr
#' \code{extract_bi} returns list of biased items from each \code{dif.analysis} method \cr
#' \code{format_flex} returns a \code{\link[flextable]{flextable}} object \cr
#'

sum_score <- function(scale.data, drops = NULL, poly = integer()){

  # unit scale all polytomous items
  if(length(poly) > 0){

    scale.data[,poly] <- apply(scale.data[,poly], 2, function(x) x / max(x, na.rm = TRUE))

  }

  if(!is.null(drops)){

    # calculating rest score
    score <- apply(scale.data[,-c(drops)], 1, sum)

  } else {

    # calculating total score
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

bias_plot <- function(dif.analysis){

  test.plot <- plot(dif.analysis$IRT$dif.mod, type = "score")
  # se.plot <- plot(dif.analysis$IRT$dif.mod, type = "SE")
  # Note: MI argument for bootstrap SEs only available for SingleGroupClass

  # extract data from mirt plot objects
  plot.data <- data.frame(group = score.plot$panel.args.common$groups,
                          x = score.plot$panel.args[[1]]$x,
                          y = score.plot$panel.args[[1]]$y)
  # se = se.plot$panel.args[[1]]$y)

  # calculate reformat data and calculate bias
  bias.data <- merge(plot.data[plot.data$group == dif.group2,],
                     plot.data[plot.data$group == dif.group1,],
                     by = "x")
  bias.data$bias <- bias.data$y.x - bias.data$y.y
  # group2 - group1; consistent w/ calc_smd

  # # group size
  # ns <- table(inputs$dif.group)
  #
  # # calculate SE(bias); similar pooling to calc_smd but for SE
  # sigmanum <- (ns[[1]] - 1) * (bias.data$se.y*ns[[1]]) +
  #   (ns[[2]] - 1) * (bias.data$se.x*ns[[2]])
  # sigmadenom <- (ns[[1]] + ns[[2]] - 2)
  # bias.data$se <- sqrt(sigmanum / sigmadenom) * sqrt((1/ns[[1]]) + (1/ns[[2]]))

  test.plot <- ggplot(data = plot.data, aes(x = x, y = y, group = group)) +
    geom_line(aes(color = group), lwd = .8) +
    # geom_ribbon(aes(ymin = y - 1.96 * se,
    #                 ymax = y + 1.96 * se,
    #                 fill = group), alpha = .4) +
    scale_x_continuous(breaks = seq(-6, 6, 1)) +
    labs(x = expression(theta),
         y = "Expected Measure Score") +
    theme(legend.position = "top")

  bias.plot <- ggplot(data = bias.data, aes(x = x, y = bias)) +
    geom_line() +
    # geom_ribbon(aes(ymin = bias - 1.96 * se,
    #                 ymax = bias + 1.96 * se),
    #             alpha = .4) +
    scale_x_continuous(breaks = seq(-6, 6, 1)) +
    labs(x = expression(theta),
         y = "Score Bias")

  plots <- list(Score = test.plot,
                Bias = bias.plot)
  return(plots)
}

# # classify::wlord - calls a cpp file
# wlord <- function(probs=NULL, cats=NULL){
#
#   #Wrapper for Wingersky Lord
#   if(!is.numeric(cats)){
#     stop("Item categories need to be specified as a numeric vector.")
#   }
#   if(!is.matrix(probs)){
#     stop("Item probabilities need to be specified as a matrix.")
#   }
#   if(nrow(probs)!=length(cats)){
#     stop(paste("Categories Specified for:",length(cats), "items, but probabilities provided for" ,nrow(probs),"items."))
#   }
#   if(min(cats)<2){
#     stop("Minimum number of item categories is 2.")
#   }
#
#   ret <- .Call( "rcpp_w_lord_c", probs, cats,PACKAGE = "classify" )
#   return(ret)
# }