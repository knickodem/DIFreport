#' LOESS DIF method
#'
#' Visual inspection of DIF with LOESS regression curves
#'
#' @param item.data data frame of item responses with subjects in rows and items in columns
#' @param dif.groups factor vector of group membership for which DIF is evaluated.
#' @param match.type character indicating whether \code{match.scores} is a total summed score ("Total") or
#' the summed score excluding the item under investigation ("Rest").
#' @param match.scores A numeric vector (if \code{match.type = "Total"}) or
#' \code{list} of \code{ncol(item.data)} numeric vectors (if \code{match.type = "Rest"})
#' of match scores used as a predictor in the LOESS regression.
#'
#' @details
#' For each item in \code{item.data}, \code{\link[stats]{loess}} is called to regress the
#' item responses on the scores supplied to \code{match.scores}. A separate model is run for each \code{dif.groups} level.
#'
#' @return A list containing:
#' \itemize{
#'   \item the plot of LOESS curves for each \code{dif.groups} level for every item in \code{item.data}
#'   \item the data used to generate the plot.
#' }
#'
#' @import ggplot2
#' @export

dif_loess <- function(item.data, dif.groups, match.type, match.scores){

  ## Number of items in the measure
  nitems <- ncol(item.data) # this could be different than n.items in dif_analysis

  if(match.type == "Rest"){

    pred.scores <- 0:(nitems - 1)  # score range to use with predict function

    ## Running loess method on each item
    loess.list <- vector(mode = "list", length = nitems)
    for(i in 1:nitems){

      loess.list[[i]] <- run_loess(item.data = item.data,
                                   dif.groups = dif.groups,
                                   item = i,
                                   match.scores = match.scores[[i]],
                                   pred.scores = pred.scores)
    }


  } else if(match.type == "Total"){

    pred.scores <- 0:nitems   # score range to use with predict function

    ## Running loess method on each item
    loess.list <- lapply(c(1:nitems),
                         run_loess,
                         item.data = item.data,
                         dif.groups = dif.groups,
                         match.scores = match.scores,
                         pred.scores = pred.scores)


  } else {
    stop("match.type argument must be 'Rest' or 'Total'")
  }

  ## Converting list elements into single dataframe
  loess.df <- Reduce(rbind, loess.list)
  loess.df$item <- factor(loess.df$item, levels = names(item.data))
  loess.df$dif.groups <- factor(loess.df$dif.groups, levels = levels(dif.groups))

  ## Plotting the results
  max.val <- apply(item.data, 2, max, na.rm = TRUE) # highest response option
  poly <- which(max.val > 1) # number of polytomous items
  if(length(poly) > 0){

    ylab <- "Predicted Item Score"
    if(var(max.val) == 0) scales <- "fixed" else   scales <- "free_y"

  } else {

    scales <- "fixed"
    ylab <- "Prob(Correct)"
  }

  plot <- ggplot(loess.df, aes(x = score, y = prob, group = dif.groups)) +
    geom_line(aes(color = dif.groups), lwd = .8) +
    geom_ribbon(aes(ymin = prob - 1.96 * se,
                    ymax = prob + 1.96 * se,
                    fill = dif.groups), alpha = .4) +
    labs(x = paste(match.type, "Score"),
         y = ylab) + # changing legend name trickier b/c both color and fill scales
    scale_color_brewer(palette = "Set2") +
    scale_fill_brewer(palette = "Set2") +
    theme_bw(base_size = 18) +
    facet_wrap(~item, ncol = 6, scales = scales) +
    theme(legend.position = "top",
          legend.title = element_blank())

  ## Output data and plot in a list
  loess <- list(data = loess.df,
                plot = plot)

  return(loess)
}

#' LOESS regression for DIF analysis
#'
#' Internal function for dif_loess
#'
#' @param item integer; item in \code{item.data} under investigation for DIF
#' @param pred.scores range of scores to use in \code{\link[stats]{predict}}
#' for generating the LOESS curves. The values are defined in \code{link[DIFreport]{dif_loess}}.
#' @inheritParams dif_loess
#'
#' @return a \code{data.frame}
#'
#' @noRd

run_loess <- function(item.data, dif.groups,
                      item, match.scores,
                      pred.scores){

  # Appending match.scores scores to item data
  item.data$score <- match.scores

  # Model
  mod <- paste0(names(item.data)[item], " ~ score")

  # levels of the grouping variable
  dg.levs <- levels(dif.groups)
  num.levs <- length(dg.levs)

  # loess and predicted probability by group
  pred <- vector(mode = "list", length = num.levs)
  for(g in 1:length(dg.levs)){
    fit <- stats::loess(mod, data = subset(item.data, dif.groups == dg.levs[[g]]))
    pred[[g]] <- predict(fit, newdata = pred.scores, se = T)
  }

  # Storage - one row for each pred.scores x group combination
  loess.data <- data.frame(
    score = rep(pred.scores, times = num.levs),
    prob = unlist(lapply(1:num.levs, function(x) pred[[x]]$fit)),
    se = unlist(lapply(1:num.levs, function(x) pred[[x]]$se.fit)),
    item = names(item.data)[item], #rep(names(item.data)[item], times = nitems*num.levs),
    dif.groups = rep(dg.levs, each = length(pred.scores))
  )

  return(loess.data)
}