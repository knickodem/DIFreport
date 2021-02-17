#' LOESS DIF method
#'
#' Visual inspection of DIF with LOESS regression curves
#'
#' @param item.data data frame of item responses with subjects in rows and items in columns
#' @param dif.group.id factor vector of group membership for which DIF is evaluated.
#' @param match.type character indicating whether \code{match.scores} is a
#' total summed score ("Total") or the summed score excluding the item under
#' investigation ("Rest").
#' @param match.scores A numeric vector (if \code{match.type = "Total"}) or
#' \code{list} of \code{ncol(item.data)} numeric vectors (if \code{match.type = "Rest"})
#' of match scores used as a predictor in the LOESS regression.
#'
#' @details
#' For each item in \code{item.data}, \code{\link[stats]{loess}} is called to regress the
#' item responses on the scores supplied to \code{match.scores}. A separate model is run for each \code{dif.group.id} level.
#'
#' @return A list containing:
#' \itemize{
#'   \item the plot of LOESS curves for each \code{dif.group.id} level for every item in \code{item.data}
#'   \item the data used to generate the plot.
#' }
#'
#' @export

dif_loess <- function(item.data, dif.group.id, match.type, match.scores){

  ## Number of items in the measure
  nitems <- ncol(item.data) # this could be different than n.items in dif_analysis

  if(match.type == "Rest"){

    pred.scores <- 0:(nitems - 1)  # score range to use with predict function

    ## Running loess method on each item
    loess.list <- list()
    for(i in 1:nitems){

      loess.list[[i]] <- run_loess(item.data = item.data,
                                   dif.group.id = dif.group.id,
                                   item = i,
                                   match.scores = match.scores[[i]],
                                   pred.scores = pred.scores,
                                   nitems = nitems)
    }


  } else if(match.type == "Total"){

    pred.scores <- 0:nitems   # score range to use with predict function
    nitems1 <- nitems + 1    # Needed for the item and group columns in loess.df

    ## Running loess method on each item
    loess.list <- lapply(c(1:nitems),
                         run_loess,
                         item.data = item.data,
                         dif.group.id = dif.group.id,
                         match.scores = match.scores,
                         pred.scores = pred.scores,
                         nitems = nitems1)


  } else {
    stop("match.type argument must be 'Rest' or 'Total'")
  }

  ## Converting list elements into single dataframe
  # group legend currently prints levels in alphabetical order rather than factor order
  loess.df <- Reduce(rbind, loess.list)
  loess.df$item <- factor(loess.df$item, levels = names(item.data))

  ## Plotting the results
  poly <- which(apply(item.data, 2, max, na.rm = TRUE) > 1)
  if(length(poly) > 0){

    scales <- "free_y"
    ylab <- "Predicted Item Score"

  } else {

    scales <- "fixed"
    ylab <- "Prob(Correct)"
  }

  plot <- ggplot(loess.df, aes(x = score, y = prob, group = dif.group.id)) +
    geom_line(aes(color = dif.group.id), lwd = .8) +
    geom_ribbon(aes(ymin = prob - 1.96 * se,
                    ymax = prob + 1.96 * se,
                    fill = dif.group.id), alpha = .4) +
    labs(x = paste(match.type, "Score"),
         y = ylab) + # changing legend name trickier b/c both color and fill scales
    #theme_bw(base_size = 16) +
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
#' for generating the LOESS curves. The values are defined in \code{link[WBdif]{dif_loess}}.
#' @param nitem number of items in the scale. Defined in \code{link[WBdif]{dif_loess}}.
#' @return a \code{data.frame}


run_loess <- function(item.data, dif.group.id,
                      item, match.scores,
                      pred.scores, nitems){

  # Appending match.scores scores to item data
  item.data$score <- match.scores

  # Model
  mod <- paste0(names(item.data)[item], " ~ score")

  # levels of the grouping variable
  dif.group1 <- levels(dif.group.id)[1]
  dif.group2 <- levels(dif.group.id)[2]

  # Loess by dif.group.id
  g1.fit <-  stats::loess(mod, data = subset(item.data, dif.group.id == dif.group1))
  g2.fit <-  stats::loess(mod, data = subset(item.data, dif.group.id == dif.group2))

  # predicted probability by dif.group.id
  g1.pred <- predict(g1.fit, newdata = pred.scores, se = T)
  g2.pred <- predict(g2.fit, newdata = pred.scores, se = T)

  # Storage - one row for each pred.scores x dif.group.id combination
  loess.data <- data.frame(
    score = rep(pred.scores, times = 2),
    prob = c(g1.pred$fit, g2.pred$fit),
    se = c(g1.pred$se.fit, g2.pred$se.fit),
    item = rep(names(item.data)[item], times = nitems*2),
    dif.group.id = rep(c(dif.group1, dif.group2), each = nitems)
  )

  return(loess.data)
}