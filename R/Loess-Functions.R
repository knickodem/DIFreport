#' LOESS DIF method
#'
#' Visual inspection of DIF with LOESS regression curves
#'
#' @param scale.data data frame of dichotomous item responses with subjects in rows
#' and items in columns
#' @param dif.group factor vector of group membership for which DIF is evaluated.
#' @param score.type character indicating whether `match` is a
#' total summed score ("Total") or the summed score excluding the item under
#' investigation ("Rest").
#' @param match numeric vector used as the predictor in the LOESS regression.
#' In `get_loess`, if `score.type` = "Rest", `match` will be a list of vectors -
#' one for each item in `scale.data`.
#' @param item integer; item in `scale.data` under investigation for DIF
#' @param pred.scores range of scores to use in [stats::predict()] for generating
#' the LOESS curves. The values are defined in `get_loess`and passed to `run_loess`.
#' @param nitem number of items in the scale. Calculated in `get_loess`
#' and passed to `run_loess`.
#'
#' @details
#' For a single item in `scale.data`, `run_loess` regresses the item responses on the
#' scores supplied to `match`. A separate model is run for each `dif.group` level.
#' `get_loess` is a wrapper around `run_loess` that both initiates the call to
#' `run_loess` for each item and uses the results to plot the LOESS curves.
#'
#' @return A two-element list containing 1) the plot of LOESS curves for each
#' `dif.group` level for every item in `scale.data` and 2) the data used to generate
#' the plot (returned from `run_loess`).
#'
#' @export

get_loess <- function(scale.data, dif.group, score.type, match){

  ## Number of items in the measure
  nitems <- ncol(scale.data) # this could be different than n.items in dif_analysis

  if(score.type == "Rest"){

    pred.scores <- 0:(nitems - 1)  # score range to use with predict function

    ## Running loess method on each item
    loess.list <- list()
    for(i in 1:nitems){

      loess.list[[i]] <- run_loess(scale.data = scale.data,
                                   dif.group = dif.group,
                                   item = i,
                                   match = match[[i]],
                                   pred.scores = pred.scores,
                                   nitems = nitems)
    }


  } else if(score.type == "Total"){

    pred.scores <- 0:nitems   # score range to use with predict function
    nitems1 <- nitems + 1    # Needed for the item and group columns in loess.df

    ## Running loess method on each item
    loess.list <- lapply(c(1:nitems),
                         Run_loess,
                         scale.data = scale.data,
                         dif.group = dif.group,
                         match = match,
                         pred.scores = pred.scores,
                         nitems = nitems1)


  } else {
    stop("score.type argument must be 'Rest' or 'Total'")
  }

  ## Converting list elements into single dataframe
  # group legend currently prints levels in alphabetical order rather than factor order
  loess.df <- Reduce(rbind, loess.list)
  loess.df$item <- factor(loess.df$item, levels = names(scale.data))

  ## Plotting the results

  plot <- ggplot(loess.df, aes(x = score, y = prob, group = dif.group)) +
    geom_line(aes(color = dif.group), lwd = .8) +
    geom_ribbon(aes(ymin = prob - 1.96 * se,
                    ymax = prob + 1.96 * se,
                    fill = dif.group), alpha = .4) +
    labs(x = paste(score.type, "Score"), y = "Prob(Correct)") +
    #theme_bw(base_size = 16) +
    facet_wrap(~ item, ncol = 6) +
    theme(legend.position = "top")

  ## Output data and plot in a list
  loess <- list(data = loess.df,
                plot = plot)

  return(loess)
}



#' @rdname get_loess
#' @export

run_loess <- function(scale.data, dif.group,
                      item, match,
                      pred.scores, nitems){

  # Appending match scores to item data
  scale.data$score <- match

  # Model
  mod <- paste0(names(scale.data)[item], " ~ score")

  # levels of the grouping variable
  dif.group1 <- levels(dif.group)[1]
  dif.group2 <- levels(dif.group)[2]

  # Loess by dif.group
  g1.fit <-  loess(mod, data = subset(scale.data, dif.group == dif.group1))
  g2.fit <-  loess(mod, data = subset(scale.data, dif.group == dif.group2))

  # predicted probability by dif.group
  g1.pred <- predict(g1.fit, newdata = pred.scores, se = T)
  g2.pred <- predict(g2.fit, newdata = pred.scores, se = T)

  # Storage - one row for each pred.scores x dif.group combination
  loess.data <- data.frame(
    score = rep(pred.scores, times = 2),
    prob = c(g1.pred$fit, g2.pred$fit),
    se = c(g1.pred$se.fit, g2.pred$se.fit),
    item = rep(names(scale.data)[item], times = nitems*2),
    dif.group = rep(c(dif.group1, dif.group2), each = nitems)
  )

  return(loess.data)
}
