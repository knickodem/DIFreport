#' Plotting Treatment Effects
#'
#' Functions to visualize treatment effect and bias magnitude
#'
#' @param dif.analysis an object returned from \code{dif_analysis}

bias_plot <- function(dif.analysis){

  ## levels of the dif.group variable
  dif.group1 <- levels(dif.analysis$inputs$dif.group)[[1]]
  dif.group2 <- levels(dif.analysis$inputs$dif.group)[[2]]

  ## Test characteristic curve data for each dif.group
  Theta <- matrix(seq(-6, 6, length.out = 200))
  exp1 <- data.frame(group = dif.group1,
                     x = Theta,
                     y = expected.test(dif.analysis$IRT$dif.mod, group = 1, Theta = Theta))
  exp2 <- data.frame(group = dif.group2,
                     x = Theta,
                     y = expected.test(dif.analysis$IRT$dif.mod, group = 2, Theta = Theta))

  ## Expected Measure Score Plot
  plot.data <- rbind(exp1, exp2)
  score.plot <- ggplot(data = plot.data, aes(x = x, y = y, group = group)) +
    geom_line(aes(color = group), lwd = .8) +
    # geom_ribbon(aes(ymin = y - 1.96 * se,
    #                 ymax = y + 1.96 * se,
    #                 fill = group), alpha = .4) +
    scale_x_continuous(breaks = seq(-6, 6, 1)) +
    labs(x = expression(theta),
         y = "Expected Total Score") +
    theme(legend.position = "top",
          panel.grid.minor=element_blank())

  ## Bias Plot
  bias.data <- merge(exp2, exp1, by = "x")
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

  bias.plot <- ggplot(data = bias.data, aes(x = x, y = bias)) +
    geom_line() +
    # geom_ribbon(aes(ymin = bias - 1.96 * se,
    #                 ymax = bias + 1.96 * se),
    #             alpha = .4) +
    scale_x_continuous(breaks = seq(-6, 6, 1)) +
    labs(x = expression(theta),
         y = "Score Bias") +
    theme(panel.grid.minor=element_blank())

  plots <- list(Score = score.plot,
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