#' Plots of test bias
#'
#' Plots test characteristic curves from a two-group IRT model and their difference.
#'
#' @param dif.models Output from \code{\link[WBdif]{dif_models}}.
#
#' @details
#' This is a \code{ggplot2} wrapper for plotting test characteristic curves (TCCs). It calls \code{\link[mirt]{expected.test}} and plots the resulting TCCs for both levels of \code{dif.group.id}, as well as their difference.
#'
#' @return
#' A \code{list} of two \code{ggplot} objects.
#' @export

bias_plots <- function(dif.models){

  # Levels of the dif.group variable

  dif.groups <- levels(dif.models$inputs$dif.group.id)
  dif.group.order <- order(dif.groups)
  n.dif.groups <- 2

  # Range for plots
  n.bins <- 200
  theta <- seq(-6, 6, length.out = n.bins)
  Theta <- matrix(theta) # required by mirt::expected.test

  # Storage
  data <- vector("list", n.dif.groups)

  for (i in 1:n.dif.groups) {

    # Get empirical pdfs for factor scores
    #f.scores <- fscores(extract.group(dif.models$dif.mod, dif.group.order[i]),
    #                   method = "WLE")
    #pdf <- approxfun(density(f.scores), rule = 2)

    # Get test characteristic curve
    tcc <- mirt::expected.test(dif.models$dif.mod, group = dif.group.order[i], Theta = Theta)

    # Save
    data[[i]] <- data.frame(Group = dif.groups[i],
                            x = theta,
                            #pdf = pdf(theta),
                            tcc = tcc)
  }

  # TCC plot
  long.data <- Reduce(rbind, data)

  tcc.plot <- ggplot(data = long.data, aes(x = x, group = Group)) +
                geom_line(aes(y = tcc, color = Group), lwd = 1, lty = 1) +
                ggtitle("Test characteristic curves (TCCs)") +
                scale_x_continuous(breaks = seq(-6, 6, 1), name = expression(theta)) +
                scale_y_continuous(name = "Expected total score") +
                theme(legend.position = "right",
                     panel.grid.minor = element_blank(),
                     text = element_text(size = 14))


  ## Bias Plot

  wide.data <- merge(data[[1]], data[[2]], by = "x")
  wide.data$bias <- wide.data$tcc.x - wide.data$tcc.y

  main <- paste0("Test score bias: ",
                dif.groups[dif.group.order[1]], " - ",
                dif.groups[dif.group.order[2]])

  bias.plot <- ggplot(data = wide.data, aes(x = x, y = bias)) +
                geom_line(lwd = 1, lty = 1) +
                scale_x_continuous(breaks = seq(-6, 6, 1), name = expression(theta)) +
                scale_y_continuous(name = "Bias (difference of TCCs)") +
               ggtitle(main) +
                theme(panel.grid.minor = element_blank(),
                      text = element_text(size=14))

  return(list(tcc.plot, bias.plot))
}


#' Forest plots of effect sizes
#'
#' Plots standardized treatment effects and their confidence intervals.
#'
#' @param effects Output from \code{\link[WBdif]{effect_robustness}}.
#
#' @details
#' This is a \code{ggplot2} wrapper for producing forest plots of effect sizes.
#'
#' @return
#' A \code{ggplot} object.


effects_plot <- function(effects){
  effects$min <- effects$effect.size - 1.96 * effects$effect.size.se
  effects$max <- effects$effect.size + 1.96 * effects$effect.size.se

  ggplot(data = effects, aes(x = effect.size, y = score.type,
                             xmin = min, xmax = max)) +
    geom_vline(xintercept = 0, colour = "black" , size = 1 , linetype = 2) +
    geom_pointrange(size = 1) +
    xlab("Treatment Effect [95% CI]") +
    ylab("Scoring Method") +
    # ggtitle(main) +
    theme(strip.placement = "outside",
          strip.text.y = element_text(angle = 180,vjust = 1, face = "bold"),
          strip.background = element_blank(),
          panel.spacing = unit(0,"cm"),
          panel.background = element_blank(),
          panel.border = element_blank(),
          panel.grid.minor=element_blank(),
          axis.line = element_line(colour = "black"),
          text = element_text(size=14))
}
