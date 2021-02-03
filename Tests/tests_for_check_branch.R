
## Example dataset
load("/Users/halpin/OneDrive - University of North Carolina at Chapel Hill/UNC_stat_projects/WB_project/Bangladesh/Data/Bangladesh_Recoded.RData")

midline <- bang.recode[bang.recode$line == "Mid",]

mid.prep <- lapply(domain.items, function(x)
                    {dif_prep(measure.data = midline[, x],
                              dif.group = midline$gender,
                              tx.group = midline$tx,
                              na0 = FALSE)})
i <- 2

dif.analysis <- dif_analysis(measure.data = mid.prep[[i]]$measure.data,
                             dif.group = mid.prep[[i]]$tx.group,
                             methods = c("loess", "IRT"),
                             score.type = "Rest")

names(dif.analysis)
names(dif.analysis$IRT)
dif.analysis$IRT$biased.items
dif.models <- dif_models(dif.analysis)
bias.plots <- bias_plots(dif.models)
bias.plots[[2]]


bias_plots <- function(dif.models){

  # Levels of the dif.group variable
  dif.group1 <- levels(dif.models$inputs$dif.group)[[1]]
  dif.group2 <- levels(dif.models$inputs$dif.group)[[2]]
  dif.group.order <- order(c(dif.group1,  dif.group2))

  # Get empirical pdfs for factor scores
  f.scores1 <- fscores(extract.group(dif.models$dif.mod, dif.group.order[1]),
                     method = "WLE")

  f.scores2 <- fscores(extract.group(dif.models$dif.mod, dif.group.order[2]),
                     method = "WLE")
  pdf1 <- approxfun(density(f.scores1), rule = 2)
  pdf2 <- approxfun(density(f.scores2), rule = 2)


  # Get test characteristic curve for each dif.group
  theta <- seq(-6, 6, length.out = 200)
  Theta <- matrix(theta)
  tcc1 <- expected.test(dif.models$dif.mod, group = dif.group.order[1], Theta = Theta)
  tcc2 <- expected.test(dif.models$dif.mod, group = dif.group.order[2], Theta = Theta)

  # Set up data
  data1 <- data.frame(Group = dif.group1,
                     x = theta,
                     pdf = pdf1(theta),
                     tcc = tcc1)

  data2 <- data.frame(Group = dif.group2,
                     x = theta,
                     pdf = pdf2(theta),
                     tcc = tcc2)


  # TCC plot
  long.data <- rbind(data1, data2)
  coeff <-  max(long.data$tcc) / max(long.data$pdf) / 3 * 2
  long.data$rescale.pdf <- long.data$pdf * coeff

  tcc.plot <- ggplot(data = long.data, aes(x = x, group = Group)) +
                geom_line(aes(y = tcc, color = Group), lwd = 1, lty = 1) +
                geom_smooth(aes(y = rescale.pdf, color = Group),
                  se = F, span = .25, lwd = 1, lty = 2) +
                ggtitle("IRT Test characteristics curves and empirical densities") +
                scale_x_continuous(breaks = seq(-6, 6, 1), name = expression(theta)) +
                scale_y_continuous(limit = c(0,max(long.data$tcc)),
                  oob = squish,
                  name = "Expected total score (solid)",
                  sec.axis = sec_axis(~./coeff, name = "Density (dashed)")) +
                theme(legend.position = "bottom",
                     panel.grid.minor = element_blank(),
                     text = element_text(size=18))



  ## Bias Plot
  wide.data <- merge(data2, data1, by = "x")
  wide.data$bias <- wide.data$tcc.x - wide.data$tcc.y
  wide.data$w.bias <- wide.data$tcc.x*wide.data$pdf.x - wide.data$tcc.y*wide.data$pdf.y
  head(wide.data)
  bias.plot.title <- paste0("Density-weighted total score bias: ",
                            dif.group2, " - ", dif.group1)

  bias.plot <- ggplot(data = wide.data, aes(x = x, y = w.bias)) +
                geom_line(lwd = 1, lty = 1) +
                scale_x_continuous(breaks = seq(-6, 6, 1), name = expression(theta)) +
                scale_y_continuous(name = "Weighted bias") +
               ggtitle(bias.plot.title) +
                theme(panel.grid.minor = element_blank(),
                      text = element_text(size=18))

  plots <- list(TCC = tcc.plot,
                Bias = bias.plot)
  return(plots)
}



dif_models <- function(dif.analysis, biased.items = "IRT"){

# Biased items can be one of c("IRT", "Logistic", "MH") or a list vector of integers indicating which items in measure.data should be treated as biased.

  if (is.na(biased.items)) {
    biased.items <-  NULL
  } else if (biased.items == "IRT") {
    biased.items <- dif.analysis$IRT$biased.items
  } else if (biased.items == "MH") {
    biased.items <- dif.analysis$MH$biased.items
  } else if (biased.items == "logistic") {
    biased.items <- dif.analysis$logistic$biased.items
  } else if (is.integer(biased.items)) {
     if (mean(biased.items%in%1:ncol(dif.analysis$input$data)) != 1){
        biased.items <- NULL
     }
  } else {
    biased.items <- NULL
  }

  if (is.null(biased.items)) {
    stop(paste("biased.items is not a non-empty subset of items to be analyzed"))
  }

  ### Run multigroup IRT models

  inputs <- dif.analysis$inputs
  names(dif.analysis$IRT)
  # Remove items with no variance, if they exist

  if (length(inputs$no.var.by.group.items) > 0) {
    inputs$data <- inputs$data[-c(inputs$no.var.by.group.items)]
  }

  ## IRT model with parameters constrained to be equal between groups
  no.dif.mod <- dif.analysis$IRT$no.dif.mod

  if (is.null(no.dif.mod)) {
    no.dif.mod <-
      mirt::multipleGroup(data = inputs$data,
                          model = 1,
                          group = inputs$dif.group,
                          invariance = c('slopes', 'intercepts',
                                         'free_var','free_means'))
  }


  ## IRT model with biased items freed over groups
    dif.mod <-
      mirt::multipleGroup(data = inputs$data,
                          model = 1,
                          group = inputs$dif.group,
                          invariance = c(names(inputs$data)[-biased.items],
                                         'free_var','free_means'))

  return(list(no.dif.mod = no.dif.mod, dif.mod = dif.mod, inputs = inputs))
}



    ## Estimating IRT scores
    theta.bo <- mirt::fscores(mod.bo, method = irt.scoring)

    ## Estimating standardized mean differences
    delta.bo <- smd_wrapper(score = theta.bo,
                            dif.group = dif.group,
                            tx.group = tx.group,
                            clusters = clusters)
  }



}