# ------------------- Loess ------------------------------------

get_loess <- function(scale.data, group, score.type, match.on){
  
  ## Number of items in the measure
  nitems <- ncol(scale.data) # this could be different than n.items in dif_analysis
  
  if(score.type == "Rest"){
    
    pred.scores <- 0:(nitems - 1)  # score range to use with predict function
    
    ## Running loess method on each item
    loess.list <- list()
    for(i in 1:nitems){
      
      loess.list[[i]] <- run_loess(scale.data = scale.data, item = i,
                                   group = group, match = match.on[[i]],
                                   pred.scores = pred.scores,
                                   nitems = nitems)
    }
    
    
  } else if(score.type == "Total"){
    
    pred.scores <- 0:nitems   # score range to use with predict function
    nitems1 <- nitems + 1    # Needed for the item and group columns in loess.data
    
    ## Running loess method on each item
    loess.list <- lapply(c(1:nitems),
                         Run_loess, scale.data = scale.data,
                         group = group, match = match.on,
                         pred.scores = pred.scores,
                         nitems = nitems1)
    
    
  } else {
    stop("score.type argument must be 'Rest' or 'Total'")
  }
  
  ## Converting list elements into single dataframe
  loess.df <- Reduce(rbind, loess.list)
  loess.df$item <- factor(loess.df$item, levels = names(scale.data))
  
  ## Plotting the results
  plot <- ggplot(loess.df, aes(x = score, y = prob, group = group)) +  # group variable currently prints in alphabetical order
    geom_line(aes(color = group), lwd = .8) +
    geom_ribbon(aes(ymin = prob - 1.96 * se,
                    ymax = prob + 1.96 * se,
                    fill = group), alpha = .4) +
    labs(x = paste(score.type, "Score"), y = "Prob(Correct)") +
    #theme_bw(base_size = 16) +
    facet_wrap(~ item, ncol = 6) +
    theme(legend.position = "top")
  
  ## Output data and plot in a list
  loess <- list(data = loess.df,
                plot = plot)
  
  return(loess)
}



#### the loess method ####
run_loess <- function(scale.data, item, group, match,
                      pred.scores, nitems){
  
  # Appending match scores to item data
  scale.data$score <- match
  
  # Model
  mod <- paste0(names(scale.data)[item], " ~ score")
  
  # levels of the grouping variable
  group1 <- levels(group)[1]
  group2 <- levels(group)[2]
  
  # Loess by group
  g1.fit <-  loess(mod, data = subset(scale.data, group == group1))
  g2.fit <-  loess(mod, data = subset(scale.data, group == group2))
  
  # predicted probability by group
  g1.pred <- predict(g1.fit, newdata = pred.scores, se = T)
  g2.pred <- predict(g2.fit, newdata = pred.scores, se = T)
  
  # Storage - one row for each pred.scores x group combination
  loess.data <- data.frame(
    score = rep(pred.scores, times = 2),
    prob = c(g1.pred$fit, g2.pred$fit),
    se = c(g1.pred$se.fit, g2.pred$se.fit),
    item = rep(names(scale.data)[item], times = nitems*2),
    group = rep(c(group1, group2), each = nitems)
  )
  
  return(loess.data)
}