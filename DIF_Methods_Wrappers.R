######################################
#                                    #
#    Wrappers for the DIF Methods    #
#                                    #
######################################

# These are the middle manager functions b/t the specialist DIF_Methods_Functions and the executive Measure_Level_Wrapper that 

## Packages
# Note: these could be placed in more strategic places given that they are not always needed depending on the analysis choices
library(ggplot2)
library(mirt)


###############
#### Loess ####

Get_loess <- function(scaledat, group, scoreType, match_on){
  
  ## Number of items in the measure
  n_items <- ncol(scaledat)
  
  if(scoreType == "Rest"){
    
    pred_scores <- 0:(n_items - 1)  # score range to use with predict function
    
    ## Running loess method on each item
    loess_list <- list()
    for(i in 1:n_items){
      
      loess_list[[i]] <- Run_loess(scaledat = scaledat, theItem = i,
                                   group = group, match = match_on[[i]],
                                   pred_scores = pred_scores,
                                   n_items = n_items)
    }
    
    
  } else if(scoreType == "Total"){
    
    pred_scores <- 0:(n_items) # score range to use with predict function
    n_items1 <- n_items + 1    # Needed for the item and group columns in loess_data
    
    ## Running loess method on each item
    loess_list <- lapply(c(1:n_items),
                         Run_loess, scaledat = scaledat,
                         group = group, match = match_on,
                         pred_scores = pred_scores,
                         n_items = n_items1)
    
    
  } else {
    stop("scoreType argument must be 'Rest' or 'Total'")
  }
  
  ## Converting list elements into single dataframe
  loess_df <- Reduce(rbind, loess_list)
  loess_df$item <- factor(loess_df$item, levels = names(scaledat))
  
  ## Plotting the results
  p <- ggplot(loess_df, aes(x = score, y = prob, group = group)) +
    geom_line(aes(color = group), lwd = .8) +
    geom_ribbon(aes(ymin = prob - 1.96*SE,
                    ymax = prob + 1.96*SE,
                    fill = group), alpha = .4) +
    labs(x = paste(scoreType, "Score"), y = "Item Regression") +
    theme_bw(base_size = 16) +
    facet_wrap(~ item)
  
  ## Output data and plot in a list
  loess <- list(data = loess_df,
                plot = p)
  
  return(loess)
}



#######################################


#########################
#### Mantel-Haenszel ####

Get_MH <- function(scaledat, group, scoreType, stage1.match_scores, strata = NULL){
  
  
  ## Number of items in the measure
  n_items <- ncol(scaledat)
  
  #### MH testing stage 1 - initial DIF items ####
  
  ## Using rest scores as the matching criterion
  if(scoreType == "Rest"){
    
    # Storage
    stage1 <- list()
    
    # Loop over items
    for(i in 1:n_items){
      
      stage1[[i]] <- Run_MH(scaledat = scaledat, theItem = i, 
                            group = group, match = stage1.match_scores[[i]],
                            strata = strata)
    }
    
  } else if(scoreType == "Total"){
    
    stage1 <- lapply(1:n_items, Run_MH, scaledat = scaledat, 
                     group = group, match = stage1.match_scores,
                     strata = strata)
  }
  
  # Converting list elements into single dataframe
  MH1 <- Reduce(rbind, stage1)
  
  # Benjamini–Hochberg procedure for false discovery rate < 5%
  MH1$bias <- p.adjust(MH1$pvalue, method = "BH") < .05
  
  #### MH testing stage 2 - Refinement/purification of match_score criterion ####
  # The items to exclude based on initial MH DIF analysis
  MH_drops <- which(MH1$bias == 1)
  
  if(scoreType == "Rest"){
    
    # Storage
    stage2 <- list()
    
    # Loop over items
    for(i in 1:n_items) {
      
      # Recalculate rest score while also removing biased items identified in stage 1
      stage2.match_scores <- Get_MatchScore(scaledat = scaledat, drops = c(i, MH_drops))
      
      stage2[[i]] <- Run_MH(scaledat = scaledat, theItem = i, 
                       group = group, match = stage2.match_scores,
                       strata = strata)
      
    }
    
  } else if(scoreType == "Total"){
    
    # Recalculate rest score while also removing biased items identified in stage 1
    stage2.match_scores <- Get_MatchScore(scaledat = scaledat, drops = c(MH_drops))
    
    stage2 <- lapply(1:n_items, Run_MH, scaledat = scaledat, 
                     group = group, match = stage2.match_scores,
                     strata = strata)
    
  }
  
  # Converting list elements into single dataframe
  MH2 <- Reduce(rbind, stage2)
  
  # Benjamini–Hochberg procedure for false discovery rate = 5%
  MH2$bias <- p.adjust(MH2$pvalue, method = "BH") < .05
  
  # Output dataframe combining stage 1 and stage 2
  names(MH1)[-1] <- paste0("Initial_", names(MH1)[-1])
  names(MH2)[-1] <- paste0("Refined_", names(MH2)[-1])
  MH <- cbind(MH1, MH2[,-1])
  
  
  return(MH)
}
