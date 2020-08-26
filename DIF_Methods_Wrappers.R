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
library(xtable)



# ------------------- Loess ------------------------------------

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
    
    pred_scores <- 0:n_items   # score range to use with predict function
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
  p <- ggplot(loess_df, aes(x = score, y = prob, group = group)) +  # group variable currently prints in alphabetical order
    geom_line(aes(color = group), lwd = .8) +
    geom_ribbon(aes(ymin = prob - 1.96*SE,
                    ymax = prob + 1.96*SE,
                    fill = group), alpha = .4) +
    labs(x = paste(scoreType, "Score"), y = "Prob(Correct)") +
    #theme_bw(base_size = 16) +
    facet_wrap(~ item)
  
  ## Output data and plot in a list
  loess <- list(data = loess_df,
                plot = p)
  
  return(loess)
}


#######################################


# ----------------- Mantel-Haenszel -------------------------

Get_MH <- function(scaledat, group, scoreType,
                   stage1.match_scores, strata = NULL){
  
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
  names(MH1)[names(MH1 == "pvalue")] <- "p"
  MH1$bias <- p.adjust(MH1$p, method = "BH") < .05
  
  #### MH testing stage 2 - Refinement/purification of match_score criterion ####
  # The items to exclude based on initial MH DIF analysis
  MH_drops <- which(MH1$bias == 1)
  
  if(length(MH_drops) > 0){
  
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
      
    } else if(scoreType == "Total") {
      
      # Recalculate rest score while also removing biased items identified in stage 1
      stage2.match_scores <- Get_MatchScore(scaledat = scaledat, drops = c(MH_drops))
      
      stage2 <- lapply(1:n_items, Run_MH, scaledat = scaledat, 
                       group = group, match = stage2.match_scores,
                       strata = strata)
      
    }
    
    # Converting list elements into single dataframe
    MH2 <- Reduce(rbind, stage2)
    
    # Benjamini–Hochberg procedure for false discovery rate = 5%
    names(MH2)[names(MH2 == "pvalue")] <- "p"
    MH2$bias <- p.adjust(MH2$p, method = "BH") < .05
    
    # Output dataframe combining stage 1 and stage 2
    names(MH1)[-1] <- paste0("Initial_", names(MH1)[-1])
    names(MH2)[-1] <- paste0("Refined_", names(MH2)[-1])
    
    MH <- list(Item = cbind(MH1, MH2[,-1]),
               Biased_Items = which(MH2$Refined_bias == 1))   # Special use cases might throw errors here
    
  } else {
    
    #names(MH1)[-1] <- paste0("Initial_", names(MH1)[-1])
    MH <- list(Item = MH1,
               Biased_Items = "No uniform DIF was detected")
  }
  
  return(MH)
}


###############################################################



# ----------- Logistic Regression -----------------------------

Get_Logistic <- function(scaledat, group, scoreType, match_on){
  
  ## Number of items in the measure
  n_items <- ncol(scaledat)

  ## Setting up match scores for use in Run_GlobalLogistic
  if(scoreType == "Rest"){
    
    match_list <- match_on
    
  } else if(scoreType == "Total"){
    
    match_list <- rep(list(match_on), n_items)
  }
  
  #### Testing for uniform and non-uniform DIF across all items ####
  globalLogistic <- Run_GlobalLogistic(scaledat = scaledat, group = group, match_list = match_list)
  
  
  if(length(globalLogistic$DIF_type) > 0){
    
    #### Testing each item for DIF ####
    
    ## Stage 1 - Initial DIF
    stage1Log <- list()
    for(i in names(scaledat)){
      
      stage1Log[[i]] <- Run_ItemLogistic(itemlogdf = globalLogistic$Data[globalLogistic$Data$item == i, ],
                                    which.type = globalLogistic$DIF_type)
      
    }
    
    ## Model comparison
    stage1modcompdf <- Reduce(rbind, stage1Log)
    
    # Benjamini–Hochberg procedure for false discovery rate < 5%
    names(stage1modcompdf)[names(stage1modcompdf == "pvalue")] <- "p"
    stage1modcompdf$bias <- p.adjust(stage1modcompdf$p, method = "BH") < .05
    
    # ## Based on OR
    # stage1paramsdf <- lapply(stage1Log, function(x){Reduce(rbind, x["Parameter_Estimates"])})
    # stage1paramsdf <- Reduce(rbind, stage1paramsdf)
    # # Benjamini–Hochberg procedure for false discovery rate < 5%
    # stage1paramsdf$bias <- p.adjust(stage1paramsdf$pvalue, method = "BH") < .05
    

     #### Stage 2 - Refinement/purification of match_on criterion ####
    # The items to exclude based on initial Logistic DIF analysis
    Log_drops <- which(stage1modcompdf$bias == 1)
    
    if(length(Log_drops) > 0){
      
      # Storage
      stage2Log <- list()
      
      if(scoreType == "Rest"){
        
        for(i in 1:n_items){
          
          # Recalculate rest score while also removing biased items identified in stage 1
          stage2.match_on <- Get_MatchScore(scaledat = scaledat, drops = c(i, Log_drops))
          
          # filtering based on the item and adding re-calculated rest score
          tempitemlogdf <- globalLogistic$Data[globalLogistic$Data$item == names(scaledat)[[i]], ]
          tempitemlogdf$score <- stage2.match_on
          
          stage2Log[[i]] <- Run_ItemLogistic(itemlogdf = tempitemlogdf,
                                             which.type = globalLogistic$DIF_type)
          
        }
        
      } else if(scoreType == "Total"){
        
        # Recalculate total score while removing biased items identified in stage 1
        stage2.match_on <- Get_MatchScore(scaledat = scaledat, drops = c(Log_drops))
        
        for(i in 1:n_items){
          
          # filtering based on the item and adding re-calculated total score
          tempitemlogdf <- globalLogistic$Data[globalLogistic$Data$item == names(scaledat)[[i]], ]
          tempitemlogdf$score <- stage2.match_on
          
          stage2Log[[i]] <- Run_ItemLogistic(itemlogdf = tempitemlogdf,
                                             which.type = globalLogistic$DIF_type)
        }
      }
      
      ## Model comparison
      stage2modcompdf <- Reduce(rbind, stage2Log)
      
      # Benjamini–Hochberg procedure for false discovery rate < 5%
      names(stage2modcompdf)[names(stage2modcompdf == "pvalue")] <- "p"
      stage2modcompdf$bias <- p.adjust(stage2modcompdf$p, method = "BH") < .05
      
      
      #### Output dataframe combining stage 1 and stage 2 ####
      names(stage1modcompdf)[-c(1:2)] <- paste0("Initial_", names(stage1modcompdf)[-c(1:2)])
      names(stage2modcompdf)[-c(1:2)] <- paste0("Refined_", names(stage2modcompdf)[-c(1:2)])
      ItemLog <- cbind(stage1modcompdf, stage2modcompdf[,-c(1:2)])
      
      LogisticDIF <- list(Global = globalLogistic$Model_Comparison,
                         Item = ItemLog,
                         Biased_Items = which(stage2modcompdf$Refined_bias == 1))
    } else {
    
     LogisticDIF <- list(Global = globalLogistic$Model_Comparison,
                         Item = stage1modcompdf,
                         Biased_Items = "No DIF was detected")
    } 
    
  } else {
    
    LogisticDIF <- list(Global = globalLogistic$Model_Comparison,
                        Item = "No item DIF was detected through logistic model comparisons",
                        Biased_Items = "No DIF was detected")
  }

  return(LogisticDIF)
  
}

#####################################################


# ---------- Item Response Theory --------------------------------

Get_IRT <- function(scaledat, group){
  
  #### Comparing configural, metric, and scalar models ####
  globalIRT <- tryCatch(expr = {
    Run_GlobalIRT(scaledat = scaledat, group = group)
  },
  error = function(e){
    message("Did not run IRT method. Possible empty cell(s) in the item by group frequency tables. If so, remove the item from MeasureData.")
    warning(paste("IRT", e))
    return(NULL)}
  )
  
  if(is.null(globalIRT)){
    
    IRTdif <- NULL
    
  } else {
    
    cols <- 1:ncol(scaledat)
    
    ## If DIF was detected in the global tests
    if(length(globalIRT$Params_With_DIF) > 0){
      
      ## Stage 1 - Initial DIF
      stage1IRT <- lapply(cols, Run_ItemIRT, GlobalResults = globalIRT, which.model = "nodif_mod")
      # if we want more flexibility in the code, might need to use a for loop instead
      
      # convert list to df
      stage1IRTdf <- Reduce(rbind, stage1IRT)
      
      # Benjamini–Hochberg procedure for false discovery rate < 5%
      stage1IRTdf$bias <- p.adjust(stage1IRTdf$p, method = "BH") < .05
      
      # The items to free in stage 2
      IRT_free <- which(stage1IRTdf$bias == 1)
      
      # Extracting X2 test results from initial run for output
      InitialIRTdf <- cbind(rownames(stage1IRTdf),
                            #data.frame(Parameter = globalIRT$Params_With_DIF),
                            data.frame(stage1IRTdf[,6:9], row.names = NULL))
      names(InitialIRTdf) <- c("Item", #"Parameter",
                               "X2", "df", "p", "bias")
      
      
      if(length(IRT_free) > 0){
        names(InitialIRTdf)[2:5] <- paste0("Initial_", names(InitialIRTdf)[2:5])
        
        ## Stage 2 - Refine/Purify
        # Re-estimate scalar model while freeing IRT_free items
        globalIRT$Stage2_Scalar_Mod <- multipleGroup(scaledat, model = 1, group = group,
                                                     invariance = c('free_var','free_means', names(scaledat)[-IRT_free]))
        
        
        stage2IRT <- lapply(cols[-IRT_free], Run_ItemIRT, GlobalResults = globalIRT, which.model = "Stage2_Scalar_Mod")
        # if we want more flexiblity in the code, might need to use a for loop instead
        
        # convert list to df
        stage2IRTdf <- Reduce(rbind, stage2IRT)
        
        # Benjamini–Hochberg procedure for false discovery rate < 5%
        stage2IRTdf$bias <- p.adjust(stage2IRTdf$p, method = "BH") < .05
        
        ## Extracting X2 test results from refined run for output
        RefinedIRTdf <- cbind(rownames(stage2IRTdf), data.frame(stage2IRTdf[,6:9], row.names = NULL))
        names(RefinedIRTdf) <- c("Item", "Refined_X2", "Refined_df", "Refined_p", "Refined_bias")
        
        ## Combining initial and refined output
        ItemIRT <- merge(x = InitialIRTdf, y = RefinedIRTdf, by = "Item", all.x = TRUE, all.y = FALSE)
        
        ## Ordering items
        ItemIRT$Item <- factor(ItemIRT$Item, levels = names(scaledat))
        ItemIRT <- ItemIRT[order(ItemIRT$Item), ]
        row.names(ItemIRT) <- NULL
        bi <- which(ItemIRT$Refined_bias == TRUE | is.na(ItemIRT$Refined_bias))
        
        ## Merging initial and refined stage results
        IRTdif <- list(Global = globalIRT$Model_Comparison,
                       Item = ItemIRT,
                       Biased_Items = bi,
                       Scalar_Mod = globalIRT$Scalar_Mod)
        
      } else {
        
        InitialIRTdf$Item <- factor(InitialIRTdf$Item, levels = names(scaledat))
        InitialIRTdf <- InitialIRTdf[order(InitialIRTdf$Item), ]
        row.names(InitialIRTdf) <- NULL
        
        IRTdif <- list(Global = globalIRT$Model_Comparison,
                       Item = InitialIRTdf,
                       Biased_Items = "No DIF was detected",
                       Scalar_Mod = globalIRT$Scalar_Mod)
        message("Global IRT model comparisons suggested DIF, but none was found when through inidividual item comparisons.")
      }
      
    }  else {
      
      IRTdif <- list(Global = globalIRT$Model_Comparison,
                     Item = "No item DIF was detected through IRT model comparisons",
                     Biased_Items = "No DIF was detected",
                     Scalar_Mod = globalIRT$Scalar_Mod)
    }
  }
  
  return(IRTdif)
  
}

##############################################