#### High level (Measure level) function ####
## Function currently assumes groupvar has 2 levels
WB_analysis <- function(data, items, groupvar, scoreType = c("Rest", "Total"),
                        methods = c("loess", "MH", "logistic", "IRT"),        # could incorporate a shortcut for "all"
                        MHstrata = NULL){ 
  
  ## subsetting data and removing wave identifier from column names
  MeasureData <- data[grep(items, names(data))]
  names(MeasureData) <- substr(names(MeasureData), 1, nchar(names(MeasureData)) - 2)
  
  ## Handling missing data in the items
  # Identifying cases with NA on all items or in grouping variable
  drop_cases <- apply(is.na(MeasureData), 1, mean) != 1             # cases with all NA
  drop_cases <- ifelse(is.na(data[[groupvar]]), FALSE, drop_cases)  # cases with NA for group
  # Note: drop_cases = FALSE are the cases that will be removed from the analysis
  
  MeasureData[is.na(MeasureData)] <- 0                  # Replacing NA with 0
  MeasureData <- MeasureData[drop_cases, ]              # dropping cases with all NA
  
  # Number of items in the measure; length(items) should work too
  n_items <- ncol(MeasureData)
  
  # Name grouping variable and its levels
  group <- data[drop_cases, ][[groupvar]]
  
  
  ## COULD IMPROVE EFFICIENCY BY CALCULATING TOTAL AND REST SCORES HERE
  ## RATHER THAN WITHIN EACH FUNCTION
  
  ##
  ## Item level functions ##
  ##
  
  #### LOESS ####
  if("loess" %in% methods){
    
    ## score range to use with predict function
    if(scoreType == "Rest"){
      pred_scores <- 0:(n_items - 1)  
    } else if(scoreType == "Total"){
      pred_scores <- 0:(n_items)
    } else {
      stop("scoreType argument must be 'Rest' or 'Total'")
    }
    
    ## Storage
    gg_data <- list()
    
    ## Loop over items
    for(i in 1:n_items){
      
      ## Running loess method
      gg_data[[i]] <- Run_loess(scaledat = MeasureData, theItem = i,
                                group, pred_scores, n_items, scoreType)
    }
    
    ## Reformatting list elements into single dataframe
    gg_data <- Reduce(rbind, gg_data)
    
    ## Plotting the results
    p <- ggplot(gg_data, aes(x = score, y = prob, group = group)) +
      geom_line(aes(color = group), lwd = .8) +
      geom_ribbon(aes(ymin = prob - 1.96*SE,
                      ymax = prob + 1.96*SE,
                      fill = group), alpha = .4) +
      labs(x = paste(scoreType, "Score"), y = "Item Regression") +
      theme_bw(base_size = 16) +
      facet_wrap(~ item)
    
    ## Output data and plot in a list
    loess <- list(data = gg_data,
                  plot = p)
  } else{
    
    loess <- NULL
    
  }
  
  #### Mantel-Haenszel ####
  if("MH" %in% methods){
    
    ## MH testing stage 1
    # Storage
    MH1 <- data.frame(item = names(MeasureData), Initial_OR = NA, 
                      Initial_lower = NA, Initial_upper = NA, 
                      Initial_pvalue = NA, Initial_bias = 0)
    
    # Loop over items
    for(i in 1:n_items){
      
      stage1 <- Run_MH(scaledat = MeasureData, theItem = i, 
                       group = group, scoreType = scoreType,
                       strata = MHstrata)
      
      MH1$Initial_OR[i] <- stage1$estimate
      MH1$Initial_lower[i] <- stage1$conf.int[1]
      MH1$Initial_upper[i] <- stage1$conf.int[2]
      MH1$Initial_pvalue[i] <- stage1$p.value
    }
    
    # Benjamini–Hochberg procedure for false discovery rate < 5%
    MH1$Initial_bias <- p.adjust(MH1$Initial_pvalue, method = "BH") < .05
    
    ## MH testing stage 2
    # The items to exclude based on initial test
    item_drops <- which(MH1$Initial_bias == 1) 
    
    # Storage
    MH2 <- data.frame(item = names(MeasureData), Refine_OR = NA,
                      Refine_lower = NA, Refine_upper = NA,
                      Refine_pvalue = NA, Refine_bias = 0)
    
    # Loop over items
    for(i in 1:n_items) {
      
      stage2 <- Run_MH(scaledat = MeasureData, theItem = i, 
                       group = group, scoreType = scoreType,
                       strata = MHstrata, Stage2 = item_drops)
      
      MH2$Refine_OR[i] <- stage2$estimate
      MH2$Refine_lower[i] <- stage2$conf.int[1]
      MH2$Refine_upper[i] <- stage2$conf.int[2]
      MH2$Refine_pvalue[i] <- stage2$p.value
    }
    
    # Benjamini–Hochberg procedure for false discovery rate = 5%
    MH2$Refine_bias <- p.adjust(MH2$Refine_pvalue, method = "BH") < .05
    
    # Output dataframe combining stage 1 and stage 2
    MH <- cbind(MH1, MH2[,-1])
    
  } else{
    
    MH <- NULL
    
  }
  
  #### Logistic Regression ####
  if("logistic" %in% methods){
    
    # Blank dataframe for adding model chisquare comparison with 2 df
    log_mod_comp <- data.frame(item = NA, deviance = NA, pvalue = NA)
    
    # Blank dataframe for adding Slope parameters for uniform and nonuniform dif
    slope_params <- data.frame(item = NA, Type = NA, Estimate = NA, SE = NA, z = NA, pvalue = NA, OR = NA)
    
    # Loop over items
    for(i in 1:n_items) {
    
    logresults <- Run_logistic(scaledat = MeasureData, theItem = i, 
                               group = group, scoreType = scoreType)
    
    log_mod_comp <- rbind(log_mod_comp, logresults$modtest)
    slope_params <- rbind(slope_params, logresults$slopes)
      
    }
    
    # Benjamini–Hochberg procedure for false discovery rate = 5%
    log_mod_comp$bias <- p.adjust(log_mod_comp[,3], method = "BH") < .05
    slope_params$bias <- p.adjust(slope_params[,6], method = "BH") < .05
    
    # Output a list containing two dataframes
    logistic <- list(Model_Comparison = log_mod_comp[-1, ],
                     Slope_Parameters = slope_params[-1, ])
    
  } else{
    
    logistic <- NULL
    
  }
  
  
  #### Item Response Theory ####
  if("IRT" %in% methods){
    
    Run_IRT()
    
  } else{
    
    IRT <- NULL
    
  }
  
  all <- list(loess = loess,
              MH = MH,
              logistic = logistic,
              IRT = IRT)
  
  return(all)
  
}
