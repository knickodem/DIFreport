##############################
#                            #
#    Executive functions     #
#                            #
##############################


#### Function specifically for cleaning World Bank Data ####
WB_Data_Prep <- function(data, items, groupvar){
  
  ## subsetting data and removing wave identifier from column names
  MeasureData <- data[grep(items, names(data))]
  names(MeasureData) <- substr(names(MeasureData), 1, nchar(names(MeasureData)) - 2)
  
  ## Handling missing data in the items
  # Identifying cases with NA on all items or in grouping variable
  drop_cases <- apply(is.na(MeasureData), 1, mean) != 1             # cases with all NA
  drop_cases <- ifelse(is.na(data[[groupvar]]), FALSE, drop_cases)  # cases with NA for group
  # Note: In drop_cases, the FALSE elements are the cases that will be removed from the analysis
  
  MeasureData[is.na(MeasureData)] <- 0                  # Replacing NA with 0
  MeasureData <- MeasureData[drop_cases, ]              # dropping cases with all NA
  
  # Vector of the grouping variable and its levels
  group <- data[drop_cases, ][[groupvar]]
  
  output <- list(MeasureData = MeasureData,
                 GroupVector = group)
  
  return(output)
  
}


#### Wrapper function around the DIF Methods to conduct the full analysis ####
DIF_analysis <- function(MeasureData, groupvec, scoreType = c("Rest", "Total"),
                        methods = c("loess", "MH", "logistic", "IRT"),        # could incorporate a shortcut for "all"
                        MHstrata = NULL){ 
  
  ## Number of items in the measure
  n_items <- ncol(MeasureData)

  ## Calculating vector of total scores or list of rest scores
  if(scoreType == "Rest"){ # Returns a list with n_items elements of length = nrow(MeasureData)
    
    match_scores <- lapply(c(1:n_items), Get_MatchScore, scaledat = MeasureData) 
    
  } else if(scoreType == "Total"){ # a single vector of length = nrow(MeasureData)
    
    match_scores <- Get_MatchScore(scaledat = MeasureData, drops = NULL)
    
  } else {
    stop("scoreType argument must be 'Rest' or 'Total'")
  }
  
  
  #### LOESS ####
  if("loess" %in% methods){
    
    loess <- Get_loess(scaledat = MeasureData,
                       group = groupvec,
                       scoreType = scoreType,
                       match_on = match_scores)

  } else{
    
    loess <- NULL
    
  }
  
  #### Mantel-Haenszel ####
  if("MH" %in% methods){
    
    MH <- Get_MH(scaledat = MeasureData,
                 group = groupvec,
                 scoreType = scoreType,
                 stage1.match_scores = match_scores,
                 strata = MHstrata) 

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
    
    logresults <- Run_logisticByItem(scaledat = MeasureData, theItem = i, 
                               group = groupvec, scoreType = scoreType)
    
    log_mod_comp <- rbind(log_mod_comp, logresults$modtest)
    slope_params <- rbind(slope_params, logresults$slopes)
      
    }
    
    ## COULD CONVERT THE ABOVE TO LAPPLY WITH REDUCE AS WE DO IN THE LOESS ANALYSIS
    
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
    
    IRT <- Run_IRT(scaledat = MeasureData, group = groupvec)
    
  } else{
    
    IRT <- NULL
    
  }
  
  all <- list(loess = loess,
              MH = MH,
              logistic = logistic,
              IRT = IRT)
  
  return(all)
  
}
