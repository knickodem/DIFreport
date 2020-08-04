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
    
    
    
    logistic <- Get_Logistic(scaledat = MeasureData,
                             group = groupvec,
                             scoreType = scoreType,
                             match_on = match_scores)
    
    
  } else{
    
    logistic <- NULL
    
  }
  
  
  #### Item Response Theory ####
  if("IRT" %in% methods){
    
    IRT <- Get_IRT(scaledat = MeasureData, group = groupvec)
    
  } else{
    
    IRT <- NULL
    
  }
  
  all <- list(loess = loess,
              MH = MH,
              logistic = logistic,
              IRT = IRT)
  
  return(all)
  
}


#### Use information produced from `DIF_analysis` to compare treatment effect estimates ####
CompareTreatmentEffects <- function(DIFanalysis){}  # Need to alter DIF_analysis output to include all information needed to run comparison
# pass items that we want to check
# breakdown by conditional (eg. gender) or unconditional treatment effect
# observed score with dropped items or fit irt with identified items not constrianed over groups (ml factor scores)
#   - score, group