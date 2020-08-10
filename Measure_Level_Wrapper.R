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
# biased.items - vector of items in `MeasureData` to exclude from score calculation; currently only accepts locations, not names
CompareTreatmentEffects <- function(MeasureData, groupvec, biased.items, IRTmethod = "ML"){
  
  ## levels of the grouping variable
  group1 <- levels(groupvec)[1] 
  group2 <- levels(groupvec)[2]
  
#### Using Total Score ####
  # The standardized mean difference with all items included
  total <- Get_MatchScore(MeasureData)
  mean_total <- tapply(total, groupvec, mean, na.rm = T)
  sd_total <- tapply(total, groupvec, sd, na.rm = T)
  n_total <- tapply(total, groupvec, length)
  delta_total <- Get_smd(m1 = mean_total[[group1]], m2 = mean_total[[group2]],
                   sd1 = sd_total[[group1]], sd2 = sd_total[[group2]],
                   n1 = n_total[[group1]], n2 = n_total[[group2]])
  
  # Reliability of total score
  r_total <- psy::cronbach(MeasureData)$alpha
  
  # The standardized mean difference with bias items omitted
  star <- Get_MatchScore(MeasureData, drops = biased.items)
  mean_star <- tapply(star, groupvec, mean, na.rm = T)
  sd_star <- tapply(star, groupvec, sd, na.rm = T)
  n_star <- tapply(star, groupvec, length)
  delta_star <- Get_smd(m1 = mean_star[[group1]], m2 = mean_star[[group2]],
                   sd1 = sd_star[[group1]], sd2 = sd_star[[group2]],
                   n1 = n_star[[group1]], n2 = n_star[[group2]])
  
  # Reliability of total score
  r_star <- psy::cronbach(MeasureData[,-c(biased.items)])$alpha
  

#### using irt score ####
  ## REVISE WORKFLOW TO GRAB mod_scalar FROM DIF ANALYSIS RATHER THAN RE-RUNNING IT
  mod_scalar <- multipleGroup(MeasureData, model = 1, group = groupvec,
                              invariance = c('slopes', 'intercepts', 'free_var','free_means'))
  mod_bo <-   multipleGroup(MeasureData, model = 1, group = groupvec,
                                            invariance = c(names(MeasureData)[-biased.items],'free_var','free_means'))
  
  theta_scalar <- fscores(mod_scalar, method = IRTmethod)
  theta_bo <- fscores(mod_bo, method = IRTmethod)
  
  # The standardized mean difference for the scalar model
  mean_scalar <- tapply(theta_scalar , groupvec, mean, na.rm = T)
  sd_scalar <- tapply(theta_scalar , groupvec, sd, na.rm = T)
  n_scalar <- tapply(theta_scalar, groupvec, length)
  delta_scalar <- Get_smd(m1 = mean_scalar[[group1]], m2 = mean_scalar[[group2]],
                        sd1 = sd_scalar[[group1]], sd2 = sd_scalar[[group2]],
                        n1 = n_scalar[[group1]], n2 = n_scalar[[group2]])
  
  # The standardized mean difference for the bias omitted model
  mean_bo <- tapply(theta_bo , groupvec, mean, na.rm = T)
  sd_bo <- tapply(theta_bo , groupvec, sd, na.rm = T)
  n_bo <- tapply(theta_bo, groupvec, length)
  delta_bo <- Get_smd(m1 = mean_bo[[group1]], m2 = mean_bo[[group2]],
                          sd1 = sd_bo[[group1]], sd2 = sd_bo[[group2]],
                          n1 = n_bo[[group1]], n2 = n_bo[[group2]])
  
  
  effects = data.frame(Measure = c("Raw Delta", "Adj. Delta", "Reliability", "Theta Delta"),
                       All_Items = c(delta_total, (delta_total / sqrt(r_total)), r_total, delta_scalar),
                       Bias_Omitted = c(delta_star, (delta_star / sqrt(r_star)), r_star, delta_bo))
  
  
return(effects)  
  
}

# breakdown by conditional (eg. gender) or unconditional treatment effect
# observed score with dropped items or fit irt with identified items not constrianed over groups (ml factor scores)
#   - score, group


