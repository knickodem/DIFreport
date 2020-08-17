##############################
#                            #
#    Executive functions     #
#                            #
##############################

## These are the functions the user will need to specify


#### Function specifically for cleaning World Bank Data ####
WB_Data_Prep <- function(data, items, groupvar, condvar = NULL){
  
  ## Subsetting measure specific data and removing wave identifier from column names
  MeasureData <- data[grep(items, names(data))]
  names(MeasureData) <- substr(names(MeasureData), 1, nchar(names(MeasureData)) - 2)
  
  ## Identifying cases to drop based on missing data
  # Note: In drop_cases, the FALSE elements are the cases that will be removed from the analysis
  drop_cases <- apply(is.na(MeasureData), 1, mean) != 1             # cases with NA for all items
  drop_cases <- ifelse(is.na(data[[groupvar]]), FALSE, drop_cases)  # cases with NA for group

  # If examining conditional effects
  if(!is.null(condvar)){
  drop_cases <- ifelse(is.na(data[[condvar]]), FALSE, drop_cases)  # cases with NA for conditional variable
  }
  
  
  ## Dropping the identified missing data cases
  MeasureData <- MeasureData[drop_cases, ]   # from the measure response dataframe      
  group <- data[drop_cases, ][[groupvar]]    # from the grouping variable vector
  
  ## Replacing remaining NAs with 0
  MeasureData[is.na(MeasureData)] <- 0       
  
  ## If examining conditional effects
  if(!is.null(condvar)){
    
    # Dropping missing data cases from the conditional variable vector
    cond <- data[drop_cases, ][[condvar]]
    
    # Output with conditional variable vector
    output <- list(MeasureData = MeasureData,
                   GroupVector = group,
                   CondVector = cond)
    
  } else {
    
    # Output without conditional variable vector
    output <- list(MeasureData = MeasureData,
                   GroupVector = group)
  }
  
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
              IRT = IRT,
              Inputs = list(data = MeasureData,
                            group = groupvec,
                            scoreType = scoreType))
  
  return(all)
  
}


#### Use information produced from `DIF_analysis` to compare treatment effect estimates ####
# biased.items - vector of items in `MeasureData` to exclude from score calculation; currently only accepts locations, not names
# IRT based deltas will be NaNs if any IRT score is +/- Inf
# Might need to include functionality if there are no biased.items
CompareTreatmentEffects <- function(MeasureData, groupvec,
                                    biased.items, mod_scalar = NULL, IRTmethod = "ML"){
  
  
  #### Using Total Score ####
  
  ## Calculating scores
  total <- Get_MatchScore(MeasureData)
  star <- Get_MatchScore(MeasureData, drops = biased.items)
  
  ## Standardized mean differences
  delta_total <- smd_wrapper(score = total, gp = groupvec, denom = "control")
  delta_star <- smd_wrapper(score = star, gp = groupvec, denom = "control")
  
  ## Reliabilities
  r_total <- psy::cronbach(MeasureData)$alpha
  r_star <- psy::cronbach(MeasureData[,-c(biased.items)])$alpha
  

  #### Using IRT Score ####
  
  ## IRT model with parameters constrained to be equal between groups
  if(is.null(mod_scalar)){
    
    mod_scalar <- multipleGroup(MeasureData, model = 1, group = groupvec,
                                invariance = c('slopes', 'intercepts', 'free_var','free_means'))
    
  }
  ## IRT model with parameter constraints freed for biased.items
  mod_bo <-   multipleGroup(MeasureData, model = 1, group = groupvec,
                            invariance = c(names(MeasureData)[-biased.items],'free_var','free_means'))
  
  ## Calculating IRT scores
  theta_scalar <- fscores(mod_scalar, method = IRTmethod)
  theta_bo <- fscores(mod_bo, method = IRTmethod)
  
  ## Calculating standardized mean differences
  delta_scalar <- smd_wrapper(score = theta_scalar, gp = groupvec, denom = "control")
  delta_bo <- smd_wrapper(score = theta_bo, gp = groupvec, denom = "control")
  
  
  #### Compiling Results ####  
  # effects = data.frame(Measure = c("Raw Delta", "Adj. Delta", "Reliability", "Theta Delta"),
  #                      All_Items = c(delta_total, (delta_total / sqrt(r_total)), r_total, delta_scalar),
  #                      Bias_Omitted = c(delta_star, (delta_star / sqrt(r_star)), r_star, delta_bo))
  
  effectsdf <- data.frame(Items = c("All Items", "Bias Omitted"),
                          `Raw Delta` = c(delta_total, delta_star),
                          `Adj. Delta` = c((delta_total / sqrt(r_total)), (delta_star / sqrt(r_star))),
                          Reliability = c(r_total, r_star),
                          `Theta Delta` = c(delta_scalar, delta_bo),
                          check.names = FALSE)
  
  effects <- list(Effects_Table = effectsdf,
                  Comparison = paste(levels(groupvec)[[2]], "-", levels(groupvec)[[1]]))
  
  
  return(effects)  
  
}
