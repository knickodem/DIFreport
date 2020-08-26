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
  
  #### Identifying problematic items ####
  MDorig <- MeasureData # saving original data with all items for use in loess
  
  ## Items with no variance
  var_check <- lapply(MeasureData, var)
  nvi <- which(var_check == 0)
  
  ## Items with no variance for one group (think about other ways to do this)
  var_check_group <- lapply(MeasureData, function(x){sum(table(x, groupvec) == 0)})
  nvgi <- which(var_check_group > 0)
  
  ## Removing items with no variance from the data, if they exist
  if(length(nvi) > 0){
  MeasureData <- MeasureData[-c(nvi)]
  }
  
  ## Number of items in the measure for dif analysis
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
    
    
    if(length(nvi) > 0){
      
      ## Calculating vector of total scores or list of rest scores
      if(scoreType == "Rest"){ # Returns a list with n_items elements of length = nrow(MeasureData)
        
        loess_match <- lapply(c(1:ncol(MDorig)), Get_MatchScore, scaledat = MDorig) 
        
      } else if(scoreType == "Total"){ # a single vector of length = nrow(MeasureData)
        
        loess_match <- Get_MatchScore(scaledat = MDorig, drops = NULL)
        
      }  else {
        stop("that's weird")
      } 
    } else {
      loess_match <- match_scores
    }
    
    
    
    loess <- Get_loess(scaledat = MDorig,
                       group = groupvec,
                       scoreType = scoreType,
                       match_on = loess_match)
    
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
    
  } else {
    
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
    
    ## Removing items with no within group variance, if they exist
    if(length(nvgi) > 0){
      MeasureData <- MeasureData[-c(nvgi)]
    }
    
    IRT <- Get_IRT(scaledat = MeasureData, group = groupvec) # also drops items with no variance for a particular group
    
  } else{
    
    IRT <- NULL
    
  }
  
  all <- list(loess = loess,
              MH = MH,
              logistic = logistic,
              IRT = IRT,
              Inputs = list(data = MDorig,
                            group = groupvec,
                            scoreType = scoreType,
                            No_Var_Items = nvi,
                            No_Var_by_Group_Items = nvgi))
  
  return(all)
  
}


#### Use information produced from `DIF_analysis` to compare treatment effect estimates ####
# biased.items - vector of items in `MeasureData` to exclude from score calculation; currently only accepts locations, not names
# IRT based deltas will be NaNs if any IRT score is +/- Inf
CompareTreatmentEffects <- function(MeasureData, groupvec,
                                    biased.items, no.var.items = numeric(),
                                    mod_scalar = NULL, IRTmethod = "ML"){
  
  
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
  
  # Removing items with no variance, if they exist
  if(length(no.var.items) > 0){
    MeasureData <- MeasureData[-c(no.var.items)]
  }
  
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
                          `Theta Delta` = c(delta_scalar, delta_bo),
                          `Raw Delta` = c(delta_total, delta_star),
                          `Adj. Delta` = c((delta_total / sqrt(r_total)), (delta_star / sqrt(r_star))),
                          Reliability = c(r_total, r_star),
                          check.names = FALSE)
  
  effects <- list(Effects_Table = effectsdf,
                  Comparison = paste(levels(groupvec)[[2]], "-", levels(groupvec)[[1]]))
  
  
  return(effects)  
  
}

# Options for bias_method are "MH", "logistic", and "IRT"
# IRT_score_method is passed to mirt::fscores
# To request conditional treatment effects, provide DIF_analysis results using the conditional variable to DIF_Results argument,
# and the vector of treatment condition to conditional argument
Get_Report <- function(DIF_Results, Dataset_Name, Measure_Name, Comparison_Name = "Treatment Condition",
                       bias_method = "IRT", IRT_score_method = "WLE",
                       conditional = NULL){
  
  ## Determining whether the bias_method is was used for the DIF analysis
  if(is.null(DIF_Results[[bias_method]])){
    
    stop(paste(bias_method, "DIF analysis was not found. Use the DIF_analysis function with", bias_method, "in the methods argument"))
    
  } else {
    
    BIs <- Biased_Items_by_Method(DIF_Results)[[bias_method]]
    
  }
  
  ## Gathering information for summary report
  n_items <- ncol(DIF_Results$Inputs$data)                    # total items in measure
  n_biased <- length(DIF_Results[[bias_method]]$Biased_Items) # number of items identified as biased
  item_name_range <- paste(names(DIF_Results$Inputs$data)[[1]], "-", names(DIF_Results$Inputs$data)[[n_items]])
  nvi <- ifelse(length(DIF_Results$Inputs$No_Var_Items) !=0,
                paste(names(DIF_Results$Inputs$No_Var_Items), collapse = ", "), "none")     # Names of items with no variance
  nvgi <- ifelse(length(DIF_Results$Inputs$No_Var_by_Group_Items) !=0,
                 paste(names(DIF_Results$Inputs$No_Var_by_Group_Items), collapse = ", "), "none") # names of items with no within group variance
  
  # levels of the comparison variable
  comp1 <- levels(DIF_Results$Inputs$group)[[1]]
  comp2 <- levels(DIF_Results$Inputs$group)[[2]]
  comparison_levs <- paste(comp1, comp2, sep = ", ") 
  
  ## Determining whether any biased items were identified. If not, run report without robustness check
  if(is.character(BIs)){
    
   NoDIFmessage <- paste0(BIs, " by ", bias_method, ". Therefore, a treatment effect robustness check was not performed.")
    
    n_biased <- 0
    bias_type <- "none"
    
    ## Naming output file and running report
    filename <- paste0(Dataset_Name, "-", Measure_Name, " by ", Comparison_Name, ".html")
    rmarkdown::render("Bias_Correction_Report.Rmd",
                      output_file = filename)
    } else {
  
  ## Gather info uniquely for each DIF method
  if(bias_method == "MH"){
    
    bias_type <- "uniform"
    
    MHtemp <- DIF_Results$MH$Item
    
    toward1 <- nrow(MHtemp[MHtemp$Refined_bias == TRUE & MHtemp$Refined_OR < 1, ])
    toward2 <- nrow(MHtemp[MHtemp$Refined_bias == TRUE & MHtemp$Refined_OR > 1, ])
    
  } else if(bias_method == "logistic"){
    
    bias_type <- ifelse(DIF_Results$logistic$Item$dif.type[[1]] == "uni", "uniform",
                        ifelse(DIF_Results$logistic$Item$dif.type[[1]] == "non", "non-uniform", "uniform and non-uniform"))
    
    
  } else if(bias_method == "IRT"){
    
    bias_type <- ifelse(DIF_Results$IRT$Item$Parameter[[1]] == "d", "uniform",
                        ifelse(DIF_Results$IRT$Item$Parameter[[1]] == "a1", "non-uniform", "uniform and non-uniform"))
    
  }
  
  
  
  if(is.null(conditional)){ # For the unconditional effects
    
    UnconditionalEffects <- CompareTreatmentEffects(MeasureData = DIF_Results$Inputs$data,
                                                    groupvec = DIF_Results$Inputs$group,
                                                    biased.items = BIs,
                                                    no.var.items = c(DIF_Results$Inputs$No_Var_Items, DIF_Results$InPuts$No_Var_by_Group_Items),
                                                    mod_scalar = DIF_Results$IRT$Scalar_Mod,  # Automatically pulls from DIF_Results, but could make this an option
                                                    IRTmethod = IRT_score_method)
    
  } else if(length(levels(conditional)) == 2){
    
    # levels of the condition variable (currently intended to be treatment condition)
    cond1 <- levels(conditional)[[1]]
    cond2 <- levels(conditional)[[2]]
    condition_levs <- paste(cond1, cond2, sep = ", ") # combining levels for printing in report
    
    
    ## Treatment effect subset by comp1
    ConditionalEffects1 <- CompareTreatmentEffects(MeasureData = DIF_Results$Inputs$data[DIF_Results$Inputs$group == comp1, ],
                                                   groupvec = conditional[DIF_Results$Inputs$group == comp1],
                                                   biased.items = BIs,
                                                   no.var.items = c(DIF_Results$Inputs$No_Var_Items, DIF_Results$InPuts$No_Var_by_Group_Items),
                                                   mod_scalar = NULL,
                                                   IRTmethod = "WLE")
    
    ## Treatment effect subset by comp2
    ConditionalEffects2 <- CompareTreatmentEffects(MeasureData = DIF_Results$Inputs$data[DIF_Results$Inputs$group == comp2, ],
                                                   groupvec = conditional[DIF_Results$Inputs$group == comp2],
                                                   biased.items = BIs,
                                                   no.var.items = c(DIF_Results$Inputs$No_Var_Items, DIF_Results$InPuts$No_Var_by_Group_Items),
                                                   mod_scalar = NULL,
                                                   IRTmethod = "WLE")

    ## Calculating interaction
    cond1mat <- as.matrix(ConditionalEffects1$Effects_Table[,c(2,3,4)]) # Converting numeric columns in dataframe to matrix
    cond2mat <- as.matrix(ConditionalEffects2$Effects_Table[,c(2,3,4)])
    CondEffDiff <- as.data.frame(cond1mat - cond2mat)                   # Calculating the difference
    InteractionEffects <- cbind(data.frame(Item = ConditionalEffects1$Effects_Table$Items), CondEffDiff) # converting back to dataframe
    
    
  } else {
    stop("conditional must be a factor with two levels")
  }
  
  ## Naming output file and running report
  filename <- paste0(Dataset_Name, "-", Measure_Name, " by ", Comparison_Name, ".html")
  rmarkdown::render("Bias_Correction_Report.Rmd",
                    output_file = filename)
    }
}
