#####################################################
#                                                   #
#    DIF Methods and Other Specialized functions    #
#                                                   #
#####################################################


# These functions carry out the DIF analysis. Some run at the item level while others are at the measure level
# Also included are other specialized functions, such as calculating the rest or total scores


#### Calculate Match Score ####
# scaledat - dataframe of item responses used to calculate the score
# drops - vector of items in `scaledat` to exclude from score calculation; currently only accepts locations, not names
Get_MatchScore <- function(scaledat, drops = NULL){
  
  # calculating rest or total score
  if(!is.null(drops)){
    
    score <- apply(scaledat[,-c(drops)], 1, sum)
    
  } else {
    
    score <- apply(scaledat, 1, sum)
  }
  
  return(score)
}

#### Calculate standardized mean difference from two independent or paired groups ####
## SD can be pooled by providing sd1 and sd2
Get_smd <- function(m1, m2, sd1, sd2 = NULL, n1, n2, sample = "ind"){
  
  
  # raw mean difference
  md <- (m1 - m2)
  
  # Use only SD from group 1 or an overall SD
  if(is.null(sd2)){
    
    sigma <- sd1
    
  } else {
    
    # sigma for independent groups
    if(sample == "ind"){
      
      sigmanum <- (n1 - 1) * (sd1^2) + (n2 - 1) * (sd2^2)
      sigmadenom <- (n1 + n2 - 2)
      sigma <- sqrt(sigmanum / sigmadenom)
      
    } else{ 
      
      # sigma for paired groups
      sigma <- sqrt((sd1^2 + sd2^2) / 2)
      
    }
  }
  
  # Calculating d
  d <- md / sigma
  
  return(d)
}



########################################
#### A function for each DIF method ####

#### the loess method ####
Run_loess <- function(scaledat, theItem, group, match,
                      pred_scores, n_items){

  # Appending match scores to item data
  scaledat$score <- match
  
  # Model
  mod <- paste0(names(scaledat)[theItem], " ~ score")
  
  # levels of the grouping variable
  group1 <- levels(group)[1] 
  group2 <- levels(group)[2]
  
  # Loess by group
  g1_fit <-  loess(mod, data = subset(scaledat, group == group1))
  g2_fit <-  loess(mod, data = subset(scaledat, group == group2))
  
  # predicted probability by group
  g1_pred <- predict(g1_fit, newdata = pred_scores, se = T)
  g2_pred <- predict(g2_fit, newdata = pred_scores, se = T)
  
  # Storage - one row for each pred_scores x group combination
  loess_data <- data.frame(
    score = rep(pred_scores, times = 2),
    prob = c(g1_pred$fit, g2_pred$fit),
    SE = c(g1_pred$se.fit, g2_pred$se.fit),
    item = rep(names(scaledat)[theItem], times = n_items*2),
    group = rep(c(group1, group2), each = n_items)
  )
  
  return(loess_data)
}

#### The Mantel-Haenszel method with refinement iteration (Stage 2) ####
Run_MH <- function(scaledat, theItem, group, match, strata = NULL){
  

  if(is.null(strata)){ # stratifies on all scores with > 1 value
    
    m <- table(match) - 1                               # frequency (-1) of each score; WHY DO THIS STEP? COULD CHANGE m == 0 to m == 1? ALSO, WE NEED FREQUENCY CONDITIONED BY GROUP
    drop <- !match %in% as.numeric(names(m[m == 0]))    # observations to drop if score frequency is too low

    ## Runs MH test and catches any errors
    mh <- tryCatch(expr = {
      mantelhaen.test(scaledat[drop, theItem], group[drop], match[drop], exact = T)
    },
    error = function(e){
      message(paste("Original error:", e))
      warning("Empty cell(s) in the two-way MH tables. Use strata argument to try different cut thresholds on the stratifying variable.")
      return(list(estimate = NA,
                  conf.int = c(NA, NA),
                  p.value = NA))}
    )
  } else{ # stratifies based on input strata

    stratum <- cut(match, unique(quantile(match, strata, type = 1))) # categorizes observations into strata
    
    ## Runs MH test and catches any errors
    mh <- tryCatch(expr = {
      mantelhaen.test(scaledat[, theItem], group, stratum, exact = T)
    },
    error = function(e){
      message(paste("Original error:", e))
      warning("Empty cell(s) in the two-way MH tables. Use strata argument to try different cut thresholds on the stratifying variable.")
      return(list(estimate = NA,
                  conf.int = c(NA, NA),
                  p.value = NA))}
    )
  }
  
  MHout <- data.frame(item = names(scaledat)[[theItem]],
                      OR = mh$estimate[[1]], 
                      lower = mh$conf.int[[1]],
                      upper = mh$conf.int[[2]], 
                      pvalue = mh$p.value)
    
  return(MHout)
  
}

#### the logistic regression method ####

Run_GlobalLogistic <- function(scaledat, group, match_list){
  
  #### Omnibus test for DIF  ####
  long_data <- data.frame(response = NA, score = NA, item = NA, group = NA)
  
  ## Gather item information into a long format dataframe
  for (i in 1:ncol(scaledat)) {
    
    long_temp <- data.frame(response = scaledat[,i],
                            score = match_list[[i]],
                            item = rep(names(scaledat)[i], nrow(scaledat)),
                            group = as.numeric(group)-1)
    
    long_data <- rbind(long_data, long_temp)
  }
  
  
  long_data <- long_data[-1, ]  # removes first row which is all NA
  long_data$GroupByScore <- long_data$score * long_data$group  # Calculating group by score interaction term
  
  
  ## Baseline model, then adding grouping variable
  mod0 <- glm(response ~ -1 + item + score:item, data = long_data, family = binomial)                # baseline
  mod1 <- glm(response ~ -1 + item + score:item + group:item, data = long_data, family = binomial)   # uniform DIF
  mod2 <- glm(response ~ -1 + item + score:item + group:item + GroupByScore:item, data = long_data, family = binomial) # nonuniform DIF
  
  ## Omnibus test for any DIF 
  anyDIF.test <- anova(mod0, mod1, mod2, test = "LRT")
  anyDIF.test$Model <- c("Baseline", "Uniform", "Non-uniform")
  
  # Possible DIF to test for
  testfor <- c("uni", "non")
  
  # Is there non-uniform DIF?
  if(anyDIF.test$`Pr(>Chi)`[[3]] > .05){
    
    testfor <- testfor[-2] # If not, drop non-uniform indicator
    
  }
  
  # Is there uniform DIF
  if(anyDIF.test$`Pr(>Chi)`[[2]] > .05){
    
    testfor <- tryCatch(expr = testfor[-1],   # If not, then drop uniform indicator
                        error = function(e){
                          return(character()) # if there was no dif, returns an empty vector
                        })
  }
  
  GlobalResults <- list(Model_Comparison = anyDIF.test,
                        DIF_type = testfor,
                        Baseline_Mod = mod0,
                        Uniform_Mod = mod1,
                        Nonuniform_Mod = mod2,
                        Data = long_data)
  
  return(GlobalResults)
  
  
}

#### logistic regression models for individual items ####
Run_ItemLogistic <- function(itemlogdf, which.type){
  
  
  if(length(which.type) == 2){
    
    # Models
    itemmod0 <- glm(response ~ 1 + score, data = itemlogdf, family = binomial)
    # itemmod1 <- glm(response ~ 1 + score + group, data = itemlogdf, family = binomial)               # uniform
    itemmod2 <- glm(response ~ 1 + score + group + GroupByScore, data = itemlogdf, family = binomial) # nonuniform
    
    # Model Comparison
    modcomp <- anova(itemmod0, itemmod2, test = "LRT")
    
    # # Parameter Magnitude
    # slopes <- as.data.frame(summary(itemmod2)$coefficients)
    # slopes <- slopes[row.names(slopes) %in% c("group", "GroupByScore"), ]  # removing intercept and score rows
    
  } else {
    
    
    if("uni" %in% which.type){
      
      # Models
      itemmod0 <- glm(response ~ 1 + score, data = itemlogdf, family = binomial)
      itemmod1 <- glm(response ~ 1 + score + group, data = itemlogdf, family = binomial) # uniform
      
      # Model Comparison
      modcomp <- anova(itemmod0, itemmod1, test = "LRT")
      
      # # Parameter Magnitude
      # slopes <- as.data.frame(summary(itemmod1)$coefficients)
      # slopes <- slopes[row.names(slopes) %in% c("group"), ]  # removing intercept and score rows
    }
    
    if("non" %in% which.type){
      
      # Models
      itemmod1 <- glm(response ~ 1 + score + group, data = itemlogdf, family = binomial) # uniform
      itemmod2 <- glm(response ~ 1 + score + group + GroupByScore, data = itemlogdf, family = binomial) # nonuniform
      
      # Model Comparison
      modcomp <- anova(itemmod1, itemmod2, test = "LRT")
      
      # # Parameter Magnitude
      # slopes <- as.data.frame(summary(itemmod2)$coefficients)
      # slopes <- slopes[row.names(slopes) %in% c("GroupByScore"), ]  # removing intercept and score rows
      
    }
  }
  

  
  modtest <- data.frame(item = itemlogdf$item[[1]],
                        dif.type = ifelse(length(which.type) == 2, "both", which.type),
                        deviance = modcomp$Deviance[-1],
                        df = modcomp$Df[-1],
                        pvalue = modcomp$`Pr(>Chi)`[-1])
  
  # paramtest <- data.frame(item = itemlogdf$item[[1]],
  #                         parameter = row.names(slopes),
  #                         estimate = slopes$Estimate,
  #                         se = slopes$`Std. Error`,
  #                         z = slopes$`z value`,
  #                         pvalue = slopes$`Pr(>|z|)`,
  #                         OR = exp(slopes$Estimate))
  
  # output <- list(Model_Comparison = modtest,
  #                Parameter_Estimates = paramtest)
  
  return(modtest)
  
}

#### the IRT approach ####

Run_GlobalIRT <- function(scaledat, group){
  
  ## Fitting nested models - Fit 2PL models with varying constraints
  mod_configural <- multipleGroup(scaledat, model = 1, group = group, SE = T)
  mod_metric <- multipleGroup(scaledat, model = 1, group = group,
                              invariance = c('slopes', 'free_var'), SE = T)
  mod_scalar <- multipleGroup(scaledat, model = 1, group = group,
                              invariance = c('slopes', 'intercepts', 'free_var','free_means'))
  
  ## Model comparisons
  uniform.tab <- anova(mod_configural, mod_metric, verbose = FALSE) # tests uniform dif
  nonunif.tab <- anova(mod_metric, mod_scalar, verbose = FALSE) # tests non-uniform dif
  

  ## Extracting comparison results
  tab <- rbind(nonunif.tab, uniform.tab[2,])
  tab$model <- c("scalar", "metric", "configural")
  tab <- tab[, -c(1:5)]
  

  ## Do we need to test for DIF at the item-level, and if so, which parameter?
  
  # possible parameters to inspect
  paramswithdif <- c("a1", "d")
  
  # Is there uniform dif?
  if(tab$p[[3]] > .05){  
    
    paramswithdif <- paramswithdif[-2] # If not, drop intercept parameter
    
  }
  
  # Is there non-uniform dif?
  if(tab$p[[2]] > .05){
    
    paramswithdif <- tryCatch(expr = paramswithdif[-1], # If not, then drop slope parameter
                          error = function(e){
                            return(character()) # if there was no dif, returns an empty vector
                            })
  }
  
  
  ## Compiling results to output
  GlobalResults <- list(Model_Comparison = tab,
                        Params_With_DIF = paramswithdif,
                        Scalar_Mod = mod_scalar,
                        Metric_Mod = mod_metric,
                        Config_Mod = mod_configural)
  
  return(GlobalResults)
    
}


## Don't really need Run_ItemIRT as a separate function, but keeping it for now in case we want to include additional capabilities
Run_ItemIRT <- function(GlobalResults, which.model = "Scalar_Mod", items2test){ # might need to add ...
  
  ## Identifying items with DIF
  dif <- DIF(GlobalResults[[which.model]], which.par = GlobalResults$Params_With_DIF,
             items2test = items2test, 
             scheme = "drop", #seq_stat = .05, max_run = 2, 
             Wald = FALSE) # , p.adjust = "BH", ...


return(dif)
}
