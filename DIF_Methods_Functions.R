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

OmnibusLogistic <- function(){
  
  
}

Run_logisticByItem <- function(itemlogdf, theItem){
  

  ### logistic regression models for individual items ---
  
  itemmod0 <- glm(response ~ 1 + score, data = itemlogdf, family = binomial)
  itemmod1 <- glm(response ~ 1 + score + group, data = itemlogdf, family = binomial)
  itemmod2 <- glm(response ~ 1 + score + group + interaction, data = log_data, family = binomial)
  
  # Overall test of uniform or nonuniform DIF
  tab1 <- anova(mod0, mod2, test = "LRT")
  modtest <- data.frame(item = names(scaledat)[theItem],
                        deviance = tab1$Deviance[[2]],
                        pvalue = tab1$`Pr(>Chi)`[[2]])
  
  slopes <- as.data.frame(summary(mod2)$coefficients)                   # extracting slope parameters
  slopes <- slopes[row.names(slopes) %in% c("group", "interaction"), ]  # removing intercept and score rows
  slopes$OR <- exp(slopes$Estimate)                                     # Computing odds ratios
  slopes <- tibble::rownames_to_column(slopes, "Type")                  # adding rownames to the df
  slopes$item <- names(scaledat)[theItem]                               # adding item identifier
  slopes <- slopes[,c(7,1:6)]                                           # and moving it to the first column
  names(slopes) <- c("item", "Type", "Estimate", "SE", "z", "pvalue", "OR")
  
  output <- list(modtest = modtest,
                 slopes = slopes)
  
  return(output)
  
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
  
  if(tab$p[[3]] > .05){  # Was there uniform dif?
    
    paramswithdif <- paramswithdif[-2] # If not, drop intercept parameter
    
  }
  
  if(tab$p[[2]] > .05){ # Was there non-uniform dif?
    
    paramswithdif <- tryCatch(expr = paramswithdif[-1],
                          error = function(e){return(FALSE)})
  }
  
  
  ## Compiling results to output
  GlobalResults <- list(Model_Comparison = tab,
                        Params_With_DIF = paramswithdif,
                        Scalar_Mod = mod_scalar,
                        Metric_Mod = mod_metric,
                        Config_Mod = mod_configural)
  
  return(GlobalResults)
    
}



Run_ItemIRT <- function(GlobalResults, which.model = "Scalar_Mod", items2test){ # might need to add ...
  
  ## Identifying items with DIF
  # 
  dif <- DIF(GlobalResults[[which.model]], which.par = GlobalResults$Params_With_DIF,
             items2test = items2test, 
             scheme = "drop", #seq_stat = .05, max_run = 2, 
             Wald = FALSE) # , p.adjust = "BH", ...
  # dif$bias <- dif[, 4] < .05
  # dif <- dif[, -3]
  

# 
# difout <- list(modtest = tab,
#                itemdif = dif)

return(dif)
}
