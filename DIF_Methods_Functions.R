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

Run_logisticByItem <- function(scaledat, theItem, group, scoreType){
  
  # calculating rest or total score
  if(scoreType == "Rest"){
    
    scaledat$score <- apply(scaledat[,-theItem], 1, sum)
    
  } else {
    
    scaledat$score <- apply(scaledat, 1, sum)  
  }
  
  # Compiling the dataframe for use in the logistic regression models
  log_data <- data.frame(response = scaledat[,theItem],           # 1/0 response to the item
                          score = scaledat$score,           # rest or total score calculated above
                          group = as.numeric(group)-1       # converting group from factor to 1/0 dummy variable
  )
  
  log_data$interaction <- log_data$score * log_data$group  # calculating the group x score interaction term
  
  
  ### logistic regression models ---
  
  mod0 <- glm(response ~ 1 + score, data = log_data, family = binomial)
  # mod1 <- glm(response ~ 1 + score + group, data = log_data, family = binomial)
  mod2 <- glm(response ~ 1 + score + group + interaction, data = log_data, family = binomial)
  
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

Run_IRT <- function(scaledat, group){
  
  # Nested models
  mod_configural <- multipleGroup(scaledat, model = 1, group = group, SE = T) # assumes 2PL model
  mod_metric <- multipleGroup(scaledat, model = 1, group = group,
                              invariance = c('slopes', 'free_var'), SE = T)
  
  # Model comparison
  tab1 <- anova(mod_configural, mod_metric)
  
  
  mod_scalar <- multipleGroup(scaledat, model = 1, group = group,
                              invariance = c('slopes', 'intercepts', 'free_var','free_means'))
  # Model comparison
  tab2 <- anova(mod_metric, mod_scalar) # If non-sig, no bias; otherwise uniform dif
  
  tab <- rbind(tab2, tab1[2,])
  tab$model <- c("scalar", "metric", "configural")
  tab <- tab[, -c(1:5)]
  
  # Finding Diffy items
  # Assume metric (no mean dif between groups), test equality constraints per item -- need to sort out if this is really a good idea...
  dif <- DIF(mod_configural, which.par = c('a1', 'd'), scheme = "add", Wald = T, p.adjust = "BH")
  dif$bias <- dif[, 4] < .05
  dif <- dif[, -3]
  
  difout <- list(modtest = tab,
                 itemdif = dif)
  
  return(difout)
}
