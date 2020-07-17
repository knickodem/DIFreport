
# for each item: calculate rest score, run 4 dif methods

# These functions are expected to run at the item level. Typically a for loop in the Measure_Level_Wrapper initiates these function

########################################
#### A function for each DIF method ####

#### the loess method ####
Run_loess <- function(scaledat, theItem,
                      group, pred_scores, n_items, scoreType){
  
  # calculating rest or total score
  if(scoreType == "Rest"){
    scaledat$score <- apply(scaledat[,-theItem], 1, sum)
  } else {
    scaledat$score <- apply(scaledat, 1, sum)  
  }
  
  # Model
  mod <- paste0(names(scaledat)[theItem], " ~ score")
  
  group1 <- levels(group)[1] 
  group2 <- levels(group)[2]
  
  # Loess by group
  g1_fit <-  loess(mod, subset(scaledat, group == group1))
  g2_fit <-  loess(mod, subset(scaledat, group == group2))
  
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
Run_MH <- function(scaledat, theItem, group, scoreType, strata = NULL, Stage2 = NULL){
  
  ## calculating rest or total score
  if(scoreType == "Rest"){
    scaledat$score <- apply(scaledat[, -c(theItem, Stage2)], 1, sum)
  } else {
    scaledat$score <- apply(scaledat[, -c(Stage2)], 1, sum)  
  }
  
  if(is.null(strata)){ # stratifies on all scores with > 1 value
    
    m <- table(scaledat$score) - 1                               # frequency (-1) of each score; WHY DO THIS STEP? COULD CHANGE m == 0 to m == 1? ALSO, WE NEED FREQUENCY CONDITIONED BY GROUP
    drop <- !scaledat$score %in% as.numeric(names(m[m == 0]))    # observations to drop if score frequency is too low
    mh <- mantelhaen.test(scaledat[drop, theItem], group[drop], scaledat$score[drop], exact = T)
    
    ## Runs MH test and catches any errors
    mh <- tryCatch(expr = {
      mantelhaen.test(scaledat[drop, theItem], group[drop], scaledat$score[drop], exact = T)
    },
    error = function(e){
      message(paste("Original error:", e))
      warning("Empty cell(s) in the two-way MH tables. Use strata argument to try different cut thresholds on the stratifying variable.")
      return(list(estimate = NA,
                  conf.int = c(NA, NA),
                  p.value = NA))}
    )
  } else{ # stratifies based on input strata (currently set for deciles based on WB_analysis function)

    deciles <- cut(scaledat$score, unique(quantile(scaledat$score, strata, type = 1))) # categorizes observations into deciles
    
    ## Runs MH test and catches any errors
    mh <- tryCatch(expr = {
      mantelhaen.test(scaledat[, theItem], group, deciles, exact = T)
    },
    error = function(e){
      message(paste("Original error:", e))
      warning("Empty cell(s) in the two-way MH tables. Use strata argument to try different cut thresholds on the stratifying variable.")
      return(list(estimate = NA,
                  conf.int = c(NA, NA),
                  p.value = NA))}
    )
  }
    
  return(mh)
  
}

#### the logistic regression method ####

Run_logistic <- function(scaledat, theItem, group, scoreType){
  
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
