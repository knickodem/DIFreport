# ----------------- Mantel-Haenszel -------------------------

get_mh <- function(scale.data, group, score.type,
                   match.on, strata = NULL){
  
  ## Number of items in the measure
  nitems <- ncol(scale.data)
  
  #### MH testing stage 1 - initial DIF items ####
  
  ## Using rest scores as the matching criterion
  if(score.type == "Rest"){
    
    # Storage
    stage1 <- list()
    
    # Loop over items
    for(i in 1:nitems){
      
      stage1[[i]] <- run_mh(scale.data = scale.data,
                            item = i, 
                            group = group,
                            match = match.on[[i]],
                            strata = strata)
    }
    
  } else if(score.type == "Total"){
    
    stage1 <- lapply(1:nitems, run_mh,
                     scale.data = scale.data, 
                     group = group,
                     match = match.on,
                     strata = strata)
  }
  
  # Converting list elements into single dataframe
  mh1 <- Reduce(rbind, stage1)
  
  # Benjamini–Hochberg procedure for false discovery rate < 5%
  mh1$bias <- p.adjust(mh1$p, method = "BH") < .05
  
  #### mh testing stage 2 - Refinement/purification of match_score criterion ####
  # The items to exclude based on initial mh DIF analysis
  mh_drops <- which(mh1$bias == 1)
  
  if(length(mh_drops) > 0){
    
    if(score.type == "Rest"){
      
      # Storage
      stage2 <- list()
      
      # Loop over items
      for(i in 1:nitems) {
        
        # Recalculate rest score while also removing biased items identified in stage 1
        match.on2 <- sum_score(scale.data = scale.data, drops = c(i, mh_drops))
        
        stage2[[i]] <- run_mh(scale.data = scale.data,
                              item = i, 
                              group = group,
                              match = match.on2,
                              strata = strata)
        
      }
      
    } else if(score.type == "Total") {
      
      # Recalculate rest score while also removing biased items identified in stage 1
      match.on2 <- sum_score(scale.data = scale.data, drops = c(mh_drops))
      
      stage2 <- lapply(1:nitems, run_mh,
                       scale.data = scale.data, 
                       group = group,
                       match = match.on2,
                       strata = strata)
      
    }
    
    # Converting list elements into single dataframe
    mh2 <- Reduce(rbind, stage2)
    
    # Benjamini–Hochberg procedure for false discovery rate = 5%
    mh2$bias <- p.adjust(mh2$p, method = "BH") < .05
    
    # Output dataframe combining stage 1 and stage 2
    names(mh1)[-1] <- paste0("Initial_", names(mh1)[-1])
    names(mh2)[-1] <- paste0("Refined_", names(mh2)[-1])
    
    mh <- list(item.results = cbind(mh1, mh2[,-1]),
               biased.items = which(mh2$Refined_bias == 1))   # Special use cases might throw errors here
    
  } else {
    
    #names(mh1)[-1] <- paste0("Initial_", names(mh1)[-1])
    mh <- list(item.results = mh1,
               biased.items = "No DIF was detected")
  }
  
  return(mh)
}


#### The Mantel-Haenszel method ####
run_mh <- function(scale.data, item, group, match, strata = NULL){
  
  
  if(is.null(strata)){ # stratifies on all scores with > 1 value
    
    m <- table(match) - 1                               # frequency (-1) of each score; WHY DO THIS STEP? COULD CHANGE m == 0 to m == 1? 
    drop <- !match %in% as.numeric(names(m[m == 0]))    # observations to drop if score frequency is too low
    
    ## Runs mh test and catches any errors
    mh <- tryCatch(expr = {
      mantelhaen.test(x = scale.data[drop, item],
                      y = group[drop],
                      z = match[drop],
                      exact = T)
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
    
    ## Runs mh test and catches any errors
    mh <- tryCatch(expr = {
      mantelhaen.test(x = scale.data[, item],
                      y = group,
                      z = stratum,
                      exact = T)
    },
    error = function(e){
      message(paste("Original error:", e))
      warning("Empty cell(s) in the two-way MH tables. Use strata argument to try different cut thresholds on the stratifying variable.")
      return(list(estimate = NA,
                  conf.int = c(NA, NA),
                  p.value = NA))}
    )
  }
  
  mh.out <- data.frame(item = names(scale.data)[[item]],
                      OR = mh$estimate[[1]],       # > 1 if item favors group2, < 1 if item favors group1
                      lower = mh$conf.int[[1]],
                      upper = mh$conf.int[[2]],
                      p = mh$p.value)
  
  return(mh.out)
  
}