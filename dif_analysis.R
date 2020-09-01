dif_analysis <- function(measure.data,
                         group,
                         score.type = c("Rest", "Total"),
                         methods = c("loess", "MH", "logistic", "IRT"),   # could incorporate a shortcut for "all"
                         strata = NULL){ 
  
  #### Identifying problematic items ####
  md.orig <- measure.data # saving original data with all items for use in loess
  
  ## Items with no variance
  var.check <- lapply(measure.data, var)
  nvi <- which(var.check == 0)
  
  ## Items with no variance for one group (think about other ways to do this)
  var.check.group <- lapply(measure.data, function(x) { sum(table(x, group) == 0) })
  nvgi <- which(var.check.group > 0)
  
  ## Removing items with no variance from the data, if they exist
  if(length(nvi) > 0){
    measure.data <- measure.data[-c(nvi)]
  }
  
  ## Number of items in the measure for dif analysis
  n.items <- ncol(measure.data)
  
  ## Calculating vector of total scores or list of rest scores
  if(score.type == "Rest"){ # Returns a list with n_items elements of length = nrow(measure.data)
    
    match.scores <- lapply(c(1:n.items), sum_score, scale.data = measure.data) 
    
  } else if(score.type == "Total"){ # a single vector of length = nrow(measure.data)
    
    match.scores <- sum_score(scale.data = measure.data, drops = NULL)
    
  } else {
    stop("score.type argument must be 'Rest' or 'Total'")
  }
  
  
  #### LOESS ####
  if("loess" %in% methods){
    
    
    if(length(nvi) > 0){
      
      ## Calculating vector of total scores or list of rest scores
      if(score.type == "Rest"){ # Returns a list with n_items elements of length = nrow(measure.data)
        
        loess.match <- lapply(c(1:ncol(md.orig)), sum_score, scale.data = md.orig) 
        
      } else if(score.type == "Total"){ # a single vector of length = nrow(measure.data)
        
        loess.match <- sum_score(scale.data = md.orig, drops = NULL)
        
      }  else {
        stop("that's weird")
      } 
    } else {
      loess.match <- match.scores
    }
    
    
    
    loess <- get_loess(scale.data = md.orig,
                       group = group,
                       score.type = score.type,
                       match.on = loess.match)
    
  } else{
    
    loess <- NULL
    
  }
  
  #### Mantel-Haenszel ####
  if("MH" %in% methods){
    
    mh <- get_mh(scale.data = measure.data,
                 group = group,
                 score.type = score.type,
                 match.on = match.scores,
                 strata = strata) 
    
  } else {
    
    mh <- NULL
    
  }
  
  #### Logistic Regression ####
  if("logistic" %in% methods){
    
    
    
    logistic <- get_Logistic(scale.data = measure.data,
                             group = group,
                             score.type = score.type,
                             match.on = match.scores)
    
    
  } else{
    
    logistic <- NULL
    
  }
  
  
  #### Item Response Theory ####
  if("IRT" %in% methods){
    
    ## Removing items with no within group variance, if they exist
    if(length(nvgi) > 0){
      measure.data <- measure.data[-c(nvgi)]
    }
    
    irt <- get_IRT(scale.data = measure.data, group = group) # also drops items with no variance for a particular group
    
  } else{
    
    irt <- NULL
    
  }
  
  all <- list(loess = loess,
              mh = mh,
              logistic = logistic,
              irt = irt,
              inputs = list(data = md.orig,
                            group = group,
                            score.type = score.type,
                            no.var.items = nvi,
                            no.var.by.group.items = nvgi))
  
  return(all)
  
}
