#### Use information produced from `DIF_analysis` to compare treatment effect estimates ####
# biased.items - vector of items in `scale.data` to exclude from score calculation; currently only accepts locations, not names
# IRT based deltas will be NaNs if any IRT score is +/- Inf
get_robustness <- function(scale.data,
                           group,
                           biased.items = NULL,
                           no.var.items = numeric(),
                           nodif.mod = NULL,
                           irt.method = "WLE"){
  
  
  #### Using Total Score ####
  
  total <- sum_score(scale.data)
  delta.total <- smd_wrapper(score = total, group = group, denom = "control")
  r.total <- psy::cronbach(scale.data)$alpha
  
  # Kludge to run without biased items (KN: In Get_Report, this function is only run when there are BIs. So this decision is currently never made)
  if (is.null(biased.items)) {
    star <- total
    delta.star <- delta.total
    r.star <- r.total 
  } else {  
    star <- sum_score(scale.data, drops = biased.items)
    delta.star <- smd_wrapper(score = star, group = group, denom = "control")
    r.star <- psy::cronbach(scale.data[,-c(biased.items)])$alpha
  }  
  
  
  #### Using IRT Score ####
  
  # Removing items with no variance, if they exist
  if(length(no.var.items) > 0){
    scale.data <- scale.data[-c(no.var.items)]
  }
  
  ## IRT model with parameters constrained to be equal between groups
  if(is.null(nodif.mod)){
    
    nodif.mod <- multipleGroup(scale.data, model = 1, group = group,
                               invariance = c('slopes', 'intercepts',
                                              'free_var','free_means'))
    
  }
  ## IRT model with parameter constraints freed for biased.items
  
  if(is.null(biased.items)) { #Kludge to run without biased items
    
    mod.bo <- nodif.mod 
  
    } else {  
      
    mod.bo <- multipleGroup(scale.data, model = 1, group = group,
                            invariance = c(names(scale.data)[-biased.items],
                                           'free_var','free_means'))
  }
  
  ## Calculating IRT scores
  theta.scalar <- fscores(nodif.mod, method = irt.method)
  theta.bo <- fscores(mod.bo, method = irt.method)
  
  ## Calculating standardized mean differences
  delta.scalar <- smd_wrapper(score = theta.scalar, group = group, denom = "control")
  delta.bo <- smd_wrapper(score = theta.bo, group = group, denom = "control")
  
  
  #### Compiling Results ####  
  # effects = data.frame(Measure = c("Raw Delta", "Adj. Delta", "Reliability", "Theta Delta"),
  #                      All_Items = c(delta.total, (delta.total / sqrt(r.total)), r.total, delta_scalar),
  #                      Bias_Omitted = c(delta.star, (delta.star / sqrt(r.star)), r.star, delta_bo))
  
  effectsdf <- data.frame(Items = c("All Items", "Bias Omitted"),
                          `IRT Delta` = c(delta.scalar, delta.bo),
                          `Scale Delta` = c(delta.total, delta.star),
                          `Adj. Delta` = c((delta.total / sqrt(r.total)), (delta.star / sqrt(r.star))),
                          Reliability = c(r.total, r.star),
                          check.names = FALSE)
  
  effects <- list(effects.table = effectsdf,
                  comparison = paste(levels(group)[[2]], "-", levels(group)[[1]]))
  
  
  return(effects)  
  
}


#### Calculate standardized mean difference from two independent or paired groups ####
## SD can be pooled by providing sd1 and sd2
calc_smd <- function(m1, m2, sd1, sd2 = NULL, n1, n2, sample = "ind"){
  
  
  # raw mean difference
  md <- (m2 - m1)
  
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

smd_wrapper <- function(score, group, denom = c("control", "pooled")){
  
  ## levels of the grouping variable
  group1 <- levels(group)[1]
  group2 <- levels(group)[2]
  
  means <- tapply(score, group, mean, na.rm = T)
  sds <- tapply(score, group, sd, na.rm = T)
  
  if(denom == "control"){
    
    delta <- calc_smd(m1 = means[[group1]],
                     m2 = means[[group2]],
                     sd1 = sds[[group1]])
    
  } else if(denom == "pooled"){
    
    ns <- tapply(score, group, length)
    delta <- calc_smd(m1 = means[[group1]],
                     m2 = means[[group2]],
                     sd1 = sds[[group1]],
                     sd2 = sds[[group2]],
                     n1 = ns[[group1]],
                     n2 = ns[[group2]])
    
    
  }
  return(delta)
}