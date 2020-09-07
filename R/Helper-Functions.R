#### Calculate Match Score ####
# scaledat - dataframe of item responses used to calculate the score
# drops - vector of items in `scaledat` to exclude from score calculation; currently only accepts locations, not names
sum_score <- function(scale.data, drops = NULL){
  
  # calculating rest or total score
  if(!is.null(drops)){
    
    score <- apply(scale.data[,-c(drops)], 1, sum)
    
  } else {
    
    score <- apply(scale.data, 1, sum)
  }
  
  return(score)
}



#### Shortcut for extracting the biased items identified by each DIF method ####
extract_biased_items <- function(dif.results){
  
  dr <- dif.results[c("MH", "logistic", "IRT")]
  dr <- dr[!sapply(dr,is.null)]
  
  bi <- lapply(dr, "[[", "biased.items")
  
  
  return(bi)
}

#### Converting dataframe to print-ready table ####
# boldbias can be "no" (default) for no bolding, "item" if an item table, or "global" for a global table
format_flex <- function(df, boldbias = "no", digits = 3){
  
  numericcols <- which(unlist(lapply(df, is.numeric)))
  
  ftab <- flextable(df)
  ftab <- colformat_num(ftab, j = numericcols, digits = digits)
  
  if(boldbias == "item"){
    
    lastcol <- names(df)[[ncol(df)]]
    ftab <- bold(ftab, i = as.formula(paste("~", lastcol,"== TRUE")), part =  "body")
    ftab <- bold(ftab, i = as.formula(paste("~ is.na(", lastcol,")")), part =  "body")
    
  } else if(boldbias == "global"){
    
    ftab <- bold(ftab, i = ~ p < .05, part =  "body")
  }
  
  ftab <- autofit(ftab)
  
  return(ftab)
}