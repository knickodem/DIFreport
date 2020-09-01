#### Function specifically for cleaning World Bank Data ####
wb_data_prep <- function(data, items, groupvar, condvar = NULL){
  
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
