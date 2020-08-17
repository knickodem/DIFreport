

#####################################
#### Importing Input Information ####

## Data
MalawiData <- read.csv("child.tests_items_wide.csv")

## Defining labels for possible grouping variables
MalawiData$cr_gender <- factor(MalawiData$cr_gender, labels = c("Male", "Female"))
MalawiData$treated <- factor(MalawiData$treated, labels = c("Control", "Tx"))

## Correcting data entry errors
MalawiData$recog4_3 <- ifelse(MalawiData$recog4_3 == 3, NA, MalawiData$recog4_3)
MalawiData$recog12_3 <- ifelse(MalawiData$recog12_3 == 2, NA, MalawiData$recog12_3)
MalawiData$recog15_3 <- ifelse(MalawiData$recog15_3 == 9, NA, MalawiData$recog15_3)

## Items for each measure
MalawiMeasures <- list(MDAT_language.Midline = "l[0-9]+_2",
                       MDAT_motor.Midline = "fm[0-9]+_2",
                       PPVT.Endline = "ppvt[0-9]+_3",
                       Kaufman_hand_movement.Endline = "hm[0-9]+_3",
                       Kaufman_triangles.Endline = "t[0-9]+_3",
                       Kaufman_number_recall.Endline = "nr[0-9]+_3",
                       EGMA_number_recognition.Endline = "recog[0-9]+_3",
                       EGMA_quantity_discrimination.Endline = "quant[0-9]+_3",
                       EGMA_addition.Endline = "add[0-9]+_3")

## Using deciles of rest (or total) score for strata (to avoid empty cells in the two-way MH tables)
tenths <- seq(0, 1, by = .1)

####################################################


################################################
#### Running analysis and Generating Report ####


#### Importing Functions ####
source("DIF_Methods_Functions.R")
source("DIF_Methods_Wrappers.R")
source("Measure_Level_Wrapper.R")


#### Number Recognition Test Run ####

## Formatting Data
# DIF by Treatment condition; Conditioning effects by gender
NumberRecog <- WB_Data_Prep(data = MalawiData, items = MalawiMeasures$EGMA_number_recognition.Endline,
                                 groupvar = "treated", condvar = "cr_gender")


## Detecting DIF by Treatment condition
# Using Total scores; deciles for MH
NumberRecog_Tx <- DIF_analysis(MeasureData = NumberRecog$MeasureData,
                                  groupvec = NumberRecog$GroupVector,
                                  scoreType = "Total", methods = c("loess", "MH", "logistic", "IRT"),
                                  MHstrata = tenths)

# View(NumberRecog_Tx$MH$Item)
# View(NumberRecog_Tx$logistic$Item)
# View(NumberRecog_Tx$IRT$Item)
# 
# NumberRecog_Tx$MH$Biased_Items
# NumberRecog_Tx$logistic$Biased_Items
# NumberRecog_Tx$IRT$Biased_Items


test <- DIF_analysis(MeasureData = NumberRecog$MeasureData,
                                   groupvec = NumberRecog$CondVector,
                                   scoreType = "Total", methods = c("MH", "IRT"),
                                   MHstrata = tenths)


Tx <- CompareTreatmentEffects(MeasureData = NumberRecog$MeasureData,
                              groupvec = NumberRecog$GroupVector,
                              biased.items = NumberRecog_Tx$IRT$Biased_Items,
                              mod_scalar = NumberRecog_Tx$IRT$Scalar_Mod,
                              IRTmethod = "WLE")


# Gender <- CompareTreatmentEffects(MeasureData = NumberRecog$MeasureData,
#                                   groupvec = NumberRecog$CondVector,
#                                   biased.items = NumberRecog_Tx$IRT$Biased_Items,
#                                   mod_scalar = NumberRecog_Tx$IRT$Scalar_Mod,
#                                   IRTmethod = "WLE")





#### Generate Report ####

## What info do we need to pull from DIF_analysis to automate CompareTreatmentEffects and report in the RMD?
# Which items are biased? Use it in CompareTreatmentEffects - now output from DIF_analysis for each method
# type of bias - Logistic and IRT both have indicators
# Direction of uniform bias - can only get from MH

Measure_Name <- "Malawi"
Assessment_Name <- gsub("_", " ", gsub("\\.", " at ", "EGMA_number_recognition.Endline"))
n_items <- ncol(NumberRecog$MeasureData)
item_name_range <- paste(names(NumberRecog$MeasureData)[[1]], "-", names(NumberRecog$MeasureData)[[n_items]])
bias_method <- "IRT"
n_biased <- length(NumberRecog_Tx$IRT$Biased_Items)
bias_type <- ifelse(NumberRecog_Tx$IRT$Item$Parameter[[1]] == "d", "uniform",
                    ifelse(NumberRecog_Tx$IRT$Item$Parameter[[1]] == "a1", "non-uniform", "uniform and non-uniform"))


grouping_var <- "Tx Condition"
grouping_levs <- paste(levels(NumberRecog$GroupVector), collapse = ", ")

conditioning_var <- "Gender"
conditioning_levs <- paste(levels(NumberRecog$CondVector), collapse = ", ")

DIF_Results <- NumberRecog_Tx

UnconditionalEffects <- Tx
ConditionalEffects1 <- Males
ConditionalEffects2 <- Females




nrbi <- Biased_Items_by_Method(NumberRecog_Tx)[["MH"]]

class(nrbi)


Get_Report(NumberRecog_Tx, Measure_Name = "Malawi", Assessment_Name = gsub("_", " ", gsub("\\.", " at ", "EGMA_number_recognition.Endline")))


# Options for bias_method are "MH", "logistic", and "IRT"
# IRT_score_method is passed to mirt::fscores
Get_Report <- function(DIF_Results, Measure_Name, Assessment_Name, Comparison_Name = "Treatment Condition",
                       bias_method = "IRT", IRT_score_method = "WLE",
                       conditional = NULL){
  
  ## Determining if any DIF items were detected
  if(is.null(DIF_Results[[bias_method]])){
    
    stop(paste(bias_method, "DIF analysis was not found. Use the DIF_analysis function with", bias_method, "in the methods argument"))
  
    } else{
    
    BIs <- Biased_Items_by_Method(DIF_Results)[[bias_method]]
    
    if(is.character(BIs)){
      stop(paste(BIs, "by", bias_method))}
  }
  
  
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
  
  
  n_items <- ncol(DIF_Results$Inputs$data)                    # total items in measure
  n_biased <- length(DIF_Results[[bias_method]]$Biased_Items) # number of items identified as biased
  item_name_range <- paste(names(DIF_Results$Inputs$data)[[1]], "-", names(DIF_Results$Inputs$data)[[n_items]])
  
  grouping_levs <- paste(levels(DIF_Results$group), collapse = ", ") # levels of the grouping variable
  
  if(is.null(conditional)){
    
    UnconditionalEffects <- CompareTreatmentEffects(MeasureData = DIF_Results$Inputs$data,
                                                    groupvec = DIF_Results$Inputs$group,
                                                    biased.items = BIs,
                                                    mod_scalar = DIF_Results$IRT$Scalar_Mod,  # Automatically pulls from DIF_Results, but could make this an option
                                                    IRTmethod = IRT_score_method)
    
  } else if(length(levels(conditional)) == 2){  # Need to incorporate a check that DIF_Results also involves the conditioning var
    
    ## levels of the conditioning variable
    cond1 <- levels(conditional)[1] 
    cond2 <- levels(conditional)[2]
    
    grouping_levs <- paste(cond1, cond2, sep = ", ") # combinging levels for printing in the report
    
    ConditionalEffects1 <- CompareTreatmentEffects(MeasureData = NumberRecog$MeasureData[NumberRecog$CondVector == "Male" ,],
                                     groupvec = NumberRecog$GroupVector[NumberRecog$CondVector == "Male"],
                                     biased.items = NumberRecog_Tx$IRT$Biased_Items,
                                     mod_scalar = NULL,
                                     IRTmethod = "WLE")
    
    ConditionalEffects2 <- CompareTreatmentEffects(MeasureData = NumberRecog$MeasureData[NumberRecog$CondVector == "Female" ,],
                                       groupvec = NumberRecog$GroupVector[NumberRecog$CondVector == "Female"],
                                       biased.items = NumberRecog_Tx$IRT$Biased_Items,
                                       mod_scalar = NULL,
                                       IRTmethod = "WLE")
    
  } else {
    stop("conditional must be a factor with two levels")
  }
  
  
  rmarkdown::render("Sample Report.Rmd")
}






######################################################


  

