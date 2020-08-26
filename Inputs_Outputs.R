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


#### Preparing Malawi Data ####

WB_Measures <- purrr::map(.x = MalawiMeasures, 
                          ~WB_Data_Prep(data = MalawiData,
                                        items = .x,
                                        groupvar = "treated",   # Treamtent condition as grouping variable
                                        condvar = "cr_gender")) # Gender as conditioning variable


#### Test Runs ####
## Using Rest scores; deciles for MH
# Unconditional Example
Unconditional1 <- DIF_analysis(MeasureData = WB_Measures[[1]]$MeasureData,
                               groupvec = WB_Measures[[1]]$GroupVector,     # For unconditional, use vector for treatment condition
                               scoreType = "Rest",
                               methods = c("loess", "MH", "logistic", "IRT"),
                               MHstrata = tenths)

Get_Report(DIF_Results = Unconditional1,
           Dataset_Name = "Malawi",
           Measure_Name = gsub("_", " ", gsub("\\.", " at ", names(WB_Measures)[1])),
           bias_method = "IRT",
           conditional = NULL) # the default


# Conditional Example
Conditional7 <- DIF_analysis(MeasureData = WB_Measures[[7]]$MeasureData,
                             groupvec = WB_Measures[[7]]$CondVector,        # for conditional, use vector for conditioning variable (e.g., Gender)
                             scoreType = "Rest",
                             methods = c("loess", "MH", "logistic", "IRT"),
                             MHstrata = tenths)

Get_Report(DIF_Results = Conditional7,
           Dataset_Name = "Malawi",
           Measure_Name = gsub("_", " ", gsub("\\.", " at ", names(WB_Measures)[7])),
           bias_method = "IRT",
           conditional = WB_Measures[[7]]$GroupVector) # use the treatment condition vector here


# Run all 

for (i in 1:length(MalawiMeasures)){
  
  Unconditional <- DIF_analysis(MeasureData = WB_Measures[[i]]$MeasureData,
                                 groupvec = WB_Measures[[i]]$GroupVector,     # For unconditional, use vector for treatment condition
                                 scoreType = "Rest",
                                 methods = c("loess", "MH", "logistic", "IRT"),
                                 MHstrata = tenths)
  
  Get_Report(DIF_Results = Unconditional,
             Dataset_Name = "Malawi",
             Measure_Name = gsub("_", " ", gsub("\\.", " at ", names(WB_Measures)[1])),
             bias_method = "IRT",
             conditional = NULL) # the default
  
  Conditional <- DIF_analysis(MeasureData = WB_Measures[[i]]$MeasureData,
                               groupvec = WB_Measures[[i]]$CondVector,        # for conditional, use vector for conditioning variable (e.g., Gender)
                               scoreType = "Rest",
                               methods = c("loess", "MH", "logistic", "IRT"),
                               MHstrata = tenths)
  
  Get_Report(DIF_Results = Conditional,
             Dataset_Name = "Malawi",
             Measure_Name = gsub("_", " ", gsub("\\.", " at ", names(WB_Measures)[7])),
             bias_method = "IRT",
             conditional = WB_Measures[[7]]$GroupVector) # use the treatment condition vector here
    
}

