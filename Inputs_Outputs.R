

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

WB_Measures <- purrr::map(.x = MalawiMeasures, ~WB_Data_Prep(data = MalawiData, items = .x,
                                                             groupvar = "treated", condvar = "cr_gender"))

# Uncond_DIF_Total <- purrr::map(.x = WB_Measures, ~DIF_analysis(MeasureData = .x$MeasureData,
#                                                                groupvec = .x$GroupVector,
#                                                                scoreType = "Total",
#                                                                methods = c("loess", "MH", "logistic", "IRT"),
#                                                                MHstrata = tenths))


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


NumberRecog_Gender <- DIF_analysis(MeasureData = NumberRecog$MeasureData,
                                   groupvec = NumberRecog$CondVector,
                                   scoreType = "Total", methods = c("loess", "MH", "IRT"),
                                   MHstrata = tenths)

# Tx <- CompareTreatmentEffects(MeasureData = NumberRecog$MeasureData,
#                               groupvec = NumberRecog$GroupVector,
#                               biased.items = NumberRecog_Tx$IRT$Biased_Items,
#                               mod_scalar = NumberRecog_Tx$IRT$Scalar_Mod,
#                               IRTmethod = "WLE")

#### Generate Report ####

## Unconditional
Get_Report(DIF_Results = NumberRecog_Tx,
           Assessment_Name = "Malawi",
           Measure_Name = gsub("_", " ", gsub("\\.", " at ", "EGMA_number_recognition.Endline")))

## Conditional
Get_Report(DIF_Results = NumberRecog_Gender,
           Assessment_Name = "Malawi",
           Measure_Name = gsub("_", " ", gsub("\\.", " at ", "EGMA_number_recognition.Endline")),
           Comparison_Name = "Gender",
           bias_method = "MH",
           conditional = NumberRecog$GroupVector)



######################################################

#### Other Test Runs ####
# Using Rest scores; deciles for MH
UnconditionalTest <- DIF_analysis(MeasureData = WB_Measures$MDAT_language.Midline$MeasureData,
                                 groupvec = WB_Measures$MDAT_language.Midline$GroupVector,
                                 scoreType = "Rest", methods = c("loess", "MH", "logistic", "IRT"),
                                 MHstrata = tenths)

Get_Report(DIF_Results = UnconditionalTest,
           Assessment_Name = "Malawi",
           Measure_Name = gsub("_", " ", gsub("\\.", " at ", names(WB_Measures)[1])),
           bias_method = "IRT") # returns error that "No DIF was detected by logistic/MH/IRT"

## Get_Report needs an output option when no dif was detected



  

