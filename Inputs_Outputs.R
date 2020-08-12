

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


source("DIF_Methods_Functions.R")
source("DIF_Methods_Wrappers.R")
source("Measure_Level_Wrapper.R")


#### Number Recognition Test Run ####

## Conditional effects
NumberRecog <- WB_Data_Prep(data = MalawiData, items = MalawiMeasures$EGMA_number_recognition.Endline,
                                 groupvar = "treated", condvar = "cr_gender")

NumberRecog_Tx <- DIF_analysis(MeasureData = NumberRecog$MeasureData,
                                  groupvec = NumberRecog$GroupVector,
                                  scoreType = "Total", methods = c("loess", "MH", "logistic", "IRT"),
                                  MHstrata = tenths)

View(NumberRecog_Tx$MH$Item)
View(NumberRecog_Tx$logistic$Item)
View(NumberRecog_Tx$IRT$Item)

NumberRecog_Tx$MH$Biased_Items
NumberRecog_Tx$logistic$Biased_Items
NumberRecog_Tx$IRT$Biased_Items


## What info do we need to pull from DIF_analysis to automate CompareTreatmentEffects and report in the RMD?
# Which items are biased? Use it in CompareTreatmentEffects - now output from DIF_analysis for each method
# type of bias - Logistic and IRT both have indicators
# Direction of uniform bias - can only get from MH


NumberRecog_Gender <- DIF_analysis(MeasureData = NumberRecog$MeasureData,
                                   groupvec = NumberRecog$CondVector,
                                   scoreType = "Total", methods = c("loess", "MH", "logistic", "IRT"),
                                   MHstrata = tenths)

NumberRecog_Gender$loess$plot
NumberRecog_Gender$MH$Biased_Items
NumberRecog_Gender$logistic$Biased_Items
NumberRecog_Gender$IRT$Biased_Items



View(NumberRecog_Gender$MH$Item)
View(NumberRecog_Gender$logistic$Item)


Tx <- CompareTreatmentEffects(MeasureData = NumberRecog$MeasureData,
                              groupvec = NumberRecog$GroupVector,
                              biased.items = NumberRecog_Tx$IRT$Biased_Items,
                              mod_scalar = NumberRecog_Tx$IRT$Scalar_Mod,
                              IRTmethod = "WLE")




Males <- CompareTreatmentEffects(MeasureData = NumberRecog$MeasureData[NumberRecog$CondVector == "Male" ,],
                                 groupvec = NumberRecog$GroupVector[NumberRecog$CondVector == "Male"],
                                 biased.items = NumberRecog_Tx$IRT$Biased_Items,
                                 mod_scalar = NULL,
                                 IRTmethod = "WLE")

Females <- CompareTreatmentEffects(MeasureData = NumberRecog$MeasureData[NumberRecog$CondVector == "Female" ,],
                                 groupvec = NumberRecog$GroupVector[NumberRecog$CondVector == "Female"],
                                 biased.items = NumberRecog_Tx$IRT$Biased_Items,
                                 mod_scalar = NULL,
                                 IRTmethod = "WLE")



#### 

Language <- WB_Data_Prep(data = MalawiData, items = MalawiMeasures$MDAT_language.Midline,
                                        groupvar = "treated", condvar = "cr_gender")

Language_Tx <- DIF_analysis(MeasureData = Language$MeasureData,
                               groupvec = Language$GroupVector,
                               scoreType = "Rest", methods = c("loess", "MH", "logistic", "IRT"),
                               MHstrata = tenths)

Language_Tx$loess$plot
Language_Tx$MH$Biased_Items
Language_Tx$logistic$Item
Language_Tx$IRT$

#### Generate Report ####


rmarkdown::render("Bias_Correction_Report.Rmd")



######################################################


  

