

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


#### Temporary Test runs ####
## Preparing Data for DIF_analysis
NumberRecog_Gender <- WB_Data_Prep(data = MalawiData, items = MalawiMeasures$EGMA_number_recognition.Endline, groupvar = "cr_gender")
NumberRecog_Treated <- WB_Data_Prep(data = MalawiData, items = MalawiMeasures$EGMA_number_recognition.Endline, groupvar = "treated")


Motor_Gender <- WB_Data_Prep(data = MalawiData, items = MalawiMeasures$MDAT_motor.Midline, groupvar = "cr_gender")


x_Gender <- WB_Data_Prep(data = MalawiData, items = MalawiMeasures$MDAT_motor.Midline, groupvar = "cr_gender")

Hand_Treated <- WB_Data_Prep(data = MalawiData, items = MalawiMeasures$Kaufman_hand_movement.Endline, groupvar = "treated")

## Gender_Rest
# Total worked too
NumberRecog_Gender_Rest <- DIF_analysis(MeasureData = NumberRecog_Gender$MeasureData,
                                        groupvec = NumberRecog_Gender$GroupVector,
                                        scoreType = "Rest", methods = c("loess", "MH", "logistic", "IRT"),
                                          MHstrata = tenths)


MeasureData = NumberRecog_Gender$MeasureData
groupvec = NumberRecog_Gender$GroupVector
scoreType = "Rest"

## Treated_Total
# Total worked too
NumberRecog_Treated_Total<- DIF_analysis(MeasureData = NumberRecog_Treated$MeasureData,
                                        groupvec = NumberRecog_Treated$GroupVector,
                                        scoreType = "Total", methods = c("loess", "MH", "logistic", "IRT"),
                                        MHstrata = tenths)

NumberRecod_Treated_Total_Effects <- CompareTreatmentEffects(MeasureData = NumberRecog_Treated$MeasureData,
                                                             groupvec = NumberRecog_Treated$GroupVector,
                                                             biased.items = 7)


Hand_Treated_Rest <- DIF_analysis(MeasureData = Hand_Treated$MeasureData,
                                  groupvec = Hand_Treated$GroupVector,
                                  scoreType = "Rest", methods = c("loess", "MH", "logistic", "IRT"),
                                  MHstrata = tenths)



View(Hand_Treated_Rest$MH)




## All measures
library(tictoc)

tic()
AllMeasuresPrepped <- purrr::map(.x = MalawiMeasures,
                                         ~WB_Data_Prep(data = MalawiData, items = .x, groupvar = "cr_gender"))
toc()
## currently breaks at stage2IRTdf[,6:9] has incorrect number of dimensions
tic()
AllMeasures_Gender_Rest <- purrr::map(.x = AllMeasuresPrepped,
                                      ~DIF_analysis(MeasureData = .x$MeasureData, groupvec = .x$GroupVector,
                                                    scoreType = "Rest", methods = c("loess","MH", "logistic", "IRT"),
                                                    MHstrata = tenths))
toc()


freqcheck <- purrr::map(.x = names(AllMeasuresPrepped$Kaufman_hand_movement.Endline$MeasureDat),
                        ~table(AllMeasuresPrepped$Kaufman_hand_movement.Endline$MeasureData[[.x]],
                               AllMeasuresPrepped$Kaufman_hand_movement.Endline$GroupVector, useNA = "ifany"))

#### Generate Report ####

DIF_Results <- NumberRecog_Treated_Total

rmarkdown::render("Bias_Correction_Report.Rmd")



######################################################


  

