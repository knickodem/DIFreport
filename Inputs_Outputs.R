

#####################################
#### Importing Input Information ####

## Packages
# Note: these could be placed in more strategic places given that they are not always needed depending on the analysis choices
library(ggplot2)
library(mirt)

## Data
MalawiData <- read.csv("child.tests_items_wide.csv")

## Defining labels for possible grouping variables
MalawiData$cr_gender <- factor(MalawiData$cr_gender, labels = c("Male", "Female"))
MalawiData$treated <- factor(MalawiData$treated, labels = c("Control", "Tx"))

## Correcting data entry errors
MalawiData$recog4_3 <- ifelse(MalawiData$recog4_3 == 3, NA, MalawiData$recog4_3)
MalawiData$recog12_3 <- ifelse(MalawiData$recog4_3 == 2, NA, MalawiData$recog4_3)
MalawiData$recog15_3 <- ifelse(MalawiData$recog4_3 == 9, NA, MalawiData$recog4_3)

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
source("Measure_Level_Wrapper.R")


#### Temporary Test runs ####
## Single measure runs
Number_Recog_Gender_Rest <- WB_analysis(data = MalawiData, items = MalawiMeasures$EGMA_number_recognition.Endline,
                                          groupvar = "cr_gender", scoreType = "Rest", methods = c("loess", "MH", "logistic", "IRT"),
                                          MHstrata = tenths)


Number_Recog_Gender_Total <- WB_analysis(data = MalawiData, items = MalawiMeasures$EGMA_number_recognition.Endline,
                        groupvar = "cr_gender", scoreType = "Total", methods = c("loess", "MH", "logistic", "IRT"),
                        MHstrata = tenths)


## All measures
library(tictoc)
tic()
allmeasuretry_treated_rest <- purrr::map(.x = MalawiMeasures, ~WB_analysis(data = MalawiData, items = .x,
                       groupvar = "cr_gender", scoreType = "Rest", methods = c("loess","MH", "logistic", "IRT"),
                       MHstrata = tenths))
toc()




#### Generate Report ####

rmarkdown::render("Bias_Correction_Report.Rmd")



######################################################

###############################
#### Temporary for Testing ####

## grouping variable - make factor before putting into function
table(MalawiData$cr_gender, useNA = "always")

mmData <- lapply(MalawiMeasures,
                 function(x){MalawiData[grep(x, names(MalawiData))]})

MeasureData <- mmData$EGMA_number_recognition.Endline

#################################

  

