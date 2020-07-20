

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
source("Measure_Level_Wrapper.R")


#### Temporary Test runs ####
NumberRecog <- WB_Data_Prep(data = MalawiData, items = MalawiMeasures$EGMA_number_recognition.Endline, groupvar = "cr_gender")

## Single measure runs
NumberRecog_Gender_Rest <- DIF_analysis(MeasureData = NumberRecog$MeasureData, groupvec = NumberRecog$GroupVector,
                                        scoreType = "Rest", methods = c("loess", "MH"),
                                          MHstrata = tenths)


Number_Recog_Gender_Total <- WB_analysis(data = MalawiData, items = MalawiMeasures$EGMA_number_recognition.Endline,
                        groupvar = "cr_gender", scoreType = "Total", methods = c("loess"),
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
data = MalawiData
items = MalawiMeasures$EGMA_number_recognition.Endline
groupvar = "cr_gender"
scoreType = "Total"

loopfunc <- function(x){
  
  gg_data <- list(); for(i in 1:n_items){gg_data[[i]] <- Run_loess(scaledat = x, theItem = i, group, pred_scores, n_items, scoreType)}
}

mb <- microbenchmark(
  lapply = lapply(c(1:n_items), Run_loess, scaledat = MeasureData, group = group, pred_scores = pred_scores, n_items = n_items, scoreType = scoreType),
  map = purrr::map(1:n_items, ~Run_loess(scaledat = MeasureData, theItem = .x, group, pred_scores, n_items, scoreType)),
  loop = loopfunc(MeasureData),
  times = 100)

checkred <- Reduce(rbind, check)
mapred <- Reduce(rbind, mapcheck)

all.equal(checkred, mapred)
#################################

  

