

#####################################
#### Importing Input Information ####

## Packages
library(ggplot2)

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

###################################3

## grouping variable - make factor before putting into function
table(MalawiData$cr_gender, useNA = "always")

mmData <- lapply(MalawiMeasures,
                      function(x){MalawiData[grep(x, names(MalawiData))]})

MeasureData <- mmData$EGMA_number_recognition.Endline



#### Loading and Running analysis functions ####

source("DIF_Methods_Functions.R")
source("Measure_Level_Wrapper.R")

## Test runs
firsttry <- WB_analysis(data = MalawiData, items = MalawiMeasures$EGMA_number_recognition.Endline,
                        groupvar = "cr_gender", scoreType = "Rest", methods = c("loess", "MH"),
                        MHstrata = tenths)

allmeasuretry <- purrr::map(.x = MalawiMeasures, ~WB_analysis(data = MalawiData, items = .x,
                       groupvar = "cr_gender", scoreType = "Rest", methods = c("MH"),
                       MHstrata = tenths))


#### Errors ####
# EGMA_number_recognition.Endline, Item 4 (recog4_3);
# the stage1 MH test produces a statistic, df, and p-value, but no estimate or CI
# table(MalawiData$recog4_3, useNA = "always") shows a 3 in one cell (instead of 0/1)

library(dplyr)
check <- MalawiData[384:403] %>%
  tidyr::gather(Item, Response) %>%
  group_by(Item, Response) %>% summarize(n = n()) %>%
  mutate(r = n())
# recog12_3 has a value of 2 in one cell
# recog15_3 has a value of 9 in one cell


## Function arguments:
# data - a dataframe containing items and grouping variable
# items - character string
# grouping variable - 
# DIF methods - LOESS by group, MH test, Logistic regression, IRT
# Score options - total, rest
# MHstrata - 
  
  
### Alternatives ####
# short <- data[names(data) %in% MalawiMeasures$PPVT.Endline]
# MeasureDatamap <- purrr::map(.x = MalawiMeasures2, ~data[grep(.x, names(data))])

