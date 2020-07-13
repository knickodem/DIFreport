


#### Importing Input Information ####
## Packages
library(ggplot2)

## Data
MalawiData <- read.csv("child.tests_items_wide.csv")

## Defining labels for possible grouping variables
MalawiData$cr_gender <- factor(MalawiData$cr_gender, labels = c("Male", "Female"))
MalawiData$treated <- factor(MalawiData$treated, labels = c("Control", "Tx"))

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

# Using deciles of rest (or total) score for strata (to avoid empty cells in the two-way MH tables)
tenths <- seq(0, 1, by = .1)

###################################3

# ## grouping variable - make factor before putting into function
# table(MalawiData$cr_gender, useNA = "always")
# 
# mmData <- lapply(MalawiMeasures,
#                       function(x){MalawiData[grep(x, names(MalawiData))]})
# 
# MeasureData <- mmData$MDAT_language.Midline

#### Loading and Running analysis functions ####

source("Measure_Level_Wrapper.R")
source("DIF_Methods_Functions.R")

## Test runs
firsttry <- WB_analysis(data = MalawiData, items = MalawiMeasures$MDAT_language.Midline,
                        groupvar = "cr_gender", scoreType = "Rest", methods = c("loess", "MH"),
                        MHstrata = tenths)

allmeasuretry <- purrr::map(.x = MalawiMeasures, ~WB_analysis(data = MalawiData, items = .x,
                       groupvar = "cr_gender", scoreType = "Rest", methods = c("loess", "MH"),
                       MHstrata = tenths)) # currently breaks because cell count = 0 in a MH test





## Function arguments:
# data - a dataframe containing items and grouping variable
# items - character string
# grouping variable - 
# DIF methods - LOESS by group, MH test, Logistic regression, IRT
# Score options - total, rest
  
  
### Alternatives ####
# MalawiMeasures <- list(MDAT_language.Midline = paste0("l", c(20:48), "_2"),
#                        MDAT_motor.Midline = paste0("fm", c(20:43), "_2"),
#                        PPVT.Endline = paste0("ppvt", c(13:120), "_3"),
#                        Kaufman_hand_movement.Endline = paste0("hm", c(1:23), "_3"),
#                        Kaufman_triangles.Endline = paste0("t", c(4:27), "_3"),
#                        Kaufman_number_recall.Endline = paste0("nr", c(1:22), "_3"),
#                        EGMA_number_recognition.Endline = paste0("recog", c(1:20), "_3"),
#                        EGMA_quantity_discrimination.Endline = paste0("quant", c(1:10), "_3"),
#                        EGMA_addition.Endline = paste0("add", c(1:20), "_3"))
# 
# short <- data[names(data) %in% MalawiMeasures$PPVT.Endline]
# MeasureDatamap <- purrr::map(.x = MalawiMeasures2, ~data[grep(.x, names(data))])

