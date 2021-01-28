# Summary:

# IDELA in bangladesh did not have measurement bias by gender or by windex. For the latter, I think the problem was mainly lack of variability on the windex items. There was very large bias by preschool attendence, but I am not sure if this was actually part of the Tx

#load("/Users/halpin/OneDrive - University of North Carolina at Chapel Hill/UNC_stat_projects/WB_project/Bangladesh/Data/Bangladesh_Recoded.RData")


# MDAT in Malawi
MalawiData <- read.csv("/Users/halpin/OneDrive - University of North Carolina at Chapel Hill/UNC_stat_projects/WB_project/child.tests_items_wide.csv")
names(MalawiData)[[1]] <- "cbcc_id"

## Defining labels for possible grouping variables
MalawiData$cr_gender <- factor(MalawiData$cr_gender, labels = c("Male", "Female"))
MalawiData$treated <- factor(MalawiData$treated, labels = c("Control", "Tx"))

## Correcting data entry errors
MalawiData$recog4_3 <- ifelse(MalawiData$recog4_3 == 3, NA, MalawiData$recog4_3)
MalawiData$recog12_3 <- ifelse(MalawiData$recog12_3 == 2, NA, MalawiData$recog12_3)
MalawiData$recog15_3 <- ifelse(MalawiData$recog15_3 == 9, NA, MalawiData$recog15_3)

## Treatment: compare group 4 to group 0 -- this was the largest effect on MDAT reported by Ozler et a
head(MalawiData)
MalawiData$tx4 <- NA
MalawiData$tx4[MalawiData$lottery_group == 4] <- 1
MalawiData$tx4[MalawiData$lottery_group == 1] <- 0

## Age
table(MalawiData$ageyr_1, useNA = "always")
MalawiData$age <- NA
MalawiData$age[MalawiData$ageyr_1 < 4] <- 0
MalawiData$age[MalawiData$ageyr_1 >= 4] <- 1
table(MalawiData$ageyr_1, MalawiData$age, useNA = "always")


## Items for each measure
MalawiMeasures <- list(MDAT_language.Midline = "^l[0-9]+_2",
                       MDAT_motor.Midline = "fm[0-9]+_2",
                       PPVT.Endline = "ppvt[0-9]+_3",
                       Kaufman_hand_movement.Endline = "hm[0-9]+_3",
                       Kaufman_triangles.Endline = "^t[0-9]+_3",
                       Kaufman_number_recall.Endline = "nr[0-9]+_3",
                       EGMA_number_recognition.Endline = "recog[0-9]+_3",
                       EGMA_quantity_discrimination.Endline = "quant[0-9]+_3",
                       EGMA_addition.Endline = "add[0-9]+_3")

## Using deciles of rest (or total) score for strata (to avoid empty cells in the two-way MH tables)
tenths <- seq(0, 1, by = .1)

wb_measures <- purrr::map(.x = MalawiMeasures,
                          ~dif_prep(measure.data = MalawiData[grep(.x, names(MalawiData))],
                                    tx.group = MalawiData$tx4,
                                    dif.group = MalawiData$age,
                                    clusters = MalawiData$cbcc_id,
                                    na0 = TRUE))