# Summary:

# MDAT in Malawi: there is not significant gender or age based DIF

#MalawiData <- read.csv("/Users/halpin/OneDrive - University of North Carolina at Chapel Hill/UNC_stat_projects/WB_project/child.tests_items_wide.csv")

# IDELA in Bangladesh: did not have measurement bias by gender or by windex. For the latter, I think the problem was mainly lack of variability on the windex items. There was very large bias by preschool attendence, but I am not sure if this was actually part of the Tx
# PH edits 01/26/2021: Summary, no DIF by windex; big DIF by Preshool but this is part of the Tx...

# Bangladesh analyses
load("/Users/halpin/OneDrive - University of North Carolina at Chapel Hill/UNC_stat_projects/WB_project/Bangladesh/Data/Bangladesh_Recoded.RData")

# windex variable
 temp <- haven::read_spss("/Users/halpin/OneDrive - University of North Carolina at Chapel Hill/UNC_stat_projects/WB_project/Bangladesh/Data/Baseline Data/Household.sav")
 head(temp)

windex_names <- c("P1_10", "P3_3", "P3_4", "P3_5", "P3_6", "P4_3")
windex_vars <- temp[windex_names]
names(windex_vars) <- c("maternal_ed","electicity", "roof", "walls", "toilet", "food")

windex_vars$maternal_ed[windex_vars$maternal_ed == 99] <- NA
windex_vars$maternal_ed[windex_vars$maternal_ed < 2] <- 0
windex_vars$maternal_ed[windex_vars$maternal_ed >= 2] <- 1

windex_vars$roof[windex_vars$roof == 6] <- NA
windex_vars$roof[windex_vars$roof < 4] <- 0
windex_vars$roof[windex_vars$roof >= 4] <- 1

windex_vars$walls[windex_vars$walls == 6] <- NA
windex_vars$walls[windex_vars$walls < 4] <- 0
windex_vars$walls[windex_vars$walls >= 4] <- 1

windex_vars$toilet[windex_vars$toilet == 6] <- NA
windex_vars$toilet[windex_vars$toilet > 1] <- 0

windex_vars$food[windex_vars$food < 4] <- 0
windex_vars$food[windex_vars$food == 4] <- 1
windex_vars <- as.data.frame((windex_vars))

windex_vars$windex <- apply(windex_vars, 1, mean, na.rm = T)
windex_vars$windex_cut <- as.factor((windex_vars$windex < .5)*1)

table(windex_vars$windex)

# merge
windex_vars$sid <- temp$sid
#
midline <- bang.recode[bang.recode$line == "Mid",]
midline <- merge(midline, windex_vars, by.x = "sid", by.y = "sid", all.x = T)
midline <- as.data.frame(midline)
head(midline)
midline$maternal_ed <- droplevels(as.factor(midline$maternal_ed))

mid.prep <- lapply(domain.items,
                   function(x) {dif_prep(measure.data = midline[, x],
                                         dif.group = midline$windex_cut,
                                         tx.group = midline$tx,
                                         na0 = FALSE)})

names(mid.prep) <- names(domain.items)

for(j in 1:3) {
  i <- names(mid.prep)[3]
  mid.con <- dif_analysis(measure.data = mid.prep[[i]]$measure.data,
                          dif.group = mid.prep[[i]]$dif.group,
                          methods = c("loess", "IRT"),
                          score.type = "Rest")

  dif_report(dif.analysis = mid.con,
             file.name = paste(gsub("_", " ", i), "Midline", sep = " at "),
             dataset.name = "Bangladesh",
             measure.name = paste(gsub("_", " ", i), "Midline", sep = " at "),
             dif.group.name = "Preschool",
             bias.method = "IRT",
             irt.scoring = "WLE",
             tx.group = mid.prep[[i]]$tx.group)
}





# Preschool variable (but it is post tx)
#temp <- haven::read_spss("/Users/halpin/OneDrive - University of North Carolina at Chapel Hill/UNC_stat_projects/WB_project/Bangladesh/Data/Midline Data/Midline_IDELA.sav")
#
# temp <- merge(midline, temp, by.x = "sid", by.y = "SID", all.x = T)
# midline$PreschoolType <- temp$PreschoolType
# midline$preschool <- midline$PreschoolType
# midline$preschool[midline$PreschoolType < 6] <- 1
# midline$preschool[midline$PreschoolType == 6] <- 0
# names(midline)
# table(midline$tx, midline$PreschoolType)
# table(midline$preschool, midline$PreschoolType)








# Malawi analyses
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
                                    dif.group = MalawiData$cr_gender,
                                    clusters = MalawiData$cbcc_id,
                                    na0 = TRUE))
 i = 2
 unconditional <- dif_analysis(measure.data = wb_measures[[i]]$measure.data,
                                 dif.group = wb_measures[[i]]$tx.group,
                                 score.type = "Rest",
                                 methods = c("loess", "MH", "logistic", "IRT"),
                                 match.bins = tenths)

  mn <- gsub("_", " ", gsub("\\.", " at ", names(wb_measures)[[i]]))

  dif_report(dif.analysis = unconditional,
             file.name = paste0("Malawi-", mn, " by Tx"),
             measure.name = mn,
             dif.group.name = "Treatment Condition",
             dataset.name = "Malawi",
             bias.method = "IRT",
             irt.scoring = "WLE",
             clusters = wb_measures[[i]]$clusters)

  conditional <- dif_analysis(measure.data = wb_measures[[i]]$measure.data,
                               dif.group = wb_measures[[i]]$dif.group,
                               score.type = "Rest",
                               methods = c("loess", "MH", "logistic", "IRT"),
                               match.bins = tenths)

  mn <- gsub("_", " ", gsub("\\.", " at ", names(wb_measures)[[i]]))

  dif_report(dif.analysis = conditional,
             file.name = paste0("Malawi-", mn, " by Gender"),
             measure.name = mn,
             dif.group.name = "gender",
             dataset.name = "Malawi",
             bias.method = "IRT",
             irt.scoring = "WLE",
             tx.group = wb_measures[[i]]$tx.group,
             clusters = wb_measures[[i]]$clusters)
