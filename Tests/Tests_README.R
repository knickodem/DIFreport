#### Script for testing README examples ####

# ---- Creating the example dataset ------------------

## Data
MalawiData <- read.csv("C:/Users/kylenick/University of North Carolina at Chapel Hill/Halpin, Peter Francis - UNC_stat_projets/WB_project/child.tests_items_wide.csv")
names(MalawiData)[[1]] <- "cbcc_id"

## Defining labels for possible grouping variables
MalawiData$cr_gender <- factor(MalawiData$cr_gender, labels = c("Male", "Female"))
MalawiData$treated <- factor(MalawiData$treated, labels = c("Control", "Tx"))

## Correcting data entry errors
MalawiData$recog4_3 <- ifelse(MalawiData$recog4_3 == 3, NA, MalawiData$recog4_3)
MalawiData$recog12_3 <- ifelse(MalawiData$recog12_3 == 2, NA, MalawiData$recog12_3)
MalawiData$recog15_3 <- ifelse(MalawiData$recog15_3 == 9, NA, MalawiData$recog15_3)


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

mdat <- MalawiData[c(1, 4, 9, 11, grep(MalawiMeasures$MDAT_language.Midline, names(MalawiData)))]
names(mdat) <- sub("_2", "", names(mdat))
names(mdat)[1:4] <- c("clusterid", "id", "treated", "gender")
# mdat <- mdat[complete.cases(mdat),]

# save(mdat, file = "mdat.rda")


# ---- Running README examples -----------------------

data("mdat")

## Unconditional - Basic
mdat_tx <- dif_data_prep(item.data = mdat[5:ncol(mdat)],
                         dif.group.id = mdat$treated,
                         cluster.id = mdat$clusterid,
                         std.group = "Control", # "Control" is a value in mdat$treated
                         na.to.0 = TRUE)

summary_report(dif.data = mdat_tx,
               file.name = "DIF-Effects-Tx-MDAT-Language",
               report.type = "dif.effects",
               report.title = "MDAT Language: DIF by Treatment Condition",
               measure.name = "MDAT Language",
               dataset.name = "Malawi")

## Conditional - Basic
mdat_gender <- dif_data_prep(item.data = mdat[5:ncol(mdat)],
                             dif.group.id = mdat$gender,
                             tx.group.id = mdat$treated,
                             cluster.id = mdat$clusterid,
                             std.group = "Control")

summary_report(dif.data = mdat_gender,
               file.name = "DIF-Effects-Gender-MDAT-Language",
               report.type = "dif.effects",
               report.title = "MDAT Language: DIF by Gender",
               measure.name = "MDAT Language",
               dataset.name = "Malawi")


## Advanced

dif.analysis <- dif_analysis(dif.data = mdat_gender,
                             dif.methods =  c("loess", "MH", "logistic", "IRT"),
                             match.type = "Total",
                             match.bins = seq(0, 1, by = .1)) # use deciles of matching var in MH

extract_biased_items(dif.analysis)
biased_items_table(dif.analysis)


dif_report(dif.analysis = dif.analysis,
           file.name = "DIF-Only-Gender-MDAT-Language",
           report.title = "MDAT Language: DIF by Gender",
           measure.name = "MDAT Language",
           dataset.name = "Malawi",
           biased.items = "IRT")

# both kinda clunky
biased.items <- Reduce(unique, extract_biased_items(dif.analysis))
as.integer(row.names(biased_items_table(dif.analysis)))

# dif.models <- dif_models(dif.analysis = dif.analysis, biased.items = "logistic")
dif.models <- dif_models(dif.analysis = dif.analysis, biased.items = biased.items)
effect.robustness <- effect_robustness(dif.models = dif.models, irt.scoring = "WLE")



dif_effect_report(dif.analysis = dif.analysis,
                  dif.models = dif.models,
                  effect.robustness = effect.robustness,
                  file.name = "Logistic-Gender-MDAT-Language",
                  report.title = "MDAT Language: Gender DIF and Tx Effects",
                  measure.name = "MDAT Language",
                  dataset.name = "Malawi",
                  biased.items = "logistic")

# pdf version
dif_effect_report(dif.analysis = dif.analysis,
                  dif.models = dif.models,
                  effect.robustness = effect.robustness,
                  file.name = "Logistic-Gender-MDAT-Language",
                  report.format = "powerpoint_presentation",
                  report.title = "MDAT Language: Gender DIF and Tx Effects",
                  measure.name = "MDAT Language",
                  biased.items = "logistic")
