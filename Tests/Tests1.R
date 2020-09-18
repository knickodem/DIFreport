

## Testing
  # 1. Build package
  # 2. load package
  # 3. Run tests

## To get path to rmd files included with package, use:
  # system.file("rmd", "file.Rmd", package = "packagename")
  # ## [1] "c:/R/R-3.1.3/library/packagename/rmd/file.Rmd"

  # For more info: https://stackoverflow.com/questions/30377213/how-to-include-rmarkdown-file-in-r-package

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

####################################################


################################################
#### Running analysis and Generating Report ####


#### Preparing Malawi Data ####

WB_Measures <- purrr::map(.x = MalawiMeasures,
                          ~wb_data_prep(data = MalawiData,
                                        wb.items = .x,
                                        tx.group.name = "treated",   # Treatment condition as grouping variable
                                        dif.group.name = "cr_gender")) # Gender as conditioning variable


#### Test Runs ####
## Using Rest scores; deciles for MH

# unconditional Example
tictoc::tic()
unconditional1 <- dif_analysis(measure.data = WB_Measures[[1]]$measure.data,
                               dif.group = WB_Measures[[1]]$tx.group,     # For unconditional, use vector for treatment condition
                               score.type = "Rest",
                               methods = c("loess", "MH", "logistic", "IRT"),
                               match.bins = tenths)

dif_report(dif.analysis = unconditional1,
           dataset.name = "Malawi",
           measure.name = gsub("_", " ", gsub("\\.", " at ", names(WB_Measures)[1])),
           dif.group.name = "Treatment Condition",
           bias.method = "IRT",
           irt.scoring = "WLE")
tictoc::toc() # seconds
# conditional Example
tictoc::tic()
conditional1 <- dif_analysis(measure.data = WB_Measures[[1]]$measure.data,
                               dif.group = WB_Measures[[1]]$dif.group,     # For unconditional, use vector for treatment condition
                               score.type = "Rest",
                               methods = c("loess", "MH", "logistic", "IRT"),
                               match.bins = tenths)

dif_report(dif.analysis = conditional1,
           dataset.name = "Malawi",
           measure.name = gsub("_", " ", gsub("\\.", " at ", names(WB_Measures)[1])),
           dif.group.name = "Gender",
           bias.method = "IRT",
           irt.scoring = "WLE",
           tx.group = WB_Measures[[1]]$tx.group)
tictoc::toc() #  seconds




# unconditional Example
# using Total scores
tictoc::tic()
unconditional2 <- dif_analysis(measure.data = WB_Measures[[2]]$measure.data,
                               dif.group = WB_Measures[[2]]$tx.group,     # For unconditional, use vector for treatment condition
                               score.type = "Total",
                               methods = c("loess", "MH", "logistic", "IRT"),
                               match.bins = tenths)

dif_report(dif.analysis = unconditional2,
           dataset.name = "Malawi",
           measure.name = gsub("_", " ", gsub("\\.", " at ", names(WB_Measures)[2])),
           dif.group.name = "Treatment Condition",
           bias.method = "IRT",
           irt.scoring = "WLE")
tictoc::toc() # seconds

# conditional

tictoc::tic()
conditional2 <- dif_analysis(measure.data = WB_Measures[[2]]$measure.data,
                             dif.group = WB_Measures[[2]]$dif.group,     # For unconditional, use vector for treatment condition
                             score.type = "Total",
                             methods = c("loess", "MH", "logistic", "IRT"),
                             match.bins = tenths)

dif_report(dif.analysis = conditional2,
           dataset.name = "Malawi",
           measure.name = gsub("_", " ", gsub("\\.", " at ", names(WB_Measures)[2])),
           dif.group.name = "Gender",
           bias.method = "IRT",
           irt.scoring = "WLE",
           tx.group = WB_Measures[[2]]$tx.group)
tictoc::toc() #  seconds





