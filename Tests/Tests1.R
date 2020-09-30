

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



#### Run all unconditional Reports ####
library(tictoc)

for(i in 1:length(WB_Measures)){

  tic(as.character(i))

  unconditional <- dif_analysis(measure.data = WB_Measures[[i]]$measure.data,
                                 dif.group = WB_Measures[[i]]$tx.group,     # For unconditional, use vector for treatment condition
                                 score.type = "Rest",
                                 methods = c("loess", "MH", "logistic", "IRT"),
                                 match.bins = tenths)

  dif_report(dif.analysis = unconditional,
             dataset.name = "Malawi",
             measure.name = gsub("_", " ", gsub("\\.", " at ", names(WB_Measures)[i])),
             dif.group.name = "Treatment Condition",
             bias.method = "IRT",
             irt.scoring = "WLE")


  toc(log = TRUE)
}

timing.log.unc <- tic.log(format = TRUE)
tic.clearlog()



#### Run All Conditional Reports ####

for(i in 1:length(WB_Measures)){

  tic(as.character(i))

  conditional <- dif_analysis(measure.data = WB_Measures[[i]]$measure.data,
                               dif.group = WB_Measures[[i]]$dif.group,     # For unconditional, use vector for treatment condition
                               score.type = "Rest",
                               methods = c("loess", "MH", "logistic", "IRT"),
                               match.bins = tenths)

  dif_report(dif.analysis = conditional,
             dataset.name = "Malawi",
             measure.name = gsub("_", " ", gsub("\\.", " at ", names(WB_Measures)[i])),
             dif.group.name = "Gender",
             bias.method = "IRT",
             irt.scoring = "WLE",
             tx.group = WB_Measures[[i]]$tx.group)


  toc(log = TRUE)
}


timing.log.con <- tic.log(format = TRUE)
tic.clearlog()








#### Individual Test Runs ####
## Using Rest scores; deciles for MH

# unconditional Example
tictoc::tic()
unconditional3 <- dif_analysis(measure.data = WB_Measures[[3]]$measure.data,
                               dif.group = WB_Measures[[3]]$tx.group,     # For unconditional, use vector for treatment condition
                               score.type = "Rest",
                               methods = c("loess", "MH", "logistic", "IRT"),
                               match.bins = tenths)

extract_bi(unconditional3)

dif_report(dif.analysis = unconditional3,
           dataset.name = "Malawi",
           measure.name = gsub("_", " ", gsub("\\.", " at ", names(WB_Measures)[3])),
           dif.group.name = "Treatment Condition",
           bias.method = "IRT",
           irt.scoring = "WLE")
tictoc::toc() # 70-90 seconds
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
tictoc::toc() # 247 seconds




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
tictoc::toc() # 33-46 seconds

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



tempfunc <- function(dif.analysis, bias.method){

  dif.type = dif.analysis[[bias.method]]$dif.type
  bi.list <- extract_bi(dif.analysis)
  bi <- bi.list[[bias.method]]

  if (dif.type == "uniform"){
    if(bias.method %in% c("MH", "logistic")){

      uni.temp <- dif.analysis[[bias.method]]$item.level

      toward1 <- nrow(uni.temp[uni.temp$refined.bias == TRUE & uni.temp$refined.OR < 1, ])
      toward2 <- nrow(uni.temp[uni.temp$refined.bias == TRUE & uni.temp$refined.OR > 1, ])

    } else if(bias.method == "IRT"){

      ## extract biased item parameter estimates for each dif.group
      ## from global.irt uniform.mod
      g1 <- mirt::coef(dif.analysis$IRT$uniform.mod, IRTpars = TRUE)[[1]][c(bi)]
      g1.df <- as.data.frame(Reduce(rbind, g1))

      g2 <- mirt::coef(dif.analysis$IRT$uniform.mod, IRTpars = TRUE)[[2]][c(bi)]
      g2.df <- as.data.frame(Reduce(rbind, g2))

      # calculate difference in b (difficulty) parameter
      b.diff <- g1.df$b - g2.df$b

      toward1 <- length(d.diff[b.diff < 0])
      toward2 <- length(d.diff[b.diff > 0])

    }
  }
  list(toward1 = toward1,
       toward2 = toward2)
}

tempfunc(dif.analysis = conditional2, bias.method = "IRT")
