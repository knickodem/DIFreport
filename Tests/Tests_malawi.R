
## To get path to rmd files included with package, use:
  # system.file("rmd", "file.Rmd", package = "packagename")
  # ## [1] "c:/R/R-3.1.3/library/packagename/rmd/file.Rmd"

  # For more info: https://stackoverflow.com/questions/30377213/how-to-include-rmarkdown-file-in-r-package

#### README Examples ####
data("mdatlang")

## Conditional - All-in-One
conditional <- dif_data_prep(item.data = mdatlang[5:ncol(mdatlang)],
                             tx.group.id = mdatlang$treated,
                             dif.group.id = mdatlang$gender,
                             cluster.id = mdatlang$clusterid,
                             std.group = NULL, # When NULL, the pooled standard deviation is used
                             na.to.0 = TRUE)

summary_report(dif.data = conditional,
               report.type = "dif.effects",
               report.title = "Gender DIF Effects on MDAT Language",
               measure.name = "MDAT Language",
               file.name = "DIF-Effects-Gender-MDAT-Language",
               dataset.name = "Malawi",
               methods = c("loess", "MH", "logistic", "IRT"),
               bias.method = "IRT",
               match.type  = "Total")

## Unconditional - All-in-One
unconditional <- dif_data_prep(item.data = mdatlang[5:ncol(mdatlang)],
                               tx.group.id = mdatlang$treated,
                               dif.group.id = NULL,
                               cluster.id = mdatlang$clusterid,
                               std.group = "Control", # "Control" is a value in mdatlang$treated
                               na.to.0 = TRUE)

summary_report(dif.data = unconditional,
               report.type = "dif.effects",
               report.title = "Tx DIF Effects on MDAT Language",
               measure.name = "MDAT Language",
               file.name = "DIF-Effects-Tx-MDAT-Language",
               dataset.name = "Malawi",
               methods = c("loess", "MH", "logistic", "IRT"),
               bias.method = "IRT",
               match.type  = "Total")


## Step-by-Step
prepped <- dif_data_prep(item.data = mdatlang[5:ncol(mdatlang)],
                         tx.group.id = mdatlang$treated,
                         dif.group.id = mdatlang$gender,
                         cluster.id = mdatlang$clusterid,
                         std.group = NULL, # When NULL, the pooled standard deviation is used
                         na.to.0 = TRUE)

dif.analysis <- dif_analysis(dif.data = prepped,
                             methods =  c("loess", "MH", "logistic", "IRT"),
                             match.type = "Rest",
                             match.bins = seq(0, 1, by = .1))

dif_report(dif.analysis = dif.analysis,
           report.type = "dif.only",
           report.title = "Gender DIF in MDAT Language",
           measure.name = "MDAT Language",
           file.name = "DIF-Only-Gender-MDAT-Language",
           dataset.name = "Malawi",
           bias.method = "IRT")

dif.models <- dif_models(dif.analysis = dif.analysis, biased.items = "logistic")
effect.robustness <- effect_robustness(dif.models = dif.models, std.group = prepped$std.group, irt.scoring = "WLE")

dif_report(dif.analysis = dif.analysis,
           dif.models = dif.models,
           effect.robustness = effect.robustness,
           report.type = "dif.effects",
           report.title = "Gender DIF in MDAT Language",
           measure.name = "MDAT Language",
           file.name = "Logistic-Gender-MDAT-Language",
           dataset.name = "Malawi",
           bias.method = "logistic")



#### Testing #####
data("mdatlang")

mdatprepped.tx <- dif_data_prep(item.data = mdatlang[5:ncol(mdatlang)],
                             tx.group.id = mdatlang$treated,
                             dif.group.id = NULL,
                             cluster.id = mdatlang$clusterid,
                             std.group = "Control",
                             na.to.0 = TRUE)

unc <- dif_analysis(mdatprepped.tx)
uncmod <- dif_models(unc)
er <- effect_robustness(uncmod, std.group = mdatprepped.tx$std.group)
alph <- coeff_alpha(uncmod, std.group = mdatprepped.tx$std.group)
mapply(effects_table, er, alph)


dif_report(dif.analysis = unc, dif.models = uncmod, effect.robustness = er,
           report.type = "dif.effects", report.title = "Gender DIF Effects in MDAT Language",
           measure.name = "MDAT Language", file.name = "Gender-DIF-Effects-MDAT-Language")


dif_synopsis(mdatprepped, report.type = "dif.effects",
              report.title = "Gender DIF Effects in MDAT Language",
              measure.name = "MDAT Language", file.name = "MDAT-Language-Rest-Test",
              methods = c("loess", "MH", "logistic", "IRT"), bias.method = "IRT",
              match.type  = "Rest", match.bins = seq(0, 1, by = .1),
              irt.scoring = "WLE")

dif_synopsis(mdatprepped, report.type = "dif.only",
              report.title = "Gender DIF in MDAT Language",
              measure.name = "MDAT Language", file.name = "Gender-DIF-MDAT-Language",
              methods = c("loess", "MH", "logistic", "IRT"), bias.method = "IRT",
              match.type  = "Total", match.bins = seq(0, 1, by = .1),
              irt.scoring = "WLE")

prepped <- dif_data_prep(item.data = mdatlang[5:ncol(mdatlang)],
                         tx.group.id = mdatlang$treated,
                         dif.group.id = mdatlang$gender,
                         cluster.id = mdatlang$clusterid,
                         std.group = NULL, # When NULL, a pooled sd is used
                         na.to.0 = TRUE)

dif_synopsis(dif.data = prepped,
             report.type = "dif.effects",
             report.title = "Gender DIF Effects on MDAT Language",
             measure.name = "MDAT Language",
             file.name = "DIF-Effects-Gender-MDAT-Language",
             dataset.name = "Malawi",
             methods = c("loess", "MH", "logistic", "IRT"),
             bias.method = "IRT",
             match.type  = "Total")

prepped <- dif_data_prep(item.data = mdatlang[5:ncol(mdatlang)],
                         tx.group.id = mdatlang$treated,
                         dif.group.id = NULL,
                         cluster.id = mdatlang$clusterid,
                         std.group = "Control", # "Control" is a value in mdatlang$treated
                         na.to.0 = TRUE)

dif_synopsis(dif.data = prepped,
             report.type = "dif.effects",
             report.title = "Tx DIF Effects on MDAT Language",
             measure.name = "MDAT Language",
             file.name = "DIF-Effects-Tx-MDAT-Language",
             dataset.name = "Malawi",
             methods = c("loess", "MH", "logistic", "IRT"),
             bias.method = "IRT",
             match.type  = "Total")



load("Bangladesh_Recoded.RData")
midline <- bang.recode[bang.recode$line == "Mid",]

i <- 1

# data prep is now via dif_data_prep
bang.tx.data <- dif_data_prep(item.data = midline[domain.items[[i]]],
                          tx.group.id = midline$tx,
                          dif.group.id = NULL,
                          cluster.id = NULL,
                          std.group = NULL,
                          na.to.0 = F)

dif_synopsis(bang.tx.data, report.type = "dif.effects",
              report.title = "Tx DIF at Midline",
              measure.name = "English Literacy",
              dataset.name = "Bangladesh",
              file.name = "Tx-DIF-EngLit-Mid",
              methods = c("loess", "IRT"), bias.method = "IRT",
              irt.scoring = "WLE")

dif.analysis <- dif_analysis(dif.data = bang.tx.data,
                             methods = c("loess", "IRT"))
dif.models <- dif_models(dif.analysis, biased.items = "IRT")
effects.list <- effect_robustness(dif.models)

alphas.list <- coeff_alpha(dif.models)
effects.tables <- mapply(effects_table, effects.list, alphas.list)
effects.plots <- lapply(effects.list, effects_plot)
bias.plots <- bias_plots(dif.models)



# ----- Malawi -------------------------------------

#####################################
#### Importing Input Information ####

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

## Using deciles of rest (or total) score for strata (to avoid empty cells in the two-way MH tables)
tenths <- seq(0, 1, by = .1)

####################################################


################################################
#### Running analysis and Generating Report ####


wb_measures <- purrr::map(.x = MalawiMeasures,
                          ~dif_prep(measure.data = MalawiData[grep(.x, names(MalawiData))],
                                    tx.group = MalawiData$treated,   # Treatment condition as grouping variable
                                    dif.group = MalawiData$cr_gender, # Gender as conditioning variable
                                    clusters = MalawiData$cbcc_id,
                                    na0 = TRUE))

# mdatlang <- MalawiData[c(1, 4, 9, 11, grep(MalawiMeasures$MDAT_language.Midline, names(MalawiData)))]
# names(mdatlang) <- sub("_2", "", names(mdatlang))
# names(mdatlang)[1:4] <- c("clusterid", "id", "treated", "gender")
# mdatlang <- mdatlang[complete.cases(mdatlang),]
#
# save(mdatlang, file = "mdatlang.rda")



#### Run all unconditional Reports ####
library(tictoc)
i <- 1
for(i in 1){  #:length(WB_Measures)

  tic(as.character(i))

  unconditional <- dif_analysis(measure.data = wb_measures[[i]]$measure.data,
                                 dif.group = wb_measures[[i]]$tx.group,     # For unconditional, use vector for treatment condition
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


  toc(log = TRUE)
}

timing.log.unc <- tic.log(format = TRUE)
tic.clearlog()

ts <- sum_score(unconditional$inputs$data, poly = unconditional$inputs$poly.items)


est_smd(outcome = ts, groups = unconditional$inputs$dif.group)

ns <- tapply(ts, g, length)
cs1 <- table(clus[g == levels(g)[[1]]])
nu1 <- (ns[[1]]^2 - sum(cs1^2)) / (ns[[1]] * (length(cs1) - 1))
cs2 <- table(clus[g == levels(unconditional$inputs$dif.group)[[2]]])
nu2 <- (ns[[2]]^2 - sum(cs2^2)) / (ns[[2]] * (length(cs2) - 1))

mean(c(nu1, nu2))


cs <- tapply(ts, list(g, clus), length)
nus <- (apply(cs, 1, sum, na.rm = TRUE)^2 - apply(cs^2, 1, sum, na.rm = TRUE)) /
  (apply(cs, 1, sum) * (apply(cs, 1, length) - 1))
cluster.n <- mean(nus)

## ICC
variances <- lme4::VarCorr(lme4::lmer(outcome ~ 1 + (1|clusters)))
variances <- c(as.numeric(variances), attr(variances, "sc")^2)
icc <- variances[[1]] / sum(variances)


#### Run All Conditional Reports ####

for(i in 1){ #:length(wb_measures)

  tic(as.character(i))


  conditional <- dif_analysis(measure.data = wb_measures[[i]]$measure.data,
                               dif.group = wb_measures[[i]]$dif.group,
                               score.type = "Rest",
                               methods = c("loess", "MH", "logistic", "IRT"),
                               match.bins = tenths)

  mn <- gsub("_", " ", gsub("\\.", " at ", names(wb_measures)[[i]]))

  dif_report(dif.analysis = conditional,
             file.name = paste0("Malawi-", mn, " by Gender"),
             measure.name = mn,
             dif.group.name = "Gender",
             dataset.name = "Malawi",
             bias.method = "IRT",
             irt.scoring = "WLE",
             tx.group = wb_measures[[i]]$tx.group,
             clusters = wb_measures[[i]]$clusters)

  toc(log = TRUE)
}

timing.log.con <- tic.log(format = TRUE)
tic.clearlog()




# ---- other scripts ---------------------------------------------

#### Individual Test Runs ####
## Using Rest scores; deciles for MH

# unconditional Example
tictoc::tic()
unconditional3 <- dif_analysis(measure.data = WB_Measures[[3]]$measure.data,
                               dif.group = WB_Measures[[3]]$tx.group,     # For unconditional, use vector for treatment condition
                               score.type = "Rest",
                               methods = c("loess", "logistic"),
                               match.bins = tenths)

extract_bi(unconditional3)

dif_report(dif.analysis = unconditional3,
           dataset.name = "Test",
           measure.name = gsub("_", " ", gsub("\\.", " at ", names(WB_Measures)[3])),
           dif.group.name = "Treatment Condition",
           bias.method = "logistic",
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
