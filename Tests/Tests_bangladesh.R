# ----- Bangladesh ---------------------------

load("C:/Users/kylenick/University of North Carolina at Chapel Hill/Halpin, Peter Francis - UNC_stat_projets/WB_project/Bangladesh/Data/Bangladesh_Recoded.RData")

load("/Users/peterhalpin/OneDrive - University of North Carolina at Chapel Hill/UNC_stat_projects/WB_project/Bangladesh/Data/Bangladesh_Recoded.RData")


# <<<<<<< Updated upstream
# # load("~/OneDrive\ -\ University\ of\ North\ Carolina\ at\ Chapel\ Hill/UNC_stat_projects/WB_project/Bangladesh/Data/Bangladesh_Recoded.RData")
# =======
#######################
####    Baseline    ####

baseline <- bang.recode[bang.recode$line == "Base",]


## data prep
base.prep <- lapply(domain.items,
                   function(x) {dif_prep(measure.data = baseline[, x],
                                         dif.group = baseline$gender,
                                         tx.group = baseline$tx,
                                         na0 = FALSE)})
names(base.prep) <- names(domain.items)

tictoc::tic()
for(i in names(domain.items)){
  # tictoc::tic(i)

  ## Unconditional
  base.unc <- dif_analysis(measure.data = base.prep[[i]]$measure.data,
                          dif.group = base.prep[[i]]$tx.group,
                          methods = c("loess", "IRT"),
                          score.type = "Rest")

  dif_report(dif.analysis = base.unc,
             dataset.name = "Bangladesh",
             measure.name = paste(gsub("_", " ", i), "Baseline", sep = " at "),
             dif.group.name = "Treatment Condition",
             bias.method = "IRT",
             irt.scoring = "WLE")

  ## Conditional
  base.con <- dif_analysis(measure.data = base.prep[[i]]$measure.data,
                          dif.group = base.prep[[i]]$dif.group,
                          methods = c("loess", "IRT"),
                          score.type = "Rest")

  dif_report(dif.analysis = base.con,
             dataset.name = "Bangladesh",
             measure.name = paste(gsub("_", " ", i), "Baseline", sep = " at "),
             dif.group.name = "Gender",
             bias.method = "IRT",
             irt.scoring = "WLE",
             tx.group = base.prep[[i]]$tx.group)
# tictoc::toc()
}
tictoc::toc()

#############################
# >>>>>>> Stashed changes


#######################
####    Midline    ####

midline <- bang.recode[bang.recode$line == "Mid",]


## data prep
mid.prep <- lapply(domain.items,
                   function(x) {dif_prep(measure.data = midline[, x],
                                         dif.group = midline$gender,
                                         tx.group = midline$tx,
                                         na0 = FALSE)})
names(mid.prep) <- names(domain.items)

for(i in names(domain.items)){

  ## Unconditional
  mid.unc <- dif_analysis(measure.data = mid.prep[[i]]$measure.data,
                          dif.group = mid.prep[[i]]$tx.group,
                          methods = c("loess", "IRT"),
                          score.type = "Rest")

  dif_report(dif.analysis = mid.unc,
             dataset.name = "Bangladesh",
             measure.name = paste(gsub("_", " ", i), "Midline", sep = " at "),
             dif.group.name = "Treatment Condition",
             bias.method = "IRT",
             irt.scoring = "WLE")

  ## Conditional
  mid.con <- dif_analysis(measure.data = mid.prep[[i]]$measure.data,
                          dif.group = mid.prep[[i]]$dif.group,
                          methods = c("loess", "IRT"),
                          score.type = "Rest")

  dif_report(dif.analysis = mid.con,
             dataset.name = "Bangladesh",
             measure.name = paste(gsub("_", " ", i), "Midline", sep = " at "),
             dif.group.name = "Gender",
             bias.method = "IRT",
             irt.scoring = "WLE",
             tx.group = mid.prep[[i]]$tx.group)

}

#########################################

#######################
####    Endline    ####


endline <- bang.recode[bang.recode$line == "End",]

## data prep
end.prep <- lapply(domain.items,
                   function(x) {dif_prep(measure.data = endline[, x],
                                         dif.group = endline$gender,
                                         tx.group = endline$tx,
                                         na0 = FALSE)})
names(end.prep) <- names(domain.items)

lapply(end.prep, function(x) nrow(x$measure.data))


#### Emergent Literacy ####
#Log-likelihood was decreasing near the ML solution. EM method may be unstable
## Unconditional
end.el.unc <- dif_analysis(measure.data = end.prep$Emergent_Literacy$measure.data,
                      dif.group = end.prep$Emergent_Literacy$tx.group,
                      methods = c("loess", "IRT"),
                      score.type = "Rest")




class(end.el.unc$IRT$dif.mod)
bias.method <- "IRT"

get_plot <- function(dif.analysis, bias.method = "IRT"){
  if(bias.method == "IRT"){

    score.plot <- plot(dif.analysis$IRT$dif.mod, type = "score")

  } else{
    print("wut")
  }
}

test <- bias_plot(dif.analysis = end.el.unc) #
test2 <- get_plot(dif.analysis = end.el.unc)



dif_report(dif.analysis = end.el.unc,
           file.name = "Bangladesh-Literacy by Treatment.html",
           measure.name = "Emergent Literacy",
           dif.group.name = "Treatment Condition",
           dataset.name = "Bangladesh",
           report.title = "DIF Analysis of Emergent Literacy by Treatment Condition in Bangladesh",
           bias.method = "IRT",
           irt.scoring = "WLE")

## Conditional
end.el.con <- dif_analysis(measure.data = end.prep$Emergent_Literacy$measure.data,
                           dif.group = end.prep$Emergent_Literacy$dif.group,
                           methods = c("loess", "IRT"),
                           score.type = "Rest")

dif_report(dif.analysis = end.el.con,
           file.name = "Bangladesh-Literacy by Gender.html",
           measure.name = "Emergent Literacy",
           dif.group.name = "Gender",
           dataset.name = "Bangladesh",
           report.title = "DIF Analysis of Emergent Literacy by Gender in Bangladesh",
           bias.method = "IRT",
           irt.scoring = "WLE",
           tx.group = end.prep$Emergent_Literacy$tx.group)

for(i in names(domain.items)){

  ## Unconditional
  end.unc <- dif_analysis(measure.data = end.prep[[i]]$measure.data,
                             dif.group = end.prep[[i]]$tx.group,
                             methods = c("loess", "IRT"),
                             score.type = "Rest")

  dif_report(dif.analysis = end.unc,
             file.name = paste0("Bangladesh-", i, " by Treatment.html"),
             measure.name = gsub("_", " ", i),
             dif.group.name = "Treatment Condition",
             dataset.name = "Bangladesh",
             report.title = paste0("DIF Analysis of ", gsub("_", " ", i),
                                   " by Treatment Condition in Bangladesh"),
             bias.method = "IRT",
             irt.scoring = "WLE")

  ## Conditional
  end.con <- dif_analysis(measure.data = end.prep[[i]]$measure.data,
                             dif.group = end.prep[[i]]$dif.group,
                             methods = c("loess", "IRT"),
                             score.type = "Rest")

  dif_report(dif.analysis = end.con,
             dataset.name = "Bangladesh",
             measure.name = gsub("_", " ", i),
             dif.group.name = "Gender",
             bias.method = "IRT",
             irt.scoring = "WLE",
             tx.group = end.prep[[i]]$tx.group)

}


is.na(el.un)

apply(end.el, 1, mean, na.rm = T)
is.na(end.el)
temp <- as.data.frame(end.el)
head(temp)
temp_na <- is.na(temp)
apply(temp_na, 1, mean)
end.tx

