# ----- Bangladesh ---------------------------

# load("C:/Users/kylenick/University of North Carolina at Chapel Hill/Halpin, Peter Francis - UNC_stat_projets/WB_project/Bangladesh/Data/Bangladesh_Recoded.RData")

load("/Users/peterhalpin/OneDrive - University of North Carolina at Chapel Hill/UNC_stat_projects/WB_project/Bangladesh/Data/Bangladesh_Recoded.RData")


# load("~/OneDrive\ -\ University\ of\ North\ Carolina\ at\ Chapel\ Hill/UNC_stat_projects/WB_project/Bangladesh/Data/Bangladesh_Recoded.RData")


#######################
####    Endline    ####


end.tx <- bang.recode[bang.recode$line == "End",]$tx
end.gen <- bang.recode[bang.recode$line == "End",]$gender

## measures
end.se <- bang.recode[bang.recode$line == "End", domain.items$Social_Emotional]
end.en <- bang.recode[bang.recode$line == "End", domain.items$Emergent_Numeracy]
end.el <- bang.recode[bang.recode$line == "End", domain.items$Emergent_Literacy]


## data prep
end.prep <- lapply(list(end.se, end.en, end.el),
                   function(x) {dif_prep(measure.data = x,
                                         dif.group = end.gen,
                                         tx.group = end.tx,
                                         na0 = FALSE)})
names(end.prep) <- names(domain.items)

lapply(end.prep, function(x) nrow(x$measure.data))


# #### Emergent Numeracy ####
# #Log-likelihood was decreasing near the ML solution. EM method may be unstable
# ## Unconditional
# end.en.unc <- dif_analysis(measure.data = end.prep$Emergent_Numeracy$measure.data,
#                       dif.group = end.prep$Emergent_Numeracy$tx.group,
#                       methods = c("loess", "IRT"),
#                       score.type = "Rest")
#
# dif_report(dif.analysis = end.en.unc,
#            dataset.name = "Bangladesh",
#            measure.name = "Emergent Numeracy",
#            dif.group.name = "Treatment Condition",
#            bias.method = "IRT",
#            irt.scoring = "WLE")
#
# ## Conditional
# end.en.con <- dif_analysis(measure.data = end.prep$Emergent_Numeracy$measure.data,
#                            dif.group = end.prep$Emergent_Numeracy$dif.group,
#                            methods = c("loess", "IRT"),
#                            score.type = "Rest")
#
# dif_report(dif.analysis = end.en.con,
#            dataset.name = "Bangladesh",
#            measure.name = "Emergent Numeracy",
#            dif.group.name = "Gender",
#            bias.method = "IRT",
#            irt.scoring = "WLE",
#            tx.group = end.prep$Emergent_Numeracy$tx.group)

for(i in names(domain.items)){

  ## Unconditional
  end.unc <- dif_analysis(measure.data = end.prep[[i]]$measure.data,
                             dif.group = end.prep[[i]]$tx.group,
                             methods = c("loess", "IRT"),
                             score.type = "Rest")

  dif_report(dif.analysis = end.unc,
             dataset.name = "Bangladesh",
             measure.name = gsub("_", " ", i),
             dif.group.name = "Treatment Condition",
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

