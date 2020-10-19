# ----- Bangladesh ---------------------------

# load("C:/Users/kylenick/University of North Carolina at Chapel Hill/Halpin, Peter Francis - UNC_stat_projets/WB_project/Bangladesh/Data/Bangladesh_Recoded.RData")

load("~/OneDrive\ -\ University\ of\ North\ Carolina\ at\ Chapel\ Hill/UNC_stat_projects/WB_project/Bangladesh/Data/Bangladesh_Recoded.RData")



################
#### Endline ###
end.tx <- bang.recode[bang.recode$line == "End",]$tx
end.gen <- bang.recode[bang.recode$line == "End",]$gender

## measures
end.en <- bang.recode[bang.recode$line == "End", domain.items$Emergent_Numeracy]
end.el <- bang.recode[bang.recode$line == "End", domain.items$Emergent_Literacy]
end.se <- bang.recode[bang.recode$line == "End", domain.items$Social_Emotional]


## Unconditional
en.un <- dif_analysis(measure.data = end.en,
                      dif.group = end.tx,
                      methods = c("loess, IRT"),
                      score.type = "Rest")


# need to fix how to calculate direction of bias (dif_report line 156) with a
# biased polytomous item; since it is uniform we should get consistent results
# regardless of which threshold is used and we know there has to be at least 2
# so could use either b1 or b2

# dif_report(dif.analysis = en.un,
#            dataset.name = "Bangladesh",
#            measure.name = "Emergent Numeracy",
#            dif.group.name = "Treatment Condition",
#            bias.method = "IRT",
#            irt.scoring = "WLE")


## Unconditional
en.conditional <- dif_analysis(measure.data = end.en,
                      dif.group = end.gen,
                      methods = c("loess", "IRT"),
                      score.type = "Rest")


dif_report(dif.analysis = en.conditional,
           dataset.name = "Bangladesh",
           measure.name = "Emergent Numeracy",
           dif.group.name = "Gender",
           bias.method = "IRT",
           irt.scoring = "WLE",
           tx.group = end.tx)

#Log-likelihood was decreasing near the ML solution. EM method may be unstable

el.un <- dif_analysis(measure.data = end.el,
                      dif.group = end.tx,
                      methods = c("loess", "IRT"),
                      score.type = "Rest")



is.na(el.un)

apply(end.el, 1, mean, na.rm = T)
is.na(end.el)
temp <- as.data.frame(end.el)
head(temp)
temp_na <- is.na(temp)
apply(temp_na, 1, mean)
end.tx

