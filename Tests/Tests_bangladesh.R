# ----- Bangladesh ---------------------------

load("C:/Users/kylenick/University of North Carolina at Chapel Hill/Halpin, Peter Francis - UNC_stat_projets/WB_project/Bangladesh/Data/Bangladesh_Recoded.RData")

load("/Users/halpin/OneDrive - University of North Carolina at Chapel Hill/UNC_stat_projects/WB_project/Bangladesh/Data/Bangladesh_Recoded.RData")


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
i = 2
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

#######################
####    Midline    ####

midline <- bang.recode[bang.recode$line == "Mid",]

# PH edits 01/26/2021: Summary, no DIF by windex; big DIF by Preshool but this is part of the Tx...

# use pre-school type as moderator (at midline)

# Data on type of pre-school attended (PreschoolType)
# temp <- haven::read_spss("/Users/halpin/OneDrive - University of North Carolina at Chapel Hill/UNC_stat_projects/WB_project/Bangladesh/Data/Midline Data/Midline_IDELA.sav")
#
# temp <- merge(midline, temp, by.x = "sid", by.y = "SID", all.x = T)
# midline$PreschoolType <- temp$PreschoolType
# midline$preschool <- midline$PreschoolType
# midline$preschool[midline$PreschoolType < 6] <- 1
# midline$preschool[midline$PreschoolType == 6] <- 0
# names(midline)
# table(midline$tx, midline$PreschoolType)
# table(midline$preschool, midline$PreschoolType)


# windex variable
# temp <- haven::read_spss("/Users/halpin/OneDrive - University of North Carolina at Chapel Hill/UNC_stat_projects/WB_project/Bangladesh/Data/Baseline Data/Household.sav")
# head(temp)
# windex_names <- c("P1_10", "P3_3", "P3_4", "P3_5", "P3_6", "P4_3")
# windex_vars <- temp[windex_names]
# names(windex_vars) <- c("maternal_ed", "electicity", "roof", "walls", "toilet", "food")
#
#
# windex_vars$maternal_ed[windex_vars$maternal_ed == 99] <- NA
# windex_vars$maternal_ed[windex_vars$maternal_ed < 2] <- 0
# windex_vars$maternal_ed[windex_vars$maternal_ed >= 2] <- 1
#
# windex_vars$roof[windex_vars$roof == 6] <- NA
# windex_vars$roof[windex_vars$roof < 4] <- 0
# windex_vars$roof[windex_vars$roof >= 4] <- 1
#
# windex_vars$walls[windex_vars$walls == 6] <- NA
# windex_vars$walls[windex_vars$walls < 4] <- 0
# windex_vars$walls[windex_vars$walls >= 4] <- 1
#
# windex_vars$toilet[windex_vars$toilet == 6] <- NA
# windex_vars$toilet[windex_vars$toilet > 1] <- 0
#
# windex_vars$food[windex_vars$food < 4] <- 0
# windex_vars$food[windex_vars$food == 4] <- 1
# windex_vars <- as.data.frame((windex_vars))
#
# windex_vars$windex <- apply(windex_vars, 1, mean, na.rm = T)
# windex_vars$windex_cut <- (windex_vars$windex < .5)*1
# table(windex_vars$windex_cut)
#
# windex_vars$sid <- temp$sid
#
# midline <- merge(midline, windex_vars, by.x = "sid", by.y = "sid", all.x = T)
# head(midline)



## data prep

mid.prep <- lapply(domain.items,
                   function(x) {dif_prep(measure.data = midline[, x],
                                         dif.group = midline$gender,
                                         tx.group = midline$tx,
                                         na0 = FALSE)})

names(mid.prep) <- names(domain.items)
i = names(mid.prep)[3]
for(i in names(domain.items)){

  ## Unconditional
  mid.unc <- dif_analysis(measure.data = mid.prep[[i]]$measure.data,
                          dif.group = mid.prep[[i]]$tx.group,
                          methods = c("loess", "IRT"),
                          score.type = "Rest")

  dif_report(dif.analysis = mid.unc,
             file.name = "Bangladesh_midline",
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
              file.name = "Bangladesh_midline_windex_literacy",
             dataset.name = "Bangladesh",
             measure.name = paste(gsub("_", " ", i), "Midline", sep = " at "),
             dif.group.name = "Preschool",
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
                                         # clusters = endline$age, #using only for testing code
                                         na0 = FALSE)})
names(end.prep) <- names(domain.items)

# lapply(end.prep, function(x) nrow(x$measure.data))


#### Emergent Literacy ####
#Log-likelihood was decreasing near the ML solution. EM method may be unstable
## Unconditional
end.el.unc <- dif_analysis(measure.data = end.prep$Emergent_Literacy$measure.data,
                      dif.group = end.prep$Emergent_Literacy$tx.group,
                      methods = c("loess", "IRT"),
                      score.type = "Rest")



bi.list <- extract_bi(end.el.unc)
bi.list <- bi.list[lengths(bi.list) != 0] # removes NULL elements
# element values are NULL, "No DIF was detected", or vector of item locations
# at least one element will be non-NULL
# if NULL, no column; if "No DIF", blank column
# Note: X = method determined item to be biased

# obtaining item names and putting in a table for report
# Note: returns NA if no biased items; blank if method not in dif.analysis

bi.df <- lapply(bi.list[lengths(bi.list) != 0], function(x){
  if(x == "No DIF was detected"){
    data.frame()
  }
})


bi.df <- data.frame(method = c("MH", "logistic", "IRT"),
                    biased.items = stack(bi.df)[[1]])

item.df <- data.frame(biased.items = names(end.el.unc$inputs$data))
for(i in names(bi.list)){
  if(is.character(bi.list[[i]])){
    temp <- data.frame(biased.items = names(end.el.unc$inputs$data),
               x = NA)
    names(temp) <- c("biased.items", i)
    item.df <- merge(item.df, temp, by = "biased.items", all.x = TRUE)
  } else {
    temp <- data.frame(biased.item = names(end.el.unc$inputs$data[bi.list[[i]]]),
                       IRT = "X")
    names(temp) <- c("biased.items", i)
    item.df <- merge(item.df, temp, by = "biased.items", all.x = TRUE)
  }
}

bi.df <- item.df[rowSums(is.na(item.df)) != ncol(item.df) - 1,]

data.frame(biased.item = names(end.el.unc$inputs$data[end.el.unc$IRT$biased.items]),
           IRT = "X")

### cluster-adjustments
ts <- sum_score(end.el.unc$inputs$data, poly = end.el.unc$inputs$poly.items)
est_smd(ts, end.el.unc$inputs$dif.group, clusters = end.prep$Emergent_Literacy$clusters)



### test score standard errors?
mirt1 <- extract.group(end.el.unc$IRT$dif.mod, group = 1)
test.plot <- plot(mirt1, type = "score", MI = 50) # why no work? Error: Must compute an information matrix

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

