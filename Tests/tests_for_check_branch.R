## Example dataset

# Summary of renaming:

  # dif_data_prep replaces dif_prep; uses new naming of inputs
  # dif_analysis stays mostly the same but renamed inputs and outputs based on data_prep
  # dif_models is new and runs the IRT models previously in effect-robustness
  # bias_plots replaces bias_plot but does the same thing
  # hedges2007 and its help functions replace est_smd
  #

# load("/Users/halpin/OneDrive - University of North Carolina at Chapel Hill/UNC_stat_projects/WB_project/Bangladesh/Data/Bangladesh_Recoded.RData")

midline <- bang.recode[bang.recode$line == "Mid",]
i <- 1

# dif_data_prep:
  # Contains some stuff previously in dif.analysis:
    # no.var.items
    # no.var.by.group.items
    # poly.items
  # Establishes new var.names used for data
    # item.data replaces measure.data
    # tx.group.id replaces tx.group
    # dif.group.id replace dif.group
    # cluster.id replaces clusters
    # na.to.0 replaces na0

dif.data <- dif_data_prep(item.data = midline[domain.items[[i]]],
                      tx.group.id = midline$tx,
                      dif.group.id = midline$gender)

# dif_analysis:
  # Same output but input is now dif.data rather than individual components thereof. If users want to run a different dif_nalyses (e.g., conditional versus unconditional) they need to prep a new dif.data object.   # Changed var.names for inputs and some outputs, but did not change var.names for functions called by dif_analysis
    # dif.data replaces measure.data and dif.group
    # match.type replaces score.type

dif.analysis <- dif_analysis(dif.data = dif.data,
                             methods = c("loess", "IRT"))

# dif_models:
  # runs the dif.mod based on biased.items from dif.analysis
  # also runs the no.dif.mod if needed, and appends dif.data to ouput
  # biased items can be a method other than loess or a list of items to treat as biased
  # all subsequent functions require dif.analysis, so if it needs to be forced to run (e.g., for debugging), manually enter and something for biased.items

dif.models <- dif_models(dif.analysis, biased.items = c(1,2,3))

# bias_plots
  # I spend a lot of time arriving back at the same plots we hand in the first place...sigh
bias.plots <- bias_plots(dif.models)
gridExtra::grid.arrange(bias.plots[[1]], bias.plots[[2]])

# effect_robustness
  # This is basically a wrapper for hedges2007, which replaces est_smd and smd_wrapper
  # The output is a list that is comparable to effects.data
  # functions for computing effect plots, tables, and coeff alpha are now external to effect_robustness
  # Takes a while to run due to calling mirt::fscores
effects.list <- effect_robustness(dif.models)

# effects_plot
  # the effect size plots previously in effect_robustness
(effects.plots <- lapply(effects.list, effects_plot))

# effects_table
  # produces the effects_table previously in effect_robustness
(effects.tables <- lapply(effects.list, effects_table))

# coeff_alpha
  # produces the alphas previously in effect robustness by calling est_alpha
(alphas.list <- coeff_alpha(dif.models))

# Add the alpha to the effects_table
(effects.tables.with.alpha <- mapply(effects_table, effects.list, alphas.list))

# I didnt sort out how to put this into the report. I am hoping the individual pieces will be easy tow work with but let me know if not. I will work on documentation over the next few days.

