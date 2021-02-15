
# WBdif

**WBdif** contains functions to detect differential item functioning
(DIF), estimate unconditional and conditional treatment effects,
evaluate the robustness of treatment effects in the presence of DIF, and
produce reports summarizing the results. Development of **WBdif** was
motivated by questions regarding the psychometric properties of the
early childhood development measures used by World Bank to evaluate
intervention impacts.

## Installation

``` r
install.packages("remotes")
remotes::install_github("knickodem/WBdif")
library(WBdif)
```

## Features

Say you have a measure with both dichotomous and polytomous items and
you want to know:

1.  Do any items show DIF by gender?
2.  What method should I use to detect DIF?
3.  What is the treatment effect of the intervention?
4.  Does the treatment effect change when accounting for biased (DIF-y)
    items?

**WBdif** has functionality to accommodate two approaches to answering
these questions depending on your motivations and the nature of the
measure (assessment/instrument/test).

### All-in-One

The first approach requires only two functions (`dif_data_prep()` and
`summary_report()`) and produces an HTML report summarizing the DIF
analysis results from up to 4 methods along with tables and plots
illustrating treatment effects with and without adjusting for item bias.

`dif_data_prep()` is a pre-processing step that organizes input
information for use in other **WBdif** functions and conducts some
checks on the suitability of the data for a DIF analysis. When
investigating treatment effect robustness, `dif_data_prep()` is also
where you specify:

  - whether the treatment effects are unconditional (i.e., only
    comparing treatment group to control group) or conditional on
    another variable (e.g., gender, race, age)
  - which standard deviation to use in the denominator of the treatment
    effect size estimation (`std.group`)
  - whether the data is clustered

`summary_report()` is then a wrapper around all of the other functions
needed to run the analysis and produce the report (hence All-in-One).
Nonetheless, the function offers a variety of options to customize the
both the analysis and reporting. For more details on each option see the
[Step-by-Step](#sbs) section. Given the All-in-One nature of
`summary_report()`, the code may take a few minutes to run before
producing the report.

**Conditional Treatment Effects**

Below we specify both `tx.group.id` and `dif.group.id` indicating that
we are interested in estimating the treatment effects conditional on
gender. The code below generates [this
report](https://htmlpreview.github.io/?https://github.com/knickodem/WBdif/blob/master/DIF-Effects-Gender-MDAT-Language.html).

``` r
data("mdatlang")

# prep data
prepped <- dif_data_prep(item.data = mdatlang[5:ncol(mdatlang)],
                         tx.group.id = mdatlang$treated,
                         dif.group.id = mdatlang$gender,
                         cluster.id = mdatlang$clusterid,
                         std.group = NULL, # When NULL, the pooled standard deviation is used
                         na.to.0 = TRUE)

summary_report(dif.data = prepped,
             report.type = "dif.effects",
             report.title = "Gender DIF Effects on MDAT Language",
             measure.name = "MDAT Language",
             file.name = "DIF-Effects-Gender-MDAT-Language",
             dataset.name = "Malawi",
             methods = c("loess", "MH", "logistic", "IRT"),
             bias.method = "IRT",
             match.type  = "Total")
```

**Unconditional Treatment Effects**

When interested in unconditional effects, only one of `tx.group.id` or
`dif.group.id` need to be supplied in `dif_data_prep()`.
`summary_report()` recognizes how the data are prepared and estimates
the appropriate treatment effect. Thus, the only arguments that might
change are `report.title` and `file.name`, which generates [this
report](https://htmlpreview.github.io/?https://github.com/knickodem/WBdif/blob/master/DIF-Effects-Tx-MDAT-Language.html).

``` r
data("mdatlang")

# prep data
prepped <- dif_data_prep(item.data = mdatlang[5:ncol(mdatlang)],
                         tx.group.id = mdatlang$treated,
                         dif.group.id = NULL,
                         cluster.id = mdatlang$clusterid,
                         std.group = "Control", # "Control" is a value in mdatlang$treated
                         na.to.0 = TRUE)

summary_report(dif.data = prepped,
             report.type = "dif.effects",
             report.title = "Tx DIF Effects on MDAT Language",
             measure.name = "MDAT Language",
             file.name = "DIF-Effects-Tx-MDAT-Language",
             dataset.name = "Malawi",
             methods = c("loess", "MH", "logistic", "IRT"),
             bias.method = "IRT",
             match.type  = "Total")
```

### Step-by-Step

If want to examine the components contributing to `summary_report()`,
you can conduct the analysis and reporting one step at a time. The first
recommended step is still `dif_data_prep()`, which operates in the same
manner as the [All-in-One](#aio) approach. Once again we have specified
both `tx.group.id` and `dif.group.id` to indicate we are interested in
DIF by Gender and the conditional treatment effects.

``` r
library(WBdif)
data("mdatlang")
prepped <- dif_data_prep(item.data = mdatlang[5:ncol(mdatlang)],
                         tx.group.id = mdatlang$treated,
                         dif.group.id = mdatlang$gender,
                         cluster.id = mdatlang$clusterid,
                         std.group = NULL, # When NULL, the pooled standard deviation is used
                         na.to.0 = TRUE)
```

**DIF Analysis**

The next step is typically `dif_analysis()`, which evaluates DIF by the
two groups in `dif.group.id` with up to 4 DIF methods:

  - LOESS regression - Generates plot of response curves for the two
    groups on each item. DIF is determined through visual inspection of
    differences in the groups’ response curves.
  - Mantel-Haenszel Test - *only available for dichotomous items* - In a
    two-stage (Initial detection and Refinement stages) process,
    identifies biased items by conducting a Mantel-Haenszel test via
    `stats::mantelhaen.test()` for each item grouped by `dif.group.id`
    and stratified by raw total score (`match.type = "Total"`) or raw
    score removing the item under investigation (`match.type = "Rest"`).
  - Logistic regression - *only available for dichotomous items* - First
    conducts an omnibus test of DIF by comparing fit of no DIF, uniform
    DIF, and non-uniform DIF logistic regression models. If the model
    comparisons suggest DIF, biased items are identified by fitting a
    separate logistic regression model on each item during an initial
    detection stage and refinement stage.
  - Item Response Theory (IRT) - First conducts an omnibus test of DIF
    by comparing the fit of no DIF, uniform DIF, and non-uniform DIF 2PL
    IRT models via `mirt::multiGroup()`. If the model comparisons
    suggest DIF, biased items are identified by iteratively freeing
    parameters and testing model fit using `mirt::DIF()`.

Each method has its own function (`dif_loess()`, `dif_mh()`,
`dif_logistic()`, and `dif_irt()`), which is called by `dif_analysis()`.

``` r
dif.analysis <- dif_analysis(dif.data = prepped,
                            methods =  c("loess", "MH", "logistic", "IRT"),
                            match.type = "Rest",
                            match.bins = seq(0, 1, by = .1))
```

If you only want a report of the DIF analysis and the treatment effect
estimates are not of interest, a report including only the DIF results
can be generated using `dif_analysis()`. The report enables comparison
of the DIF methods with one method highlighted via the `bias.method`
argument ([example report]()).

``` r
dif_report(dif.analysis = dif.analysis,
           report.type = "dif.only",
           report.title = "Gender DIF in MDAT Language",
           measure.name = "MDAT Language",
           file.name = "DIF-Only-Gender-MDAT-Language",
           dataset.name = "Malawi",
           bias.method = "IRT")
```

**Treatment Effect Estimation**

## Support and Suggestions

If you encounter any issues or have a suggestion for additional
features, please file a [Github
issue](https://github.com/knickodem/WBdif/issues) or contact us via
email.
