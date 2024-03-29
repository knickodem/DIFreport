
# DIFreport

**DIFreport is currently under construction. All information here is
provisional and subject to change.**

## Summary

Development of **DIFreport** was motivated by questions regarding the
psychometric properties of early childhood development assessments used
as outcome measures in impact evaluations. This package provides methods
for (a) evaluating differential item functioning (DIF), (b) checking
whether DIF leads to biased standardized mean difference estimates on
the unit-weighted total score or item response theory (IRT) based
scoring methods, and (c) providing DIF-corrected estimates by either
removing biased items or using an IRT models that adjust for DIF.

### Methods

The methods for evaluating DIF currently include semi-parametric
regression (LOESS), the Mantel-Haenszel test, logistic regression, and
multigroup IRT.

When evaluating the impact of DIF, standardized mean differences can be
estimated with respect to the primary grouping variable (e.g., gender,
age, and socio-economic status) as well as with respect to a
conditioning variable (e.g., intervention condition) The **DIFreport**
documentation refers to these as unconditional and conditional
standardized mean differneces, respectively.

### Workflows

**DIFreport** provides two general workflows, both of which are
illustrated below. In the “[basic](#basic)” workflow, `summary_report()`
is an omnibus function that automates the DIF analysis and, if there is
any DIF detected, computes the standardized mean differences with and
without adjusting for DIF. The results are summarized in an `rmarkdown`
produced report.

In the “[advanced](#advanced)” workflow, the steps automated by
`summary_report()` can be implemented directly, which gives more control
over which DIF analyses are conducted and which items are identified as
biased.

The both workflows are currently under construction, but particularly
the advanced workflow as the output returned by some functions are not
very user-friendly. These outputs may be converted to S3 or S4 objects
with methods (e.g., print, summary) to provide greater utility.

**DIFreport** is in the alpha stage of development. Please report any
bugs or suggestions by opening a [Github
issue](https://github.com/knickodem/DIFreport/issues).

## Installation

``` r
install.packages("remotes")
remotes::install_github("knickodem/DIFreport")
library(DIFreport)
```

# Workflows

## Dataset

Neuman, M., Ozler, B., & Fernald, L. (2013). *Protecting early childhood
development in Malawi impact evaluation survey 2013, midline*. World
Bank. <https://doi.org/10.48529/94zr-ww41>

## Basic

The basic workflow requires only two functions:

- `dif_prep()` is a pre-processing step that organizes input information
  for use in other **DIFreport** functions and conducts some checks on
  the data to preemptively address potential downstream issues.
  Additionally, different reports require different data preps.

- `summary_report()` is a wrapper around all of the other functions
  needed to run the analysis and produce the report. The function offers
  a variety of options to customize the both the analysis and reporting.
  For more details on each option see the [Advanced workflow](#advanced)
  section. Given the all-in-one nature of `summary_report()`, the code
  will usually take several minutes to run before producing the report.

### Unconditional Standardized Mean Differences

If assessing DIF with respect to only the grouping variable of interest,
and evaluating robustness of the unconditional standardized mean
differences, only `dif.group` need be supplied to `dif_prep()`. If
`cond.group` is supplied, then conditional standardized mean differences
are estimated (see [Conditional Standardized Mean
Differences](#conditional)). The function `summary_report()` recognizes
how the data are prepared and estimates the appropriate standardized
mean differences.

The code below uses the built-in dataset “mdat” to generate [this
report](https://htmlpreview.github.io/?https://github.com/knickodem/DIFreport/blob/master/README%20Example%20Reports/DIF-Effects-Tx-MDAT-Language.html).

``` r
# Load the example data (MDAT language assessment, collected in Malawi). 
data("mdat")

mdat_tx <- dif_prep(item.data = mdat[5:ncol(mdat)],
                               dif.groups = mdat$treated,
                               cluster = mdat$cluster,
                               ref.name = "Male", # "Male" is a value in mdat$gender
                               na.to.0 = TRUE)

summary_report(dif.data = mdat_tx,
               file.name = "DIF-Effects-Tx-MDAT-Language",
               report.type = "dif.effects",
               report.title = "MDAT Language: DIF by Treatment Condition",
               measure.name = "MDAT Language",
               dataset.name = "Malawi")
```

### Conditional Standardized Mean Differences

If assessing DIF with respect to a conditioning variable (e.g.,
treatment group) and evaluating robustness of the conditional
standardized mean differences (e.g., for just girls, for just boys, and
their difference) is of interest, then both `dif.groups` and
`cond.groups`need to be supplied to `dif_prep()`.

The code below generates [this
report](https://htmlpreview.github.io/?https://github.com/knickodem/DIFreport/blob/master/README%20Example%20Reports/DIF-Effects-Gender-MDAT-Language.html).

``` r
mdat_gender <- dif_prep(item.data = mdat[5:ncol(mdat)],
                             dif.groups = mdat$gender,
                             cond.groups = mdat$treated,
                             cluster = mdat$cluster,
                             ref.name = "Male") 

summary_report(dif.data = mdat_gender,
               file.name = "DIF-Effects-Gender-MDAT-Language",
               report.type = "dif.effects",
               report.title = "MDAT Language: DIF by Gender",
               measure.name = "MDAT Language",
               dataset.name = "Malawi")
```

## Advanced

The advanced workflow shows what is going on under the hood of
`summary_report()` and lets you have more control over which items are
identified as biased in the report. In this section we work through the
conditional standardized mean difference example in more detail.

The first step is still `dif_prep()`, which operates in the same manner
as the [Basic workflow](#basic). This step is repeated below.

``` r
library(DIFreport)
data("mdat")

mdat_gender <- dif_prep(item.data = mdat[5:ncol(mdat)],
                             dif.groups = mdat$gender,
                             cond.groups = mdat$treated,
                             cluster = mdat$cluster,
                             ref.name = "Male") 
```

### DIF Analysis

The `dif_analysis()` function explores DIF with respect to the
`dif.groups` using various methods. The goal is to arrive at an overall
picture of the data that does not depend on the assumptions of any one
particular method. Currently, 4 DIF methods are available for
dichotomous data, but only semi-parametric regression and IRT can be
used with multi-category (“polytomous”) assessment items.

- LOESS regression - Generates plot of response curves for the two
  groups on each item. DIF is determined through visual inspection of
  differences in the groups’ response curves.

- Mantel-Haenszel (MH) Test - *currently only available for dichotomous
  items* - Biased items are identified by conducting a Mantel-Haenszel
  test (`stats::mantelhaen.test()`) for each item by `dif.groups` table,
  stratified by either the unit-weighted total score
  (`match.type = "Total"`) or the unit-weighted total score computed
  after removing the item under investigation (`match.type = "Rest"`).
  Analysis proceeds in two stages, an initial “Purification” stage that
  tentatively identifies DIF items, and a second “Refinement” stage
  which removes the tentatively-identified items from the matching
  variable. The results of the second stage are considered more
  definitive. One common problem that arises with the MH test is when
  there are empty cells in the stratified two-way tables. A common
  remedy is to bin the stratifying variable (e.g., by deciles), as shown
  in the code below.

- Logistic regression - *only available for dichotomous items* - First
  conducts an omnibus test of DIF by comparing fit of no DIF, uniform
  DIF, and non-uniform DIF logistic regression models. If the model
  comparisons suggest DIF, biased items are identified by fitting two
  models for each item: one where all items parameters are held constant
  over groups, and one where the item in question interacts with
  `dif.groups`. Depending on the type of DIF identified in the omnibus
  test, the intercept (uniform DIF) or the intercept and slope
  (non-uniform DIF) may be interacted with `dif.groups`. The
  item-by-item testing uses the two-stage purification and refinement
  approach described in the MH Test.

- Item Response Theory (IRT) -

Each method has its own low-level function (`dif_loess()`, `dif_mh()`,
`dif_logistic()`, and `dif_irt()`), which are called by
`dif_analysis()`.

``` r
dif.analysis <- dif_analysis(dif.data = mdat_gender,
                            dif.methods =  c("loess", "MH", "logistic", "IRT"),
                            match.type = "Total",
                            match.bins = seq(0, 1, by = .1)) # use deciles of matching variable in MH
```

``` r
# View the biased items reported by "MH", "logistic", or "IRT" methods
options(knitr.kable.NA = '')
knitr::kable(biased_items_table(dif.analysis))
```

If the standardized mean difference estimates are not of interest, a
report of the DIF analysis can be generated `dif_report()`. The report
enables comparison of the DIF methods with one method highlighted via
the `biased.items` argument ([example
report](https://htmlpreview.github.io/?https://github.com/knickodem/DIFreport/blob/master/README%20Example%20Reports/DIF-Only-Gender-MDAT-Language.html)).

``` r
dif_report(dif.analysis = dif.analysis,
           file.name = "DIF-Only-Gender-MDAT-Language",
           report.title = "Gender DIF in MDAT Language",
           measure.name = "MDAT Language",
           dataset.name = "Malawi",
           biased.items = "IRT")
```

### DIF Impact

The last step in this workflow is to estimate standardized mean
differences, with and without adjustments for DIF. standardized mean
differences and their standard errors are computed using the method
described by Hedges (2007), which is applicable to cluster-randomized
designs as well as simple random samples.

Standardized Mean Differences and their standard errors are reported for
four different outcome variables.

- The unit-weighted total score computed with all items.

- The unit-weighted total score computed with DIF items omitted

- IRT scores computed using a model that constrains all items to have
  equal parameters over levels of `dif.groups`.

- IRT scores computed using a model that allows parameters of DIF-fy
  items to vary over levels of `dif.groups`.

The information supplied to `dif_prep()` (which is passed to
`dif_impact()` provides additional details about how the standardized
mean differences are computed

- The standard deviation used to standardize the effect size is given by
  `ref.name` if supplied.

- If `cluster` is provided, effect sizes and their standard errors are
  adjusted for a multi-stage sampling design in which `cluster` is the
  primary sampling unit.

A report with just the robustness of standardized mean difference
estimates to biased items, but no DIF analysis information, can be
requested from `effect_report()`. More often, the DIF analysis results
are also of interest, in which case use `dif_effect_report()` to include
summaries of both the DIF analysis and the standardized mean difference
robustness checks, such as the [report generated
here](https://htmlpreview.github.io/?https://github.com/knickodem/DIFreport/blob/master/README%20Example%20Reports/Logistic-Gender-MDAT-Language.html)

``` r
dif_effect_report(dif.analysis = dif.analysis,
           dif.models = dif.models,
           effect.robustness = effect.robustness,
           file.name = "Logistic-Gender-MDAT-Language",
           report.title = "Gender DIF in MDAT Language",
           measure.name = "MDAT Language",
           dataset.name = "Malawi",
           biased.items = "logistic")
```

### References

Hedges, L. V. (2007). Effect Sizes in Cluster-Randomized Designs.
Journal of Educational and Behavioral Statistics, 32, 341–370.
<https://doi.org/10.3102/1076998606298043>.
