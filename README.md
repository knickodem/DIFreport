Summary of updates on “check” branch
================
PFH
Feb 5, 2020

  - [WBdif](#wbdif)
      - [Installation](#installation)
      - [Features](#features)
          - [All-in-One](#all-in-one)
          - [Step-by-Step](#step-by-step)
      - [Support and Suggestions](#support-and-suggestions)

# WBdif

**WBdif** contains functions to detect differential item functioning
(DIF), estimate unconditional and conditional treatment effects,
evaluate the robustness of treatment effects in the presence of DIF, and
produce reports summarizing the results. Development of **WBdif** was
motivated by questions regarding the psychometric properties of the
early childhood development assessments used by World Bank to evaluate
intervention impacts.

## Installation

``` r
install.packages("remotes")
remotes::install_github("knickodem/WBdif")
```

## Features

Say you have a measure with both dichotomous and polytomous items and
you want to know:

1.  Do any items show DIF by gender?
2.  What method should I use to detect DIF?
3.  What is the treatment effect of the intervention?
4.  Does the treatment effect change when accounting for DIF-y items?

**WBdif** has functionality to accommodate two approaches to answering
these questions depending on your motivations and the nature of the
measure (assessment/instrument/test).

### All-in-One

The first approach requires only two functions (`dif_data_prep()` and
`dif_synopsis()`) and produces an HTML report summarizing the DIF
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

`dif_synopsis()` is then a wrapper around all of the other functions
needed to run the analysis and produce the report (hence All-in-One).
Nonetheless, the function offers a variety of options to customize the
both the analysis and reporting. For more details on each option see the
Step-by-Step section. Given the All-in-One nature of `dif_synopsis()`,
the code may take a few minutes to run before producing the report.

**Conditional Treatment Effects**

Below we specify both `tx.group.id` and `dif.group.id` indicating that
we are interested in estimating the treatment effects conditional on
gender. The code below generates [this
report](https://github.com/knickodem/WBdif/blob/master/DIF-Effects-Gender-MDAT-Language.html).

``` r
data("mdatlang")

# prep data
prepped <- dif_data_prep(item.data = mdatlang[5:ncol(mdatlang)],
                         tx.group.id = mdatlang$treated,
                         dif.group.id = mdatlang$gender,
                         cluster.id = mdatlang$clusterid,
                         std.group = NULL, # When NULL, the pooled standard deviation is used
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
```

**Unconditional Treatment Effects**

When interested in unconditional effects, only one of `tx.group.id` or
`dif.group.id` need to be supplied in `dif_data_prep()`.
`dif_synopsis()` recognizes how the data are prepared and estimates the
appropriate treatment effect. Thus, the only arguments that might change
are `report.title` and `file.name`, which generates [this
report](https://github.com/knickodem/WBdif/blob/master/DIF-Effects-Tx-MDAT-Language.html).

``` r
data("mdatlang")

# prep data
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
```

### Step-by-Step

## Support and Suggestions

If you encounter any issues or have a suggestion for additional
features, please file a [Github
issue](https://github.com/knickodem/WBdif/issues) or contact us via
email.
