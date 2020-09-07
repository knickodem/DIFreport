# WBdif

Investigating DIF in World Bank early childhood outcome measures

#  Conventions

## Style
* Files and above use caps with "-". (e.g., `File-Name.R`, `Directory-Name`)
* functions use all lower case with "\_" (e.g., `function_name`)
* variables (function args, etc) use lower case with "." (e.g.,`arg.name`)
* Hard wrap at line 90

## Some specific names
* `match` The variable for matching participants in a DIF analysis (e.g., the scale score, the rest score, factor scores)
* `dif_group` The variable with respect to which DIF is evaluated (e.g., treatment, gender)
* `tx_group` The variable with respect to which the robustness of group-mean differences on the scale score is evaluated
  * **Note:** When `dif_group == tx_group`, unconditional treatment effects are assessed for robustness, i.e., delta = E(S | Tx = 1) - E(S | Tx = 0). When `dif_group != tx_group`, conditional treatment effects are assessed for robustness, i.e.,  delta_i = E(S | Tx = 1, dif = i) - E(S | Tx = 0, dif = i) , for i = 0, 1. The interaction effect delta_1 - delta_0 is also reported.
* ...

## Summary table of variable names

| File name       | Function name | Arg name                   | Comments                                                 |
| --------------- | ------------- | -------------------------- | -------------------------------------------------------- |
| Data-Prep       |               |                            | rename from wb-data-prep                                 |
|                 | data\_prep    | data                       | same as scale.dat?                                       |
|                 |               | items                      | is this once only?                                       |
|                 |               | groupvar                   | rename to dif\_group; see README                         |
|                 |               | condvar                    | rename to tx\_group; see README                          |
|                 |               |                            |                                                          |
| DIF-Analysis    |               |                            |                                                          |
|                 | dif\_analysis | measure.data               | different from data                                      |
|                 |               | group                      | dif\_group or tx\_group?; See README                     |
|                 |               | score.type                 |                                                          |
|                 |               | methods                    |                                                          |
|                 |               | strata                     | unclear -- match.bins?                                   |
|                 |               |                            |                                                          |
| DIF-Report      |               |                            |                                                          |
|                 | dif-report    | dif.results                | If output of dif\_analysis use dif.analsyis              |
|                 |               | dataset.name               | is this data or measure.data?                            |
|                 |               | measure.name               | is this the same as measure.data?                        |
|                 |               | comparison.name            | suggested rename: dif.group.name                         |
|                 |               | bias.method                |                                                          |
|                 |               | irt.method                 | suggested rename: irt.scoring                            |
|                 |               | conditional                | dreprecate; use dif\_group and tx\_group; see README     |
|                 |               |                            |                                                          |
| LOESS-Functions |               |                            |                                                          |
|                 | get\_loess    | scale.data                 |                                                          |
|                 |               | group                      | rename to dif\_group; see README                         |
|                 |               | score.type                 |                                                          |
|                 |               | match.on                   |                                                          |
|                 |               |                            |                                                          |
|                 | run\_loess    | scale.data                 |                                                          |
|                 |               | group                      |                                                          |
|                 |               | match                      |                                                          |
|                 |               | pred.scores                |                                                          |
|                 |               | the.item                   |                                                          |
|                 |               |                            |                                                          |
|   ...           |               |                            |                                                          |
|                 |               |                            |                                                          |

# To Do list (Or put this somewhere else?)

## Current

* Refactor code so that files are organized by functions (e.g., IRT, LOESS, Robustness) rather than by level (e.g., user-facing, wrappers, and computations)

* Apply and document naming conventions over functions

* Produce function level documentation via `devtools::document()`

* Update report output
  * to indicate which varibles are biased
  * the direction of bias of any biased variabes (e.g., by comparing difficulties in IRT)
  * report standardized interaction effect via pooled variance

## Planned

...