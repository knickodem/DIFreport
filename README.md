# WBdif

Investigating DIF in World Bank early childhood outcome measures

#  Conventions

## Style
* Files and above use caps with "-". (e.g., `File-Name.R`, `Directory-Name`)
* functions use all lower case with "\_" (e.g., `function_name`)
* variables (function args, etc) use lower case with "." (e.g.,`arg.name`)
* column names are a bit of a mish-mash because different packages used in the analysis have varied naming conventions
* Hard wrap at line 90

## Some specific names
* `match` The variable (or list of vectors for rest score) for matching participants in a DIF analysis (e.g., the scale score, the rest score, factor scores)
* `dif.group` The variable with respect to which DIF is evaluated (e.g., treatment, gender)
* `tx.group` The variable with respect to which the robustness of group-mean differences on the scale score is evaluated
  * **Note:** When `dif.group == tx.group`, unconditional treatment effects are assessed for robustness, i.e., delta = E(S | Tx = 1) - E(S | Tx = 0). When `dif_group != tx_group`, conditional treatment effects are assessed for robustness, i.e.,  delta_i = E(S | Tx = 1, dif = i) - E(S | Tx = 0, dif = i) , for i = 0, 1. The interaction effect delta_1 - delta_0 is also reported.
* ...

## Summary table of variable names

| File name       | Function name | Arg name                   | Comments                                                 |
| --------------- | ------------- | -------------------------- | -------------------------------------------------------- |
| Data-Prep       |               |                            | This might be a Malawi specific function                 |
|                 |               |                            |   Depends on consistency with other datasets             |
|                 | data\_prep    | data                       | includes item responses and grouping variables           |
|                 |               | wb.items                   | is this once only?  Yup. Changed name to be wb specific  |
|                 |               | tx.group                   | Required character input; Output depends on dif.group input   |
|                 |               | dif.group                  | Optional Character input;
|                 |               |                            |   Always output vector used in other functions           |
|                 |               |                            |                                                          |
| DIF-Analysis    |               |                            |                                                          |
|                 | dif\_analysis | measure.data               | only item responses;                                       |
|                 |               | dif.group                  | dif.group or tx.group?; See README                      |
|                 |               | score.type                 |                                                           |
|                 |               | methods                    |                                                           |
|                 |               | match.bins                 |                                                           |
| DIF-Report      |               |                            |                                                           |
|                 |               |                            |                                                           |
|                 | dif\_report   | dif.analysis               | If output of dif\_analysis use dif.analsyis               |
|                 |               | dataset.name               | Character; printed in report title & summary (e.g. Malawi)|
|                 |               | measure.name               | Character; printed in report title & summary (Kaufman Triangles) |
|                 |               | dif.group.name             |                                                           |
|                 |               | bias.method                |                                                          |
|                 |               | irt.scoring                |                             |
|                 |               | tx.group                   | Only required if dif.group != treatment indicator        |
|                 |               |                            |                                                          |
| LOESS-Functions |               |                            |                                                          |
|                 | get\_loess    | scale.data                 |                                                          |
|                 |               | dif.group                  |                         |
|                 |               | score.type                 |                                                          |
|                 |               | match                      |                                                          |
|                 |               |                            |                                                          |
|                 | run\_loess    | scale.data                 |                                                          |
|                 |               | dif.group                  |                                                          |
|                 |               | match                      |                                                          |
|                 |               | pred.scores                |                                                          |
|                 |               | item                       |                                                          |
|                 |               | nitem                      |                                                          |
|                 |               |                            |                                                          |
|                 |               |                            |                                                          |
| MH-Functions    |               |                            |                                                          |
|                 | get\_mh       | scale.data                 |                                                          |
|                 |               | dif.group                  |                                                          |
|                 |               | score.type                 |                                                          |
|                 |               | match                      |                                                          |
|                 |               | match.bins                 |                                                          |
|                 |               |                            |                                                          |
|                 | run\_mh       | scale.data                 |                                                          |
|                 |               | dif.group                  |                                                          |
|                 |               | item                       |                                                          |
|                 |               | match                      |                                                          |
|                 |               | match.bins                 |                                                          |
|                 |               |                            |                                                          |
| Logistic-Functions |            |                            |                                                          |
|                 | get\_logistic | scale.data                 |                                                          |
|                 |               | dif.group                  |                                                          |
|                 |               | score.type                 |                                                          |
|                 |               | match                      |                                                          |
|                 |               |                            |                                                          |
|                 |               |                            |                                                          |
|                 | run\_global\_logistic | scale.data                           |                                                          |
|                 |               | dif.group                           |                                                          |
|                 |               | match                           |                                                          |
|                 |               |                            |                                                          |
|                 | run\_item\_logistic | item.data                           |                                                          |
|                 |               | dif.type                           |                                                          |
|                 |               |                            |                                                          |
| IRT-Functions   |               |                            |                                                          |
|                 | get\_irt      | scale.data                 |                                                          |
|                 |               | dif.group                  |                                                          ||                 |               |                            |                                                          |
|                 |               |                            |                                                          |
|                 | run\_global\_irt| scale.data                 |                                                          |
|                 |               | dif.group                  |                                                          ||                 |               |                            |                                                          |
|                 |               |                            |                                                          |
|                 | run\_item\_irt| global.irt                 |                                                          |
|                 |               | which.model                |                                                          |
|                 |               | items2test                 |                                                          |
|                 |               |                            |                                                          |
| Robustness-     |               |                            |                                                          |
|    Functions    |               |                            |                                                          |
|                 |               |                            |                                                          |
|                 |               |                            |                                                          |
|                 |               |                            |                                                          |
|                 |               |                            |                                                          |

# To Do list (Or put this somewhere else?)

## Current

* Refactor code so that files are organized by functions (e.g., IRT, LOESS, Robustness) rather than by level (e.g., user-facing, wrappers, and computations)

* Apply and document naming conventions over functions

* Produce function level documentation via `devtools::document()`

* Update report output
  * to indicate which items are biased
  * the direction of bias of any biased items (e.g., by comparing difficulties in IRT)
  * report standardized interaction effect via pooled variance

## Planned

...