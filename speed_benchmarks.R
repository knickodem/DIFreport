##########################

#### Speed benchmarks ####

##########################

#### Run_loess ####
loopfunc <- function(x){
  
  gg_data <- list(); for(i in 1:n_items){gg_data[[i]] <- Run_loess(scaledat = x, theItem = i, group, pred_scores, n_items, scoreType)}
}

mb <- microbenchmark(
  lapply = lapply(c(1:n_items), Run_loess, scaledat = MeasureData, group = group, pred_scores = pred_scores, n_items = n_items, scoreType = scoreType),
  map = purrr::map(1:n_items, ~Run_loess(scaledat = MeasureData, theItem = .x, group, pred_scores, n_items, scoreType)),
  loop = loopfunc(MeasureData),
  times = 100)

# Unit: milliseconds
# expr      min       lq     mean    median       uq      max neval
# lapply 743.2580 769.0357 1355.836 1205.6590 1946.882 2210.239   100
# map    742.1154 769.4960 1363.320  986.6364 1953.376 2286.834   100
# loop   744.1545 769.8498 1412.430 1914.1626 1951.862 2197.985   100





###########################################
# Alternative approach using difR package #
###########################################

library(difR)

#### MH ####

MHdif <- difMH(Data = NumberRecog$MeasureData, group = NumberRecog$GroupVector,
               focal.name = "Female", match = "score", MHstat = "logOR",
               purify = TRUE, nrIter = 2, p.adjust.method = "BH")

MHdif

## Would need to write a function to extract the relevant info into the dataframe output we desire
## This could also require running difMH twice, once with MHstat = "MHChisq", and once with "logOR"
## Also, we should specify the vector for match rather than use the difMH default


#### Logistic ###

Logdif <- difLogistic(Data = NumberRecog$MeasureData, group = NumberRecog$GroupVector,
                      focal.name = "Female", match = "score", type = "both", criterion = "Wald",
                      p.adjust.method = "BH")

Logdif

## Again, would need to write a function to extract what we need since the difR output is disjointed
## Need to review the workhorse functions to determine what procedures are exactly being used, and how many tests are conducted


# Overall, using the difR package doesn't seem like it will save a whole lot of time.


#### Sample data for exploration ####
test <- data.frame(a = rbinom(n = 20, size = 1, prob = .5),
                   b = rbinom(n = 20, size = 1, prob = .3),
                   c = rbinom(n = 20, size = 1, prob = .8),
                   d = rbinom(n = 20, size = 1, prob = .5))

tot <- Get_MatchScore(test)
drop <- Get_MatchScore(test, drops = 2)

tot == drop

dp <- integer()
drop3 <- Get_MatchScore(test, drops = dp)

tot == drop3
drop == drop3
