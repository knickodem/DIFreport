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
