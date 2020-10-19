
all.inputs <- conditional$inputs
bi.list <- extract_bi(conditional)
bi.df <- lapply(bi.list,
                function(x){paste(names(all.inputs$data)[x], collapse = ", ")})

item_by_method <-
  data.frame(bi.df)
bi.df <- data.frame(method = c("MH", "logistic", "IRT"),
                    biased.items = stack(bi.df)[[1]])