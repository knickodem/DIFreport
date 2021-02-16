#' Table of Biased Items
#'
#' Compiles table of biased items identified by selected DIF methods
#'
#' @param dif.analysis Output from \code{\link[WBdif]{dif_analysis}}
#'
#' @return a \code{data.frame} with biased items in rows and DIF methods in columns

biased_items_table <- function(dif.analysis){

  item.data <- dif.analysis$inputs$item.data

  # extracting biased items from each method
  biased.items.list <- extract_biased_items(dif.analysis)
  biased.items.list <- biased.items.list[lengths(biased.items.list) != 0] # removes NULL elements

  # creating table of biased items by method table for report
  item.df <- data.frame(biased.items = names(item.data))
  for(i in names(biased.items.list)){
    if(is.character(biased.items.list[[i]])){
      temp <- data.frame(biased.items = names(item.data),
                         x = NA)
      names(temp) <- c("biased.items", i)
      item.df <- merge(item.df, temp, by = "biased.items", all.x = TRUE)
    } else {
      temp <- data.frame(biased.item = names(item.data[biased.items.list[[i]]]),
                         IRT = "X")
      names(temp) <- c("biased.items", i)
      item.df <- merge(item.df, temp, by = "biased.items", all.x = TRUE)
    }
  }
  # remove unbiased items
  biased.items.df <- item.df[rowSums(is.na(item.df)) != ncol(item.df) - 1,]

  return(biased.items.df)

}

extract_biased_items <- function(dif.analysis){

  da <- dif.analysis[c("MH", "logistic", "IRT")]
  # da <- da[!sapply(da,is.null)]

  all.biased.items <- lapply(da, "[[", "biased.items")

  return(all.biased.items)
}

#' Table of Treatment Effect Estimates
#'
#' Compiles table of treatment effect estimates
#'
#' @param effects Output from \code{\link[WBdif]{effect_robustness}}
#' @param alphas (optional) \code{vector} of score reliabilities (e.g., alpha, omega) to append to the table.
#'
#' @return a \code{data.frame} of treatment effect estimates and their standard errors.

effects_table <- function(effects, alphas = NULL) {
  row.names(effects) <- NULL # sigh
  dat <- data.frame(Items = c("All Items", "Bias Omitted"),
                    `IRT effect size` = effects[1:2,1],
                    `IRT SE` = effects[1:2,2],
                    `Total score effect size` = effects[3:4,1],
                    `Total score SE` = effects[3:4,2],
                    check.names = FALSE)
  if(!is.null(alphas)) {
    dat$`Total score alpha` <- alphas
  }
  dat
}