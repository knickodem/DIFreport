#' Malawi Developmental Assessment Tool (MDAT): Language
#'
#' A dataset containing the 26-item MDAT language assessment and indicators for treatment condition and gender
#'
#' @docType data
#'
#' @usage data(mdat)
#'
#' @format A data frame with 2122 rows and 30 variables:
#' \describe{
#'   \item{clusterid}{cluster identifier}
#'   \item{id}{subject identifier}
#'   \item{treated}{treatment condition; "Tx", "Control"}
#'   \item{gender}{gender; "Male", "Female"}
#'   \item{l20 - l48}{dichotomous (1/0) item responses}
#' }
#' @source
#' Kalanda, M. (2016). Effects of quality improvement strategies on early childhood development in community-based childcare centers in Malawi: A randomized trial. \emphasis{World Bank}. \url{https://www.worldbank.org/en/programs/sief-trust-fund/brief/effects-of-quality-improvement-strategies-on-early-childhood-development-in-community-based-childcare-centers-in-malawi}
#' Neuman, M.,  Ozler, B., & Fernald, L. (2013). \emphasis{Protecting early childhood development in Malawi impact evaluation survey 2013, midline}. World Bank. \url{https://doi.org/10.48529/94zr-ww41}
"mdat"