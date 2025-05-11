#' A block design for an experiment in human-computer interaction
#'
#' This is a block design to compare two methods (mouse and stylus) of drawing a map in a computer file. The design involved 12 subjects randomised in 6 days and 2 tests (morning/afternoon) within each day, across 2 rooms. The design is based on 2x2 Latin squares; see Example 7 Brien and Bailey (2006) for more details.
#'
#' @format A data frame of 24 observations on 7 variables. The 7 variables/factors included in the design are:
#' \describe{
#'   \item{Subject}{Categoric factor with levels 1-12.}
#'   \item{Day}{Categoric factor with levels 1-6.}
#'   \item{Room}{Categoric factor with levels A and B.}
#'   \item{Period}{Categoric factor with levels Morning and Afternoon.}
#'   \item{Method}{Categoric factor with levels Mouse and Stylus.}
#'   \item{Sequence}{Categoric factor with levels 1 and 2.}
#'   \item{Test}{Categoric factor with levels 1-24.}
#' }
#'
#' @source Brien, C.J. and Bailey, R.A. (2006). "Multiple randomizations (with discussion)". *Journal of the Royal Statistical Society B*, 68, pp. 571-609.
#'
#' @examples
#' data("human")
#' human
#'
#' @keywords datasets
#' @name human
#' @docType data
#' @usage data(human)
NULL
