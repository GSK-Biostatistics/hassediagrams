#' A cross-nested design for an analytical method investigation
#'
#' The reliability of an analytical method was assessed in an experiment consisting of three batches of material, analysed by four analysts, two at each site. Within each site, there were two chromatographic systems and two columns. For each batch/analyst/system/column combination, two preparations (dissolved samples) were made. From each preparation, two injections were performed.
#'
#' @format A data frame of 192 observations on 8 variables/factors:
#' \describe{
#'   \item{Site}{Categoric factor with levels 1 and 2.}
#'   \item{Analyst}{Categoric factor with levels 1-4.}
#'   \item{Run}{Categoric factor with levels 1-16.}
#'   \item{Prep}{Categoric factor with levels 1-96.}
#'   \item{Injection}{Categoric factor with levels 1-192.}
#'   \item{System}{Categoric factor with levels 1-4.}
#'   \item{Column}{Categoric factor with levels 1-4.}
#'   \item{Batch}{Categoric factor with levels 1-3.}
#' }
#'
#' @source Bate, S.T. and Chatfield, M.J. (2016). "Identifying the Structure of the Experimental Design". *Journal of Quality Technology* 48, pp. 343-364.
#'
#' @examples
#' data("analytical")
#' analytical
#'
#' @keywords datasets
#' @name analytical
#' @docType data
#' @usage data(analytical)
NULL
