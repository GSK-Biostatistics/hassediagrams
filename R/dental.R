#' A crossover design for a dental study
#'
#' This is a crossover design (Study H) given in Newcombe et al. (1995) to study the effects of CHX rinses on 4 day plaque regrowth. The study consisted of 24 patients assessed over 3 treatment periods. The purpose of the study was to compare 2 CHX rinses with saline. The design is based on pairs of Latin squares balanced for carry over.
#'
#' @format A crossover design of 72 runs on 5 variables/factors. The 5 variables/factors included in the design are:
#' \describe{
#'   \item{Sequence}{Categoric factor with levels 1-6.}
#'   \item{Subject}{Categoric factor with levels 1-32.}
#'   \item{Period}{Categoric factor with levels 1-3.}
#'   \item{Treatment}{Categoric factor with levels CHX1, CHX2 and Saline.}
#'   \item{Observation}{Categoric factor with levels 1-72.}
#' }
#'
#' @source Newcombe, R.G., Addy, M. and McKeown, S. (1995). "Residual effect of chlorhexidine gluconate in 4-day plaque regrowth crossover trials, and its implications for study design". *Journal of Periodontal Research*, 30, 5, pp. 319-324.
#'
#' @examples
#' data("dental")
#' dental
#'
#' @keywords datasets
#' @name dental
#' @docType data
#' @usage data(dental)
NULL
