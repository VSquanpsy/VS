#' @name PISA2018HK
#'
#' @title PISA 2018 Hong Kong data subset
#'
#' @description A subset of data from the PISA 2018 data
#'
#' @docType data
#'
#' @usage data(PISA2018HK)
#'
#' @format
#' A data frame with 6,037 rows and 10 columns:
#' \describe{
#'   \item{SCH_ID}{School ID}
#'   \item{STD_ID}{Student ID}
#'   \item{FEMALE}{Student gender. 1 = Female; 0 = Male.}
#'   \item{GRADE}{Student grade}
#'   \item{IMMIG}{Immigrant status. 1 = Native; 2 = 2nd Generation; 3 = 1st Generation}
#'   \item{JOYREAD}{Joy/Like reading}
#'   \item{ESCS}{Index of economic, social and cultural status}
#'   \item{HEDRES}{Home educational resources}
#'   \item{STIMREAD}{Teacher's stimulation of reading engagement perceived by student}
#'   \item{TEACHSUP}{Teacher support in test language lessons}
#'   ...
#' }
#'
#' @source This dataset was retrieved from <https://www.oecd.org/pisa/data/2018database/>.
#'   Only 10 variables of the Hong Kong data were extracted and converted to an R dataset.
"PISA2018HK"