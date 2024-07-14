#' @name PISA2012HK
#'
#' @title PISA 2012 Hong Kong data subset
#'
#' @description A subset of data from the PISA 2012 data
#'
#' @docType data
#'
#' @usage data(PISA2012HK)
#'
#' @format
#' A data frame with 4,670 rows and 21 columns:
#' \describe{
#'   \item{SCH_ID}{School ID}
#'   \item{STD_ID}{Student ID}
#'   \item{PV1MATH}{Mathematics achievement (plausible value 1)}
#'   \item{PV1READ}{Reading achievement (plausible value 1)}
#'   \item{PV1SCIE}{Science achievement (plausible value 1)}
#'   \item{IMMIG}{Immigrant status. 1 = Native; 2 = 2nd Generation; 3 = 1st Generation}
#'   \item{FAMSTRUC}{Family structure. 1 = Single parent; 2 = Two parents; 3 = Other}
#'   \item{GRADE}{Student grade}
#'   \item{GENDER}{Student gender. 1 = Female; 2 = Male.}
#'   \item{ESCS}{Index of economic, social and cultural status}
#'   \item{HEDRES}{Home educational resources}
#'   \item{HOMEPOS}{Home possessions}
#'   \item{CULTPOS}{Cultural possessions}
#'   \item{MATHEFF}{Mathematics self-efficacy}
#'   \item{MATINTFC}{Mathematics intentions}
#'   \item{INSTMOT}{Instrumental motivation for Mathematics}
#'   \item{INTMAT}{Mathematics interest}
#'   \item{MATBEH}{Mathematics behaviour}
#'   \item{MATWKETH}{Mathematics Work Ethic}
#'   \item{SUBNORM}{Subjective norms in Mathematics}
#'   \item{PERSEV}{Perseverance}
#' }
#'
#' @source This dataset was retrieved from
#'   <https://www.oecd.org/pisa/pisaproducts/pisa2012database-downloadabledata.htm>.
#'   Only 21 variables of the Hong Kong data were extracted and converted to an R dataset.
"PISA2012HK"


#' @name FSdata
#'
#' @title Demo data for latent moderation model
#'
#' @description There are 3 latent variables formed by this dataset:
#'   Dispositional envy (DES) is measured by three indicators: DESP1, DESP2, and DESP3;
#'   Loneliness (LS) is measured by two indicators: UCLA1 and UCLA2;
#'   and Holistic thinking (AHS) is measured by three indicators: AHSP1, AHSP2, and AHSP3.
#'   It is hypothesized that the effect of DES on LS is moderated by AHS.
#'
#' @docType data
#'
#' @usage data(FSdata)
#'
#' @format
#' A data frame with 403 rows and 9 columns:
#' \describe{
#'   \item{ID}{ID}
#'   \item{DESP1}{Dispositional envy item 1}
#'   \item{DESP2}{Dispositional envy item 2}
#'   \item{DESP3}{Dispositional envy item 3}
#'   \item{UCLA1}{Loneliness item 1}
#'   \item{UCLA2}{Loneliness item 2}
#'   \item{AHSP1}{Holistic thinking item 1}
#'   \item{AHSP2}{Holistic thinking item 2}
#'   \item{AHSP3}{Holistic thinking item 3}
#' }
"FSdata"


#' @name SampleData4A
#'
#' @title Sample data for Tutorial Chapter 4 sample output
#'
#' @description A dataset for a simple moderated mediation model
#'
#' @docType data
#'
#' @usage data(SampleData4A)
#'
#' @format
#' A data frame with 100 rows and 4 columns:
#' \describe{
#'   \item{Y}{DV}
#'   \item{X}{IV}
#'   \item{M}{Mediator}
#'   \item{W}{Moderator}
#' }
"SampleData4A"


#' @name SampleData4B
#'
#' @title Sample data for Tutorial Chapter 4 another sample output
#'
#' @description A dataset for a second-order moderated mediation model
#'
#' @docType data
#'
#' @usage data(SampleData4B)
#'
#' @format
#' A data frame with 4,670 rows and 4 columns:
#' \describe{
#'   \item{Y}{DV}
#'   \item{M}{Mediator}
#'   \item{X}{IV}
#'   \item{W1}{Moderator 1}
#'   \item{W2}{Moderator 2}
#'   \item{W3}{Moderator 3}
#'   \item{W4}{Moderator 4}
#' }
"SampleData4B"

