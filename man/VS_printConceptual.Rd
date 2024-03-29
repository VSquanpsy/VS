% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vs_print_results.R
\name{VS_printConceptual}
\alias{VS_printConceptual}
\title{Print VS conceptual model}
\usage{
VS_printConceptual(output = NULL)
}
\arguments{
\item{output}{A \code{VS} list object created by \code{\link{VS}} or
\code{\link{VS_groupEqual}}.}
}
\description{
Print the \code{VS} conceptual model in matrix form.
}
\examples{
modelspec <- '
 PV1MATH = MATHEFF
 MATHEFF->PV1MATH = ESCS + HEDRES
 HEDRES = ESCS
 '
effectspec <- '
 IV1 = MATHEFF
 DV1 = PV1MATH
 '
VS_model <- VS(PISA2012HK, model = modelspec, effect = effectspec, scale = "center")

VS_printConceptual(VS_model)

}
