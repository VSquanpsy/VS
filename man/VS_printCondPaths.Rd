% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vs_print_results.R
\name{VS_printCondPaths}
\alias{VS_printCondPaths}
\title{Print table labels and remarks}
\usage{
VS_printCondPaths(output = NULL)
}
\arguments{
\item{output}{A \code{VS} list object created by \code{\link{VS}} or
\code{\link{VS_groupEqual}}.}
}
\description{
If any, print the labels of the conditional effect paths and the remarks for the
  \code{VS} conditional effect table.
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

VS_printCondPaths(VS_model)

}
