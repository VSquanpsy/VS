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
 JOYREAD = HEDRES
 HEDRES->JOYREAD = STIMREAD
 STIMREAD = FEMALE
 '
effectspec <- '
 IV1 = HEDRES
 DV1 = JOYREAD
 '
VS_model <- VS(PISA2018HK, model = modelspec, effect = effectspec, scale = "center",
               categorical = "FEMALE")

VS_printConceptual(VS_model)

}