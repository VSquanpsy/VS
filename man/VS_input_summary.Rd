% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vs_print_results.R
\name{VS_input_summary}
\alias{VS_input_summary}
\title{Print information of input data}
\usage{
VS_input_summary(output = NULL)
}
\arguments{
\item{output}{A \code{VS} list object created by \code{\link{VS}} or
\code{\link{VS_groupEqual}}.}
}
\description{
Print the number of variables, list of variables, and number of observations of the \code{VS}
input data.
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

VS_input_summary(VS_model)

}
