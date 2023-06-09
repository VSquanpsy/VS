% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vs_print_results.R
\name{VS_summary}
\alias{VS_summary}
\title{Print summary of VS model}
\usage{
VS_summary(output = NULL)
}
\arguments{
\item{output}{A \code{VS} list object created by \code{\link{VS}} or
\code{\link{VS_groupEqual}}.}
}
\description{
Print the summary of the \code{VS} model results include the input data summary,
conceptual model, output data summary, covariance matrix, correlation matrix,
mean vector, working model, conditional effect table, \code{lavaan} model summary,
and parameter estimates with confidence intervals.
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

VS_summary(VS_model)

}
