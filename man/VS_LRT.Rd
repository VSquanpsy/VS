% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vs_get_results.R
\name{VS_LRT}
\alias{VS_LRT}
\title{Likelihood ratio test}
\usage{
VS_LRT(output, ...)
}
\arguments{
\item{output}{A \code{VS} list object created by \code{\link{VS}} or \code{\link{VS_groupEqual}}.}
}
\value{
An object of class \code{anova}. When given a single argument, it simply returns the
  test statistic of this model. When given a sequence of objects, this function tests the models
  against one another in the increasing order of df.
}
\description{
Compute the likelihood ratio test statistics comparing nested \code{VS} models.
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
# VS multi-sample model with centered data
VS_group <- VS(PISA2012HK, model = modelspec, effect = effectspec, group = "IMMIG",
               scale = "center")
# VS multi-sample model constrainted by group with centered data
VS_group_equal <- VS_groupEqual(VS_group, effect.equal = 2)

model_test <- VS_LRT(VS_group, VS_group_equal)

}
