% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vs_get_results.R
\name{VS_getIwz}
\alias{VS_getIwz}
\title{Index for evaluating a genuine moderated mediation effect, I_wz}
\usage{
VS_getIwz(output = NULL)
}
\arguments{
\item{output}{A \code{VS} list object created by \code{\link{VS}} or
\code{\link{VS_groupEqual}}.}
}
\value{
A numerical value of an I_wz for a \code{VS} list object or a vector
of the I_wz's of all groups for a \code{VS} multi-sample list object
}
\description{
Compute the index for evaluating a genuine moderated mediation effect (I_wz)
by log-transformation method if the input \code{VS} model is a dual-stage
moderated mediation model.
}
\details{
In a dual-stage moderated mediation model, I_wz is a correlation-based index that
evaluates whether a moderated mediation effect is likely to be spurious.  As a general
rule of thumb, the effect is considered to be a genuine one when I_wz > -.8.
}
\examples{
modelspec <- '
 JOYREAD = TEACHSUP STIMREAD
 STIMREAD = TEACHSUP
 STIMREAD->JOYREAD = GRADE
 TEACHSUP->STIMREAD = FEMALE
 '
effectspec <- '
 IV1 = TEACHSUP
 DV1 = JOYREAD
 '
VS_model <- VS(PISA2018HK, model = modelspec, effect = effectspec, scale = "center",
               categorical = "FEMALE")
VS_group <- VS(PISA2018HK, model = modelspec, effect = effectspec, group = "IMMIG",
               scale = "center", categorical = "FEMALE")

# get the Iwz for a VS model
Iwz <- VS_getIwz(VS_model)

# get a vector of Iwz's of all groups for a VS multi-sample model
Iwz <- VS_getIwz(VS_group)

}
\references{
Chi Kit Jacky Ng, Lok Yin Joyce Kwan & Wai Chan (2023). A Note on Evaluating the
  Moderated Mediation Effect. Structural Equation Modeling: A Multidisciplinary
  Journal. \doi{https://doi.org/10.1080/10705511.2023.2201396}
}
