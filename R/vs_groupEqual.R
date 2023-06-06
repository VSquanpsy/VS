#' Multi-sample constrainted conditional path models
#'
#' Add user-specified group equal constraints to a \code{VS} multi-sample configural invariance
#' model, run the \code{VS} multi-sample constrainted model by \code{lavaan}, and store required
#' information for further result computations.
#'
#' @param output A \code{VS} list object containing the information of the \code{VS} multi-sample
#'   model created by \code{\link{VS}}.
#' @param effect.equal A numerical value or a vector of numerical values of the use-specified
#'   group equal constraints chosen from the \code{VS} conceptual model.
#'
#' @return A list containing the information of the \code{VS} model.
#'
#' \describe{
#'  \item{\code{Version}}{\code{VS} version.}
#'  \item{\code{Multisample}}{A logical indicating whether the output list is a \code{VS}
#'    multi-sample object.}
#'  \item{\code{Input}}{A data frame containing the input data.}
#'  \item{\code{Conceptual}}{A matrix presentation of the \code{VS} conceptual model.}
#'  \item{\code{Output}}{A data frame containing the output data used in the analysis. Missing
#'    values are removed by list-wise deletion. Data is centered if \code{scale = "center"} or
#'    standardized if \code{scale = "std"}.}
#'  \item{\code{Categorical}}{A vector of character variables containing the names of categorical
#'    variables}
#'  \item{\code{Int_labels}}{A data frame storing the information of the interactions created.}
#'  \item{\code{Scale}}{A character variable indicating the scale of the output data
#'    (\code{"Raw"}, \code{"Centered"}, \code{"Standardized"}, \code{"Centered by group"}, or
#'    \code{"Standardized by group"}).}
#'  \item{\code{Estimator}}{A character variable indicating the estimator to be used in the
#'    \code{VS} model (\code{"ML"}, \code{"MLR"}, or \code{"GLS"}).}
#'  \item{\code{Eq_spec}}{A data frame storing the matrix-form equation specification of the
#'    \code{VS} working model.}
#'  \item{\code{Cov_spec}}{A data frame storing the matrix-form variance and covariance
#'    specification of the \code{VS} working model.}
#'  \item{\code{Order}}{A data frame storing the matrix-form order of moderation of paths
#'    corresponding to the equation specification.}
#'  \item{\code{AP}}{A list containing the information of the additional parameters for the
#'    conditional effects.}
#'  \item{\code{Group}}{A character variable indicating the group variable for the multi-sample
#'    conditional path model.}
#'  \item{\code{N_groups}}{A numerical value containing the number of groups for the multi-sample
#'    conditional path model.}
#'  \item{\code{GroupID}}{A vector containing the group IDs of the group variable for the
#'    multi-sample conditional path model.}
#'  \item{\code{Constraints}}{A numerical value or a vector of numerical values indicating the
#'    equal group model constraints set.}
#'  \item{\code{Syntax}}{The \code{lavaan} model syntax of the VS working model.}
#'  \item{\code{Model}}{An object of class \code{lavaan} containing the \code{VS} multi-sample
#'    constrainted model.}
#'  \item{\code{Bootstrap}}{A numerical value indicating the number of bootstrap draws. If \code{0},
#'    bootstrapping is not used.}
#'  \item{\code{Seed}}{A numerical value indicating the random seed used for bootstrapping.
#'    If \code{Bootstrap = 0}, value is \code{NULL}.}
#'  \item{\code{CItype}}{A character variable containing the type of bootstrapping confidence
#'    interval required. If \code{Bootstrap = 0}, value is \code{NULL}.}
#'  \item{\code{CondInfo}}{A data frame storing the IV to DV, Path, Source, and Conditional effect
#'    columns of the conditional effect table.}
#'  \item{\code{CondRemarks}}{A character vector storing the descriptions of the Conditional effects
#'    and other table remarks.}
#'  \item{\code{DualStage}}{A list containing the information of dual stage moderated mediation model
#'    checking.}
#' }
#'
#' @examples
#' modelspec <- '
#'  JOYREAD = HEDRES
#'  HEDRES->JOYREAD = STIMREAD
#'  STIMREAD = FEMALE
#'  '
#' effectspec <- '
#'  IV1 = HEDRES
#'  DV1 = JOYREAD
#'  '
#' VS_group <- VS(PISA2018HK, model = modelspec, effect = effectspec, group = "IMMIG",
#'                scale = "center", categorical = "FEMALE")
#' VS_group_equal <- VS_groupEqual(VS_group, effect.equal = 2)
#'
#' @import lavaan
#'
#' @export

# VS_groupEqual
#
# Analyze multi-sample constrainted conditional path models using SEM
# with pre-defined model by VS and effect.equal constraints

VS_groupEqual <- function (output=NULL, effect.equal=NULL) {

  vs_multisample_list <- NULL

  if (is.null(output)) {
    stop("VS ERROR: Empty VS list object")
  } else if (output$Multisample == FALSE) {
    stop("VS ERROR: Please provide a VS multi-sample list object")
  } else if (!is.null(output$Constraints)) {
    stop("VS ERROR: Please provide a non-constrainted VS multi-sample list object")
  } else if (is.null(effect.equal)) {
    stop("VS ERROR: Please specific the effect.equal constraints")
  } else if (!(sum(effect.equal %in% rownames(output$CondInfo)) == length(effect.equal))) {
    stop("VS ERROR: Invalid effect.equal constraints")
  } else {

    # Create multi-sample model syntax
    VSmodel <- vs_set_equal(output$Syntax, output$N_groups, effect.equal)

    # Run multi-sample VS models
    fit <- NULL

    if (output$Bootstrap > 0) {
      set.seed(output$Bootstrap_seed)
      fit <- lavaan::lavaan(VSmodel, data = output$Output, group = output$Group, fixed.x = FALSE,
                            auto.var = TRUE, estimator = output$Estimator, se = "bootstrap",
                            bootstrap = output$Bootstrap)
    } else {
      fit <- lavaan::lavaan(VSmodel, data = output$Output, group = output$Group, fixed.x = FALSE,
                            auto.var = TRUE, estimator = output$Estimator)
    }

    vs_multisample_list <- list("Version" = output$Version,
                                "Multisample" = TRUE,
                                "Input" = output$Input,
                                "Conceptual" = output$Conceptual,
                                "Output" = output$Output,
                                "Categorical" = output$Categorical,
                                "Int_labels" = output$Int_labels,
                                "Scale" = output$Scale,
                                "Estimator" = output$Estimator,
                                "Eq_spec" = output$Eq_spec,
                                "Cov_spec" = output$Cov_spec,
                                "Order" = output$Order,
                                "AP" = output$AP,
                                "Group" = output$Group,
                                "N_groups" = output$N_groups,
                                "GroupID"= output$GroupID,
                                "Constraints" = effect.equal,
                                "Syntax" = VSmodel,
                                "Model" = fit,
                                "Bootstrap" = output$Bootstrap,
                                "Seed" = output$Seed,
                                "CItype" = output$CItype,
                                "CondInfo" = output$CondInfo,
                                "CondRemarks" = output$CondRemarks,
                                "DualStage" = output$DualStage)

  }

  invisible(vs_multisample_list)
}
