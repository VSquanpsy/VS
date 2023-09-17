#' Conditional path models by SEM
#'
#' Convert the user-specified conceptual conditional path model to \code{VS} working
#' model, run the \code{VS} working model by \code{lavaan}, and store required information
#' for further result computations.
#'
#' @param data A data.frame containing the data to be analyzed.
#' @param model A description of the user-specified conceptual model.
#' @param effect A description of the user-specified IV and DV for conditional effects.
#' @param group A character variable containing the group variable for multi-sample
#'   conditional path models.
#' @param scale A character variable containing the scaling method of the data.
#'   Can be \code{"raw"} for raw data (default), \code{"center"} for centered data, or
#'   \code{"std"} for standardized data.
#' @param categorical A vector of categorical variable names in data. Categorical
#'   variables will not be centered and standardized even it is requested in the
#'   \code{scale} argument.
#' @param estimator A character variable containing the estimator to be used in the
#' \code{VS} model. Can be \code{"ML"} for maximum likelihood (default), \code{"MLR"}
#'   = maximum likelihood with ‘HuberWhite’ robust standard errors, or \code{"GLS"} for
#'   (normal theory) generalized least squares.
#' @param bootstrap A numerical value of number of bootstrap draws. If \code{0},
#'   bootstrapping is not used.
#' @param boot.ci.type A character variable containing the type of bootstrapping confidence
#'   interval required. Can be \code{"norm"} for first order normal approximation,
#'   \code{"basic"} for basic bootstrap interval, \code{"perc"} for bootstrap percentile
#'   interval (default), or \code{"bca.simple"} for adjusted bootstrap percentile (BCa)
#'   interval with no correction for acceleration (only for bias).
#' @param local.dep A logical value indicating whether the error covariance and error-IV
#'   covariance involving the two-way interactions will be added. If value = \code{FALSE},
#'   it is recommended to use \code{"center"} or \code{"std"} as the scaling method of the
#'   data.
#'
#' @return A list containing the information of the \code{VS} model.
#'
#' \describe{
#'  \item{\code{Version}}{\code{VS} version.}
#'  \item{\code{Multisample}}{A logical value indicating whether the output list is a \code{VS}
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
#'    conditional path model. If \code{Multisample = FALSE}, value is \code{NULL}.}
#'  \item{\code{N_groups}}{A numerical value containing the number of groups for the multi-sample
#'    conditional path model. If \code{Multisample = FALSE}, value is \code{NULL}.}
#'  \item{\code{GroupID}}{A vector containing the group IDs of the group variable for the
#'    multi-sample conditional path model. If \code{Multisample = FALSE}, value is \code{NULL}.}
#'  \item{\code{Constraints}}{A numerical value or a vector of numerical values indicating the group
#'    equal model constraints set in \code{\link{VS_groupEqual}}. If not created by
#'    \code{\link{VS_groupEqual}}, value is \code{NULL}.}
#'  \item{\code{Syntax}}{The \code{lavaan} model syntax of the \code{VS} working model.}
#'  \item{\code{Model}}{An object of class \code{lavaan} containing the \code{VS} working model.}
#'  \item{\code{Bootstrap}}{A numerical value indicating the number of bootstrap draws. If \code{0},
#'    bootstrapping is not used.}
#'  \item{\code{Seed}}{A numerical value indicating the random seed used for bootstrapping.
#'    If \code{Bootstrap = 0}, value is \code{NULL}.}
#'  \item{\code{CItype}}{A character variable containing the type of bootstrapping confidence
#'    interval required. If \code{Bootstrap = 0}, value is \code{NULL}.}
#'  \item{\code{CondInfo}}{A data frame storing the IV to DV, Path, Source, and Conditional effect
#'    columns of the conditional table.}
#'  \item{\code{CondRemarks}}{A character vector storing the descriptions of the Conditional effects
#'    and other table remarks.}
#'  \item{\code{DualStage}}{A list containing the information of dual stage moderated mediation model
#'    checking.}
#' }
#'
#' @references
#' Kwan, J. L.-Y., & Chan, W. (2018). Variable System: An alternative approach for the analysis of
#'   mediated moderation.  Psychological Methods, 23(2), 262-277.
#'   \doi{https://doi.org/10.1037/met0000160}
#' Kwan, J. L. Y., Chan, W., Ng, J. C. K., Choi, C. Y. T., & Kwan, K. T. (2023, May). Conditional Path
#'   Analysis: The Analytical Framework of Mediated Moderation [Paper Presentation]. The 14th Asian
#'   Conference on the Social Sciences, Tokyo, Japan.
#'   {https://acss.iafor.org/presentation/submission70750/}
#'
#' @examples
#' modelspec <- '
#'  PV1MATH = MATHEFF
#'  MATHEFF->PV1MATH = ESCS + HEDRES
#'  HEDRES = ESCS
#'  '
#' effectspec <- '
#'  IV1 = MATHEFF
#'  DV1 = PV1MATH
#'  '
#' # VS model with centered data
#' VS_model <- VS(PISA2012HK, model = modelspec, effect = effectspec, scale = "center")
#' # VS multi-sample model with centered data
#' VS_group <- VS(PISA2012HK, model = modelspec, effect = effectspec, group = "IMMIG",
#'                scale = "center")
#'
#' @import lavaan
#' @importFrom stats na.omit
#'
#' @export

# VS main function
#
# Analyze conditional path models using Structural Equation Modeling (SEM)

VS <- function(data = NULL, model = NULL, effect = NULL, group = NULL, scale = "raw",
               categorical = NULL, estimator = "ML", bootstrap = 0, boot.ci.type = "perc",
               local.dep = TRUE) {

  VS_version <- "0.0.1"

  vs_output_list <- NULL
  vs.env <- NULL

  # Check if the group argument is valid
  multisample <- FALSE
  if (!is.null(group)) {
    if (!(group %in% colnames(data))) {
      stop("VS ERROR: Group variable not found in data")
    } else if (nrow(unique(data[group])) < 2) {
      stop("VS ERROR: Fewer than 2 groups in group variable: ", group)
    } else {
      multisample <- TRUE
    }
  }

  # Check if the scale argument is valid
  if (!(scale %in% c("raw", "center", "std"))) {
    stop ("VS ERROR: Type of scaling method must be 'raw', 'center', or 'std'")
  }

  # Check if the categorical argument is valid
  if (!is.null(categorical)) {
    for (i in 1:length(categorical)) {
      if (!(categorical[i] %in% colnames(data))) {
        stop("VS ERROR: Categorical variable not found in data: ", categorical[i])
      }
    }
  }

  # Check if the estimator argument is valid
  if (!(estimator %in% c("ML", "MLR", "GLS"))) {
    stop ("VS ERROR: Type of estimator must be 'ML', 'MLR', or 'GLS'")
  }

  # Check if number of bootstrap < 100 or > 10000 and the boot.ci.type argument is valid
  bootstrap_ci = NULL
  if (bootstrap > 0) {
    if (bootstrap < 100 || bootstrap > 10000) {
      stop("VS ERROR: Number of bootstrap should be between 100 and 10000")
    } else if (!(boot.ci.type %in% c("norm", "basic", "perc", "bca.simple"))) {
      stop("VS ERROR: Type of bootstrapping confidence interval must be 'norm', 'basic', 'perc', or 'bca.simple'")
    } else {
      bootstrap_ci <- boot.ci.type
    }
  }

  # Check if empty dataset and length of variable names > 8
  if (is.null(data)) {
    stop("VS ERROR: Missing data specification or empty dataset")
  } else {
    for (i in colnames(data)) {
      if (nchar(i) > 8) {
        stop("VS ERROR: Variable name is too long: ", i,
             "\n          Maximum length of variable names = 8")
      }
    }
  }

  vs.env <- vs_initialize(data)

  # Check if model syntax is not null
  # If yes, parse model syntax into separate paths
  if (!is.null(model)) {
    vs.env <- vs_parse_model_syntax(vs.env, data, model)
    if (vs.env$nPaths > 0) {

      # Check for invalid models
      vs_check_model(vs.env)

      # Find IVs and DVs in conceptual model and assign moderation order for each path
      vs.env <- vs_path_info(vs.env)

      # Check for dual stage moderated mediation model
      dualstage <- vs_check_dualstage(vs.env)
    }
  } else {
    stop("VS ERROR: Missing model syntax")
  }

  # Check if effect syntax is not null
  # If yes, parse effect syntax into separate paths
  if (!is.null(effect)) {
    vs.env <- vs_parse_effect_syntax(vs.env, data, effect)
  }

  if (vs.env$nPaths > 0) {

    # Create new data by listwise deletion
    groupid <- NULL
    ngroups <- NULL
    if (is.null(group)) {
      listdeldata <- as.data.frame(na.omit(data[, (vs.env$Usevar == 1)]))
    } else {
      keepvars <- (vs.env$Usevar == 1) + (colnames(data) == group)
      listdeldata <- as.data.frame(na.omit(data[, (keepvars == 1)]))
      groupid <- unique(listdeldata[group])[, 1]
      ngroups <- length(groupid)
    }

    # Check if number of observations = 0
    if (nrow(listdeldata) == 0) {
      stop("VS ERROR: Number of observations = 0 after listwise deletion")
    }

    # Create the conceptual model matrix
    conceptual <- vs_create_conceptual(vs.env, data)

    # Transform the conceptual model to the working model
    vs.env <- vs_transform(vs.env)

    # Create the output data and interaction labels with the listwise deleted data
    outdata <- vs_working_data(vs.env, listdeldata, scale, categorical, group)
    if (scale == "raw") {
      datascale <- "Raw"
    } else if (is.null(group)) {
      datascale <- ifelse(scale == "center", "Centered", "Standardized")
    } else {
      datascale <- ifelse(scale == "center", "Centered by group", "Standardized by group")
    }
    catout <- NULL
    if (!is.null(categorical)) {
      catvars <- categorical %in% colnames(outdata$Data)
      if (sum(catvars) > 0) {
        catout <- categorical[catvars]
      }
    }
    if (!is.null(group)) {
      if(!is.null(catout)) {
        if (!(group %in% catout)) {
          catout <- c(catout, group)
        }
      } else {
        catout <- c(catout, group)
      }
    }

    # Create the EQUATION, VARIANCE and COVARIANCE matrices for the working model
    vs.env <- vs_working_matrices(vs.env, local.dep)
    working_matrices <- vs.env$ExtraSave
  } else {
    stop("VS ERROR: No model paths specified.\n",
         "          Please check your model specification")
  }

  if (vs.env$nUserSpecPaths > 0) {

    # Create equations for the computation of the conditional effect for the user specified paths
    vs.env <- vs_create_equations(vs.env)
    vs.env <- vs_decompose_equations(vs.env, working_matrices)
  } else {
    warning("VS WARNING: No effect syntax provided\n",
            "              No conditional effects will be generated")
  }

  # Create lavaan model syntax for running SEM with lavaan
  VSmodel <- vs_create_model(vs.env, working_matrices$Equation, working_matrices$Covariance)
  if (!is.null(group)) {
    VSmodel <- vs_create_multisample_model(working_matrices$Equation, working_matrices$Covariance,
                                           VSmodel$AP, ngroups)
  }

  # Run VS model with lavaan
  fit <- NULL
  bootstrap_seed <- NULL
  if (bootstrap > 0) {
    bootstrap_seed <- ncol(outdata$Data) * vs.env$N + 9999
    set.seed(bootstrap_seed)
    fit <- lavaan::lavaan(VSmodel$Model_spec, data = outdata$Data, group = group, fixed.x = FALSE,
                          auto.var = TRUE, estimator = estimator, se = "bootstrap", bootstrap = bootstrap)
  } else {
    fit <- lavaan::lavaan(VSmodel$Model_spec, data = outdata$Data, group = group, fixed.x = FALSE,
                          auto.var = TRUE, estimator = estimator)
  }

  if (dualstage$Boolean == TRUE) {
    dualstage <- vs_dualstage_int(dualstage, outdata$Interactions)
  }

  cond <- NULL
  if (vs.env$nUserSpecPaths > 0) {
    cond <- vs_conditional_info(vs.env)
  }

  vs_output_list <- list("Version" = VS_version,
                         "Multisample" = multisample,
                         "Input" = data,
                         "Conceptual" = conceptual,
                         "Output" = outdata$Data,
                         "Categorical" = catout,
                         "Int_labels" = outdata$Interactions,
                         "Scale" = datascale,
                         "Estimator" = estimator,
                         "Eq_spec" = working_matrices$Equation,
                         "Cov_spec" = working_matrices$Covariance,
                         "Order" = working_matrices$Order,
                         "AP" = VSmodel$AP,
                         "Group" = group,
                         "N_groups" = ngroups,
                         "GroupID"= groupid,
                         "Constraints" = NULL,
                         "Syntax" = VSmodel$Model_spec,
                         "Model" = fit,
                         "Bootstrap" = bootstrap,
                         "Seed" = bootstrap_seed,
                         "CItype" = bootstrap_ci,
                         "CondInfo" = cond$Info,
                         "CondRemarks" = cond$Remarks,
                         "DualStage" = dualstage)

  invisible(vs_output_list)
}


# Initialize the VS environment variables

vs_initialize <- function(data) {

  vs.env <- list()

  # Variables for the conceptual model
  vs.env$N <- ncol(data)
  vs.env$IV <- rep(1, vs.env$N)
  vs.env$DV <- rep(1, vs.env$N)
  vs.env$nCptPaths <- 0
  vs.env$nCptUsePaths <- 0
  vs.env$CptPathtype <- NULL
  vs.env$CptPathfrom <- NULL
  vs.env$CptPathto <- NULL
  vs.env$CptPathname <- NULL
  vs.env$CptPathadded <- NULL
  vs.env$CptUseX <- rep(0, vs.env$N)
  vs.env$CptUseY <- rep(0, vs.env$N)
  vs.env$CptUsePathX <- NULL
  vs.env$CptUsePathY <- NULL

  # Variables for the transformed model
  vs.env$Varnames <- colnames(data)
  vs.env$new_N <- ncol(data)
  vs.env$nints <- 0
  vs.env$nPaths <- 0
  vs.env$nUsePaths <- 0
  vs.env$Pathorder <- NULL
  vs.env$Pathtype <- NULL
  vs.env$Pathfrom <- NULL
  vs.env$Pathto <- NULL
  vs.env$Pathadded <- NULL
  vs.env$Pathname <- NULL
  vs.env$PathID <- NULL
  vs.env$UseX <- rep(0, vs.env$N)
  vs.env$UseY <- rep(0, vs.env$N)
  vs.env$Usevar <- rep(0, vs.env$N)
  vs.env$UsePathX <- NULL
  vs.env$UsePathY <- NULL
  vs.env$Modpath <- NULL   # Path being moderated = 1
  vs.env$isPlug <- rep(0, vs.env$N)
  vs.env$Pathcoef <- NULL   # Numberings shown in output working model
  vs.env$Pathoutcoef <- array(0, c(1, vs.env$N, vs.env$N))
  vs.env$nintvars <- NULL
  vs.env$intvar <- NULL
  vs.env$nbase <- rep(0, vs.env$N)
  vs.env$base <- matrix(0, vs.env$N, 10)
  vs.env$UID <- NULL   # Variable ID for used variables

  # Variables for the effect specification
  vs.env$nUserSpecPaths <- 0
  vs.env$UserSpecID <- NULL
  vs.env$UserSpecDV <- NULL
  vs.env$UserSpecIV <- NULL

  # Variables for creating equations
  vs.env$EqnPathtype <- NULL
  vs.env$EqnPathfrom <- NULL
  vs.env$EqnPathto <- NULL
  vs.env$EqnPathnvars <- NULL
  vs.env$EqnPathvar <- NULL
  vs.env$Eqnnterms <- rep(0, vs.env$N)
  vs.env$focalIV <- rep(0, vs.env$N)
  vs.env$focalDV <- rep(0, vs.env$N)
  vs.env$focalMod <- rep(0, vs.env$N)
  vs.env$Eqnncoefs <- matrix(0, vs.env$N, 3)
  vs.env$Eqncoef <- array(0, c(vs.env$N, 3, 3))
  vs.env$Eqnnvars <- matrix(0, vs.env$N, 3)
  vs.env$Eqnvar <- array(0, c(vs.env$N, 3, 3))
  vs.env$EqnIV <- array(0, c(vs.env$N, 3, 3))
  vs.env$EqnMod <- array(0, c(vs.env$N, 3, 3))
  vs.env$EqntermIV <- matrix(0, vs.env$N, 3)
  vs.env$EqnDir <- NULL
  vs.env$EqnKeep <- NULL
  vs.env$nEff <- 0
  vs.env$uEff <- NULL
  vs.env$uDirEff <- NULL
  vs.env$nEffvars <- NULL
  vs.env$nEffGroups <- NULL
  vs.env$nEffterms <- NULL
  vs.env$nEffcoefs <- NULL
  vs.env$nEffPaths <- NULL
  vs.env$nEffMPaths <- matrix(0, 1, 3)
  vs.env$EffPath <- matrix(0, 1, 3)
  vs.env$Effvar <- NULL
  vs.env$EffIV <- NULL
  vs.env$EffMod <- NULL
  vs.env$EffDel <- NULL
  vs.env$EffGroup <- NULL
  vs.env$EffOrder <- NULL
  vs.env$EffMPath <- array(0, c(1, 3, 3))
  vs.env$Effcoef <- array(0, c(1, 3, 3))
  vs.env$EffoutX <- array(0, c(1, 3, 3))
  vs.env$EffoutY <- array(0, c(1, 3, 3))
  vs.env$nEffDecomposed <- rep(0, 2)
  vs.env$nEffDecvars <- matrix(0, 2, 1)
  vs.env$EffDecvar <- array(0, c(2, 1, 1))
  vs.env$EffDec <- matrix(0, 1, 3)
  vs.env$EffPresent <- NULL
  vs.env$nEffPCombo <- NULL
  vs.env$nEffSCombo <- NULL
  vs.env$nEffCombo <- NULL
  vs.env$EffCombo <- NULL
  vs.env$EffTotal <- NULL
  vs.env$nEffAPterms <- NULL
  vs.env$nEffAPcoefs <- NULL
  vs.env$EffAPcoef <- NULL

  # use to save an extra returned variable in functions
  vs.env$ExtraSave <- NULL

  vs.env
}

