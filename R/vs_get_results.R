#' Get covariance matrix
#'
#' Return the covariance matrix of the \code{VS} output data.
#'
#' @param output A \code{VS} list object created by \code{\link{VS}} or
#' \code{\link{VS_groupEqual}}.
#' @param groupid A numerical or character variable indicating any group ID for the multi-sample
#' \code{\link{VS}} output list. If \code{"printall"} (default), a list of covariance matrices
#' for all groups will be returned. If \code{Multisample} = \code{FALSE}, ignore this argument.
#'
#' @return A covariance matrix of \code{\link{VS}} output data or any group of \code{\link{VS}}
#' multi-sample output data; or a list of the covariance matrices of all groups of
#' \code{\link{VS}} multi-sample output data.
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
#' # VS multi-sample model constrainted by group with centered data
#' VS_group_equal <- VS_groupEqual(VS_group, effect.equal = 2)
#'
#' # get the covariance matrix for a VS model
#' cov <- VS_getCov(VS_model)
#'
#' # get the covariance matrix for a VS multi-sample model with IMMIG = 2
#' cov <- VS_getCov(VS_group, 2)
#'
#' # get a list of covariance matrices of all groups for a VS multi-sample model
#' cov <- VS_getCov(VS_group_equal)
#'
#' @importFrom stats cov
#'
#' @export


# Store the covariance matrix of the VS output data

VS_getCov <- function(output = NULL, groupid = "printall") {

  covariance <- NULL

  if (output$Multisample == FALSE) {
    data <- output$Output
    covariance <- cov(data)
  } else {
    if (groupid != "printall") {
      if (!(groupid %in% output$GroupID)) {
        stop("VS ERROR: Group not found - ", groupid)
      } else {
        groupvar <- colnames(output$Output) == output$Group
        data <- output$Output[output$Output[output$Group] == groupid, ][!groupvar]
        covariance <- cov(data)
      }
    } else {
      for (i in 1:output$N_groups) {
        groupvar <- colnames(output$Output) == output$Group
        data <- output$Output[output$Output[output$Group] == output$GroupID[i], ][!groupvar]
        if (i == 1) {
          covariance <- list(as.data.frame(cov(data)))
        } else {
          covariance <- c(covariance, list(as.data.frame(cov(data))))
        }
      }
      names(covariance) <- output$GroupID
    }
  }

  covariance
}


#' Get correlation matrix
#'
#' Return the correlation matrix of the \code{VS} output data.
#'
#' @param output A \code{VS} list object created by \code{\link{VS}} or
#' \code{\link{VS_groupEqual}}.
#' @param groupid A numerical or character variable indicating any group ID for the multi-sample
#' \code{\link{VS}} output list. If \code{"printall"} (default), a list of correlation matrices
#' for all groups will be returned. If \code{Multisample} = \code{FALSE}, ignore this argument.
#'
#' @return A correlation matrix of \code{\link{VS}} output data or any group of \code{\link{VS}}
#' multi-sample output data; or a list of the correlation matrices of all groups of
#' \code{\link{VS}} multi-sample output data.
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
#' # VS multi-sample model constrainted by group with centered data
#' VS_group_equal <- VS_groupEqual(VS_group, effect.equal = 2)
#'
#' # get the correlation matrix for a VS model
#' cor <- VS_getCor(VS_model)
#'
#' # get the correlation matrix for a VS multi-sample model with IMMIG = 2
#' cor <- VS_getCor(VS_group, 2)
#'
#' # get a list of correlation matrices of all groups for a VS multi-sample model
#' cor <- VS_getCor(VS_group_equal)
#'
#' @importFrom stats cor
#'
#' @export

# Store the correlation matrix of the VS output data

VS_getCor <- function(output = NULL, groupid = "printall") {

  correlation <- NULL

  if (output$Multisample == FALSE) {
    data <- output$Output
    correlation <- cor(data)
  } else {
    if (groupid != "printall") {
      if (!(groupid %in% output$GroupID)) {
        stop("VS ERROR: Group not found - ", groupid)
      } else {
        groupvar <- colnames(output$Output) == output$Group
        data <- output$Output[output$Output[output$Group] == groupid, ][!groupvar]
        correlation <- cor(data)
      }
    } else {
      for (i in 1:output$N_groups) {
        groupvar <- colnames(output$Output) == output$Group
        data <- output$Output[output$Output[output$Group] == output$GroupID[i], ][!groupvar]
        if (i == 1) {
          correlation <- list(as.data.frame(cor(data)))
        } else {
          correlation <- c(correlation, list(as.data.frame(cor(data))))
        }
      }
      names(correlation) <- output$GroupID
    }
  }

  correlation
}


#' Get mean vector
#'
#' Return the mean vector of the \code{VS} output data.
#'
#' @param output A \code{VS} list object created by \code{\link{VS}} or
#' \code{\link{VS_groupEqual}}.
#' @param groupid A numerical or character variable indicating any group ID for the multi-sample
#' \code{\link{VS}} output list. If \code{"printall"} (default), a list of mean vectors
#' for all groups will be returned. If \code{Multisample} = \code{FALSE}, ignore this argument.
#'
#' @return A mean vector of \code{\link{VS}} output data or any group of \code{\link{VS}}
#' multi-sample output data; or a list of the mean vector of all groups of
#' \code{\link{VS}} multi-sample output data.
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
#' # VS multi-sample model constrainted by group with centered data
#' VS_group_equal <- VS_groupEqual(VS_group, effect.equal = 2)
#'
#' # get the mean vector for a VS model
#' mean <- VS_getMean(VS_model)
#'
#' # get the mean vector for a VS multi-sample model with IMMIG = 2
#' mean <- VS_getMean(VS_group, 2)
#'
#' # get a list of mean vectors of all groups for a VS multi-sample model
#' mean <- VS_getMean(VS_group_equal)
#'
#' @export

# Store the mean vector of the VS output data

VS_getMean <- function(output = NULL, groupid = "printall") {

  mean <- NULL

  if (output$Multisample == FALSE) {
    data <- output$Output
    mean <- colMeans(data)
  } else {
    if (groupid != "printall") {
      if (!(groupid %in% output$GroupID)) {
        stop("VS ERROR: Group not found - ", groupid)
      } else {
        groupvar <- colnames(output$Output) == output$Group
        data <- output$Output[output$Output[output$Group] == groupid, ][!groupvar]
        mean <- colMeans(data)
      }
    } else {
      for (i in 1:output$N_groups) {
        groupvar <- colnames(output$Output) == output$Group
        data <- output$Output[output$Output[output$Group] == output$GroupID[i], ][!groupvar]
        mean_vec <- colMeans(data)
        if (i == 1) {
          mean <- list(mean_vec)
        } else {
          mean <- c(mean, list(mean_vec))
        }
      }
      names(mean) <- output$GroupID
    }
  }

  mean
}


#' Get conditional effect table
#'
#' Return the conditional effect table of the \code{VS} output data.
#'
#' @param output A \code{VS} list object created by \code{\link{VS}} or
#' \code{\link{VS_groupEqual}}.
#' @param groupid A numerical or character variable indicating any group ID for the multi-sample
#' \code{\link{VS}} output list. If \code{"printall"} (default), a list of conditional effect tables
#' for all groups will be returned. If \code{Multisample} = \code{FALSE}, ignore this argument.
#'
#' @return A conditional effect table of \code{\link{VS}} output data or any group of
#' \code{\link{VS}} multi-sample output data; or a list of the conditional effect tables
#' of all groups of \code{\link{VS}} multi-sample output data.
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
#' # VS multi-sample model constrainted by group with centered data
#' VS_group_equal <- VS_groupEqual(VS_group, effect.equal = 2)
#'
#' # get the conditional effect table for a VS model
#' effect <- VS_getEffects(VS_model)
#'
#' # get the conditional effect table for a VS multi-sample model with IMMIG = 2
#' effect <- VS_getEffects(VS_group, 2)
#'
#' # get a list of conditional effect tables of all groups for a VS multi-sample model
#' effect <- VS_getEffects(VS_group_equal)
#'
#' @import lavaan
#'
#' @export

# Store the conditional effect table of the VS model

VS_getEffects <- function(output = NULL, groupid = "printall", boot.ci.type = "perc") {

  conditional <- NULL

  if (!is.null(output$CondInfo)) {
    if (output$DualStage$Boolean == TRUE) {
      table <- as.data.frame(matrix(0, nrow(output$CondInfo), 11))
      colnames(table) <- c("IV to DV", "Path", "Source", "Conditional Effect", "Est.",
                           "S.E.", "Z-value", "p", "Upper C.I.", "Lower C.I.", "I_wz")
    } else {
      table <- as.data.frame(matrix(0, nrow(output$CondInfo), 10))
      colnames(table) <- c("IV to DV", "Path", "Source", "Conditional Effect", "Est.",
                           "S.E.", "Z-value", "p", "Upper C.I.", "Lower C.I.")
    }
    table[, 1:4] <- output$CondInfo

    if (output$Bootstrap > 0) {
      par <- lavaan::parameterEstimates(output$Model, boot.ci.type = boot.ci.type)
    } else {
      par <- lavaan::parameterEstimates(output$Model)
    }
    par <- par[c("label", "est", "se", "z", "pvalue", "ci.lower", "ci.upper")]

    if (output$Multisample == TRUE) {
      if (groupid != "printall") {
        if (!(groupid %in% output$GroupID)) {
          stop("VS ERROR: Group not found - ", groupid)
        } else {
          conditional <- table
          for (i in 1:nrow(conditional)) {
            groupnum <- which(output$GroupID == groupid)
            ap <- paste0("Eff", i, ".", groupnum)
            conditional[i, 5:10] <- par[par["label"] == ap, 2:7]
            if (output$DualStage$Boolean == TRUE) {
              if (conditional[i, 2] == output$DualStage$Path && conditional[i, 4] != "None") {
                conditional[i, 11] <- VS_getIwz(output)[groupnum]
              } else {
                conditional[i, 11] <- NA
              }
            }
          }
        }
      } else {
        for (g in 1:output$N_groups) {
          conditional_g <- table
          for (i in 1:nrow(conditional_g)) {
            ap <- paste0("Eff", i, ".", g)
            conditional_g[i, 5:10] <- par[par["label"] == ap, 2:7]
            if (output$DualStage$Boolean == TRUE) {
              if (conditional_g[i, 2] == output$DualStage$Path && conditional_g[i, 4] != "None") {
                conditional_g[i, 11] <- VS_getIwz(output)[g]
              } else {
                conditional_g[i, 11] <- NA
              }
            }
          }
          if (g == 1) {
            conditional <- list(conditional_g)
          } else {
            conditional <- c(conditional, list(conditional_g))
          }
        }
        names(conditional) <- output$GroupID
      }
    } else {
      conditional <- table
      for (i in 1:nrow(conditional)) {
        ap <- paste0("Eff", i)
        conditional[i, 5:10] <- par[par["label"] == ap, 2:7]
        if (output$DualStage$Boolean == TRUE) {
          if (conditional[i, 2] == output$DualStage$Path && conditional[i, 4] != "None") {
            if (conditional[i, 2] == output$DualStage$Path) {
              conditional[i, 11] <- VS_getIwz(output)
            }
          } else {
            conditional[i, 11] <- NA
          }
        }
      }
    }
  }

  conditional
}


#' Likelihood ratio test
#'
#' Compute the likelihood ratio test statistics comparing nested \code{VS} models.
#'
#' @param output A \code{VS} list object created by \code{\link{VS}} or \code{\link{VS_groupEqual}}.
#'
#' @return An object of class \code{anova}. When given a single argument, it simply returns the
#'   test statistic of this model. When given a sequence of objects, this function tests the models
#'   against one another in the increasing order of df.
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
#' # VS multi-sample model with centered data
#' VS_group <- VS(PISA2012HK, model = modelspec, effect = effectspec, group = "IMMIG",
#'                scale = "center")
#' # VS multi-sample model constrainted by group with centered data
#' VS_group_equal <- VS_groupEqual(VS_group, effect.equal = 2)
#'
#' model_test <- VS_LRT(VS_group, VS_group_equal)
#'
#' @import lavaan
#'
#' @export

# Compute the likelihood ratio test comparing nested models

VS_LRT <- function(output, ...) {

  anovatest <- NULL
  models <- list(output$Model)
  mcall <- match.call(expand.dots = TRUE)
  VS_list <- list(...)
  if (length(VS_list) > 0) {
    for (i in 1:length(VS_list)) {
      models <- c(models, list(VS_list[[i]]$Model))
    }
    names <- sapply(as.list(mcall)[which(c(FALSE, rep(TRUE, length(VS_list) + 1)))], function(x) deparse(x))
    models <- c(models, list(names))
    names(models)[1] <- "object"
    names(models)[1:length(VS_list) + 1] <- rep("...", length(VS_list))
    names(models)[length(VS_list) + 2] <- "model.names"
    anovatest <- do.call(lavaan::lavTestLRT, models)
  } else {
    anovatest <- lavaan::lavTestLRT(models)
  }

  anovatest
}


#' Index for evaluating a genuine moderated mediation effect, I_wz
#'
#' Compute the index for evaluating a genuine moderated mediation effect (I_wz)
#' by log-transformation method if the input \code{VS} model is a dual-stage
#' moderated mediation model.
#'
#' @param output A \code{VS} list object created by \code{\link{VS}} or
#' \code{\link{VS_groupEqual}}.
#'
#' @return A numerical value of an I_wz for a \code{VS} list object or a vector
#' of the I_wz's of all groups for a \code{VS} multi-sample list object
#'
#' @details
#' In a dual-stage moderated mediation model, I_wz is a correlation-based index that
#' evaluates whether a moderated mediation effect is likely to be spurious.  As a general
#' rule of thumb, the effect is considered to be a genuine one when I_wz > -.8.
#'
#' @references
#' Ng, C. K. J., Kwan, L. Y. J., & Chan, W. (2023). A Note on Evaluating the
#'   Moderated Mediation Effect. Structural Equation Modeling: A Multidisciplinary
#'   Journal. \doi{https://doi.org/10.1080/10705511.2023.2201396}
#'
#' @examples
#' modelspec <- '
#'  PV1MATH = MATHEFF + INTMAT
#'  MATHEFF->PV1MATH = HEDRES
#'  MATHEFF = INTMAT
#'  INTMAT->MATHEFF-> = HOMEPOS
#'  '
#' effectspec <- '
#'  IV1 = INTMAT
#'  DV1 = PV1MATH
#'  '
#' VS_model <- VS(PISA2012HK, model = modelspec, effect = effectspec, scale = "center")
#' VS_group <- VS(PISA2012HK, model = modelspec, effect = effectspec, group = "IMMIG",
#'                scale = "center")
#'
#' # get the Iwz for a VS model
#' Iwz <- VS_getIwz(VS_model)
#'
#' # get a vector of Iwz's of all groups for a VS multi-sample model
#' Iwz <- VS_getIwz(VS_group)
#'
#' @export

# Compute the correlation for spurious dual stage moderated mediation effect
# by log-transformation method

VS_getIwz <- function(output = NULL) {

  I_wz <- NULL

  if (output$DualStage$Boolean == TRUE) {
    if (output$Multisample == TRUE) {
      groupid <- unique(output$Output[output$Group])[, 1]
      ngroups <- length(groupid)
      groupvar <- colnames(output$Output) == output$Group
      groupvec <- output$Output[groupvar]

      par <- lavaan::parameterEstimates(output$Model)[c("label", "est")]
      for (i in 1:ngroups) {
        a1 <- par[par["label"] == paste0("a", output$Eq_spec[output$DualStage$M, output$DualStage$X], ".", i), 2]
        a3 <- par[par["label"] == paste0("a", output$Eq_spec[output$DualStage$M, output$DualStage$WX], ".", i), 2]
        b1 <- par[par["label"] == paste0("a", output$Eq_spec[output$DualStage$Y, output$DualStage$M], ".", i), 2]
        b3 <- par[par["label"] == paste0("a", output$Eq_spec[output$DualStage$Y, output$DualStage$ZM], ".", i), 2]

        Wp <- output$Output[output$DualStage$W][groupvec == groupid[i]] + a1 / a3
        Zp <- output$Output[output$DualStage$Z][groupvec == groupid[i]] + b1 / b3

        if (i == 1) {

        }
        I_wz <- c(I_wz, as.numeric(cor(log10(Wp^2), log10(Zp^2))))
      }
      names(I_wz) <- output$GroupID
    } else {
      par <- lavaan::parameterEstimates(output$Model)[c("label", "est")]
      a1 <- par[par["label"] == paste0("a", output$Eq_spec[output$DualStage$M, output$DualStage$X]), 2]
      a3 <- par[par["label"] == paste0("a", output$Eq_spec[output$DualStage$M, output$DualStage$WX]), 2]
      b1 <- par[par["label"] == paste0("a", output$Eq_spec[output$DualStage$Y, output$DualStage$M]), 2]
      b3 <- par[par["label"] == paste0("a", output$Eq_spec[output$DualStage$Y, output$DualStage$ZM]), 2]

      Wp <- output$Output[output$DualStage$W] + a1 / a3
      Zp <- output$Output[output$DualStage$Z] + b1 / b3

      I_wz <- as.numeric(cor(log10(Wp^2), log10(Zp^2)))
    }
  } else {
    stop("VS ERROR: Model is not a dual stage moderated mediation model")
  }

  I_wz
}
