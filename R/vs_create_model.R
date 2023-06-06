# vs_create_model
#
# Create Lavaan model syntax for running SEM with Lavaan

vs_create_model <- function(vs.env = NULL, equation = NULL, covariance = NULL) {

  VSmodel <- "\n"

  # Add Lavaan syntax for regression equations
  if (!is.null(equation)) {
    if (nrow(equation) > 0 && ncol(equation) > 0) {
      VSmodel <- paste0(VSmodel, "# regression equations\n")
      for (i in 1:nrow(equation)) {
        VSmodel <- paste0(VSmodel, "  ", rownames(equation)[i], "~")
        firstterm <- 1
        for (j in 1:ncol(equation)) {
          if (equation[i, j] > 0) {
            if (firstterm == 0) {
              VSmodel <- paste0(VSmodel, "+")
            }
            VSmodel <- paste0(VSmodel, "a", equation[i, j], "*", colnames(equation)[j])
            firstterm <- 0
          }
        }
        VSmodel <- paste0(VSmodel, "\n")
      }
    }
  }

  # Add Lavaan syntax for covariances
  if (!is.null(covariance)) {
    if (nrow(covariance) > 1) {
      firstrow <- 1
      for (i in 2:nrow(covariance)) {
        firstterm <- 1
        for (j in 1:(i - 1)) {
          if (covariance[i, j] > 0) {
            if (firstrow == 1) {
              VSmodel <- paste0(VSmodel, "\n# covariances\n")
              firstrow <- 0
            }
            if (firstterm == 1) {
              VSmodel <- paste0(VSmodel, "  ", colnames(equation)[i], "~~")
            } else {
              VSmodel <- paste0(VSmodel, "+")
            }
            VSmodel <- paste0(VSmodel, "c", covariance[i, j], "*", colnames(equation)[j])
            firstterm <- 0
          }
        }
        if (firstterm == 0) {
          VSmodel <- paste0(VSmodel, "\n")
        }
      }
    }
  }

  # Add Lavaan syntax for covariances
  if (!is.null(covariance)) {
    if (nrow(covariance) > 1) {
      firstrow <- 1
      for (i in 1:nrow(covariance)) {
        firstterm <- 1
        if (covariance[i, i] > 0) {
          if (firstrow == 1) {
            VSmodel <- paste0(VSmodel, "\n# variances\n")
            firstrow <- 0
          }
          VSmodel <- paste0(VSmodel, "  ", colnames(equation)[i], "~~")
          VSmodel <- paste0(VSmodel, "v", covariance[i, i], "*", colnames(equation)[i], "\n")
        }
      }
    }
  }

  # Add Lavaan syntax for additional parameters
  APnterms <- NULL
  APtotal <- NULL
  APncoefs <- matrix(0, 1, ncol(vs.env$nEffAPcoefs))
  APcoef <- array(0, c(1, dim(vs.env$EffAPcoef)[2], dim(vs.env$EffAPcoef)[3]))
  nap <- 0
  if (vs.env$nEff > 0) {
    for (a in 1:vs.env$nUserSpecPaths) {
      if (vs.env$nEffPCombo[a] > 0) {
        for (i in 1:vs.env$nEffPCombo[a]) {
          if (vs.env$nEffSCombo[a, i] > 0) {
            for (j in 1:vs.env$nEffSCombo[a, i]) {
              if (vs.env$nEffCombo[a, i, j] > 0) {
                TotalEff <- 0
                for (k in 1:vs.env$nEffCombo[a, i, j]) {
                  nap <- nap + 1
                  Eff <- vs.env$EffCombo[a, i, j, k]
                  TotalEff[k] <- nap
                  APnterms[nap] <- vs.env$nEffAPterms[Eff]
                  if (nap > 1) {
                    APncoefs <- rbind(APncoefs, rep(0, ncol(APncoefs)))
                    dim1 <- dim(APcoef)[1] + 1
                    arr <- array(0, c(dim1, dim(APcoef)[2], dim(APcoef)[3]))
                    arr[-dim1, , ] <- APcoef
                    APcoef <- arr
                  }
                  if (vs.env$nEffAPterms[Eff] > 0) {
                    APncoefs[nap, 1:APnterms[nap]] <- vs.env$nEffAPcoefs[Eff, 1:vs.env$nEffAPterms[Eff]]
                    for (l in 1:vs.env$nEffAPterms[Eff]) {
                      APcoef[nap, l, 1:APncoefs[nap, l]] <- vs.env$EffAPcoef[Eff, l, 1:vs.env$nEffAPcoefs[Eff, l]]
                      if (l == 1) {
                        ap <- paste0("a", vs.env$EffAPcoef[Eff, l, 1])
                      } else {
                        ap <- paste0(ap, "+a", vs.env$EffAPcoef[Eff, l, 1])
                      }
                      if (vs.env$nEffAPcoefs[Eff, l] > 1) {
                        for (m in 2:vs.env$nEffAPcoefs[Eff, l]) {
                          ap <- paste0(ap, "*a", vs.env$EffAPcoef[Eff, l, m])
                        }
                      }
                    }
                  }
                  if (nap == 1) {
                    VSmodel <- paste0(VSmodel, "\n# additional parameters\n")
                  }
                  VSmodel <- paste0(VSmodel, "  ap", nap, ":=", ap, "\n")
                }
                if (length(TotalEff) > 1) {
                  nap <- nap + 1
                  APnterms[nap] <- length(TotalEff)
                  APncoefs <- rbind(APncoefs, rep(0, ncol(APncoefs)))
                  APncoefs[nap, 1:length(TotalEff)] <- TotalEff[1:length(TotalEff)]
                  dim1 <- dim(APcoef)[1] + 1
                  arr <- array(0, c(dim1, dim(APcoef)[2], dim(APcoef)[3]))
                  arr[-dim1, , ] <- APcoef
                  APcoef <- arr
                  for (k in 1:length(TotalEff)) {
                    if (k == 1) {
                      ap <- paste0("ap", TotalEff[k])
                    } else {
                      ap <- paste0(ap, "+ap", TotalEff[k])
                    }
                  }
                  VSmodel <- paste0(VSmodel, "  ap", nap, ":=", ap, "\n")
                }
              }
            }
          }
        }
      }
    }
  }

  cat("Lavvan model syntax created...\n")

  AP <- list("Nterms" = APnterms, "Ncoefs" = APncoefs, "Coefs" = APcoef)
  Model <- list("Model_spec" = VSmodel, "AP" = AP)

  Model
}
