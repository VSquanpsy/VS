# vs_create_multisample_model
#
# Create Lavaan model syntax for running multi-sample SEM with Lavaan

vs_create_multisample_model <- function(equation = NULL, covariance = NULL, ap = NULL,
                                        ngroups = NULL) {

  VSmodel <- "\n"

  if (!is.null(equation)) {
    if (nrow(equation) > 0 && ncol(equation) > 0) {

      # add Lavaan syntax for regression equations
      VSmodel <- paste0(VSmodel, "# regression equations\n")
      for (i in 1:nrow(equation)) {
        VSmodel <- paste0(VSmodel, "  ", rownames(equation)[i], "~")
        firstterm <- 1
        for (j in 1:ncol(equation)) {
          if (equation[i, j] > 0) {
            if (firstterm == 0) {
              VSmodel <- paste0(VSmodel, "+")
            }
            VSmodel <- paste0(VSmodel, "c(a", equation[i, j], ".1")
            for (k in 2:ngroups) {
              VSmodel <- paste0(VSmodel, ",a", equation[i, j], ".", k)
            }
            VSmodel <- paste0(VSmodel, ")*", colnames(equation)[j])
            firstterm <- 0
          }
        }
        VSmodel <- paste0(VSmodel, "\n")
      }

      # add Lavaan syntax for intercepts
      VSmodel <- paste0(VSmodel, "\n# intercepts\n")
      for (i in 1:ncol(equation)) {
        VSmodel <- paste0(VSmodel, "  ", colnames(equation)[i], "~c(i", i, ".1")
        for (j in 2:ngroups) {
          VSmodel <- paste0(VSmodel, ",i", i, ".", j)
        }
        VSmodel <- paste0(VSmodel, ")*1\n")
      }
    }
  }

  # add Lavaan syntax for covariances and variances
  if (!is.null(covariance)) {
    if (nrow(covariance) > 1) {
      firstrow <- 1
      for (i in 2:nrow(covariance)) {
        firstterm <- 1
        for (j in 1:(i-1)) {
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
            VSmodel <- paste0(VSmodel, "c(c", covariance[i, j], ".1")
            for (k in 2:ngroups) {
              VSmodel <- paste0(VSmodel, ",c", covariance[i, j], ".", k)
            }
            VSmodel <- paste0(VSmodel, ")*", colnames(equation)[j])
            firstterm <- 0
          }
        }
        if (firstterm == 0) {
          VSmodel <- paste0(VSmodel, "\n")
        }
      }

      firstrow <- 1
      for (i in 1:nrow(covariance)) {
        if (covariance[i, i] > 0) {
          if (firstrow == 1) {
            VSmodel <- paste0(VSmodel, "\n# variances\n")
            firstrow <- 0
          }
          VSmodel <- paste0(VSmodel, "  ", colnames(equation)[i], "~~c(v", covariance[i, i], ".1")
          for (j in 2:ngroups) {
            VSmodel <- paste0(VSmodel, ",v", covariance[i, i], ".", j)
          }
          VSmodel <- paste0(VSmodel, ")*", colnames(equation)[i], "\n")
        }
      }
    }
  }

  # add Lavaan syntax for additional parameters
  for (i in 1:ngroups) {
    VSmodel <- paste0(VSmodel, "\n# additional parameters for group ", i, "\n")
    for (j in 1:length(ap$Nterms)) {
      apg <- paste0("  ap", j, ".", i, ":=")
      if (sum(ap$Coef[j, , ]) > 0) {
        for (k in 1:ap$Nterms[j]) {
          if (k == 1) {
            apg <- paste0(apg, "a", ap$Coef[j, k, 1], ".", i)
          } else {
            apg <- paste0(apg, "+a", ap$Coef[j, k, 1], ".", i)
          }
          if (ap$Ncoefs[j, k] > 1) {
            for (l in 2:ap$Ncoefs[j, k]) {
              apg <- paste0(apg, "*a", ap$Coef[j, k, l], ".", i)
            }
          }
        }
      } else {
        for (k in 1:ap$Nterms[j]) {
          if (k == 1) {
            apg <- paste0(apg, "ap", ap$Ncoefs[j, k], ".", i)
          } else {
            apg <- paste0(apg, "+ap", ap$Ncoefs[j, k], ".", i)
          }
        }
      }
      VSmodel <- paste0(VSmodel, apg, "\n")
    }
  }

  cat("Lavvan multi-sample model syntax created...\n")

  VSmodel <- list("Model_spec" = VSmodel, "AP" = ap)

  VSmodel
}


# Set constraints for user-selected additional parameters

vs_set_equal <- function(model = NULL, ngroups = NULL, effect.equal = NULL) {

  if (!is.null(effect.equal)) {
    equal_model <- paste0(model, "\n# constraints\n")
    for (i in 1:length(effect.equal)) {
      for (j in 1:(ngroups - 1)) {
        equal_model <- paste0(equal_model, "  ap", effect.equal[i], ".", j, "==ap", effect.equal[i], ".", j + 1, "\n")
      }
    }
  }

  cat("Lavvan multi-sample model syntax created...\n")

  equal_model
}
