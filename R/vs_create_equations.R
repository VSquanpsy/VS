# vs_create_equations
#
# Create equations for the computation of the conditional effects

vs_create_equations <- function(vs.env = NULL) {

  # Transform paths and coefficients
  all_transformed <- 0
  while (all_transformed == 0) {

    # Search for plugs
    found_mod <- 0
    for (i in 1:vs.env$nCptPaths) {
      if (vs.env$EqnPathtype[i] == 2) {
        found_mod <- 1
        plug <- vs.env$EqnPathfrom[i]

        # Search for 2nd plug on plug
        found_top <- 0
        for (j in 1:vs.env$nCptPaths) {
          if (vs.env$EqnPathtype[j] == 2 && vs.env$EqnPathto[j] == i) {
            found_top <- 1
          }
        }

        # If no 2nd plug on the plug, add moderator to equation path
        if (found_top == 0) {
          vs.env$EqnPathtype[i] <- vs.env$EqnPathtype[vs.env$EqnPathto[i]]
          vs.env$EqnPathnvars[i] <- vs.env$EqnPathnvars[i] + 1
          vs.env$EqnPathvar[i, vs.env$EqnPathnvars[i]] <- vs.env$EqnPathfrom[vs.env$EqnPathto[i]]
          vs.env$EqnPathto[i] <- vs.env$EqnPathto[vs.env$EqnPathto[i]]
        }
      }
    }

    if (found_mod == 0) {
      all_transformed <- 1
    }
  }

  # Find equation focal IV, DV, and Moderator
  for (i in 1:vs.env$N) {
    for (j in 1:vs.env$nCptPaths) {
      if (vs.env$EqnPathfrom[j] == i) {
        vs.env$focalIV[i] <- 1
      } else if (vs.env$EqnPathto[j] == i) {
        vs.env$focalDV[i] <- 1

        # Add equation terms with coefficient and variable list in DV
        if (vs.env$Eqnnterms[i] >= max(3, vs.env$Eqnnterms)) {
          vs.env$Eqnnterms[i] <- vs.env$Eqnnterms[i] + 1
          vs.env$Eqnncoefs <- cbind(vs.env$Eqnncoefs, rep(0, vs.env$N))
          arr <- array(0, c(vs.env$N, vs.env$Eqnnterms[i], 3))
          arr[, -vs.env$Eqnnterms[i], ] <- vs.env$Eqncoef
          vs.env$Eqncoef <- arr
          vs.env$Eqnnvars <- cbind(vs.env$Eqnnvars, rep(0, vs.env$N))
          arr <- array(0, c(vs.env$N, vs.env$Eqnnterms[i], 3))
          arr[, -vs.env$Eqnnterms[i], ] <- vs.env$Eqnvar
          vs.env$Eqnvar <- arr
          arr <- array(0, c(vs.env$N, vs.env$Eqnnterms[i], 3))
          arr[, -vs.env$Eqnnterms[i], ] <- vs.env$EqnIV
          vs.env$EqnIV <- arr
          arr <- array(0, c(vs.env$N, vs.env$Eqnnterms[i], 3))
          arr[, -vs.env$Eqnnterms[i], ] <- vs.env$EqnMod
          vs.env$EqnMod <- arr
          vs.env$EqntermIV <- cbind(vs.env$EqntermIV, rep(0, vs.env$N))
        } else {
          vs.env$Eqnnterms[i] <- vs.env$Eqnnterms[i] + 1
        }
        vs.env$Eqnncoefs[i, vs.env$Eqnnterms[i]] <- 1
        vs.env$Eqnnvars[i, vs.env$Eqnnterms[i]] <- 1
        vs.env$Eqncoef[i, vs.env$Eqnnterms[i], vs.env$Eqnncoefs[i, vs.env$Eqnnterms[i]]] <- j
        vs.env$Eqnvar[i, vs.env$Eqnnterms[i], vs.env$Eqnnvars[i, vs.env$Eqnnterms[i]]] <- vs.env$EqnPathfrom[j]

        # If there are variables in equation path, mark the last variable as moderator
        # Otherwise, mark the last variable as IV and store the IV
        if (vs.env$EqnPathnvars[j] > 0) {
          vs.env$EqnMod[i, vs.env$Eqnnterms[i], vs.env$Eqnnvars[i, vs.env$Eqnnterms[i]]] <- 1
        } else {
          vs.env$EqnIV[i, vs.env$Eqnnterms[i], vs.env$Eqnnvars[i, vs.env$Eqnnterms[i]]] <- 1
          vs.env$EqntermIV[i, vs.env$Eqnnterms[i]] <- vs.env$Eqnvar[i, vs.env$Eqnnterms[i], vs.env$Eqnnvars[i, vs.env$Eqnnterms[i]]]
        }

        # Add the variable in the equation path into the variable list,
        # mark all added variables as moderator except the last one
        # mark the last variable as IV and store the IV
        if (vs.env$EqnPathnvars[j] > 0) {
          for (k in 1:vs.env$EqnPathnvars[j]) {
            vs.env$Eqnnvars[i, vs.env$Eqnnterms[i]] <- vs.env$Eqnnvars[i, vs.env$Eqnnterms[i]] + 1
            vs.env$Eqnvar[i, vs.env$Eqnnterms[i], vs.env$Eqnnvars[i, vs.env$Eqnnterms[i]]] <- vs.env$EqnPathvar[j, k]
            if (k < vs.env$EqnPathnvars[j]) {
              vs.env$EqnMod[i, vs.env$Eqnnterms[i], vs.env$Eqnnvars[i, vs.env$Eqnnterms[i]]] <- 1
            } else {
              vs.env$EqnIV[i, vs.env$Eqnnterms[i], vs.env$Eqnnvars[i, vs.env$Eqnnterms[i]]] <- 1
              vs.env$EqntermIV[i, vs.env$Eqnnterms[i]] <- vs.env$Eqnvar[i, vs.env$Eqnnterms[i], vs.env$Eqnnvars[i, vs.env$Eqnnterms[i]]]
            }
          }
        }
      }
    }

    # If a variable is both IV and DV, set it as neither a focal IV nor focal DV
    if (vs.env$focalIV[i] == 1 && vs.env$focalDV[i] == 1) {
      vs.env$focalIV[i] <- 0
      vs.env$focalDV[i] <- 0
    } else if (vs.env$focalIV[i] == 1) {

      # If a focal IV is in a plug, set it as a focal Moderator
      if (vs.env$isPlug[i] == 1) {
        vs.env$focalMod[i] <- 1
      }
    }
  }

  vs.env
}


# vs_decompose_equations
#
# Decompose equations into the conditional effects for the user specified paths

vs_decompose_equations <- function(vs.env = NULL, matrices = NULL) {

  vs.env$EqnDir <- matrix(0, vs.env$N, max(vs.env$Eqnnterms, 3))
  vs.env$EqnKeep <- matrix(0, vs.env$N, max(vs.env$Eqnnterms, 3))

  for (i in 1:vs.env$N) {
    if (vs.env$Eqnnterms[i] > 0) {
      for (j in 1:vs.env$Eqnnterms[i]) {

        # Identify direct effects
        if (vs.env$Eqnnvars[i, j] == 1) {
          vs.env$EqnDir[i, j] <- 1
        }
        vs.env$EqnKeep[i, j] <- 1
        for (k in 1:vs.env$Eqnnterms[i]) {

          # Keep only highest order interactions of IVs
          foundV <- 0
          if (j != k && vs.env$Eqnnvars[i, j] < vs.env$Eqnnvars[i, k] && vs.env$EqntermIV[i, j] == vs.env$EqntermIV[i, k]) {
            matched <- rep(0, vs.env$Eqnnvars[i, k])
            for (l in 1:vs.env$Eqnnvars[i, j]) {
              for (m in 1:vs.env$Eqnnvars[i, k]) {
                if (vs.env$Eqnvar[i, j, l] == vs.env$Eqnvar[i, k, m] && matched[m] == 0) {
                  matched[m] <- 1
                  foundV <- foundV + 1
                  break
                }
              }
            }
          }
          if (foundV == vs.env$Eqnnvars[i, j]) {
            vs.env$EqnKeep[i, j] <- 0
          }
        }
      }
    }
  }

  # Decompose equations into the conditional effects for each user specified paths
  vs.env$nEffPCombo <- rep(0, vs.env$nUserSpecPaths)
  vs.env$nEffSCombo <- matrix(0, vs.env$nUserSpecPaths, 1)
  vs.env$nEffCombo <- array(0, c(vs.env$nUserSpecPaths, 1, 1))
  vs.env$nEffTotal <- array(0, c(vs.env$nUserSpecPaths, 1, 1))
  vs.env$EffCombo <- array(0, c(vs.env$nUserSpecPaths, 1, 1, 1))
  vs.env$EffTotal <- array(0, c(vs.env$nUserSpecPaths, 1, 1, 1))
  for (a in 1:vs.env$nUserSpecPaths) {

    # Direct effects for main conditional effect terms
    vs.env <- vs_decompose_direct(vs.env, a)

    # Highest order interactions for main conditional effect terms
    vs.env <- vs_decompose_interactions(vs.env, a)

    # Move focal IV to path, pick up paths with focal DV, and tidy up effect orders
    vs.env <- vs_clean_effects(vs.env, a)

    # create and store conditional effect paths and mark effects with same path and sources
    vs.env <- vs_declare_effects(vs.env, a)
  }

  # add additional effect terms from each main conditional effect term
  vs.env <- vs_add_effect_terms(vs.env, matrices)

  cat("Effect decomposition completed...\n")

  vs.env
}


# vs_decompose_direct
#
# Decompose equations of direct effects into the conditional effects for the user specified paths

vs_decompose_direct <- function(vs.env = NULL, EffectID = NULL) {

  # Pick up Direct effects
  for (i in 1:vs.env$N) {
    if (i == vs.env$UserSpecDV[EffectID]) {
      for (j in 1:vs.env$Eqnnterms[i]) {
        if (vs.env$EqnDir[i, j] == 1) {
          vs.env$nEff <- vs.env$nEff + 1
          vs.env$nEffPaths[vs.env$nEff] <- 1
          vs.env$nEffGroups[vs.env$nEff] <- 0
          vs.env$nEffterms[vs.env$nEff] <- 1
          vs.env$EffDel[vs.env$nEff] <- 0
          vs.env$nEffvars[vs.env$nEff] <- vs.env$Eqnnvars[i, j]
          vs.env$Effvar <- rbind(vs.env$Effvar, c(vs.env$Eqnvar[i, j, ], rep(0, max(ncol(vs.env$Effvar), 3)-3)))
          vs.env$EffIV <- rbind(vs.env$EffIV, c(vs.env$EqnIV[i, j, ], rep(0, max(ncol(vs.env$EffIV), 3)-3)))
          vs.env$EffMod <- rbind(vs.env$EffMod, c(vs.env$EqnMod[i, j, ], rep(0, max(ncol(vs.env$EffMod), 3)-3)))
          vs.env$EffGroup <- rbind(vs.env$EffGroup, rep(0, max(ncol(vs.env$EffGroup), 3)))
          vs.env$EffOrder <- rbind(vs.env$EffOrder, rep(0, max(ncol(vs.env$EffOrder), 3)))
          if (vs.env$nEff > 1) {
            vs.env$EffPath <- rbind(vs.env$EffPath, rep(0, ncol(vs.env$EffPath)))
            vs.env$nEffMPaths <- rbind(vs.env$nEffMPaths, rep(0, max(ncol(vs.env$nEffMPaths), 3)))
            arr <- array(0, c(vs.env$nEff, dim(vs.env$EffMPath)[2], dim(vs.env$EffMPath)[3]))
            arr[-vs.env$nEff, , ] <- vs.env$EffMPath
            vs.env$EffMPath <- arr
            arr <- array(0, c(vs.env$nEff, dim(vs.env$Effcoef)[2], dim(vs.env$Effcoef)[3]))
            arr[-vs.env$nEff, , ] <- vs.env$Effcoef
            vs.env$Effcoef <- arr
            arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutX)[2], dim(vs.env$EffoutX)[3]))
            arr[-vs.env$nEff, , ] <- vs.env$EffoutX
            vs.env$EffoutX <- arr
            arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutY)[2], dim(vs.env$EffoutY)[3]))
            arr[-vs.env$nEff, , ] <- vs.env$EffoutY
            vs.env$EffoutY <- arr
          }
          vs.env$EffPath[vs.env$nEff, vs.env$nEffPaths[vs.env$nEff]] <- i
          vs.env$nEffcoefs <- rbind(vs.env$nEffcoefs, rep(0, max(ncol(vs.env$nEffcoefs), 3)))
          vs.env$nEffcoefs[vs.env$nEff, 1] <- vs.env$Eqnncoefs[i, j]
          vs.env$Effcoef[vs.env$nEff, 1, 1:vs.env$nEffcoefs[vs.env$nEff, 1]] <- vs.env$Eqncoef[i, j, 1:vs.env$nEffcoefs[vs.env$nEff, 1]]
          vs.env$EffoutX[vs.env$nEff, 1, 1:vs.env$nEffcoefs[vs.env$nEff, 1]] <- rep(vs.env$EqntermIV[i, j], vs.env$nEffcoefs[vs.env$nEff, 1])
          vs.env$EffoutY[vs.env$nEff, 1, 1:vs.env$nEffcoefs[vs.env$nEff, 1]] <- rep(i, vs.env$nEffcoefs[vs.env$nEff, 1])
        }
      }
    }
  }

  # Decomposition of Direct effects
  all_decomposed <- 0
  start <- 1
  while (all_decomposed == 0) {
    all_decomposed <- 1
    if (EffectID > 1) {
      start <- vs.env$uEff[EffectID-1] + 1
    }
    if (vs.env$nEff >= start) {
      for (i in start:vs.env$nEff) {
        j <- vs.env$nEffvars[i]
        while (j > 0) {
          if (vs.env$Effvar[i, j] > 0) {
            if (vs.env$Effvar[i, j] != vs.env$UserSpecIV[EffectID] && vs.env$focalIV[vs.env$Effvar[i, j]] == 0) {
              all_decomposed <- 0

              # Move mediator to path
              mediator <- vs.env$Effvar[i, j]
              vs.env$nEffPaths[i] <- vs.env$nEffPaths[i] + 1
              if (vs.env$nEffPaths[i] > ncol(vs.env$EffPath)) {
                vs.env$EffPath <- cbind(vs.env$EffPath, rep(0, nrow(vs.env$EffPath)))
              }
              vs.env$EffPath[i, vs.env$nEffPaths[i]] <- mediator

              # Remove mediator from variables
              if (j < vs.env$nEffvars[i]) {
                vs.env$Effvar[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$Effvar[i, (j+1):vs.env$nEffvars[i]]
                vs.env$EffIV[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$EffIV[i, (j+1):vs.env$nEffvars[i]]
              }
              vs.env$Effvar[i, vs.env$nEffvars[i]] <- 0
              vs.env$EffIV[i, vs.env$nEffvars[i]] <- 0
              vs.env$nEffvars[i] <- vs.env$nEffvars[i] - 1

              # Find the first direct effect term
              for (k in 1:vs.env$Eqnnterms[mediator]) {
                if (vs.env$EqnDir[mediator, k] == 1) {
                  first_term <- k
                  break
                }
              }

              # Add terms if more than one direct effect terms
              if (first_term < vs.env$Eqnnterms[mediator]) {
                for (k in (first_term+1):vs.env$Eqnnterms[mediator]) {
                  if (vs.env$EqnDir[mediator, k] == 1) {
                    vs.env$nEff <- vs.env$nEff + 1
                    vs.env$nEffPaths[vs.env$nEff] <- vs.env$nEffPaths[i]
                    vs.env$nEffGroups[vs.env$nEff] <- 0
                    vs.env$nEffterms[vs.env$nEff] <- 1
                    vs.env$EffDel[vs.env$nEff] <- 0
                    vs.env$nEffvars[vs.env$nEff] <- vs.env$nEffvars[i]
                    vs.env$Effvar <- rbind(vs.env$Effvar, vs.env$Effvar[i, ])
                    vs.env$EffIV <- rbind(vs.env$EffIV, c(rep(1, vs.env$nEffvars[i]), rep(0, (ncol(vs.env$EffIV)-vs.env$nEffvars[i]))))
                    vs.env$EffMod <- rbind(vs.env$EffMod, rep(0, ncol(vs.env$EffMod)))
                    vs.env$EffGroup <- rbind(vs.env$EffGroup, rep(0, ncol(vs.env$EffGroup)))
                    vs.env$EffOrder <- rbind(vs.env$EffOrder, rep(0, ncol(vs.env$EffOrder)))
                    if (vs.env$nEff > 1) {
                      vs.env$EffPath <- rbind(vs.env$EffPath, rep(0, ncol(vs.env$EffPath)))
                      vs.env$nEffMPaths <- rbind(vs.env$nEffMPaths, rep(0, ncol(vs.env$nEffMPaths)))
                      arr <- array(0, c(vs.env$nEff, dim(vs.env$EffMPath)[2], dim(vs.env$EffMPath)[3]))
                      arr[-vs.env$nEff, , ] <- vs.env$EffMPath
                      vs.env$EffMPath <- arr
                      arr <- array(0, c(vs.env$nEff, dim(vs.env$Effcoef)[2], dim(vs.env$Effcoef)[3]))
                      arr[-vs.env$nEff, , ] <- vs.env$Effcoef
                      vs.env$Effcoef <- arr
                      arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutX)[2], dim(vs.env$EffoutX)[3]))
                      arr[-vs.env$nEff, , ] <- vs.env$EffoutX
                      vs.env$EffoutX <- arr
                      arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutY)[2], dim(vs.env$EffoutY)[3]))
                      arr[-vs.env$nEff, , ] <- vs.env$EffoutY
                      vs.env$EffoutY <- arr
                    }
                    vs.env$EffPath[vs.env$nEff, 1:vs.env$nEffPaths[vs.env$nEff]] <- vs.env$EffPath[i, 1:vs.env$nEffPaths[i]]
                    vs.env$nEffcoefs <- rbind(vs.env$nEffcoefs, rep(0, ncol(vs.env$nEffcoefs)))
                    vs.env$nEffcoefs[vs.env$nEff, 1] <- vs.env$nEffcoefs[i, 1]
                    vs.env$Effcoef[vs.env$nEff, 1, 1:vs.env$nEffcoefs[vs.env$nEff, 1]] <- vs.env$Effcoef[i, 1, 1:vs.env$nEffcoefs[i, 1]]
                    vs.env$EffoutX[vs.env$nEff, 1, 1:vs.env$nEffcoefs[vs.env$nEff, 1]] <- vs.env$EffoutX[i, 1, 1:vs.env$nEffcoefs[i, 1]]
                    vs.env$EffoutY[vs.env$nEff, 1, 1:vs.env$nEffcoefs[vs.env$nEff, 1]] <- vs.env$EffoutY[i, 1, 1:vs.env$nEffcoefs[i, 1]]

                    vs.env$Effvar[vs.env$nEff, 1:vs.env$Eqnnvars[mediator, k]+vs.env$nEffvars[vs.env$nEff]] <- vs.env$Eqnvar[mediator, k, 1:vs.env$Eqnnvars[mediator, k]]
                    vs.env$EffIV[vs.env$nEff, 1:vs.env$Eqnnvars[mediator, k]+vs.env$nEffvars[vs.env$nEff]] <- rep(1, vs.env$Eqnnvars[mediator, k])
                    vs.env$EffMod[vs.env$nEff, 1:vs.env$Eqnnvars[mediator, k]+vs.env$nEffvars[vs.env$nEff]] <- rep(0, vs.env$Eqnnvars[mediator, k])
                    vs.env$nEffvars[vs.env$nEff] <- vs.env$nEffvars[vs.env$nEff] + vs.env$Eqnnvars[mediator, k]
                    ncoefs <- vs.env$nEffcoefs[vs.env$nEff, 1]
                    for (l in 1:vs.env$Eqnncoefs[mediator, k]) {
                      ncoefs <- ncoefs + 1
                      if (ncoefs > dim(vs.env$Effcoef)[3]) {
                        arr <- array(0, c(vs.env$nEff, dim(vs.env$Effcoef)[2], ncoefs))
                        arr[, , -ncoefs] <- vs.env$Effcoef
                        vs.env$Effcoef <- arr
                        arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutX)[2], ncoefs))
                        arr[, , -ncoefs] <- vs.env$EffoutX
                        vs.env$EffoutX <- arr
                        arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutY)[2], ncoefs))
                        arr[, , -ncoefs] <- vs.env$EffoutY
                        vs.env$EffoutY <- arr
                      }
                    }
                    vs.env$Effcoef[vs.env$nEff, 1, 1:vs.env$Eqnncoefs[mediator, k]+vs.env$nEffcoefs[vs.env$nEff, 1]] <- vs.env$Eqncoef[mediator, k, 1:vs.env$Eqnncoefs[mediator, k]]
                    vs.env$EffoutX[vs.env$nEff, 1, 1:vs.env$Eqnncoefs[mediator, k]+vs.env$nEffcoefs[vs.env$nEff, 1]] <- rep(vs.env$EqntermIV[mediator, k], vs.env$Eqnncoefs[mediator, k])
                    vs.env$EffoutY[vs.env$nEff, 1, 1:vs.env$Eqnncoefs[mediator, k]+vs.env$nEffcoefs[vs.env$nEff, 1]] <- rep(mediator, vs.env$Eqnncoefs[mediator, k])
                    vs.env$nEffcoefs[vs.env$nEff, 1] <- vs.env$nEffcoefs[vs.env$nEff, 1] + vs.env$Eqnncoefs[mediator, k]
                  }
                }
              }

              # Add the first direct term
              vs.env$Effvar[i, 1:vs.env$Eqnnvars[mediator, first_term]+vs.env$nEffvars[i]] <- vs.env$Eqnvar[mediator, first_term, 1:vs.env$Eqnnvars[mediator, first_term]]
              vs.env$EffIV[i, 1:vs.env$Eqnnvars[mediator, first_term]+vs.env$nEffvars[i]] <- rep(1, vs.env$Eqnnvars[mediator, first_term])
              vs.env$EffMod[i, 1:vs.env$Eqnnvars[mediator, first_term]+vs.env$nEffvars[i]] <- rep(0, vs.env$Eqnnvars[mediator, first_term])
              vs.env$nEffvars[i] <- vs.env$nEffvars[i] + vs.env$Eqnnvars[mediator, first_term]
              ncoefs <- vs.env$nEffcoefs[i, 1]
              for (l in 1:vs.env$Eqnncoefs[mediator, first_term]) {
                ncoefs <- ncoefs + 1
                if (ncoefs > dim(vs.env$Effcoef)[3]) {
                  arr <- array(0, c(vs.env$nEff, dim(vs.env$Effcoef)[2], ncoefs))
                  arr[, , -ncoefs] <- vs.env$Effcoef
                  vs.env$Effcoef <- arr
                  arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutX)[2], ncoefs))
                  arr[, , -ncoefs] <- vs.env$EffoutX
                  vs.env$EffoutX <- arr
                  arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutY)[2], ncoefs))
                  arr[, , -ncoefs] <- vs.env$EffoutY
                  vs.env$EffoutY <- arr
                }
              }
              vs.env$Effcoef[i, 1, 1:vs.env$Eqnncoefs[mediator, first_term]+vs.env$nEffcoefs[i, 1]] <- vs.env$Eqncoef[mediator, first_term, 1:vs.env$Eqnncoefs[mediator, first_term]]
              vs.env$EffoutX[i, 1, 1:vs.env$Eqnncoefs[mediator, first_term]+vs.env$nEffcoefs[i, 1]] <- rep(vs.env$EqntermIV[mediator, first_term], vs.env$Eqnncoefs[mediator, first_term])
              vs.env$EffoutY[i, 1, 1:vs.env$Eqnncoefs[mediator, first_term]+vs.env$nEffcoefs[i, 1]] <- rep(mediator, vs.env$Eqnncoefs[mediator, first_term])
              vs.env$nEffcoefs[i, 1] <- vs.env$nEffcoefs[i, 1] + vs.env$Eqnncoefs[mediator, first_term]
            }
          }
          j <- j - 1
        }
      }
    }
  }
  vs.env$uDirEff[EffectID] <- vs.env$nEff

  vs.env
}


# vs_decompose_interactions
#
# Decompose equations of highest order interactions into the conditional effects for the user specified paths

vs_decompose_interactions <- function(vs.env = NULL, EffectID = NULL) {

  # Pick up highest order interactions
  for (i in 1:vs.env$N) {
    if (vs.env$Eqnnterms[i] > 0) {
      for (j in 1:vs.env$Eqnnterms[i]) {
        if (vs.env$EqnKeep[i, j] == 1) {
          vs.env$nEff <- vs.env$nEff + 1
          vs.env$nEffPaths[vs.env$nEff] <- 1
          vs.env$nEffGroups[vs.env$nEff] <- 1
          vs.env$nEffterms[vs.env$nEff] <- 1
          vs.env$EffDel[vs.env$nEff] <- 0
          vs.env$nEffvars[vs.env$nEff] <- vs.env$Eqnnvars[i, j]
          vs.env$Effvar <- rbind(vs.env$Effvar, c(vs.env$Eqnvar[i, j, ], rep(0, max(ncol(vs.env$Effvar), 3)-3)))
          vs.env$EffIV <- rbind(vs.env$EffIV, c(vs.env$EqnIV[i, j, ], rep(0, max(ncol(vs.env$EffIV), 3)-3)))
          vs.env$EffMod <- rbind(vs.env$EffMod, c(vs.env$EqnMod[i, j, ], rep(0, max(ncol(vs.env$EffMod), 3)-3)))
          vs.env$EffGroup <- rbind(vs.env$EffGroup, c(rep(1, vs.env$nEffvars[vs.env$nEff]), rep(0, max(ncol(vs.env$EffGroup), 3)-vs.env$nEffvars[vs.env$nEff])))
          vs.env$EffOrder <- rbind(vs.env$EffOrder, rep(0, max(ncol(vs.env$EffOrder), 3)))
          vs.env$nEffMPaths <- rbind(vs.env$nEffMPaths, rep(0, max(ncol(vs.env$nEffMPaths), 3)))
          if (vs.env$nEff > 1) {
            vs.env$EffPath <- rbind(vs.env$EffPath, rep(0, ncol(vs.env$EffPath)))
            arr <- array(0, c(vs.env$nEff, dim(vs.env$EffMPath)[2], dim(vs.env$EffMPath)[3]))
            arr[-vs.env$nEff, , ] <- vs.env$EffMPath
            vs.env$EffMPath <- arr
            arr <- array(0, c(vs.env$nEff, dim(vs.env$Effcoef)[2], dim(vs.env$Effcoef)[3]))
            arr[-vs.env$nEff, , ] <- vs.env$Effcoef
            vs.env$Effcoef <- arr
            arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutX)[2], dim(vs.env$EffoutX)[3]))
            arr[-vs.env$nEff, , ] <- vs.env$EffoutX
            vs.env$EffoutX <- arr
            arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutY)[2], dim(vs.env$EffoutY)[3]))
            arr[-vs.env$nEff, , ] <- vs.env$EffoutY
            vs.env$EffoutY <- arr
          }
          vs.env$EffPath[vs.env$nEff, vs.env$nEffPaths[vs.env$nEff]] <- i
          for (k in 1:vs.env$nEffvars[vs.env$nEff]) {
            vs.env$EffOrder[vs.env$nEff, k] <- vs.env$Eqnnvars[i, j] - k
            if (vs.env$EffMod[vs.env$nEff, k] == 1) {
              vs.env$nEffMPaths[vs.env$nEff, k] <- vs.env$nEffMPaths[vs.env$nEff, k] + 1
              vs.env$EffMPath[vs.env$nEff, k, vs.env$nEffMPaths[vs.env$nEff, k]] <- vs.env$Effvar[vs.env$nEff, k]
            }
          }
          vs.env$nEffcoefs <- rbind(vs.env$nEffcoefs, rep(0, max(ncol(vs.env$nEffcoefs), 3)))
          vs.env$nEffcoefs[vs.env$nEff, 1] <- vs.env$Eqnncoefs[i, j]
          vs.env$Effcoef[vs.env$nEff, 1, 1:vs.env$nEffcoefs[vs.env$nEff, 1]] <- vs.env$Eqncoef[i, j, 1:vs.env$nEffcoefs[vs.env$nEff, 1]]
          vs.env$EffoutX[vs.env$nEff, 1, 1:vs.env$nEffcoefs[vs.env$nEff, 1]] <- rep(vs.env$EqntermIV[i, j], vs.env$nEffcoefs[vs.env$nEff, 1])
          vs.env$EffoutY[vs.env$nEff, 1, 1:vs.env$nEffcoefs[vs.env$nEff, 1]] <- rep(i, vs.env$nEffcoefs[vs.env$nEff, 1])
        }
      }
    }
  }

# Check for highest order interactions
#  for (i in (vs.env$uDirEff[EffectID]+1):vs.env$nEff) {
#    statement <- paste0(vs.env$Varnames[vs.env$EffPath[i, 1]], ": ")
#    for (j in 1:vs.env$nEffcoefs[i, 1]) {
#      if (j == 1) {
#        statement <- paste0(statement, "a", vs.env$Effcoef[i, 1, j])
#      } else {
#        statement <- paste0(statement, " * a", vs.env$Effcoef[i, 1, j])
#      }
#    }
#    for (j in 1:vs.env$nEffvars[i]) {
#      statement <- paste0(statement, " * ", vs.env$Varnames[vs.env$Effvar[i, j]])
#      if (vs.env$EffIV[i, j] == 1) {
#        statement <- paste0(statement, "(I)")
#      }
#      if (vs.env$EffMod[i, j] == 1) {
#        statement <- paste0(statement, "(M)")
#      }
#      statement <- paste0(statement, "(", vs.env$EffOrder[i, j])
#      if (vs.env$EffMod[i, j] == 1) {
#        statement <- paste0(statement, ",", vs.env$EffMPath[i, j, 1])
#      }
#      statement <- paste0(statement, ")")
#    }
#    cat(statement, "\n")
#  }

# Check for focal IV and focal moderators
#  statement <- "Focal IV:"
#  for (i in 1:vs.env$N) {
#    if (vs.env$focalIV[i] == 1) {
#      statement <- paste(statement, vs.env$Varnames[i])
#    }
#  }
#  cat(statement, "\n")
#  statement <- "Focal Mod:"
#  for (i in 1:vs.env$N) {
#    if (vs.env$focalMod[i] == 1) {
#      statement <- paste(statement, vs.env$Varnames[i])
#    }
#  }
#  cat(statement, "\n")

  # Decomposition of moderators in highest order interactions
  all_decomposed <- 0
  start <- vs.env$uDirEff[EffectID] + 1
  while (all_decomposed == 0) {
    all_decomposed <- 1
    if (vs.env$nEff >= start) {
      for (i in start:vs.env$nEff) {
        j <- vs.env$nEffvars[i]
        while (j > 0) {
          if (vs.env$EffMod[i, j] == 1 && vs.env$focalMod[vs.env$Effvar[i, j]] == 0) {
            all_decomposed <- 0

            # Store moderator info
            moderator <- vs.env$Effvar[i, j]
            modgroup <- vs.env$EffGroup[i, j]
            modorder <- vs.env$EffOrder[i, j]
            nmodpaths <- vs.env$nEffMPaths[i, j]
            if (nmodpaths > 0) {
              modpath <- vs.env$EffMPath[i, j, 1:vs.env$nEffMPaths[i, j]]
            }

            # Remove moderator from variables
            if (j < vs.env$nEffvars[i]) {
              vs.env$Effvar[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$Effvar[i, (j+1):vs.env$nEffvars[i]]
              vs.env$EffIV[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$EffIV[i, (j+1):vs.env$nEffvars[i]]
              vs.env$EffMod[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$EffMod[i, (j+1):vs.env$nEffvars[i]]
              vs.env$EffGroup[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$EffGroup[i, (j+1):vs.env$nEffvars[i]]
              vs.env$EffOrder[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$EffOrder[i, (j+1):vs.env$nEffvars[i]]
              vs.env$EffMPath[i, j:(vs.env$nEffvars[i]-1), ] <- vs.env$EffMPath[i, (j+1):vs.env$nEffvars[i], ]
              vs.env$nEffMPaths[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$nEffMPaths[i, (j+1):vs.env$nEffvars[i]]

            }
            vs.env$Effvar[i, vs.env$nEffvars[i]] <- 0
            vs.env$EffIV[i, vs.env$nEffvars[i]] <- 0
            vs.env$EffMod[i, vs.env$nEffvars[i]] <- 0
            vs.env$EffGroup[i, vs.env$nEffvars[i]] <- 0
            vs.env$EffOrder[i, vs.env$nEffvars[i]] <- 0
            if (vs.env$nEffMPaths[i, vs.env$nEffvars[i]] > 0) {
              vs.env$EffMPath[i, vs.env$nEffvars[i], 1:vs.env$nEffMPaths[i, vs.env$nEffvars[i]]] <- rep(0, vs.env$nEffMPaths[i, vs.env$nEffvars[i]])
              vs.env$nEffMPaths[i, vs.env$nEffvars[i]] <- 0
            }
            vs.env$nEffvars[i] <- vs.env$nEffvars[i] - 1

            # Find the first highest order interaction
            for (k in 1:vs.env$Eqnnterms[moderator]) {
              if (vs.env$EqnKeep[moderator, k] == 1) {
                first_term <- k
                break
              }
            }

            # Add terms if more than one highest order interactions
            if (first_term < vs.env$Eqnnterms[moderator]) {
              for (k in (first_term+1):vs.env$Eqnnterms[moderator]) {
                if (vs.env$EqnKeep[moderator, k] == 1) {
                  vs.env$nEff <- vs.env$nEff + 1
                  vs.env$nEffPaths[vs.env$nEff] <- vs.env$nEffPaths[i]
                  vs.env$nEffvars[vs.env$nEff] <- vs.env$nEffvars[i]
                  vs.env$nEffGroups[vs.env$nEff] <- 1
                  vs.env$nEffterms[vs.env$nEff] <- 1
                  vs.env$EffDel[vs.env$nEff] <- 0

                  vs.env$Effvar <- rbind(vs.env$Effvar, vs.env$Effvar[i, ])
                  vs.env$EffIV <- rbind(vs.env$EffIV, vs.env$EffIV[i, ])
                  vs.env$EffMod <- rbind(vs.env$EffMod, vs.env$EffMod[i, ])
                  vs.env$EffGroup <- rbind(vs.env$EffGroup, vs.env$EffGroup[i, ])
                  vs.env$EffOrder <- rbind(vs.env$EffOrder, vs.env$EffOrder[i, ])
                  vs.env$nEffMPaths <- rbind(vs.env$nEffMPaths, vs.env$nEffMPaths[i, ])
                  if (vs.env$nEff > 1) {
                    vs.env$EffPath <- rbind(vs.env$EffPath, rep(0, ncol(vs.env$EffPath)))
                    arr <- array(0, c(vs.env$nEff, dim(vs.env$EffMPath)[2], dim(vs.env$EffMPath)[3]))
                    arr[-vs.env$nEff, , ] <- vs.env$EffMPath
                    vs.env$EffMPath <- arr
                    arr <- array(0, c(vs.env$nEff, dim(vs.env$Effcoef)[2], dim(vs.env$Effcoef)[3]))
                    arr[-vs.env$nEff, , ] <- vs.env$Effcoef
                    vs.env$Effcoef <- arr
                    arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutX)[2], dim(vs.env$EffoutX)[3]))
                    arr[-vs.env$nEff, , ] <- vs.env$EffoutX
                    vs.env$EffoutX <- arr
                    arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutY)[2], dim(vs.env$EffoutY)[3]))
                    arr[-vs.env$nEff, , ] <- vs.env$EffoutY
                    vs.env$EffoutY <- arr
                  }
                  if (vs.env$nEffvars[vs.env$nEff] > 0) {
                    for (l in 1:vs.env$nEffvars[vs.env$nEff]) {
                      if (vs.env$EffMod[vs.env$nEff, l] == 1) {
                        vs.env$EffMPath[vs.env$nEff, l, ] <- vs.env$EffMPath[i, l, ]
                      }
                    }
                  }
                  vs.env$EffPath[vs.env$nEff, 1:vs.env$nEffPaths[vs.env$nEff]] <- vs.env$EffPath[i, 1:vs.env$nEffPaths[i]]
                  vs.env$nEffcoefs <- rbind(vs.env$nEffcoefs, rep(0, ncol(vs.env$nEffcoefs)))
                  vs.env$nEffcoefs[vs.env$nEff, 1] <- vs.env$nEffcoefs[i, 1]
                  vs.env$Effcoef[vs.env$nEff, 1, 1:vs.env$nEffcoefs[vs.env$nEff, 1]] <- vs.env$Effcoef[i, 1, 1:vs.env$nEffcoefs[i, 1]]
                  vs.env$EffoutX[vs.env$nEff, 1, 1:vs.env$nEffcoefs[vs.env$nEff, 1]] <- vs.env$EffoutX[i, 1, 1:vs.env$nEffcoefs[i, 1]]
                  vs.env$EffoutY[vs.env$nEff, 1, 1:vs.env$nEffcoefs[vs.env$nEff, 1]] <- vs.env$EffoutY[i, 1, 1:vs.env$nEffcoefs[i, 1]]

                  if (vs.env$Eqnnvars[moderator, k] > 0) {
                    if (j <= vs.env$nEffvars[vs.env$nEff]) {
                      nvars <- vs.env$nEffvars[vs.env$nEff]
                      for (l in 1:vs.env$Eqnnvars[moderator, k]) {
                        nvars <- nvars + 1
                        if (nvars > ncol(vs.env$Effvar)) {
                          vs.env$Effvar <- cbind(vs.env$Effvar, rep(0, nrow(vs.env$Effvar)))
                          vs.env$EffIV <- cbind(vs.env$EffIV, rep(0, nrow(vs.env$EffIV)))
                          vs.env$EffMod <- cbind(vs.env$EffMod, rep(0, nrow(vs.env$EffMod)))
                          vs.env$EffGroup <- cbind(vs.env$EffGroup, rep(0, nrow(vs.env$EffGroup)))
                          vs.env$EffOrder <- cbind(vs.env$EffOrder, rep(0, nrow(vs.env$EffOrder)))
                          vs.env$nEffMPaths <- cbind(vs.env$nEffMPaths, rep(0, nrow(vs.env$nEffMPaths)))
                          arr <- array(0, c(vs.env$nEff, nvars, dim(vs.env$EffMPath)[3]))
                          arr[, -nvars, ] <- vs.env$EffMPath
                          vs.env$EffMPath <- arr
                        }
                      }
                      vs.env$Effvar[vs.env$nEff, j:vs.env$nEffvars[vs.env$nEff]+vs.env$Eqnnvars[moderator, k]] <- vs.env$Effvar[vs.env$nEff, j:vs.env$nEffvars[vs.env$nEff]]
                      vs.env$EffIV[vs.env$nEff, j:vs.env$nEffvars[vs.env$nEff]+vs.env$Eqnnvars[moderator, k]] <- vs.env$EffIV[vs.env$nEff, j:vs.env$nEffvars[vs.env$nEff]]
                      vs.env$EffMod[vs.env$nEff, j:vs.env$nEffvars[vs.env$nEff]+vs.env$Eqnnvars[moderator, k]] <- vs.env$EffMod[vs.env$nEff, j:vs.env$nEffvars[vs.env$nEff]]
                      vs.env$EffGroup[vs.env$nEff, j:vs.env$nEffvars[vs.env$nEff]+vs.env$Eqnnvars[moderator, k]] <- vs.env$EffGroup[vs.env$nEff, j:vs.env$nEffvars[vs.env$nEff]]
                      vs.env$EffOrder[vs.env$nEff, j:vs.env$nEffvars[vs.env$nEff]+vs.env$Eqnnvars[moderator, k]] <- vs.env$EffOrder[vs.env$nEff, j:vs.env$nEffvars[vs.env$nEff]]
                      vs.env$nEffMPaths[vs.env$nEff, j:vs.env$nEffvars[vs.env$nEff]+vs.env$Eqnnvars[moderator, k]] <- vs.env$nEffMPaths[vs.env$nEff, j:vs.env$nEffvars[vs.env$nEff]]
                      vs.env$EffMPath[vs.env$nEff, j:vs.env$nEffvars[vs.env$nEff]+vs.env$Eqnnvars[moderator, k], ] <- vs.env$EffMPath[vs.env$nEff, j:vs.env$nEffvars[vs.env$nEff], ]
                    }
                    vs.env$nEffvars[vs.env$nEff] <- vs.env$nEffvars[vs.env$nEff] + vs.env$Eqnnvars[moderator, k]

                    vs.env$Effvar[vs.env$nEff, j:(j+vs.env$Eqnnvars[moderator, k]-1)] <- vs.env$Eqnvar[moderator, k, 1:vs.env$Eqnnvars[moderator, k]]
                    vs.env$EffIV[vs.env$nEff, j:(j+vs.env$Eqnnvars[moderator, k]-1)] <- rep(0, vs.env$Eqnnvars[moderator, k])
                    vs.env$EffMod[vs.env$nEff, j:(j+vs.env$Eqnnvars[moderator, k]-1)] <- rep(1, vs.env$Eqnnvars[moderator, k])
                    vs.env$EffGroup[vs.env$nEff, j:(j+vs.env$Eqnnvars[moderator, k]-1)] <- rep(1, vs.env$Eqnnvars[moderator, k])
                    vs.env$EffOrder[vs.env$nEff, j:(j+vs.env$Eqnnvars[moderator, k]-1)] <- (vs.env$Eqnnvars[moderator, k]-1+modorder):modorder
                    if (vs.env$Eqnnvars[moderator, k] > 1) {
                      vs.env$nEffMPaths[vs.env$nEff, j:(j+vs.env$Eqnnvars[moderator, k]-2)] <- rep(1, vs.env$Eqnnvars[moderator, k]-1)
                      vs.env$EffMPath[vs.env$nEff, j:(j+vs.env$Eqnnvars[moderator, k]-2), 1] <- vs.env$Effvar[vs.env$nEff, j:(j+vs.env$Eqnnvars[moderator, k]-2)]
                    }
                    vs.env$nEffMPaths[vs.env$nEff, j+vs.env$Eqnnvars[moderator, k]-1] <- nmodpaths + 1
                    if (nmodpaths > 0) {
                      if (nmodpaths + 1 > dim(vs.env$EffMPath)[3]) {
                        for (k in 1:(nmodpaths-dim(vs.env$EffMPath)[3]+1)) {
                          arr <- array(0, c(vs.env$nEff, dim(vs.env$EffMPath)[2], (dim(vs.env$EffMPath)[3]+1)))
                          arr[, , -(dim(vs.env$EffMPath)[3]+1)] <- vs.env$EffMPath
                          vs.env$EffMPath <- arr
                        }
                      }
                      vs.env$EffMPath[vs.env$nEff, j+vs.env$Eqnnvars[moderator, k]-1, 1:nmodpaths] <- modpath
                    }
                    vs.env$EffMPath[vs.env$nEff, j+vs.env$Eqnnvars[moderator, k]-1, nmodpaths+1] <- vs.env$Effvar[vs.env$nEff, j+vs.env$Eqnnvars[moderator, k]-1]
                  }

                  if (vs.env$Eqnncoefs[moderator, k] > 0) {
                    ncoefs <- vs.env$nEffcoefs[vs.env$nEff, 1]
                    for (l in 1:vs.env$Eqnncoefs[moderator, k]) {
                      ncoefs <- ncoefs + 1
                      if (ncoefs > dim(vs.env$Effcoef)[3]) {
                        arr <- array(0, c(vs.env$nEff, dim(vs.env$Effcoef)[2], ncoefs))
                        arr[, , -ncoefs] <- vs.env$Effcoef
                        vs.env$Effcoef <- arr
                        arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutX)[2], ncoefs))
                        arr[, , -ncoefs] <- vs.env$EffoutX
                        vs.env$EffoutX <- arr
                        arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutY)[2], ncoefs))
                        arr[, , -ncoefs] <- vs.env$EffoutY
                        vs.env$EffoutY <- arr
                      }
                    }
                    vs.env$Effcoef[vs.env$nEff, 1, 1:vs.env$Eqnncoefs[moderator, k]+vs.env$nEffcoefs[vs.env$nEff, 1]] <- vs.env$Eqncoef[moderator, k, 1:vs.env$Eqnncoefs[moderator, k]]
                    vs.env$EffoutX[vs.env$nEff, 1, 1:vs.env$Eqnncoefs[moderator, k]+vs.env$nEffcoefs[vs.env$nEff, 1]] <- rep(vs.env$EffoutX[vs.env$nEff, 1, 1], vs.env$Eqnncoefs[moderator, k])
                    vs.env$EffoutY[vs.env$nEff, 1, 1:vs.env$Eqnncoefs[moderator, k]+vs.env$nEffcoefs[vs.env$nEff, 1]] <- rep(vs.env$EffPath[i, 1], vs.env$Eqnncoefs[moderator, k])
                    vs.env$nEffcoefs[vs.env$nEff, 1] <- vs.env$nEffcoefs[vs.env$nEff, 1] + vs.env$Eqnncoefs[moderator, k]
                  }
                }
              }
            }

            # Add the first highest order interaction
            if (vs.env$Eqnnvars[moderator, first_term] > 0) {
              if (j <= vs.env$nEffvars[i]) {
                nvars <- vs.env$nEffvars[i]
                for (l in 1:vs.env$Eqnnvars[moderator, first_term]) {
                  nvars <- nvars + 1
                  if (nvars > ncol(vs.env$Effvar)) {
                    vs.env$Effvar <- cbind(vs.env$Effvar, rep(0, nrow(vs.env$Effvar)))
                    vs.env$EffIV <- cbind(vs.env$EffIV, rep(0, nrow(vs.env$EffIV)))
                    vs.env$EffMod <- cbind(vs.env$EffMod, rep(0, nrow(vs.env$EffMod)))
                    vs.env$EffGroup <- cbind(vs.env$EffGroup, rep(0, nrow(vs.env$EffGroup)))
                    vs.env$EffOrder <- cbind(vs.env$EffOrder, rep(0, nrow(vs.env$EffOrder)))
                    vs.env$nEffMPaths <- cbind(vs.env$nEffMPaths, rep(0, nrow(vs.env$nEffMPaths)))
                    arr <- array(0, c(vs.env$nEff, nvars, dim(vs.env$EffMPath)[3]))
                    arr[, -nvars, ] <- vs.env$EffMPath
                    vs.env$EffMPath <- arr
                  }
                }
                vs.env$Effvar[i, j:vs.env$nEffvars[i]+vs.env$Eqnnvars[moderator, first_term]] <- vs.env$Effvar[i, j:vs.env$nEffvars[i]]
                vs.env$EffIV[i, j:vs.env$nEffvars[i]+vs.env$Eqnnvars[moderator, first_term]] <- vs.env$EffIV[i, j:vs.env$nEffvars[i]]
                vs.env$EffMod[i, j:vs.env$nEffvars[i]+vs.env$Eqnnvars[moderator, first_term]] <- vs.env$EffMod[i, j:vs.env$nEffvars[i]]
                vs.env$EffGroup[i, j:vs.env$nEffvars[i]+vs.env$Eqnnvars[moderator, first_term]] <- vs.env$EffGroup[i, j:vs.env$nEffvars[i]]
                vs.env$EffOrder[i, j:vs.env$nEffvars[i]+vs.env$Eqnnvars[moderator, first_term]] <- vs.env$EffOrder[i, j:vs.env$nEffvars[i]]
                vs.env$nEffMPaths[i, j:vs.env$nEffvars[i]+vs.env$Eqnnvars[moderator, first_term]] <- vs.env$nEffMPaths[i, j:vs.env$nEffvars[i]]
                vs.env$EffMPath[i, j:vs.env$nEffvars[i]+vs.env$Eqnnvars[moderator, first_term], ] <- vs.env$EffMPath[i, j:vs.env$nEffvars[i], ]
              }
              vs.env$nEffvars[i] <- vs.env$nEffvars[i] + vs.env$Eqnnvars[moderator, first_term]

              vs.env$Effvar[i, j:(j+vs.env$Eqnnvars[moderator, first_term]-1)] <- vs.env$Eqnvar[moderator, first_term, 1:vs.env$Eqnnvars[moderator, first_term]]
              vs.env$EffIV[i, j:(j+vs.env$Eqnnvars[moderator, first_term]-1)] <- rep(0, vs.env$Eqnnvars[moderator, first_term])
              vs.env$EffMod[i, j:(j+vs.env$Eqnnvars[moderator, first_term]-1)] <- rep(1, vs.env$Eqnnvars[moderator, first_term])
              vs.env$EffGroup[i, j:(j+vs.env$Eqnnvars[moderator, first_term]-1)] <- rep(1, vs.env$Eqnnvars[moderator, first_term])
              vs.env$EffOrder[i, j:(j+vs.env$Eqnnvars[moderator, first_term]-1)] <- (vs.env$Eqnnvars[moderator, first_term]-1+modorder):modorder
              if (vs.env$Eqnnvars[moderator, first_term] > 1) {
                vs.env$nEffMPaths[i, j:(j+vs.env$Eqnnvars[moderator, first_term]-2)] <- rep(1, vs.env$Eqnnvars[moderator, first_term]-1)
                vs.env$EffMPath[i, j:(j+vs.env$Eqnnvars[moderator, first_term]-2), 1] <- vs.env$Effvar[i, j:(j+vs.env$Eqnnvars[moderator, first_term]-2)]
              }
              vs.env$nEffMPaths[i, j+vs.env$Eqnnvars[moderator, first_term]-1] <- nmodpaths + 1
              if (nmodpaths > 0) {
                if (nmodpaths + 1 > dim(vs.env$EffMPath)[3]) {
                  for (k in 1:(nmodpaths-dim(vs.env$EffMPath)[3]+1)) {
                    arr <- array(0, c(vs.env$nEff, dim(vs.env$EffMPath)[2], (dim(vs.env$EffMPath)[3]+1)))
                    arr[, , -(dim(vs.env$EffMPath)[3]+1)] <- vs.env$EffMPath
                    vs.env$EffMPath <- arr
                  }
                }
                vs.env$EffMPath[i, j+vs.env$Eqnnvars[moderator, first_term]-1, 1:nmodpaths] <- modpath
              }
              vs.env$EffMPath[i, j+vs.env$Eqnnvars[moderator, first_term]-1, nmodpaths+1] <- vs.env$Effvar[i, j+vs.env$Eqnnvars[moderator, first_term]-1]
            }

            if (vs.env$Eqnncoefs[moderator, first_term] > 0) {
              ncoefs <- vs.env$nEffcoefs[i, 1]
              for (k in 1:vs.env$Eqnncoefs[moderator, first_term]) {
                ncoefs <- ncoefs + 1
                if (ncoefs > dim(vs.env$Effcoef)[3]) {
                  arr <- array(0, c(vs.env$nEff, dim(vs.env$Effcoef)[2], ncoefs))
                  arr[, , -ncoefs] <- vs.env$Effcoef
                  vs.env$Effcoef <- arr
                  arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutX)[2], ncoefs))
                  arr[, , -ncoefs] <- vs.env$EffoutX
                  vs.env$EffoutX <- arr
                  arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutY)[2], ncoefs))
                  arr[, , -ncoefs] <- vs.env$EffoutY
                  vs.env$EffoutY <- arr
                }
              }
              vs.env$Effcoef[i, 1, 1:vs.env$Eqnncoefs[moderator, first_term]+vs.env$nEffcoefs[i, 1]] <- vs.env$Eqncoef[moderator, first_term, 1:vs.env$Eqnncoefs[moderator, first_term]]
              vs.env$EffoutX[i, 1, 1:vs.env$Eqnncoefs[moderator, first_term]+vs.env$nEffcoefs[i, 1]] <- rep(vs.env$EffoutX[i, 1, 1], vs.env$Eqnncoefs[moderator, first_term])
              vs.env$EffoutY[i, 1, 1:vs.env$Eqnncoefs[moderator, first_term]+vs.env$nEffcoefs[i, 1]] <- rep(vs.env$EffPath[i, 1], vs.env$Eqnncoefs[moderator, first_term])
              vs.env$nEffcoefs[i, 1] <- vs.env$nEffcoefs[i, 1] + vs.env$Eqnncoefs[moderator, first_term]
            }
          }
          j <- j - 1
        }
      }
    }
  }

# Check results
#  cat(paste("Moderator decomposed: N terms =", vs.env$nEff, "\n"))
#  start <- 1
#  if (EffectID > 1) {
#    start <- vs.env$uEff[EffectID-1] + 1
#  }
#  for (i in start:vs.env$nEff) {
#    statement <- vs.env$Varnames[vs.env$EffPath[i, 1]]
#    if (vs.env$nEffPaths[i] > 1) {
#      for (j in 2:vs.env$nEffPaths[i]) {
#        statement <- paste(statement, "<-", vs.env$Varnames[vs.env$EffPath[i, j]])
#      }
#    }
#    statement <- paste0(statement, ":")
#    if (vs.env$nEffcoefs[i, 1] > 0) {
#      for (j in 1:vs.env$nEffcoefs[i, 1]) {
#        statement <- paste0(statement, " a", vs.env$Effcoef[i, 1, j], "(", vs.env$Varnames[vs.env$EffoutX[i, 1, j]], ",", vs.env$Varnames[vs.env$EffoutY[i, 1, j]], ")")
#      }
#    }
#    if (vs.env$nEffvars[i] > 0) {
#      for (j in 1:vs.env$nEffvars[i]) {
#        statement <- paste0(statement, " ", vs.env$Varnames[vs.env$Effvar[i, j]])
#        if (vs.env$nEffMPaths[i, j] > 0) {
#          for (k in 1:vs.env$nEffMPaths[i, j]) {
#            statement <- paste0(statement, "(", vs.env$Varnames[vs.env$EffMPath[i, j, k]], ")")
#          }
#        }
#      }
#    }
#    cat(i, statement, "\n")
#  }

  # Decomposition of mediators in highest order interactions
  all_decomposed <- 0
  start <- vs.env$uDirEff[EffectID] + 1
  while (all_decomposed == 0) {
    all_decomposed <- 1
    if (vs.env$nEff >= start) {
      for (i in start:vs.env$nEff) {
        j <- vs.env$nEffvars[i]
        while (j > 0) {
          if (vs.env$EffIV[i, j] == 1 && vs.env$Effvar[i, j] != vs.env$UserSpecIV[EffectID] && vs.env$focalIV[vs.env$Effvar[i, j]] == 0) {
            all_decomposed <- 0

            # Move mediator to path
            mediator <- vs.env$Effvar[i, j]
            vs.env$nEffPaths[i] <- vs.env$nEffPaths[i] + 1
            vs.env$EffPath[i, vs.env$nEffPaths[i]] <- mediator

            # Remove mediator from variables
            if (j < vs.env$nEffvars[i]) {
              vs.env$Effvar[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$Effvar[i, (j+1):vs.env$nEffvars[i]]
              vs.env$EffIV[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$EffIV[i, (j+1):vs.env$nEffvars[i]]
              vs.env$EffMod[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$EffMod[i, (j+1):vs.env$nEffvars[i]]
              vs.env$EffGroup[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$EffGroup[i, (j+1):vs.env$nEffvars[i]]
              vs.env$EffOrder[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$EffOrder[i, (j+1):vs.env$nEffvars[i]]
              vs.env$EffMPath[i, j:(vs.env$nEffvars[i]-1), ] <- vs.env$EffMPath[i, (j+1):vs.env$nEffvars[i], ]
              vs.env$nEffMPaths[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$nEffMPaths[i, (j+1):vs.env$nEffvars[i]]
            }
            vs.env$Effvar[i, vs.env$nEffvars[i]] <- 0
            vs.env$EffIV[i, vs.env$nEffvars[i]] <- 0
            vs.env$EffMod[i, vs.env$nEffvars[i]] <- 0
            vs.env$EffGroup[i, vs.env$nEffvars[i]] <- 0
            vs.env$EffOrder[i, vs.env$nEffvars[i]] <- 0
            if (vs.env$nEffMPaths[i, vs.env$nEffvars[i]] > 0) {
              vs.env$EffMPath[i, vs.env$nEffvars[i], 1:vs.env$nEffMPaths[i, vs.env$nEffvars[i]]] <- rep(0, vs.env$nEffMPaths[i, vs.env$nEffvars[i]])
              vs.env$nEffMPaths[i, vs.env$nEffvars[i]] <- 0
            }
            vs.env$nEffvars[i] <- vs.env$nEffvars[i] - 1

            # Find the first highest order interaction
            for (k in (vs.env$uDirEff[EffectID]+1):vs.env$nEff) {
              if (vs.env$EffPath[k, 1] == mediator) {
                first_term <- k
                break
              }
            }

            # Add terms if more than one highest order interactions
            if (first_term < vs.env$nEff) {
              for (k in (first_term+1):vs.env$nEff) {
                if (vs.env$EffPath[k, 1] == mediator) {
                  vs.env$nEff <- vs.env$nEff + 1
                  vs.env$nEffPaths[vs.env$nEff] <- vs.env$nEffPaths[i]
                  vs.env$nEffvars[vs.env$nEff] <- vs.env$nEffvars[i]
                  vs.env$nEffGroups[vs.env$nEff] <- vs.env$nEffGroups[i] + 1
                  vs.env$nEffterms[vs.env$nEff] <- 1
                  vs.env$EffDel[vs.env$nEff] <- 0

                  vs.env$Effvar <- rbind(vs.env$Effvar, vs.env$Effvar[i, ])
                  vs.env$EffIV <- rbind(vs.env$EffIV, vs.env$EffIV[i, ])
                  vs.env$EffMod <- rbind(vs.env$EffMod, vs.env$EffMod[i, ])
                  vs.env$EffGroup <- rbind(vs.env$EffGroup, vs.env$EffGroup[i, ])
                  vs.env$EffOrder <- rbind(vs.env$EffOrder, vs.env$EffOrder[i, ])
                  vs.env$nEffMPaths <- rbind(vs.env$nEffMPaths, vs.env$nEffMPaths[i, ])
                  if (vs.env$nEff > 1) {
                    vs.env$EffPath <- rbind(vs.env$EffPath, rep(0, ncol(vs.env$EffPath)))
                    arr <- array(0, c(vs.env$nEff, dim(vs.env$EffMPath)[2], dim(vs.env$EffMPath)[3]))
                    arr[-vs.env$nEff, , ] <- vs.env$EffMPath
                    vs.env$EffMPath <- arr
                    arr <- array(0, c(vs.env$nEff, dim(vs.env$Effcoef)[2], dim(vs.env$Effcoef)[3]))
                    arr[-vs.env$nEff, , ] <- vs.env$Effcoef
                    vs.env$Effcoef <- arr
                    arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutX)[2], dim(vs.env$EffoutX)[3]))
                    arr[-vs.env$nEff, , ] <- vs.env$EffoutX
                    vs.env$EffoutX <- arr
                    arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutY)[2], dim(vs.env$EffoutY)[3]))
                    arr[-vs.env$nEff, , ] <- vs.env$EffoutY
                    vs.env$EffoutY <- arr
                  }
                  if (vs.env$nEffvars[vs.env$nEff] > 0) {
                    for (l in 1:vs.env$nEffvars[vs.env$nEff]) {
                      if (vs.env$EffMod[vs.env$nEff, l] == 1) {
                        vs.env$EffMPath[vs.env$nEff, l, ] <- vs.env$EffMPath[i, l, ]
                      }
                    }
                  }
                  vs.env$EffPath[vs.env$nEff, 1:vs.env$nEffPaths[vs.env$nEff]] <- vs.env$EffPath[i, 1:vs.env$nEffPaths[i]]
                  if (vs.env$nEffPaths[k] > 1) {
                    for (l in 2:vs.env$nEffPaths[k]) {
                      if (vs.env$nEffPaths[vs.env$nEff] + l - 1 > ncol(vs.env$EffPath)) {
                        vs.env$EffPath <- cbind(vs.env$EffPath, rep(0, nrow(vs.env$EffPath)))
                      }
                    }
                    vs.env$EffPath[vs.env$nEff, 1:(vs.env$nEffPaths[k]-1)+vs.env$nEffPaths[vs.env$nEff]] <- vs.env$EffPath[k, 2:vs.env$nEffPaths[k]]
                    vs.env$nEffPaths[vs.env$nEff] <- vs.env$nEffPaths[vs.env$nEff] + vs.env$nEffPaths[k] - 1
                  }
                  vs.env$nEffcoefs <- rbind(vs.env$nEffcoefs, rep(0, ncol(vs.env$nEffcoefs)))
                  vs.env$nEffcoefs[vs.env$nEff, 1] <- vs.env$nEffcoefs[i, 1]
                  vs.env$Effcoef[vs.env$nEff, 1, 1:vs.env$nEffcoefs[vs.env$nEff, 1]] <- vs.env$Effcoef[i, 1, 1:vs.env$nEffcoefs[i, 1]]
                  vs.env$EffoutX[vs.env$nEff, 1, 1:vs.env$nEffcoefs[vs.env$nEff, 1]] <- vs.env$EffoutX[i, 1, 1:vs.env$nEffcoefs[i, 1]]
                  vs.env$EffoutY[vs.env$nEff, 1, 1:vs.env$nEffcoefs[vs.env$nEff, 1]] <- vs.env$EffoutY[i, 1, 1:vs.env$nEffcoefs[i, 1]]

                  if (vs.env$nEffvars[k] > 1) {
                    if (vs.env$nEffvars[vs.env$nEff] >= 1) {
                      nvars <- vs.env$nEffvars[vs.env$nEff]
                      for (l in 1:(vs.env$nEffvars[k]-1)) {
                        nvars <- nvars + 1
                        if (nvars > ncol(vs.env$Effvar)) {
                          vs.env$Effvar <- cbind(vs.env$Effvar, rep(0, nrow(vs.env$Effvar)))
                          vs.env$EffIV <- cbind(vs.env$EffIV, rep(0, nrow(vs.env$EffIV)))
                          vs.env$EffMod <- cbind(vs.env$EffMod, rep(0, nrow(vs.env$EffMod)))
                          vs.env$EffGroup <- cbind(vs.env$EffGroup, rep(0, nrow(vs.env$EffGroup)))
                          vs.env$EffOrder <- cbind(vs.env$EffOrder, rep(0, nrow(vs.env$EffOrder)))
                          vs.env$nEffMPaths <- cbind(vs.env$nEffMPaths, rep(0, nrow(vs.env$nEffMPaths)))
                          arr <- array(0, c(vs.env$nEff, nvars, dim(vs.env$EffMPath)[3]))
                          arr[, -nvars, ] <- vs.env$EffMPath
                          vs.env$EffMPath <- arr
                        }
                      }
                      vs.env$Effvar[vs.env$nEff, vs.env$nEffvars[k]:(vs.env$nEffvars[k]+vs.env$nEffvars[vs.env$nEff]-1)] <- vs.env$Effvar[vs.env$nEff, 1:vs.env$nEffvars[vs.env$nEff]]
                      vs.env$EffIV[vs.env$nEff, vs.env$nEffvars[k]:(vs.env$nEffvars[k]+vs.env$nEffvars[vs.env$nEff]-1)] <- vs.env$EffIV[vs.env$nEff, 1:vs.env$nEffvars[vs.env$nEff]]
                      vs.env$EffMod[vs.env$nEff, vs.env$nEffvars[k]:(vs.env$nEffvars[k]+vs.env$nEffvars[vs.env$nEff]-1)] <- vs.env$EffMod[vs.env$nEff, 1:vs.env$nEffvars[vs.env$nEff]]
                      vs.env$EffGroup[vs.env$nEff, vs.env$nEffvars[k]:(vs.env$nEffvars[k]+vs.env$nEffvars[vs.env$nEff]-1)] <- vs.env$EffGroup[vs.env$nEff, 1:vs.env$nEffvars[vs.env$nEff]]
                      vs.env$EffOrder[vs.env$nEff, vs.env$nEffvars[k]:(vs.env$nEffvars[k]+vs.env$nEffvars[vs.env$nEff]-1)] <- vs.env$EffOrder[vs.env$nEff, 1:vs.env$nEffvars[vs.env$nEff]]
                      vs.env$nEffMPaths[vs.env$nEff, vs.env$nEffvars[k]:(vs.env$nEffvars[k]+vs.env$nEffvars[vs.env$nEff]-1)] <- vs.env$nEffMPaths[vs.env$nEff, 1:vs.env$nEffvars[vs.env$nEff]]
                      vs.env$EffMPath[vs.env$nEff, vs.env$nEffvars[k]:(vs.env$nEffvars[k]+vs.env$nEffvars[vs.env$nEff]-1), ] <- vs.env$EffMPath[vs.env$nEff, 1:vs.env$nEffvars[vs.env$nEff], ]
                    }
                    vs.env$nEffvars[vs.env$nEff] <- vs.env$nEffvars[vs.env$nEff] + vs.env$nEffvars[k] - 1

                    vs.env$Effvar[vs.env$nEff, 1:(vs.env$nEffvars[k]-1)] <- vs.env$Effvar[k, 1:(vs.env$nEffvars[k]-1)]
                    vs.env$EffIV[vs.env$nEff, 1:(vs.env$nEffvars[k]-1)] <- vs.env$EffIV[k, 1:(vs.env$nEffvars[k]-1)]
                    vs.env$EffMod[vs.env$nEff, 1:(vs.env$nEffvars[k]-1)] <- vs.env$EffMod[k, 1:(vs.env$nEffvars[k]-1)]
                    vs.env$EffGroup[vs.env$nEff, 1:(vs.env$nEffvars[k]-1)] <- rep(vs.env$nEffGroups[vs.env$nEff], vs.env$nEffvars[k]-1)
                    vs.env$EffOrder[vs.env$nEff, 1:(vs.env$nEffvars[k]-1)] <- vs.env$EffOrder[k, 1:(vs.env$nEffvars[k]-1)]
                    vs.env$nEffMPaths[vs.env$nEff, 1:(vs.env$nEffvars[k]-1)] <- vs.env$nEffMPaths[k, 1:(vs.env$nEffvars[k]-1)]
                    vs.env$EffMPath[vs.env$nEff, 1:(vs.env$nEffvars[k]-1), ] <- vs.env$EffMPath[k, 1:(vs.env$nEffvars[k]-1), ]
                  }
                  vs.env$nEffvars[vs.env$nEff] <- vs.env$nEffvars[vs.env$nEff] + 1
                  if (vs.env$nEffvars[vs.env$nEff] > ncol(vs.env$Effvar)) {
                    vs.env$Effvar <- cbind(vs.env$Effvar, rep(0, nrow(vs.env$Effvar)))
                    vs.env$EffIV <- cbind(vs.env$EffIV, rep(0, nrow(vs.env$EffIV)))
                    vs.env$EffMod <- cbind(vs.env$EffMod, rep(0, nrow(vs.env$EffMod)))
                    vs.env$EffGroup <- cbind(vs.env$EffGroup, rep(0, nrow(vs.env$EffGroup)))
                    vs.env$EffOrder <- cbind(vs.env$EffOrder, rep(0, nrow(vs.env$EffOrder)))
                    vs.env$nEffMPaths <- cbind(vs.env$nEffMPaths, rep(0, nrow(vs.env$nEffMPaths)))
                    arr <- array(0, c(vs.env$nEff, (dim(vs.env$EffMPath)[2]+1), dim(vs.env$EffMPath)[3]))
                    arr[, -(dim(vs.env$EffMPath)[2]+1), ] <- vs.env$EffMPath
                    vs.env$EffMPath <- arr
                  }
                  vs.env$Effvar[vs.env$nEff, vs.env$nEffvars[vs.env$nEff]] <- vs.env$Effvar[k, vs.env$nEffvars[k]]
                  vs.env$EffIV[vs.env$nEff, vs.env$nEffvars[vs.env$nEff]] <- vs.env$EffIV[k, vs.env$nEffvars[k]]
                  vs.env$EffMod[vs.env$nEff, vs.env$nEffvars[vs.env$nEff]] <- vs.env$EffMod[k, vs.env$nEffvars[k]]
                  vs.env$EffGroup[vs.env$nEff, vs.env$nEffvars[vs.env$nEff]] <- vs.env$nEffGroups[vs.env$nEff]
                  vs.env$EffOrder[vs.env$nEff, vs.env$nEffvars[vs.env$nEff]] <- vs.env$EffOrder[k, vs.env$nEffvars[k]]
                  vs.env$nEffMPaths[vs.env$nEff, vs.env$nEffvars[vs.env$nEff]] <- vs.env$nEffMPaths[k, vs.env$nEffvars[k]]
                  vs.env$EffMPath[vs.env$nEff, vs.env$nEffvars[vs.env$nEff], ] <- vs.env$EffMPath[k, vs.env$nEffvars[k], ]

                  ncoefs <- vs.env$nEffcoefs[vs.env$nEff, 1]
                  for (l in 1:vs.env$nEffcoefs[k, 1]) {
                    ncoefs <- ncoefs + 1
                    if (ncoefs > dim(vs.env$Effcoef)[3]) {
                      arr <- array(0, c(vs.env$nEff, dim(vs.env$Effcoef)[2], ncoefs))
                      arr[, , -ncoefs] <- vs.env$Effcoef
                      vs.env$Effcoef <- arr
                      arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutX)[2], ncoefs))
                      arr[, , -ncoefs] <- vs.env$EffoutX
                      vs.env$EffoutX <- arr
                      arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutY)[2], ncoefs))
                      arr[, , -ncoefs] <- vs.env$EffoutY
                      vs.env$EffoutY <- arr
                    }
                  }
                  vs.env$Effcoef[vs.env$nEff, 1, 1:vs.env$nEffcoefs[k, 1]+vs.env$nEffcoefs[vs.env$nEff, 1]] <- vs.env$Effcoef[k, 1, 1:vs.env$nEffcoefs[k, 1]]
                  vs.env$EffoutX[vs.env$nEff, 1, 1:vs.env$nEffcoefs[k, 1]+vs.env$nEffcoefs[vs.env$nEff, 1]] <- vs.env$EffoutX[k, 1, 1:vs.env$nEffcoefs[k, 1]]
                  vs.env$EffoutY[vs.env$nEff, 1, 1:vs.env$nEffcoefs[k, 1]+vs.env$nEffcoefs[vs.env$nEff, 1]] <- vs.env$EffoutY[k, 1, 1:vs.env$nEffcoefs[k, 1]]
                  vs.env$nEffcoefs[vs.env$nEff, 1] <- vs.env$nEffcoefs[vs.env$nEff, 1] + vs.env$nEffcoefs[k, 1]
                }
              }
            }

            # Add the first highest order interaction
            vs.env$nEffGroups[i] <- vs.env$nEffGroups[i] + 1
            if (vs.env$nEffPaths[first_term] > 1) {
              for (k in 2:vs.env$nEffPaths[first_term]) {
                if (vs.env$nEffPaths[i] + k - 1 > ncol(vs.env$EffPath)) {
                  vs.env$EffPath <- cbind(vs.env$EffPath, rep(0, nrow(vs.env$EffPath)))
                }
              }
              vs.env$EffPath[i, 1:(vs.env$nEffPaths[first_term]-1)+vs.env$nEffPaths[i]] <- vs.env$EffPath[first_term, 2:vs.env$nEffPaths[first_term]]
              vs.env$nEffPaths[i] <- vs.env$nEffPaths[i] + vs.env$nEffPaths[first_term] - 1
            }

            if (vs.env$nEffvars[first_term] > 1) {
              if (vs.env$nEffvars[i] >= 1) {
                nvars <- vs.env$nEffvars[i]
                for (l in 1:(vs.env$nEffvars[first_term]-1)) {
                  nvars <- nvars + 1
                  if (nvars > ncol(vs.env$Effvar)) {
                    vs.env$Effvar <- cbind(vs.env$Effvar, rep(0, nrow(vs.env$Effvar)))
                    vs.env$EffIV <- cbind(vs.env$EffIV, rep(0, nrow(vs.env$EffIV)))
                    vs.env$EffMod <- cbind(vs.env$EffMod, rep(0, nrow(vs.env$EffMod)))
                    vs.env$EffGroup <- cbind(vs.env$EffGroup, rep(0, nrow(vs.env$EffGroup)))
                    vs.env$EffOrder <- cbind(vs.env$EffOrder, rep(0, nrow(vs.env$EffOrder)))
                    vs.env$nEffMPaths <- cbind(vs.env$nEffMPaths, rep(0, nrow(vs.env$nEffMPaths)))
                    arr <- array(0, c(vs.env$nEff, nvars, dim(vs.env$EffMPath)[3]))
                    arr[, -nvars, ] <- vs.env$EffMPath
                    vs.env$EffMPath <- arr
                  }
                }
                vs.env$Effvar[i, vs.env$nEffvars[first_term]:(vs.env$nEffvars[first_term]+vs.env$nEffvars[i]-1)] <- vs.env$Effvar[i, 1:vs.env$nEffvars[i]]
                vs.env$EffIV[i, vs.env$nEffvars[first_term]:(vs.env$nEffvars[first_term]+vs.env$nEffvars[i]-1)] <- vs.env$EffIV[i, 1:vs.env$nEffvars[i]]
                vs.env$EffMod[i, vs.env$nEffvars[first_term]:(vs.env$nEffvars[first_term]+vs.env$nEffvars[i]-1)] <- vs.env$EffMod[i, 1:vs.env$nEffvars[i]]
                vs.env$EffGroup[i, vs.env$nEffvars[first_term]:(vs.env$nEffvars[first_term]+vs.env$nEffvars[i]-1)] <- vs.env$EffGroup[i, 1:vs.env$nEffvars[i]]
                vs.env$EffOrder[i, vs.env$nEffvars[first_term]:(vs.env$nEffvars[first_term]+vs.env$nEffvars[i]-1)] <- vs.env$EffOrder[i, 1:vs.env$nEffvars[i]]
                vs.env$nEffMPaths[i, vs.env$nEffvars[first_term]:(vs.env$nEffvars[first_term]+vs.env$nEffvars[i]-1)] <- vs.env$nEffMPaths[i, 1:vs.env$nEffvars[i]]
                vs.env$EffMPath[i, vs.env$nEffvars[first_term]:(vs.env$nEffvars[first_term]+vs.env$nEffvars[i]-1), ] <- vs.env$EffMPath[i, 1:vs.env$nEffvars[i], ]
              }
              vs.env$nEffvars[i] <- vs.env$nEffvars[i] + vs.env$nEffvars[first_term] - 1

              vs.env$Effvar[i, 1:(vs.env$nEffvars[first_term]-1)] <- vs.env$Effvar[first_term, 1:(vs.env$nEffvars[first_term]-1)]
              vs.env$EffIV[i, 1:(vs.env$nEffvars[first_term]-1)] <- vs.env$EffIV[first_term, 1:(vs.env$nEffvars[first_term]-1)]
              vs.env$EffMod[i, 1:(vs.env$nEffvars[first_term]-1)] <- vs.env$EffMod[first_term, 1:(vs.env$nEffvars[first_term]-1)]
              vs.env$EffGroup[i, 1:(vs.env$nEffvars[first_term]-1)] <- rep(vs.env$nEffGroups[i], vs.env$nEffvars[first_term]-1)
              vs.env$EffOrder[i, 1:(vs.env$nEffvars[first_term]-1)] <- vs.env$EffOrder[first_term, 1:(vs.env$nEffvars[first_term]-1)]
              vs.env$nEffMPaths[i, 1:(vs.env$nEffvars[first_term]-1)] <- vs.env$nEffMPaths[first_term, 1:(vs.env$nEffvars[first_term]-1)]
              vs.env$EffMPath[i, 1:(vs.env$nEffvars[first_term]-1), ] <- vs.env$EffMPath[first_term, 1:(vs.env$nEffvars[first_term]-1), ]
            }
            vs.env$nEffvars[i] <- vs.env$nEffvars[i] + 1
            if (vs.env$nEffvars[i] > ncol(vs.env$Effvar)) {
              vs.env$Effvar <- cbind(vs.env$Effvar, rep(0, nrow(vs.env$Effvar)))
              vs.env$EffIV <- cbind(vs.env$EffIV, rep(0, nrow(vs.env$EffIV)))
              vs.env$EffMod <- cbind(vs.env$EffMod, rep(0, nrow(vs.env$EffMod)))
              vs.env$EffGroup <- cbind(vs.env$EffGroup, rep(0, nrow(vs.env$EffGroup)))
              vs.env$EffOrder <- cbind(vs.env$EffOrder, rep(0, nrow(vs.env$EffOrder)))
              vs.env$nEffMPaths <- cbind(vs.env$nEffMPaths, rep(0, nrow(vs.env$nEffMPaths)))
              arr <- array(0, c(vs.env$nEff, (dim(vs.env$EffMPath)[2]+1), dim(vs.env$EffMPath)[3]))
              arr[, -(dim(vs.env$EffMPath)[2]+1), ] <- vs.env$EffMPath
              vs.env$EffMPath <- arr
            }
            vs.env$Effvar[i, vs.env$nEffvars[i]] <- vs.env$Effvar[first_term, vs.env$nEffvars[first_term]]
            vs.env$EffIV[i, vs.env$nEffvars[i]] <- vs.env$EffIV[first_term, vs.env$nEffvars[first_term]]
            vs.env$EffMod[i, vs.env$nEffvars[i]] <- vs.env$EffMod[first_term, vs.env$nEffvars[first_term]]
            vs.env$EffGroup[i, vs.env$nEffvars[i]] <- vs.env$nEffGroups[i]
            vs.env$EffOrder[i, vs.env$nEffvars[i]] <- vs.env$EffOrder[first_term, vs.env$nEffvars[first_term]]
            vs.env$nEffMPaths[i, vs.env$nEffvars[i]] <- vs.env$nEffMPaths[first_term, vs.env$nEffvars[first_term]]
            vs.env$EffMPath[i, vs.env$nEffvars[i], ] <- vs.env$EffMPath[first_term, vs.env$nEffvars[first_term], ]

            ncoefs <- vs.env$nEffcoefs[i, 1]
            for (k in 1:vs.env$nEffcoefs[first_term, 1]) {
              ncoefs <- ncoefs + 1
              if (ncoefs > dim(vs.env$Effcoef)[3]) {
                arr <- array(0, c(vs.env$nEff, dim(vs.env$Effcoef)[2], ncoefs))
                arr[, , -ncoefs] <- vs.env$Effcoef
                vs.env$Effcoef <- arr
                arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutX)[2], ncoefs))
                arr[, , -ncoefs] <- vs.env$EffoutX
                vs.env$EffoutX <- arr
                arr <- array(0, c(vs.env$nEff, dim(vs.env$EffoutY)[2], ncoefs))
                arr[, , -ncoefs] <- vs.env$EffoutY
                vs.env$EffoutY <- arr
              }
            }
            vs.env$Effcoef[i, 1, 1:vs.env$nEffcoefs[first_term, 1]+vs.env$nEffcoefs[i, 1]] <- vs.env$Effcoef[first_term, 1, 1:vs.env$nEffcoefs[first_term, 1]]
            vs.env$EffoutX[i, 1, 1:vs.env$nEffcoefs[first_term, 1]+vs.env$nEffcoefs[i, 1]] <- vs.env$EffoutX[first_term, 1, 1:vs.env$nEffcoefs[first_term, 1]]
            vs.env$EffoutY[i, 1, 1:vs.env$nEffcoefs[first_term, 1]+vs.env$nEffcoefs[i, 1]] <- vs.env$EffoutY[first_term, 1, 1:vs.env$nEffcoefs[first_term, 1]]

            vs.env$nEffcoefs[i, 1] <- vs.env$nEffcoefs[i, 1] + vs.env$nEffcoefs[first_term, 1]
          }
          j <- j - 1
        }
      }
    }
  }
  vs.env$uEff[EffectID] <- vs.env$nEff

# Check results
#  cat(paste("Meidator decomposed: N terms =", vs.env$nEff, "\n"))
#  start <- 1
#  if (EffectID > 1) {
#    start <- vs.env$uEff[EffectID-1] + 1
#  }
#  for (i in start:vs.env$nEff) {
#    statement <- vs.env$Varnames[vs.env$EffPath[i, 1]]
#    if (vs.env$nEffPaths[i] > 1) {
#      for (j in 2:vs.env$nEffPaths[i]) {
#        statement <- paste(statement, "<-", vs.env$Varnames[vs.env$EffPath[i, j]])
#      }
#    }
#    statement <- paste0(statement, ":")
#    if (vs.env$nEffcoefs[i, 1] > 0) {
#      for (j in 1:vs.env$nEffcoefs[i, 1]) {
#        statement <- paste0(statement, " a", vs.env$Effcoef[i, 1, j], "(", vs.env$Varnames[vs.env$EffoutX[i, 1, j]], ",", vs.env$Varnames[vs.env$EffoutY[i, 1, j]], ")")
#      }
#    }
#    if (vs.env$nEffvars[i] > 0) {
#      for (j in 1:vs.env$nEffvars[i]) {
#        statement <- paste0(statement, " ", vs.env$Varnames[vs.env$Effvar[i, j]])
#        if (vs.env$nEffMPaths[i, j] > 0) {
#          for (k in 1:vs.env$nEffMPaths[i, j]) {
#            statement <- paste0(statement, "(", vs.env$Varnames[vs.env$EffMPath[i, j, k]], ")")
#          }
#        }
#      }
#    }
#    cat(i, statement, "\n")
#  }

  vs.env
}


# vs_clean_effects
#
# Move focal IV to path, pick up paths with focal DV, and tidy up effect order

vs_clean_effects <- function(vs.env = NULL, EffectID = NULL) {

  # Move focal IV to path and pick up paths with focal DV
  effFDV <- rep(0, vs.env$nEff)
  effFIV <- rep(0, vs.env$nEff)
  start <- 1
  if (EffectID > 1) {
    start <- vs.env$uEff[EffectID-1] + 1
  }
  if (vs.env$nEff >= start) {
    for (i in start:vs.env$nEff) {
      effFDV[i] <- vs.env$EffPath[i, 1]
      found <- 0
      for (j in 1:vs.env$nEffvars[i]) {
        if (vs.env$Effvar[i, j] == vs.env$UserSpecIV[EffectID] && vs.env$EffIV[i, j] == 1) {
          found <- 1
          if (vs.env$EffPath[i, vs.env$nEffPaths[i]] != vs.env$Effvar[i, j]) {
            vs.env$nEffPaths[i] <- vs.env$nEffPaths[i] + 1
            if (vs.env$nEffPaths[i] > ncol(vs.env$EffPath)) {
              vs.env$EffPath <- cbind(vs.env$EffPath, rep(0, nrow(vs.env$EffPath)))
            }
            vs.env$EffPath[i, vs.env$nEffPaths[i]] <- vs.env$Effvar[i, j]
            effFIV[i] <- vs.env$Effvar[i, j]

            # Remove focal IV from variables
            if (j < vs.env$nEffvars[i]) {
              vs.env$Effvar[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$Effvar[i, (j+1):vs.env$nEffvars[i]]
              vs.env$EffIV[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$EffIV[i, (j+1):vs.env$nEffvars[i]]
              vs.env$EffMod[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$EffMod[i, (j+1):vs.env$nEffvars[i]]
              vs.env$EffGroup[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$EffGroup[i, (j+1):vs.env$nEffvars[i]]
              vs.env$EffOrder[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$EffOrder[i, (j+1):vs.env$nEffvars[i]]
              vs.env$nEffMPaths[i, j:(vs.env$nEffvars[i]-1)] <- vs.env$nEffMPaths[i, (j+1):vs.env$nEffvars[i]]
              vs.env$EffMPath[i, j:(vs.env$nEffvars[i]-1), ] <- vs.env$EffMPath[i, (j+1):vs.env$nEffvars[i], ]
            }
            vs.env$Effvar[i, vs.env$nEffvars[i]] <- 0
            vs.env$EffIV[i, vs.env$nEffvars[i]] <- 0
            vs.env$EffMod[i, vs.env$nEffvars[i]] <- 0
            vs.env$EffGroup[i, vs.env$nEffvars[i]] <- 0
            vs.env$EffOrder[i, vs.env$nEffvars[i]] <- 0
            vs.env$nEffMPaths[i, vs.env$nEffvars[i]] <- 0
            vs.env$EffMPath[i, vs.env$nEffvars[i], ] <- rep(0, dim(vs.env$EffMPath)[3])
            vs.env$nEffvars[i] <- vs.env$nEffvars[i] - 1
          }
        }
      }

      # Mark effects which do not match user specification as effects to be deleted
      if (found == 0) {
        vs.env$EffDel[i] <- 1
      }
      if (effFDV[i] != vs.env$UserSpecDV[EffectID]) {
        vs.env$EffDel[i] <- 1
      }
      if (vs.env$nEffvars[i] < 1 && i > vs.env$uDirEff[EffectID]) {
        vs.env$EffDel[i] <- 1
      }
    }
  }

  # Tidy up effect orders
  if (vs.env$nEff - 1 >= start) {
    for (i in start:(vs.env$nEff-1)) {
      if (vs.env$nEff >= i + 1) {
        for (j in (i+1):vs.env$nEff) {
          swap <- 0

          # Check IVs
          if (effFIV[i] > effFIV[j]) {
            swap <- 1
          } else if (effFIV[i] == effFIV[j]) {

            # Check DVs for same IVs
            if (effFDV[i] > effFDV[j]) {
              swap <- 1
            } else if (effFDV[i] == effFDV[j]) {

              # Check number of paths for same IVs and DVs
              if (vs.env$nEffPaths[i] > vs.env$nEffPaths[j]) {
                swap <- 1
              } else if (vs.env$nEffPaths[i] == vs.env$nEffPaths[j]) {

                # Check paths if same IVs, DVs, and number of paths
                k <- vs.env$nEffPaths[i] - 1
                foundV1 <- 0
                while (k > 1) {
                  if (vs.env$EffPath[i, k] > vs.env$EffPath[j, k] && foundV1 == vs.env$nEffPaths[i] - k - 1) {
                    swap <- 1
                    break
                  } else if (vs.env$EffPath[i, k] == vs.env$EffPath[j, k]) {
                    foundV1 <- foundV1 + 1
                  }
                  k <- k - 1
                }

                # Check sources if same IVs, DVs, and paths
                if (swap == 0 && foundV1 == vs.env$nEffPaths[i] - 2) {
                  if (vs.env$nEffGroups[i] > vs.env$nEffGroups[j]) {
                    swap <- 1
                  } else if (vs.env$nEffGroups[i] == vs.env$nEffGroups[j]) {
                    if (vs.env$nEffvars[i] > vs.env$nEffvars[j]) {
                      swap <- 1
                    } else if (vs.env$nEffvars[i] == vs.env$nEffvars[j]) {
                      foundV2 <- 0
                      if (vs.env$nEffvars[i] > 0) {
                        for (k in 1:vs.env$nEffvars[i]) {
                          if (foundV2 == k - 1) {
                            if (vs.env$Effvar[i, k] > vs.env$Effvar[j, k]) {
                              swap <- 1
                              break
                            } else if (vs.env$Effvar[i, k] == vs.env$Effvar[j, k]) {
                              if (vs.env$EffGroup[i, k] > vs.env$EffGroup[j, k]) {
                                swap <- 1
                                break
                              } else if (vs.env$EffGroup[i, k] == vs.env$EffGroup[j, k]) {
                                foundV2 <- foundV2 + 1
                              }
                            }
                          }
                        }
                      }

                      # Check moderation paths if same IVs, DVs, paths, and sources
                      if (swap == 0 && foundV2 == vs.env$nEffvars[i]) {
                        foundV3 <- 0
                        if (vs.env$nEffvars[i] > 0) {
                          for (k in 1:vs.env$nEffvars[i]) {
                            if (foundV3 == k - 1) {
                              if (vs.env$nEffMPaths[i, k] > vs.env$nEffMPaths[j, k]) {
                                swap <- 1
                                break
                              } else if (vs.env$nEffMPaths[i, k] == vs.env$nEffMPaths[j, k]) {
                                l <- vs.env$nEffMPaths[i, k] - 1
                                foundV4 <- 0
                                while (l > 0) {
                                  if (vs.env$EffMPath[i, k, l] > vs.env$EffMPath[j, k, l] && foundV4 == vs.env$nEffMPaths[i] - l - 1) {
                                    swap <- 1
                                    break
                                  } else if (vs.env$EffMPath[i, k, l] == vs.env$EffMPath[j, k, l]) {
                                    foundV4 <- foundV4 + 1
                                  }
                                  l <- l - 1
                                }
                                if (foundV4 == vs.env$nEffMPaths[i, k] - 1) {
                                  foundV3 <- foundV3 + 1
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }

          # Swap effect order if needed
          if (swap == 1) {

            # Copy path i to temp
            tempn <- vs.env$nEffPaths[i]
            tempv <- vs.env$EffPath[i, ]

            # Copy path j to path i
            vs.env$nEffPaths[i] <- vs.env$nEffPaths[j]
            vs.env$EffPath[i, ] <- vs.env$EffPath[j, ]

            # Copy temp to path j
            vs.env$nEffPaths[j] <- tempn
            vs.env$EffPath[j, ] <- tempv

            # Copy source i to temp
            tempn <- vs.env$nEffvars[i]
            tempg <- vs.env$nEffGroups[i]
            tempv <- vs.env$Effvar[i, ]
            tempiv <- vs.env$EffIV[i, ]
            tempmod <- vs.env$EffMod[i, ]
            tempv1 <- vs.env$nEffMPaths[i, ]
            tempv2 <- vs.env$EffGroup[i, ]
            tempv3 <- vs.env$EffOrder[i, ]
            tempm <- vs.env$EffMPath[i, , ]

            # Copy source j to source i
            vs.env$nEffvars[i] <- vs.env$nEffvars[j]
            vs.env$nEffGroups[i] <- vs.env$nEffGroups[j]
            vs.env$Effvar[i, ] <- vs.env$Effvar[j, ]
            vs.env$EffIV[i, ] <- vs.env$EffIV[j, ]
            vs.env$EffMod[i, ] <- vs.env$EffMod[j, ]
            vs.env$nEffMPaths[i, ] <- vs.env$nEffMPaths[j, ]
            vs.env$EffGroup[i, ] <- vs.env$EffGroup[j, ]
            vs.env$EffOrder[i, ] <- vs.env$EffOrder[j, ]
            vs.env$EffMPath[i, , ] <- vs.env$EffMPath[j, , ]

            # Copy temp to source j
            vs.env$nEffvars[j] <- tempn
            vs.env$nEffGroups[j] <- tempg
            vs.env$Effvar[j, ] <- tempv
            vs.env$EffIV[j, ] <- tempiv
            vs.env$EffMod[j, ] <- tempmod
            vs.env$nEffMPaths[j, ] <- tempv1
            vs.env$EffGroup[j, ] <- tempv2
            vs.env$EffOrder[j, ] <- tempv3
            vs.env$EffMPath[j, , ] <- tempm

            # Copy coef i to temp
            tempn <- vs.env$nEffcoefs[i, 1]
            tempm <- vs.env$Effcoef[i, 1, ]
            tempm1 <- vs.env$EffoutX[i, 1, ]
            tempm2 <- vs.env$EffoutY[i, 1, ]

            # Copy coef j to coef i
            vs.env$nEffcoefs[i, 1] <- vs.env$nEffcoefs[j, 1]
            vs.env$Effcoef[i, 1, ] <- vs.env$Effcoef[j, 1, ]
            vs.env$EffoutX[i, 1, ] <- vs.env$EffoutX[j, 1, ]
            vs.env$EffoutY[i, 1, ] <- vs.env$EffoutY[j, 1, ]

            # Copy temp to coef j
            vs.env$nEffcoefs[j, 1] <- tempn
            vs.env$Effcoef[j, 1, ] <- tempm
            vs.env$EffoutX[j, 1, ] <- tempm1
            vs.env$EffoutY[j, 1, ] <- tempm2

            # swap combination info
            temp <- vs.env$EffDel[i]
            vs.env$EffDel[i] <- vs.env$EffDel[j]
            vs.env$EffDel[j] <- temp
            temp <- effFIV[i]
            effFIV[i] <- effFIV[j]
            effFIV[j] <- temp
            temp <- effFDV[i]
            effFDV[i] <- effFDV[j]
            effFDV[j] <- temp
          }
        }
      }
    }
  }

# Check results
#  cat(paste("Effects cleaned: N terms =", vs.env$nEff, "\n"))
#  start <- 1
#  if (EffectID > 1) {
#    start <- vs.env$uEff[EffectID-1] + 1
#  }
#  for (i in start:vs.env$nEff) {
#    statement <- vs.env$Varnames[vs.env$EffPath[i, 1]]
#    if (vs.env$nEffPaths[i] > 1) {
#      for (j in 2:vs.env$nEffPaths[i]) {
#        statement <- paste(statement, "<-", vs.env$Varnames[vs.env$EffPath[i, j]])
#      }
#    }
#    statement <- paste0(statement, ":")
#    if (vs.env$nEffcoefs[i, 1] > 0) {
#      for (j in 1:vs.env$nEffcoefs[i, 1]) {
#        statement <- paste0(statement, " a", vs.env$Effcoef[i, 1, j], "(", vs.env$Varnames[vs.env$EffoutX[i, 1, j]], ",", vs.env$Varnames[vs.env$EffoutY[i, 1, j]], ")")
#      }
#    }
#    if (vs.env$nEffvars[i] > 0) {
#      for (j in 1:vs.env$nEffvars[i]) {
#        statement <- paste0(statement, " ", vs.env$Varnames[vs.env$Effvar[i, j]])
#        if (vs.env$nEffMPaths[i, j] > 0) {
#          for (k in 1:vs.env$nEffMPaths[i, j]) {
#            statement <- paste0(statement, "(", vs.env$Varnames[vs.env$EffMPath[i, j, k]], ")")
#          }
#        }
#      }
#    }
#    cat(i, statement, "\n")
#  }

  vs.env
}


# vs_declare_effects
#
# create and store conditional effect paths and mark effects with same path and sources

vs_declare_effects <- function(vs.env = NULL, EffectID = NULL) {

  # Store effect paths
  if (ncol(vs.env$EffDec) < ncol(vs.env$Effvar)) {
    for (i in (ncol(vs.env$EffDec)+1):ncol(vs.env$Effvar)) {
      vs.env$EffDec <- cbind(vs.env$EffDec, rep(0, nrow(vs.env$EffDec)))
    }
  }
  start <- 1
  if (EffectID > 1) {
    start <- vs.env$uEff[EffectID-1] + 1
  }
  if (vs.env$nEff >= start) {
    for (i in start:vs.env$nEff) {
      if (i > 1) {
        vs.env$EffDec <- rbind(vs.env$EffDec, rep(0, ncol(vs.env$EffDec)))
      }
      vs.env$EffPresent[i] <- "None"
      if (vs.env$EffDel[i] == 0 && vs.env$nEffvars[i] > 0) {
        for (j in 1:vs.env$nEffvars[i]) {
          found <- 0
          if (vs.env$EffOrder[i, j] > 0) {
            if (vs.env$nEffDecomposed[vs.env$EffOrder[i, j]] > 0) {
              for (k in 1:vs.env$nEffDecomposed[vs.env$EffOrder[i, j]]) {
                if (vs.env$nEffDecvars[vs.env$EffOrder[i, j], k] == vs.env$nEffMPaths[i, j]) {
                  foundV1 <- 0
                  if (vs.env$nEffMPaths[i, j] > 0) {
                    for (l in 1:vs.env$nEffMPaths[i, j]) {
                      if (vs.env$nEffDecvars[vs.env$EffOrder[i, j]] > 0) {
                        for (m in 1:vs.env$nEffDecvars[vs.env$EffOrder[i, j], k]) {
                          if (vs.env$EffMPath[i, j, l] == vs.env$EffDecvar[vs.env$EffOrder[i, j], k, m]) {
                            foundV1 <- foundV1 + 1
                            break
                          }
                        }
                      }
                    }
                  }
                  if (foundV1 == vs.env$nEffMPaths[i, j]) {
                    found <- 1
                    vs.env$EffDec[i, j] <- k
                    break
                  }
                }
              }
            }
          }
          if (found == 0) {
            if (vs.env$nEffDecomposed[vs.env$EffOrder[i, j]] + 1 > ncol(vs.env$nEffDecvars)) {
              vs.env$nEffDecvars <- cbind(vs.env$nEffDecvars, rep(0, nrow(vs.env$nEffDecvars)))
              ndec <- ncol(vs.env$nEffDecvars) + 1
              arr <- array(0, c(2, ndec, dim(vs.env$EffDecvar)[3]))
              arr[, -ndec, ] <- vs.env$EffDecvar
              vs.env$EffDecvar <- arr
            }
            vs.env$nEffDecomposed[vs.env$EffOrder[i, j]] <- vs.env$nEffDecomposed[vs.env$EffOrder[i, j]] + 1
            if (vs.env$nEffMPaths[i, j] > dim(vs.env$EffDecvar)[3]) {
              ndec <- dim(vs.env$EffDecvar)[3]
              for (k in (dim(vs.env$EffDecvar)[3]):vs.env$nEffMPaths[i, j]) {
                ndec <- ndec + 1
                arr <- array(0, c(2, dim(vs.env$EffDecvar)[2], ndec))
                arr[, , -ndec] <- vs.env$EffDecvar
                vs.env$EffDecvar <- arr
              }
            }
            vs.env$nEffDecvars[vs.env$EffOrder[i, j], vs.env$nEffDecomposed[vs.env$EffOrder[i, j]]] <- vs.env$nEffMPaths[i, j]
            if (vs.env$nEffMPaths[i, j] > 0) {
              vs.env$EffDecvar[vs.env$EffOrder[i, j], vs.env$nEffDecomposed[vs.env$EffOrder[i, j]], 1:vs.env$nEffMPaths[i, j]] <- vs.env$EffMPath[i, j, 1:vs.env$nEffMPaths[i, j]]
            }
            vs.env$EffDec[i, j] <- vs.env$nEffDecomposed[vs.env$EffOrder[i, j]]
          }
        }
        j <- 1
        ntempsec <- 0
        tempsec <- NULL
        if (vs.env$nEffvars[i] > 0) {
          vs.env$EffPresent[i] <- ""
          while (j <= vs.env$nEffvars[i]) {
            if (vs.env$EffOrder[i, j] == 2) {
              ntempsec <- ntempsec + 1
              tempsec[ntempsec] <- vs.env$EffDec[i, j]
            } else if (vs.env$EffOrder[i, j] == 1) {
              if (vs.env$EffPresent[i] != "") {
                vs.env$EffPresent[i] <- paste0(vs.env$EffPresent[i], "*")
              }
              vs.env$EffPresent[i] <- paste0(vs.env$EffPresent[i], "F", vs.env$EffDec[i, j])
              if (ntempsec > 0) {
                vs.env$EffPresent[i] <- paste0(vs.env$EffPresent[i], "[")
                for (k in 1:ntempsec) {
                  if (k > 1) {
                    vs.env$EffPresent[i] <- paste0(vs.env$EffPresent[i], ",")
                  }
                  vs.env$EffPresent[i] <- paste0(vs.env$EffPresent[i], "S", tempsec[k])
                }
                vs.env$EffPresent[i] <- paste0(vs.env$EffPresent[i], "]")
                ntempsec <- 0
              }
            }
            j <- j + 1
          }
        }
      }
    }
  }

  # Mark effects with same path and sources
  vs.env$nEffPCombo[EffectID] <- 0
  lasteff <- 0
  start <- 1
  if (EffectID > 1) {
    start <- vs.env$uEff[EffectID-1] + 1
  }
  if (vs.env$nEff >= start) {
    for (i in start:vs.env$nEff) {
      if (vs.env$EffDel[i] == 0) {
        if (lasteff == 0) {
          if (vs.env$nEffPCombo[EffectID] + 1 > ncol(vs.env$nEffSCombo)) {
            vs.env$nEffSCombo <- cbind(vs.env$nEffSCombo, rep(0, nrow(vs.env$nEffSCombo)))
            ncombo <- ncol(vs.env$nEffSCombo)
            arr <- array(0, c(vs.env$nUserSpecPaths, ncombo, dim(vs.env$nEffCombo)[3]))
            arr[, -ncombo, ] <- vs.env$nEffCombo
            vs.env$nEffCombo <- arr
            arr <- array(0, c(vs.env$nUserSpecPaths, ncombo, dim(vs.env$EffCombo)[3], dim(vs.env$EffCombo)[4]))
            arr[, -ncombo, , ] <- vs.env$EffCombo
            vs.env$EffCombo <- arr
          }
          vs.env$nEffPCombo[EffectID] <- vs.env$nEffPCombo[EffectID] + 1
          vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]] <- 1
          vs.env$nEffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]]] <- 1
          vs.env$EffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]], vs.env$nEffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]]]] <- i
        } else if (vs.env$nEffPaths[i] == vs.env$nEffPaths[lasteff]) {
          foundV1 <- 0
          if (vs.env$nEffPaths[i] > 0){
            for (j in 1:vs.env$nEffPaths[i]) {
              if (vs.env$EffPath[i, j] == vs.env$EffPath[lasteff, j]) {
                foundV1 <- foundV1 + 1
              }
            }
          }
          if (foundV1 == vs.env$nEffPaths[i]) {
            if (vs.env$nEffvars[i] == vs.env$nEffvars[lasteff] && vs.env$nEffGroups[i] == vs.env$nEffGroups[lasteff]) {
              foundV2 <- 0
              if (vs.env$nEffvars[i] > 0) {
                for (j in 1:vs.env$nEffvars[i]) {
                  if (vs.env$Effvar[i, j] == vs.env$Effvar[lasteff, j] && vs.env$EffGroup[i, j] == vs.env$EffGroup[lasteff, j]) {
                    foundV2 <- foundV2 + 1
                  }
                }
              }
              if (foundV2 == vs.env$nEffvars[i]) {
                if (vs.env$nEffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]]] + 1 > dim(vs.env$EffCombo)[4]) {
                  ncombo <- dim(vs.env$EffCombo)[4] + 1
                  arr <- array(0, c(vs.env$nUserSpecPaths, dim(vs.env$EffCombo)[2], dim(vs.env$EffCombo)[3], ncombo))
                  arr[, , , -ncombo] <- vs.env$EffCombo
                  vs.env$EffCombo <- arr
                }
                vs.env$nEffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]]] <- vs.env$nEffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]]] + 1
                vs.env$EffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]], vs.env$nEffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]]]] <- i
              } else {
                if (vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]] + 1 > dim(vs.env$nEffCombo)[3]) {
                  ncombo <- dim(vs.env$nEffCombo)[3] + 1
                  arr <- array(0, c(vs.env$nUserSpecPaths, dim(vs.env$nEffCombo)[2], ncombo))
                  arr[, , -ncombo] <- vs.env$nEffCombo
                  vs.env$nEffCombo <- arr
                  arr <- array(0, c(vs.env$nUserSpecPaths, dim(vs.env$EffCombo)[2], ncombo, dim(vs.env$EffCombo)[4]))
                  arr[, , -ncombo, ] <- vs.env$EffCombo
                  vs.env$EffCombo <- arr
                }
                vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]] <- vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]] + 1
                vs.env$nEffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]]] <- 1
                vs.env$EffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]], vs.env$nEffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]]]] <- i
              }
            } else {
              if (vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]] + 1 > dim(vs.env$nEffCombo)[3]) {
                ncombo <- dim(vs.env$nEffCombo)[3] + 1
                arr <- array(0, c(vs.env$nUserSpecPaths, dim(vs.env$nEffCombo)[2], ncombo))
                arr[, , -ncombo] <- vs.env$nEffCombo
                vs.env$nEffCombo <- arr
                arr <- array(0, c(vs.env$nUserSpecPaths, dim(vs.env$EffCombo)[2], ncombo, dim(vs.env$EffCombo)[4]))
                arr[, , -ncombo, ] <- vs.env$EffCombo
                vs.env$EffCombo <- arr
              }
              vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]] <- vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]] + 1
              vs.env$nEffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]]] <- 1
              vs.env$EffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]], vs.env$nEffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]]]] <- i
            }
          } else {
            if (vs.env$nEffPCombo[EffectID] + 1 > ncol(vs.env$nEffSCombo)) {
              vs.env$nEffSCombo <- cbind(vs.env$nEffSCombo, rep(0, nrow(vs.env$nEffSCombo)))
              ncombo <- ncol(vs.env$nEffSCombo)
              arr <- array(0, c(vs.env$nUserSpecPaths, ncombo, dim(vs.env$nEffCombo)[3]))
              arr[, -ncombo, ] <- vs.env$nEffCombo
              vs.env$nEffCombo <- arr
              arr <- array(0, c(vs.env$nUserSpecPaths, ncombo, dim(vs.env$EffCombo)[3], dim(vs.env$EffCombo)[4]))
              arr[, -ncombo, , ] <- vs.env$EffCombo
              vs.env$EffCombo <- arr
            }
            vs.env$nEffPCombo[EffectID] <- vs.env$nEffPCombo[EffectID] + 1
            vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]] <- 1
            vs.env$nEffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]]] <- 1
            vs.env$EffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]], vs.env$nEffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]]]] <- i
          }
        } else {
          if (vs.env$nEffPCombo[EffectID] + 1 > ncol(vs.env$nEffSCombo)) {
            vs.env$nEffSCombo <- cbind(vs.env$nEffSCombo, rep(0, nrow(vs.env$nEffSCombo)))
            ncombo <- ncol(vs.env$nEffSCombo)
            arr <- array(0, c(vs.env$nUserSpecPaths, ncombo, dim(vs.env$nEffCombo)[3]))
            arr[, -ncombo, ] <- vs.env$nEffCombo
            vs.env$nEffCombo <- arr
            arr <- array(0, c(vs.env$nUserSpecPaths, ncombo, dim(vs.env$EffCombo)[3], dim(vs.env$EffCombo)[4]))
            arr[, -ncombo, , ] <- vs.env$EffCombo
            vs.env$EffCombo <- arr
          }
          vs.env$nEffPCombo[EffectID] <- vs.env$nEffPCombo[EffectID] + 1
          vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]] <- 1
          vs.env$nEffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]]] <- 1
          vs.env$EffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]], vs.env$nEffCombo[EffectID, vs.env$nEffPCombo[EffectID], vs.env$nEffSCombo[EffectID, vs.env$nEffPCombo[EffectID]]]] <- i
        }
        lasteff <- i
      }
    }
  }


# Check effects combination
#  cat("Effects combined\n")
#  for (i in 1:vs.env$nEff) {
#    if (vs.env$EffDel[i] == 0) {
#      statement <- paste(i, vs.env$Varnames[vs.env$EffPath[i, 1]])
#      if (vs.env$nEffPaths[i] > 1) {
#        for (j in 2:vs.env$nEffPaths[i]) {
#          statement <- paste(statement, "<-", vs.env$Varnames[vs.env$EffPath[i, j]])
#        }
#      }
#      statement <- paste0(statement, ":")
#      if (vs.env$nEffterms[i] > 0) {
#        for (j in 1:vs.env$nEffterms[i]) {
#          if (vs.env$nEffcoefs[i, j] > 0) {
#            for (k in 1:vs.env$nEffcoefs[i, j]) {
#              statement <- paste0(statement, " a", vs.env$Effcoef[i, j, k], "(", vs.env$Varnames[vs.env$EffoutX[i, j, k]], ",", vs.env$Varnames[vs.env$EffoutY[i, j, k]], ")")
#            }
#          }
#        }
#      }
#      if (vs.env$nEffvars[i] > 0) {
#        for (j in 1:vs.env$nEffvars[i]) {
#          statement <- paste(statement, vs.env$Varnames[vs.env$Effvar[i, j]])
#          if (vs.env$EffIV[i, j] == 1) {
#            statement <- paste(statement, "[I")
#          }
#          if (vs.env$EffMod[i, j] == 1) {
#            statement <- paste(statement, "[M")
#          }
#          statement <- paste(statement, ",", vs.env$EffGroup[i, j], ",", vs.env$EffOrder[i, j], "]")
#          if (vs.env$nEffMPaths[i, j] > 0) {
#            for (k in 1:vs.env$nEffMPaths[i, j]) {
#              statement <- paste0(statement, "(", vs.env$Varnames[vs.env$EffMPath[i, j, k]], ")")
#            }
#          }
#        }
#      }
#      statement <- paste(statement, "\n")
#      cat(statement)
#    }
#  }

  vs.env
}


# vs_add_effect_terms
#
# add additional effect terms from each main conditional effect term

vs_add_effect_terms <- function(vs.env = NULL, matrices = NULL) {

  Eqfrom <- rep(0, vs.env$nPaths)
  Eqto <- rep(0, vs.env$nPaths)
  Eqorder <- rep(0, vs.env$nPaths)
  for (i in 1:nrow(matrices$Equation)) {
    for (j in 1:ncol(matrices$Equation)) {
      if (matrices$Equation[i, j] != "") {
        Eqto[as.numeric(matrices$Equation[i, j])] <- match(rownames(matrices$Equation)[i], vs.env$Varnames)
        Eqfrom[as.numeric(matrices$Equation[i, j])] <- match(colnames(matrices$Equation)[j], vs.env$Varnames)
        Eqorder[as.numeric(matrices$Equation[i, j])] <- as.numeric(matrices$Order[i, j])
      }
    }
  }

  vs.env$nEffAPterms <- rep(0, vs.env$nEff)
  vs.env$nEffAPcoefs <- matrix(0, vs.env$nEff, 1)
  vs.env$EffAPcoef <- array(0, c(vs.env$nEff, 1, 1))

  for (i in 1:vs.env$nEff) {
    APcoef0 <- matrix(0, vs.env$nEffcoefs[i, 1], vs.env$nEffcoefs[i, 1])
    outX <- 0
    outY <- 0
    nAPcoefs0 <- 0
    group0 <- 0
    if (vs.env$EffDel[i] == 0) {

      # Get coefficients for each effect
      if (vs.env$nEffterms[i] > 0) {
        vs.env$nEffAPterms[i] <- 1
        vs.env$nEffAPcoefs[i, 1] <- vs.env$nEffcoefs[i, 1]
        for (j in 1:vs.env$nEffcoefs[i, 1]) {

          # Split coefficients into group0 of order 0 paths
          if (vs.env$EffoutX[i, 1, j] != outX || vs.env$EffoutY[i, 1, j] != outY) {
            group0 <- group0 + 1
            nAPcoefs0[group0] <- 0
            outX <- vs.env$EffoutX[i, 1, j]
            outY <- vs.env$EffoutY[i, 1, j]
          }
          nAPcoefs0[group0] <- nAPcoefs0[group0] + 1
          APcoef0[group0, nAPcoefs0[group0]] <- as.numeric(matrices$Matchcoef[vs.env$EffoutX[i, 1, j],
                                                                              vs.env$EffoutY[i, 1, j],
                                                                              vs.env$Effcoef[i, 1, j]])
        }
      }

      nterms0 <- rep(0, group0)
      ncoefs0 <- matrix(0, group0, 1)
      coef0 <- array(0, c(group0, 1, 1))

      for (j in 1:group0) {
        nAPcoefs1 <- rep(0, vs.env$nEffcoefs[i, 1])
        APcoef1 <- matrix(0, vs.env$nEffcoefs[i, 1], vs.env$nEffcoefs[i, 1])
        order1 <- matrix(0, vs.env$nEffcoefs[i, 1], vs.env$nEffcoefs[i, 1])
        group1 <- 1
        nAPcoefs1[group1] <- 1
        checked_coef <- rep(0, nAPcoefs0[j])

        # If more than one coefficients, sort coefficients according to moderation path order
        # and split coefficients into group1 of moderation paths
        if (nAPcoefs0[j] > 1) {

          # find the starting path
          for (k in 1:nAPcoefs0[j]) {
            if (Eqorder[APcoef0[j, k]] == 1) {
              if (vs.env$intvar[Eqfrom[APcoef0[j, k]] - vs.env$N, 1] %in% vs.env$Effvar[i, ]) {
                APcoef1[group1, nAPcoefs1[group1]] <- APcoef0[j, k]
                order1[group1] <- Eqorder[APcoef0[j, k]]
                checked_coef[k] <- 1
                break
              }
            } else {
              if (vs.env$intvar[Eqfrom[APcoef0[j, k]] - vs.env$N, 2] %in% vs.env$Effvar[i, ] &&
                  vs.env$intvar[Eqfrom[APcoef0[j, k]] - vs.env$N, 1] %in% vs.env$Effvar[i, ]) {
                APcoef1[group1, nAPcoefs1[group1]] <- APcoef0[j, k]
                order1[group1] <- Eqorder[APcoef0[j, k]]
                checked_coef[k] <- 1
                break
              }
            }
          }

          # Add the following paths
          while (sum(checked_coef) < nAPcoefs0[j]) {
            for (k in 1:nAPcoefs0[j]) {
              if (checked_coef[k] == 0) {

                # Same group1 as the previous path
                if (Eqorder[APcoef1[group1, nAPcoefs1[group1]]] == 2 &&
                    vs.env$nintvars[Eqfrom[APcoef1[group1, nAPcoefs1[group1]]] - vs.env$N] ==
                    vs.env$nintvars[Eqto[APcoef1[group1, nAPcoefs1[group1]]] - vs.env$N]) {
                  if (sum(vs.env$intvar[Eqfrom[APcoef0[j, k]] - vs.env$N, ] ==
                          vs.env$intvar[Eqto[APcoef1[group1, nAPcoefs1[group1]]] - vs.env$N, ]) ==
                      vs.env$nintvars[Eqto[APcoef1[group1, nAPcoefs1[group1]]] - vs.env$N]) {
                    nAPcoefs1[group1] <- nAPcoefs1[group1] + 1
                    APcoef1[group1, nAPcoefs1[group1]] <- APcoef0[j, k]
                    checked_coef[k] <- 1
                  }

                # Different group1 as the previous path
                } else {

                  # Order 1 current path
                  if (Eqorder[APcoef0[j, k]] == 1) {
                    if (vs.env$intvar[Eqfrom[APcoef0[j, k]] - vs.env$N, 1] ==
                        vs.env$intvar[Eqto[APcoef1[group1, nAPcoefs1[group1]]] - vs.env$N, 1]) {
                      group1 <- group1 + 1
                      nAPcoefs1[group1] <- 1
                      APcoef1[group1, nAPcoefs1[group1]] <- APcoef0[j, k]
                      order1[group1] <- Eqorder[APcoef0[j, k]]
                      checked_coef[k] <- 1
                    }

                  # Order 2 current path
                  } else {
                    if (vs.env$intvar[Eqfrom[APcoef0[j, k]] - vs.env$N, 2] ==
                        vs.env$intvar[Eqto[APcoef1[group1, nAPcoefs1[group1]]] - vs.env$N, 1] &&
                        vs.env$intvar[Eqfrom[APcoef0[j, k]] - vs.env$N, 1] %in% vs.env$Effvar[i, ]) {
                      group1 <- group1 + 1
                      nAPcoefs1[group1] <- 1
                      APcoef1[group1, nAPcoefs1[group1]] <- APcoef0[j, k]
                      order1[group1] <- Eqorder[APcoef0[j, k]]
                      checked_coef[k] <- 1
                    }
                  }
                }
              }
            }
          }

        # If only one coefficient, store it in the first group1
        } else {
          APcoef1[group1, nAPcoefs1[group1]] <- APcoef0[j, 1]
          order1[group1] <- Eqorder[APcoef0[j, 1]]
        }

        # Store main term and add additional terms for each group1
        nterms2 <- rep(0, group1)
        ncoefs2 <- matrix(0, group1, 1)
        coef2 <- array(0, c(group1, 1, max(nAPcoefs1)))
        if (max(order1) > 0) {
          nterms1 <- rep(0, group1)
          ncoefs1 <- matrix(0, group1, 1)
          coef1 <- array(0, c(group1, 1, max(nAPcoefs1)))
        }
        if (group1 > 2) {
          nterms0to1 <- rep(0, group1)
          ncoefs0to1 <- matrix(0, group1, 1)
          coef0to1 <- array(0, c(group1, 1, max(nAPcoefs1)))
        }

        for (k in 1:group1) {

          # Store the main paths as the first term
          nterms2[k] <- 1
          ncoefs2[k, 1] <- nAPcoefs1[k]
          coef2[k, 1, 1:nAPcoefs1[k]] <- APcoef1[k, 1:nAPcoefs1[k]]

          # add terms for order 1 and order 2 paths
          if (order1[k] > 0) {

            # add terms for order 2 paths
            if (nAPcoefs1[k] > 1) {
              nterms2[k] <- nterms2[k] + 1
              ncoefs2 <- cbind(ncoefs2, rep(0, nrow(ncoefs2)))
              ncoefs2[k, nterms2[k]] <- nAPcoefs1[k]
              arr <- array(0, c(group1, nterms2[k], dim(coef2)[3]))
              arr[, -nterms2[k], ] <- coef2
              coef2 <- arr
              ncoefs1 <- cbind(ncoefs1, rep(0, nrow(ncoefs1)))
              arr <- array(0, c(group1, nterms2[k], dim(coef1)[3]))
              arr[, -nterms2[k], ] <- coef1
              coef1 <- arr
              if (group1 > 2) {
                ncoefs0to1 <- cbind(ncoefs0to1, rep(0, nrow(ncoefs0to1)))
                arr <- array(0, c(group1, nterms2[k], dim(coef0to1)[3]))
                arr[, -nterms2[k], ] <- coef0to1
                coef0to1 <- arr
              }
              for (l in 1:ncoefs2[k, nterms2[k]]) {
                if (Eqorder[coef2[k, 1, l]] == 2) {
                  int1 <- rep(0, 2)
                  int2 <- rep(0, 2)
                  if (l > 1) {
                    int1 <- vs.env$intvar[Eqfrom[coef2[k, 1, l]] - vs.env$N, ]
                  }
                  if (l < nAPcoefs1[k]) {
                    int2 <- vs.env$intvar[Eqto[coef2[k, 1, l]] - vs.env$N, ]
                  }
                  intfrom <- 0
                  intto <- 0
                  if (l == 1) {
                    intfrom <- Eqfrom[coef2[k, 1, l]]
                  } else if (l == ncoefs2[k, nterms2[k]]) {
                    intto <- Eqto[coef2[k, 1, l]]
                  }
                  for (m in 1:vs.env$nints) {
                    if (l > 1) {
                      if (vs.env$nintvars[m] == 2) {
                        if (vs.env$intvar[m, 1] == int1[1] && vs.env$intvar[m, 2] == int1[3]) {
                          intfrom <- m + vs.env$N
                        }
                      }
                    }
                    if (l < nAPcoefs1[k]) {
                      if (vs.env$nintvars[m] == 2) {
                        if (vs.env$intvar[m, 1] == int2[1] && vs.env$intvar[m, 2] == int2[3]) {
                          intto <- m + vs.env$N
                        }
                      }
                    }
                    if (intfrom > 0 && intto > 0) {
                      break
                    }
                  }
                  for (m in 1:vs.env$nPaths) {
                    if (Eqfrom[m] == intfrom && Eqto[m] == intto) {
                      coef2[k, 2, l] <- m
                      break
                    }
                  }
                }
              }

              # Add terms for order 1 to 2 paths
              if (nAPcoefs1[k] > 2) {
                for (l in 2:(nAPcoefs1[k] - 1)) {
                  nterms2[k] <- nterms2[k] + 1
                  ncoefs2 <- cbind(ncoefs2, rep(0, nrow(ncoefs2)))
                  ncoefs2[k, nterms2[k]] <- nAPcoefs1[k]
                  arr <- array(0, c(group1, nterms2[k], dim(coef2)[3]))
                  arr[, -nterms2[k], ] <- coef2
                  coef2 <- arr
                  ncoefs1 <- cbind(ncoefs1, rep(0, nrow(ncoefs1)))
                  arr <- array(0, c(group1, nterms2[k], dim(coef1)[3]))
                  arr[, -nterms2[k], ] <- coef1
                  coef1 <- arr
                  if (group1 > 2) {
                    ncoefs0to1 <- cbind(ncoefs0to1, rep(0, nrow(ncoefs0to1)))
                    arr <- array(0, c(group1, nterms2[k], dim(coef0to1)[3]))
                    arr[, -nterms2[k], ] <- coef0to1
                    coef0to1 <- arr
                  }

                  for (m in 1:ncoefs2[k, nterms2[k]]) {
                    if (m < l) {
                      coef2[k, nterms2[k], m] <- coef2[k, 2, m]
                    } else if (m == l) {
                      intfrom <- Eqfrom[coef2[k, 2, m]]
                      intto <- Eqto[coef2[k, 1, m]]
                      for (n in 1:vs.env$nPaths) {
                        if (Eqfrom[n] == intfrom && Eqto[n] == intto) {
                          coef2[k, nterms2[k], m] <- n
                          break
                        }
                      }
                    } else {
                      coef2[k, nterms2[k], m] <- coef2[k, 1, m]
                    }
                  }
                }
              }
            }

            # Add terms for order 1 paths
            if (order1[k] == 2 || group1 > 1) {
              nterms1[k] <- nterms2[k]
              for (l in 1:nterms2[k]) {
                ncoefs1[k, l] <- ncoefs2[k, l]
                for (m in 1:ncoefs2[k, l]) {
                  if (Eqorder[coef2[k, l, m]] > 0) {
                    int1 <- 0
                    int2 <- 0
                    intfrom <- 0
                    intto <- 0
                    if (k > 1 || m > 1) {
                      int1 <- vs.env$intvar[Eqfrom[coef2[k, l, m]] - vs.env$N, ]
                      if (sum(int1 > 0) == 2) {
                        intfrom <- int1[1]
                      }
                    } else {
                      intfrom <- Eqfrom[coef2[k, l, m]]
                    }
                    if (k < group1 || m < ncoefs2[k, l]) {
                      int2 <- vs.env$intvar[Eqto[coef2[k, l, m]] - vs.env$N, ]
                      if (sum(int2 > 0) == 2) {
                        intto <- int2[1]
                      }
                    } else {
                      intto <- Eqto[coef2[k, l, m]]
                    }
                    if (intfrom == 0 || intto == 0) {
                      for (n in 1:vs.env$nints) {
                        if (intfrom == 0) {
                          if (sum(int1 > 0) == 3 && vs.env$nintvars[n] == 2) {
                            if (vs.env$intvar[n, 1] == int1[1] && vs.env$intvar[n, 2] == int1[2]) {
                              intfrom <- n + vs.env$N
                            }
                          }
                        }
                        if (intto == 0) {
                          if (sum(int2 > 0) == 3 && vs.env$nintvars[n] == 2) {
                            if (vs.env$intvar[n, 1] == int2[1] && vs.env$intvar[n, 2] == int2[2]) {
                              intto <- n + vs.env$N
                            }
                          }
                        }
                        if (intfrom > 0 && intto > 0) {
                          break
                        }
                      }
                    }
                    for (n in 1:vs.env$nPaths) {
                      if (Eqfrom[n] == intfrom && Eqto[n] == intto) {
                        coef1[k, l, m] <- n
                        break
                      }
                    }
                  }
                }
              }

              # add terms for order 0 to 1 paths
              if (group1 > 2) {
                nterms0to1[k] <- nterms1[k]
                for (l in 1:nterms0to1[k]) {
                  ncoefs0to1[k, l] <- ncoefs1[k, l]
                  if (k > 1 && k < group1) {
                    for (m in 1:ncoefs0to1[k, l]) {
                      if (m < ncoefs0to1[k, l]) {
                        coef0to1[k, l, m] <- coef1[k, l, m]
                      } else {
                        intfrom <- Eqfrom[coef1[k, l, m]]
                        intto <- Eqto[coef2[k, l, m]]
                        for (n in 1:vs.env$nPaths) {
                          if (Eqfrom[n] == intfrom && Eqto[n] == intto) {
                            coef0to1[k, l, m] <- n
                            break
                          }
                        }
                      }
                    }
                  } else {
                    for (m in 1:ncoefs0to1[k, l]) {
                      if (k == 1) {
                        coef0to1[k, l, m] <- coef1[k, l, m]
                      } else {
                        coef0to1[k, l, m] <- coef2[k, l, m]
                      }
                    }
                  }
                }
              }
            }

            # Add paths for order 1 or 2 direct moderation
            if (nterms2[k] == 1) {
              if (order1[k] == 2) {

                # Add paths for order 2 direct moderation
                nterms <- 0
                ncoefs <- NULL
                endterm <- NULL
                coef <- matrix(0, 1, 1)
                mediator2 <- NULL
                for (l in 1:vs.env$nPaths) {
                  if (Eqfrom[l] == Eqfrom[coef1[k, 1, 1]] && Eqto[l] != Eqto[coef1[k, 1, 1]]) {
                    if (Eqorder[l] < 2) {
                      nterms <- nterms + 1
                      ncoefs[nterms] <- 1
                      if (nterms == 1) {
                        coef <- matrix(0, 1, 1)
                        coef[nterms, ncoefs[nterms]] <- l
                      } else {
                        coef <- rbind(coef, l)
                      }
                      endterm[nterms] <- 0
                    } else {
                      mediator2[length(mediator2) + 1] <- Eqto[l]
                    }
                  }
                }
                if (nterms > 0) {
                  while (sum(endterm) < nterms) {
                    for (l in 1:nterms) {
                      if (endterm[l] == 0) {
                        found <- 0
                        foundpath <- NULL
                        for (m in 1:vs.env$nPaths) {
                          if (Eqorder[m] < 2 && Eqfrom[m] == Eqto[coef[l, ncoefs[l]]] && !(Eqto[m] %in% mediator2)) {
                            found <- found + 1
                            foundpath[found] <- m
                          }
                        }
                        if (found > 0) {
                          for (m in 1:found) {
                            if (m == 1) {
                              ncoefs[l] <- ncoefs[l] + 1
                              if (ncoefs[l] > ncol(coef)) {
                                coef <- cbind(coef, rep(0, nrow(coef)))
                              }
                              coef[l, ncoefs[l]] <- foundpath[m]
                              if (Eqto[foundpath[m]] == Eqto[coef1[k, 1, 1]]) {
                                endterm[l] <- 1
                              }
                            } else {
                              nterms <- nterms + 1
                              ncoefs[nterms] <- ncoefs[l]
                              endterm[nterms] <- 0
                              coef <- rbind(coef, rep(0, ncol(coef)))
                              coef[nterms, 1:(ncoefs[nterms] - 1)] <- coef[l, 1:(ncoefs[l] - 1)]
                              coef[nterms, ncoefs[nterms]] <- foundpath[m]
                              if (Eqto[foundpath[m]] == Eqto[coef1[k, 1, 1]]) {
                                endterm[nterms] <- 1
                              }
                            }
                          }
                        } else {
                          endterm[l] <- 1
                        }
                      }
                    }
                  }

                  for (l in 1:nterms) {
                    if (Eqto[coef[l, ncoefs[l]]] == Eqto[coef1[k, 1, 1]]) {
                      nterms1[k] <- nterms1[k] + 1
                      ncoefs1 <- cbind(ncoefs1, rep(0, nrow(ncoefs1)))
                      ncoefs1[k, nterms1[k]] <- ncoefs[l]
                      arr <- array(0, c(group1, nterms1[k], dim(coef1)[3]))
                      arr[, -nterms1[k], ] <- coef1
                      coef1 <- arr
                      if (dim(coef1)[3] < ncoefs[l]) {
                        dim3 <- dim(coef1)[3]
                        for (m in (dim(coef1)[3] + 1):ncoefs[l]) {
                          dim3 <- dim3 + 1
                          arr <- array(0, c(group1, dim(coef1)[2], dim3))
                          arr[, , -dim3] <- coef1
                          coef1 <- arr
                        }
                      }
                      coef1[k, nterms1[k], 1:ncoefs[l]] <- coef[l, 1:ncoefs[l]]
                    }
                  }
                }

                nterms <- 0
                ncoefs <- NULL
                endterm <- NULL
                coef <- matrix(0, 1, 1)
                mediator2 <- NULL
                for (l in 1:vs.env$nPaths) {
                  if (Eqfrom[l] == Eqfrom[coef2[k, 1, 1]] && Eqto[l] != Eqto[coef2[k, 1, 1]]) {
                    if (Eqorder[l] < 2) {
                      nterms <- nterms + 1
                      ncoefs[nterms] <- 1
                      if (nterms == 1) {
                        coef <- matrix(0, 1, 1)
                        coef[nterms, ncoefs[nterms]] <- l
                      } else {
                        coef <- rbind(coef, l)
                      }
                      endterm[nterms] <- 0
                    } else {
                      mediator2[length(mediator2) + 1] <- Eqto[l]
                    }
                  }
                }
                if (nterms > 0) {
                  while (sum(endterm) < nterms) {
                    for (l in 1:nterms) {
                      if (endterm[l] == 0) {
                        found <- 0
                        foundpath <- NULL
                        for (m in 1:vs.env$nPaths) {
                          if (Eqorder[m] < 2 && Eqfrom[m] == Eqto[coef[l, ncoefs[l]]] && !(Eqto[m] %in% mediator2)) {
                            found <- found + 1
                            foundpath[found] <- m
                          }
                        }
                        if (found > 0) {
                          for (m in 1:found) {
                            if (m == 1) {
                              ncoefs[l] <- ncoefs[l] + 1
                              if (ncoefs[l] > ncol(coef)) {
                                coef <- cbind(coef, rep(0, nrow(coef)))
                              }
                              coef[l, ncoefs[l]] <- foundpath[m]
                              if (Eqto[foundpath[m]] == Eqto[coef2[k, 1, 1]]) {
                                endterm[l] <- 1
                              }
                            } else {
                              nterms <- nterms + 1
                              ncoefs[nterms] <- ncoefs[l]
                              endterm[nterms] <- 0
                              coef <- rbind(coef, rep(0, ncol(coef)))
                              coef[nterms, 1:(ncoefs[nterms] - 1)] <- coef[l, 1:(ncoefs[l] - 1)]
                              coef[nterms, ncoefs[nterms]] <- foundpath[m]
                              if (Eqto[foundpath[m]] == Eqto[coef2[k, 1, 1]]) {
                                endterm[nterms] <- 1
                              }
                            }
                          }
                        } else {
                          endterm[l] <- 1
                        }
                      }
                    }
                  }

                  for (l in 1:nterms) {
                    if (Eqto[coef[l, ncoefs[l]]] == Eqto[coef2[k, 1, 1]]) {
                      nterms2[k] <- nterms2[k] + 1
                      ncoefs2 <- cbind(ncoefs2, rep(0, nrow(ncoefs2)))
                      ncoefs2[k, nterms2[k]] <- ncoefs[l]
                      arr <- array(0, c(group1, nterms2[k], dim(coef2)[3]))
                      arr[, -nterms2[k], ] <- coef2
                      coef2 <- arr
                      if (dim(coef2)[3] < ncoefs[l]) {
                        dim3 <- dim(coef2)[3]
                        for (m in (dim(coef2)[3] + 1):ncoefs[l]) {
                          dim3 <- dim3 + 1
                          arr <- array(0, c(group1, dim(coef2)[2], dim3))
                          arr[, , -dim3] <- coef2
                          coef2 <- arr
                        }
                      }
                      coef2[k, nterms2[k], 1:ncoefs[l]] <- coef[l, 1:ncoefs[l]]
                    }
                  }
                }

                if (group1 > 2) {
                  nterms <- 0
                  ncoefs <- NULL
                  endterm <- NULL
                  coef <- matrix(0, 1, 1)
                  mediator2 <- NULL
                  for (l in 1:vs.env$nPaths) {
                    if (Eqfrom[l] == Eqfrom[coef0to1[k, 1, 1]] && Eqto[l] != Eqto[coef0to1[k, 1, 1]]) {
                      if (Eqorder[l] < 2) {
                        nterms <- nterms + 1
                        ncoefs[nterms] <- 1
                        if (nterms == 1) {
                          coef <- matrix(0, 1, 1)
                          coef[nterms, ncoefs[nterms]] <- l
                        } else {
                          coef <- rbind(coef, l)
                        }
                        endterm[nterms] <- 0
                      } else {
                        mediator2[length(mediator2) + 1] <- Eqto[l]
                      }
                    }
                  }

                  if (nterms > 0) {
                    while (sum(endterm) < nterms) {
                      for (l in 1:nterms) {
                        if (endterm[l] == 0) {
                          found <- 0
                          foundpath <- NULL
                          for (m in 1:vs.env$nPaths) {
                            if (Eqorder[m] < 2 && Eqfrom[m] == Eqto[coef[l, ncoefs[l]]] && !(Eqto[m] %in% mediator2)) {
                              found <- found + 1
                              foundpath[found] <- m
                            }
                          }
                          if (found > 0) {
                            for (m in 1:found) {
                              if (m == 1) {
                                ncoefs[l] <- ncoefs[l] + 1
                                if (ncoefs[l] > ncol(coef)) {
                                  coef <- cbind(coef, rep(0, nrow(coef)))
                                }
                                coef[l, ncoefs[l]] <- foundpath[m]
                                if (Eqto[foundpath[m]] == Eqto[coef0to1[k, 1, 1]]) {
                                  endterm[l] <- 1
                                }
                              } else {
                                nterms <- nterms + 1
                                ncoefs[nterms] <- ncoefs[l]
                                endterm[nterms] <- 0
                                coef <- rbind(coef, rep(0, ncol(coef)))
                                coef[nterms, 1:(ncoefs[nterms] - 1)] <- coef[l, 1:(ncoefs[l] - 1)]
                                coef[nterms, ncoefs[nterms]] <- foundpath[m]
                                if (Eqto[foundpath[m]] == Eqto[coef0to1[k, 1, 1]]) {
                                  endterm[nterms] <- 1
                                }
                              }
                            }
                          } else {
                            endterm[l] <- 1
                          }
                        }
                      }
                    }

                    for (l in 1:nterms) {
                      if (Eqto[coef[l, ncoefs[l]]] == Eqto[coef0to1[k, 1, 1]]) {
                        nterms0to1[k] <- nterms0to1[k] + 1
                        ncoefs0to1 <- cbind(ncoefs0to1, rep(0, nrow(ncoefs0to1)))
                        ncoefs0to1[k, nterms0to1[k]] <- ncoefs[l]
                        arr <- array(0, c(group1, nterms0to1[k], dim(coef0to1)[3]))
                        arr[, -nterms0to1[k], ] <- coef0to1
                        coef0to1 <- arr
                        if (dim(coef0to1)[3] < ncoefs[l]) {
                          dim3 <- dim(coef0to1)[3]
                          for (m in (dim(coef0to1)[3] + 1):ncoefs[l]) {
                            dim3 <- dim3 + 1
                            arr <- array(0, c(group1, dim(coef0to1)[2], dim3))
                            arr[, , -dim3] <- coef0to1
                            coef0to1 <- arr
                          }
                        }
                        coef0to1[k, nterms0to1[k], 1:ncoefs[l]] <- coef[l, 1:ncoefs[l]]
                      }
                    }
                  }
                }
              } else if (order1[k] == 1) {

                # Add paths for order 1 direct moderation
                if (group1 > 1) {
                  nterms <- 0
                  ncoefs <- NULL
                  endterm <- NULL
                  coef <- matrix(0, 1, 1)
                  mediator1 <- NULL
                  for (l in 1:vs.env$nPaths) {
                    if (group1 == 1) {
                      condition <- Eqorder[l] != 1
                    } else {
                      condition <- Eqorder[l] > 1
                    }
                    if (Eqfrom[l] == Eqfrom[coef1[k, 1, 1]] && Eqto[l] != Eqto[coef1[k, 1, 1]]) {
                      if (condition) {
                        nterms <- nterms + 1
                        ncoefs[nterms] <- 1
                        if (nterms == 1) {
                          coef <- matrix(0, 1, 1)
                          coef[nterms, ncoefs[nterms]] <- l
                        } else {
                          coef <- rbind(coef, l)
                        }
                        endterm[nterms] <- 0
                      } else if (Eqorder[l] == 1) {
                        mediator1[length(mediator1) + 1] <- Eqto[l]
                      }
                    }
                  }

                  if (nterms > 0) {
                    while (sum(endterm) < nterms) {
                      for (l in 1:nterms) {
                        if (endterm[l] == 0) {
                          found <- 0
                          foundpath <- NULL
                          for (m in 1:vs.env$nPaths) {
                            if (group1 == 1) {
                              condition <- Eqorder[m] != 1
                            } else {
                              condition <- Eqorder[m] > 1
                            }
                            if (condition && Eqfrom[m] == Eqto[coef[l, ncoefs[l]]] && !(Eqto[m] %in% mediator1)) {
                              found <- found + 1
                              foundpath[found] <- m
                            }
                          }
                          if (found > 0) {
                            for (m in 1:found) {
                              if (m == 1) {
                                ncoefs[l] <- ncoefs[l] + 1
                                if (ncoefs[l] > ncol(coef)) {
                                  coef <- cbind(coef, rep(0, nrow(coef)))
                                }
                                coef[l, ncoefs[l]] <- foundpath[m]
                                if (Eqto[foundpath[m]] == Eqto[coef1[k, 1, 1]]) {
                                  endterm[l] <- 1
                                }
                              } else {
                                nterms <- nterms + 1
                                ncoefs[nterms] <- ncoefs[l]
                                endterm[nterms] <- 0
                                coef <- rbind(coef, rep(0, ncol(coef)))
                                coef[nterms, 1:(ncoefs[nterms] - 1)] <- coef[l, 1:(ncoefs[l] - 1)]
                                coef[nterms, ncoefs[nterms]] <- foundpath[m]
                                if (Eqto[foundpath[m]] == Eqto[coef1[k, 1, 1]]) {
                                  endterm[nterms] <- 1
                                }
                              }
                            }
                          } else {
                            endterm[l] <- 1
                          }
                        }
                      }
                    }

                    for (l in 1:nterms) {
                      if (Eqto[coef[l, ncoefs[l]]] == Eqto[coef1[k, 1, 1]]) {
                        nterms1[k] <- nterms1[k] + 1
                        ncoefs1 <- cbind(ncoefs1, rep(0, nrow(ncoefs1)))
                        ncoefs1[k, nterms1[k]] <- ncoefs[l]
                        arr <- array(0, c(group1, nterms1[k], dim(coef1)[3]))
                        arr[, -nterms1[k], ] <- coef1
                        coef1 <- arr
                        if (dim(coef1)[3] < ncoefs[l]) {
                          dim3 <- dim(coef1)[3]
                          for (m in (dim(coef1)[3] + 1):ncoefs[l]) {
                            dim3 <- dim3 + 1
                            arr <- array(0, c(group1, dim(coef1)[2], dim3))
                            arr[, , -dim3] <- coef1
                            coef1 <- arr
                          }
                        }
                        coef1[k, nterms1[k], 1:ncoefs[l]] <- coef[l, 1:ncoefs[l]]
                      }
                    }
                  }
                }

                nterms <- 0
                ncoefs <- NULL
                endterm <- NULL
                coef <- matrix(0, 1, 1)
                mediator1 <- NULL
                for (l in 1:vs.env$nPaths) {
                  if (group1 == 1) {
                    condition <- Eqorder[l] != 1
                  } else {
                    condition <- Eqorder[l] > 1
                  }
                  if (Eqfrom[l] == Eqfrom[coef2[k, 1, 1]] && Eqto[l] != Eqto[coef2[k, 1, 1]]) {
                    if (condition) {
                      nterms <- nterms + 1
                      ncoefs[nterms] <- 1
                      if (nterms == 1) {
                        coef <- matrix(0, 1, 1)
                        coef[nterms, ncoefs[nterms]] <- l
                      } else {
                        coef <- rbind(coef, l)
                      }
                      endterm[nterms] <- 0
                    } else if (Eqorder[l] == 1) {
                      mediator1[length(mediator1) + 1] <- Eqto[l]
                    }
                  }
                }

                if (nterms > 0) {
                  while (sum(endterm) < nterms) {
                    for (l in 1:nterms) {
                      if (endterm[l] == 0) {
                        found <- 0
                        foundpath <- NULL
                        for (m in 1:vs.env$nPaths) {
                          if (group1 == 1) {
                            condition <- Eqorder[m] != 1
                          } else {
                            condition <- Eqorder[m] > 1
                          }
                          if (condition && Eqfrom[m] == Eqto[coef[l, ncoefs[l]]] && !(Eqto[m] %in% mediator1)) {
                            found <- found + 1
                            foundpath[found] <- m
                          }
                        }
                        if (found > 0) {
                          for (m in 1:found) {
                            if (m == 1) {
                              ncoefs[l] <- ncoefs[l] + 1
                              if (ncoefs[l] > ncol(coef)) {
                                coef <- cbind(coef, rep(0, nrow(coef)))
                              }
                              coef[l, ncoefs[l]] <- foundpath[m]
                              if (Eqto[foundpath[m]] == Eqto[coef2[k, 1, 1]]) {
                                endterm[l] <- 1
                              }
                            } else {
                              nterms <- nterms + 1
                              ncoefs[nterms] <- ncoefs[l]
                              endterm[nterms] <- 0
                              coef <- rbind(coef, rep(0, ncol(coef)))
                              coef[nterms, 1:(ncoefs[nterms] - 1)] <- coef[l, 1:(ncoefs[l] - 1)]
                              coef[nterms, ncoefs[nterms]] <- foundpath[m]
                              if (Eqto[foundpath[m]] == Eqto[coef2[k, 1, 1]]) {
                                endterm[nterms] <- 1
                              }
                            }
                          }
                        } else {
                          endterm[l] <- 1
                        }
                      }
                    }
                  }

                  for (l in 1:nterms) {
                    if (Eqto[coef[l, ncoefs[l]]] == Eqto[coef2[k, 1, 1]]) {
                      nterms2[k] <- nterms2[k] + 1
                      ncoefs2 <- cbind(ncoefs2, rep(0, nrow(ncoefs2)))
                      ncoefs2[k, nterms2[k]] <- ncoefs[l]
                      arr <- array(0, c(group1, nterms2[k], dim(coef2)[3]))
                      arr[, -nterms2[k], ] <- coef2
                      coef2 <- arr
                      if (dim(coef2)[3] < ncoefs[l]) {
                        dim3 <- dim(coef2)[3]
                        for (m in (dim(coef2)[3] + 1):ncoefs[l]) {
                          dim3 <- dim3 + 1
                          arr <- array(0, c(group1, dim(coef2)[2], dim3))
                          arr[, , -dim3] <- coef2
                          coef2 <- arr
                        }
                      }
                      coef2[k, nterms2[k], 1:ncoefs[l]] <- coef[l, 1:ncoefs[l]]
                    }
                  }
                }

                if (group1 > 2) {
                  nterms <- 0
                  ncoefs <- NULL
                  endterm <- NULL
                  coef <- matrix(0, 1, 1)
                  mediator1 <- NULL
                  for (l in 1:vs.env$nPaths) {
                    if (group1 == 1) {
                      condition <- Eqorder[l] != 1
                    } else {
                      condition <- Eqorder[l] > 1
                    }
                    if (Eqfrom[l] == Eqfrom[coef0to1[k, 1, 1]] && Eqto[l] != Eqto[coef0to1[k, 1, 1]]) {
                      if (condition) {
                        nterms <- nterms + 1
                        ncoefs[nterms] <- 1
                        if (nterms == 1) {
                          coef <- matrix(0, 1, 1)
                          coef[nterms, ncoefs[nterms]] <- l
                        } else {
                          coef <- rbind(coef, l)
                        }
                        endterm[nterms] <- 0
                      } else if (Eqorder[l] == 1) {
                        mediator1[length(mediator1) + 1] <- Eqto[l]
                      }
                    }
                  }

                  if (nterms > 0) {
                    while (sum(endterm) < nterms) {
                      for (l in 1:nterms) {
                        if (endterm[l] == 0) {
                          found <- 0
                          foundpath <- NULL
                          for (m in 1:vs.env$nPaths) {
                            if (group1 == 1) {
                              condition <- Eqorder[m] != 1
                            } else {
                              condition <- Eqorder[m] > 1
                            }
                            if (condition && Eqfrom[m] == Eqto[coef[l, ncoefs[l]]] && !(Eqto[m] %in% mediator1)) {
                              found <- found + 1
                              foundpath[found] <- m
                            }
                          }
                          if (found > 0) {
                            for (m in 1:found) {
                              if (m == 1) {
                                ncoefs[l] <- ncoefs[l] + 1
                                if (ncoefs[l] > ncol(coef)) {
                                  coef <- cbind(coef, rep(0, nrow(coef)))
                                }
                                coef[l, ncoefs[l]] <- foundpath[m]
                                if (Eqto[foundpath[m]] == Eqto[coef0to1[k, 1, 1]]) {
                                  endterm[l] <- 1
                                }
                              } else {
                                nterms <- nterms + 1
                                ncoefs[nterms] <- ncoefs[l]
                                endterm[nterms] <- 0
                                coef <- rbind(coef, rep(0, ncol(coef)))
                                coef[nterms, 1:(ncoefs[nterms] - 1)] <- coef[l, 1:(ncoefs[l] - 1)]
                                coef[nterms, ncoefs[nterms]] <- foundpath[m]
                                if (Eqto[foundpath[m]] == Eqto[coef0to1[k, 1, 1]]) {
                                  endterm[nterms] <- 1
                                }
                              }
                            }
                          } else {
                            endterm[l] <- 1
                          }
                        }
                      }
                    }

                    for (l in 1:nterms) {
                      if (Eqto[coef[l, ncoefs[l]]] == Eqto[coef0to1[k, 1, 1]]) {
                        nterms0to1[k] <- nterms0to1[k] + 1
                        ncoefs0to1 <- cbind(ncoefs0to1, rep(0, nrow(ncoefs0to1)))
                        ncoefs0to1[k, nterms0to1[k]] <- ncoefs[l]
                        arr <- array(0, c(group1, nterms0to1[k], dim(coef0to1)[3]))
                        arr[, -nterms0to1[k], ] <- coef0to1
                        coef0to1 <- arr
                        if (dim(coef0to1)[3] < ncoefs[l]) {
                          dim3 <- dim(coef0to1)[3]
                          for (m in (dim(coef0to1)[3] + 1):ncoefs[l]) {
                            dim3 <- dim3 + 1
                            arr <- array(0, c(group1, dim(coef0to1)[2], dim3))
                            arr[, , -dim3] <- coef0to1
                            coef0to1 <- arr
                          }
                        }
                        coef0to1[k, nterms0to1[k], 1:ncoefs[l]] <- coef[l, 1:ncoefs[l]]
                      }
                    }
                  }
                }
              }
            }
          } else {

            # Add paths for order 0 paths
            nterms <- 0
            ncoefs <- NULL
            endterm <- NULL
            coef <- matrix(0, 1, 1)
            mediator0 <- NULL
            for (l in 1:vs.env$nPaths) {
              if (Eqfrom[l] == Eqfrom[coef2[k, 1, 1]] && Eqto[l] != Eqto[coef2[k, 1, 1]]) {
                if (Eqorder[l] > 0) {
                  nterms <- nterms + 1
                  ncoefs[nterms] <- 1
                  if (nterms == 1) {
                    coef <- matrix(0, 1, 1)
                    coef[nterms, ncoefs[nterms]] <- l
                  } else {
                    coef <- rbind(coef, l)
                  }
                  endterm[nterms] <- 0
                } else {
                  mediator0[length(mediator0) + 1] <- Eqto[l]
                }
              }
            }

            if (nterms > 0) {
              while (sum(endterm) < nterms) {
                for (l in 1:nterms) {
                  if (endterm[l] == 0) {
                    found <- 0
                    foundpath <- NULL
                    for (m in 1:vs.env$nPaths) {
                      if (Eqorder[m] > 0 && Eqfrom[m] == Eqto[coef[l, ncoefs[l]]] && !(Eqto[m] %in% mediator0)) {
                        found <- found + 1
                        foundpath[found] <- m
                      }
                    }
                    if (found > 0) {
                      for (m in 1:found) {
                        if (m == 1) {
                          ncoefs[l] <- ncoefs[l] + 1
                          if (ncoefs[l] > ncol(coef)) {
                            coef <- cbind(coef, rep(0, nrow(coef)))
                          }
                          coef[l, ncoefs[l]] <- foundpath[m]
                          if (Eqto[foundpath[m]] == Eqto[coef2[k, 1, 1]]) {
                            endterm[l] <- 1
                          }
                        } else {
                          nterms <- nterms + 1
                          ncoefs[nterms] <- ncoefs[l]
                          endterm[nterms] <- 0
                          coef <- rbind(coef, rep(0, ncol(coef)))
                          coef[nterms, 1:(ncoefs[nterms] - 1)] <- coef[l, 1:(ncoefs[l] - 1)]
                          coef[nterms, ncoefs[nterms]] <- foundpath[m]
                          if (Eqto[foundpath[m]] == Eqto[coef2[k, 1, 1]]) {
                            endterm[nterms] <- 1
                          }
                        }
                      }
                    } else {
                      endterm[l] <- 1
                    }
                  }
                }
              }

              for (l in 1:nterms) {
                if (Eqto[coef[l, ncoefs[l]]] == Eqto[coef2[k, 1, 1]]) {
                  nterms2[k] <- nterms2[k] + 1
                  ncoefs2 <- cbind(ncoefs2, rep(0, nrow(ncoefs2)))
                  ncoefs2[k, nterms2[k]] <- ncoefs[l]
                  arr <- array(0, c(group1, nterms2[k], dim(coef2)[3]))
                  arr[, -nterms2[k], ] <- coef2
                  coef2 <- arr
                  if (dim(coef2)[3] < ncoefs[l]) {
                    dim3 <- dim(coef2)[3]
                    for (m in (dim(coef2)[3] + 1):ncoefs[l]) {
                      dim3 <- dim3 + 1
                      arr <- array(0, c(group1, dim(coef2)[2], dim3))
                      arr[, , -dim3] <- coef2
                      coef2 <- arr
                    }
                  }
                  coef2[k, nterms2[k], 1:ncoefs[l]] <- coef[l, 1:ncoefs[l]]
                }
              }
            }
          }
        }

        if (group1 > 1) {
          if (max(order1) > 0) {

            # Stack terms of coef0to1[k, , ] to coef0to1[1, , ]
            if (group1 > 2) {
              first_nterms <- nterms0to1[1]
              first_ncoefs <- ncoefs0to1[1, 1:nterms0to1[1]]
              first_coef <- as.matrix(coef0to1[1, 1:nterms0to1[1], ])
              if (first_nterms == 1) {
                first_coef <- matrix(first_coef, nrow = 1)
              }
              addedterms <- 0
              for (a in 2:(group1-1)) {
                if (a > 2) {
                  nterms0to1[1] <- nterms0to1[1] + first_nterms
                  ncoefs0to1 <- cbind(ncoefs0to1, rep(0, first_nterms))
                  ncoefs0to1[1, (addedterms + 1):(addedterms + first_nterms)] <- first_ncoefs[1:first_nterms]
                  dim2 <- dim(coef0to1)[2]
                  for (k in 1:first_nterms) {
                    dim2 <- dim2 + 1
                    arr <- array(0, c(dim(coef0to1)[1], dim2, dim(coef0to1)[3]))
                    arr[, -dim2, ] <- coef0to1
                    coef0to1 <- arr
                  }
                  coef0to1[1, (addedterms+1):(addedterms+first_nterms), 1:ncol(first_coef)] <- first_coef
                }
                for (k in 2:group1) {
                  if (k < a) {
                    addnterms <- nterms1[k]
                    addncoefs <- ncoefs1[k, 1:nterms1[k]]
                    addcoef <- as.matrix(coef1[k, 1:nterms1[k], ])
                    if (addnterms == 1) {
                      addcoef <- matrix(addcoef, nrow = 1)
                    }
                  } else if (k == a) {
                    addnterms <- nterms0to1[k]
                    addncoefs <- ncoefs0to1[k, 1:nterms0to1[k]]
                    addcoef <- as.matrix(coef0to1[k, 1:nterms0to1[k], ])
                    if (addnterms == 1) {
                      addcoef <- matrix(addcoef, nrow = 1)
                    }
                  } else {
                    addnterms <- nterms2[k]
                    addncoefs <- ncoefs2[k, 1:nterms2[k]]
                    addcoef <- as.matrix(coef2[k, 1:nterms2[k], ])
                    if (addnterms == 1) {
                      addcoef <- matrix(addcoef, nrow = 1)
                    }
                  }
                  repeat_nterms <- nterms0to1[1] - addedterms
                  if (nterms0to1[k] > 1) {
                    repeat_coef <- as.matrix(coef0to1[1, (addedterms+1):nterms0to1[1], ])
                    if (repeat_nterms == 1) {
                      repeat_coef <- matrix(repeat_coef, nrow = 1)
                    }

                    # Repeat all terms in coef0to1[1, , ]
                    for (l in 2:addnterms) {
                      nterms0to1[1] <- nterms0to1[1] + repeat_nterms
                      ncoefs0to1 <- cbind(ncoefs0to1, rep(0, repeat_nterms))
                      ncoefs0to1[1, (nterms0to1[1] - repeat_nterms + 1):nterms0to1[1]] <- ncoefs0to1[1, (addedterms + 1):(addedterms + repeat_nterms)]
                      dim2 <- dim(coef0to1)[2]
                      for (m in 1:repeat_nterms) {
                        dim2 <- dim2 + 1
                        arr <- array(0, c(dim(coef0to1)[1], dim2, dim(coef0to1)[3]))
                        arr[, -dim2, ] <- coef0to1
                        coef0to1 <- arr
                      }
                      coef0to1[1, (nterms0to1[1] - repeat_nterms + 1):nterms0to1[1], 1:ncol(repeat_coef)] <- repeat_coef
                    }
                  }

                  # Add each term in coef0to1[k, l, ] to coef0to1[1, , ]
                  for (l in 1:addnterms) {
                    for (m in (addedterms + (l - 1) * repeat_nterms + 1):(addedterms + l * repeat_nterms)) {
                      dim3 <- dim(coef0to1)[3]
                      for (n in (ncoefs0to1[1, m] + 1):(ncoefs0to1[1, m] + addncoefs[l])) {
                        if (dim3 < n) {
                          dim3 <- dim3 + 1
                          arr <- array(0, c(dim(coef0to1)[1], dim(coef0to1)[2], dim3))
                          arr[, , -dim3] <- coef0to1
                          coef0to1 <- arr
                        }
                        coef0to1[1, m, n] <- addcoef[l, n - ncoefs0to1[1, m]]
                      }
                      ncoefs0to1[1, m] <- ncoefs0to1[1, m] + addncoefs[l]
                    }
                  }
                }
                addedterms <- nterms0to1[1]
              }
            }

            # Stack terms of coef1[k, , ] to coef1[1, , ]
            if (group1 > 1) {
              for (k in 2:group1) {
                repeat_nterms <- nterms1[1]
                if (nterms1[k] > 1) {
                  repeat_coef <- as.matrix(coef1[1, 1:nterms1[1], ])
                  if (repeat_nterms == 1) {
                    repeat_coef <- matrix(repeat_coef, nrow = 1)
                  }

                  # Repeat all terms in coef1[1, , ]
                  for (l in 2:nterms1[k]) {
                    nterms1[1] <- nterms1[1] + repeat_nterms
                    ncoefs1 <- cbind(ncoefs1, rep(0, repeat_nterms))
                    ncoefs1[1, (nterms1[1] - repeat_nterms + 1):nterms1[1]] <- ncoefs1[1, 1:repeat_nterms]
                    dim2 <- dim(coef1)[2]
                    for (m in 1:repeat_nterms) {
                      dim2 <- dim2 + 1
                      arr <- array(0, c(dim(coef1)[1], dim2, dim(coef1)[3]))
                      arr[, -dim2, ] <- coef1
                      coef1 <- arr
                    }
                    coef1[1, (nterms1[1] - repeat_nterms + 1):nterms1[1], 1:ncol(repeat_coef)] <- repeat_coef
                  }
                }

                # Add each term in coef1[k, l, ] to coef1[1, , ]
                for (l in 1:nterms1[k]) {
                  for (m in ((l - 1) * repeat_nterms + 1):(l * repeat_nterms)) {
                    dim3 <- dim(coef1)[3]
                    for (n in (ncoefs1[1, m] + 1):(ncoefs1[1, m] + ncoefs1[k, l])) {
                      if (dim3 < n) {
                        dim3 <- dim3 + 1
                        arr <- array(0, c(dim(coef1)[1], dim(coef1)[2], dim3))
                        arr[, , -dim3] <- coef1
                        coef1 <- arr
                      }
                      coef1[1, m, n] <- coef1[k, l, n - ncoefs1[1, m]]
                    }
                    ncoefs1[1, m] <- ncoefs1[1, m] + ncoefs1[k, l]
                  }
                }
              }
            }
          }

          # Stack terms of coef2[k, , ] to coef2[1, , ]
          for (k in 2:group1) {
            repeat_nterms <- nterms2[1]
            if (nterms2[k] > 1) {
              repeat_coef <- as.matrix(coef2[1, 1:nterms2[1], ])
              if (repeat_nterms == 1) {
                repeat_coef <- matrix(repeat_coef, nrow = 1)
              }

              # Repeat all terms in coef2[1, , ]
              for (l in 2:nterms2[k]) {
                nterms2[1] <- nterms2[1] + repeat_nterms
                ncoefs2 <- cbind(ncoefs2, rep(0, repeat_nterms))
                ncoefs2[1, (nterms2[1] - repeat_nterms + 1):nterms2[1]] <- ncoefs2[1, 1:repeat_nterms]
                dim2 <- dim(coef2)[2]
                for (m in 1:repeat_nterms) {
                  dim2 <- dim2 + 1
                  arr <- array(0, c(dim(coef2)[1], dim2, dim(coef2)[3]))
                  arr[, -dim2, ] <- coef2
                  coef2 <- arr
                }
                coef2[1, (nterms2[1] - repeat_nterms + 1):nterms2[1], 1:ncol(repeat_coef)] <- repeat_coef
              }
            }

            # Add each term in coef2[k, l, ] to coef2[1, , ]
            for (l in 1:nterms2[k]) {
              for (m in ((l - 1) * repeat_nterms + 1):(l * repeat_nterms)) {
                dim3 <- dim(coef2)[3]
                for (n in (ncoefs2[1, m] + 1):(ncoefs2[1, m] + ncoefs2[k, l])) {
                  if (dim3 < n) {
                    dim3 <- dim3 + 1
                    arr <- array(0, c(dim(coef2)[1], dim(coef2)[2], dim3))
                    arr[, , -dim3] <- coef2
                    coef2 <- arr
                  }
                  coef2[1, m, n] <- coef2[k, l, n - ncoefs2[1, m]]
                }
                ncoefs2[1, m] <- ncoefs2[1, m] + ncoefs2[k, l]
              }
            }
          }
        }

        # Combine coef2[1, , ], coef1[1, , ] and coef0to1[1, , ] to coef0
        nterms0[j] <- nterms2[1]
        if (group1 > 1) {
          nterms0[j] <- nterms0[j] + nterms1[1]
          if (group1 > 2) {
            nterms0[j] <- nterms0[j] + nterms0to1[1]
          }
        }
        if (ncol(ncoefs0) < nterms0[j]) {
          dim2 <- ncol(ncoefs0)
          for (k in (ncol(ncoefs0) + 1):nterms0[j]) {
            dim2 <- dim2 + 1
            ncoefs0 <- cbind(ncoefs0, rep(0, nrow(ncoefs0)))
            arr <- array(0, c(dim(coef0)[1], dim2, dim(coef0)[3]))
            arr[, -dim2, ] <- coef0
            coef0 <- arr
          }
        }
        ncoefs0[j, 1:nterms2[1]] <- ncoefs2[1, 1:nterms2[1]]
        if (group1 > 1) {
          ncoefs0[j, (nterms2[1] + 1):(nterms2[1] + nterms1[1])] <- ncoefs1[1, 1:nterms1[1]]
          if (group1 > 2) {
            ncoefs0[j, (nterms2[1] + nterms1[1] + 1):nterms0[j]] <- ncoefs0to1[1, 1:nterms0to1[1]]
          }
        }
        if (dim(coef0)[3] < max(ncoefs0)) {
          dim3 <- dim(coef0)[3]
          for (k in (dim(coef0)[3] + 1):max(ncoefs0)) {
            dim3 <- dim3 + 1
            arr <- array(0, c(dim(coef0)[1], dim(coef0)[2], dim3))
            arr[, , -dim3] <- coef0
            coef0 <- arr
          }
        }
        coef0[j, 1:nterms2[1], 1:dim(coef2)[3]] <- coef2[1, 1:nterms2[1], ]
        if (group1 > 1) {
          coef0[j, (nterms2[1] + 1):(nterms2[1] + nterms1[1]), 1:dim(coef1)[3]] <- coef1[1, 1:nterms1[1], ]
          if (group1 > 2) {
            coef0[j, (nterms2[1] + nterms1[1] + 1):nterms0[j], 1:dim(coef0to1)[3]] <- coef0to1[1, 1:nterms0to1[1], ]
          }
        }
      }

      # Copy the paths of first group0 to vs.env$nEffAPterms, vs.env$nEffAPcoefs, and vs.env$EffAPcoef
      vs.env$nEffAPterms[i] <- nterms0[1]
      if (ncol(vs.env$nEffAPcoefs) < nterms0[1]) {
        dim2 <- ncol(vs.env$nEffAPcoefs)
        for (j in (ncol(vs.env$nEffAPcoefs) + 1):nterms0[1]) {
          vs.env$nEffAPcoefs <- cbind(vs.env$nEffAPcoefs, rep(0, nrow(vs.env$nEffAPcoefs)))
          dim2 <- dim2 + 1
          arr <- array(0, c(dim(vs.env$EffAPcoef)[1], dim2, dim(vs.env$EffAPcoef)[3]))
          arr[, -dim2, ] <- vs.env$EffAPcoef
          vs.env$EffAPcoef <- arr
        }
      }
      if (dim(vs.env$EffAPcoef)[3] < dim(coef0)[3]) {
        dim3 <- dim(vs.env$EffAPcoef)[3]
        for (j in (dim(vs.env$EffAPcoef)[3] + 1):dim(coef0)[3]) {
          dim3 <- dim3 + 1
          arr <- array(0, c(dim(vs.env$EffAPcoef)[1], dim(vs.env$EffAPcoef)[2], dim3))
          arr[, , -dim3] <- vs.env$EffAPcoef
          vs.env$EffAPcoef <- arr
        }
      }
      vs.env$nEffAPcoefs[i, 1:nterms0[1]] <- ncoefs0[1, 1:nterms0[1]]
      vs.env$EffAPcoef[i, 1:nterms0[1], 1:dim(coef0)[3]] <- coef0[1, 1:nterms0[1], ]

      if (group0 > 1) {
        for (j in 2:group0) {
          repeat_nterms <- vs.env$nEffAPterms[i]
          repeat_coef <- as.matrix(vs.env$EffAPcoef[i, 1:vs.env$nEffAPterms[i], ])
          if (repeat_nterms == 1) {
            repeat_coef <- matrix(repeat_coef, nrow = 1)
          }

          # Repeat all terms in vs.env$EffAPcoef[i, , ]
          if (nterms0[j] > 1) {
            for (k in 2:nterms0[j]) {
              vs.env$nEffAPterms[i] <- vs.env$nEffAPterms[i] + repeat_nterms
              dim2 <- dim(vs.env$EffAPcoef)[2]
              for (l in 1:repeat_nterms) {
                vs.env$nEffAPcoefs <- cbind(vs.env$nEffAPcoefs, rep(0, nrow(vs.env$nEffAPcoefs)))
                dim2 <- dim2 + 1
                arr <- array(0, c(dim(vs.env$EffAPcoef)[1], dim2, dim(vs.env$EffAPcoef)[3]))
                arr[, -dim2, ] <- vs.env$EffAPcoef
                vs.env$EffAPcoef <- arr
              }
              vs.env$nEffAPcoefs[i, (vs.env$nEffAPterms[i] - repeat_nterms + 1):vs.env$nEffAPterms[i]] <- vs.env$nEffAPcoefs[i, 1:repeat_nterms]
              vs.env$EffAPcoef[i, (vs.env$nEffAPterms[i] - repeat_nterms + 1):vs.env$nEffAPterms[i], 1:ncol(repeat_coef)] <- repeat_coef
            }
          }
        }

        # Add each term in coef0[j, k, ] to vs.env$EffAPcoef[i, , ]
        for (k in 1:nterms0[j]) {
          for (l in ((k - 1) * repeat_nterms + 1):(k * repeat_nterms)) {
            dim3 <- dim(vs.env$EffAPcoef)[3]
            for (m in (vs.env$nEffAPcoefs[i, l] + 1):(vs.env$nEffAPcoefs[i, l] + ncoefs0[j, k])) {
              if (dim3 < m) {
                dim3 <- dim3 + 1
                arr <- array(0, c(dim(vs.env$EffAPcoef)[1], dim(vs.env$EffAPcoef)[2], dim3))
                arr[, , -dim3] <- vs.env$EffAPcoef
                vs.env$EffAPcoef <- arr
              }
              vs.env$EffAPcoef[i, l, m] <- coef0[j, k, m - vs.env$nEffAPcoefs[i, l]]
            }
            vs.env$nEffAPcoefs[i, l] <- vs.env$nEffAPcoefs[i, l] + ncoefs0[j, k]
          }
        }
      }
    }
  }

  vs.env
}





