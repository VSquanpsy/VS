# vs_conditional_info
#
# Store the IV, DV, paths, sources, and conditional effects for the conditional table

vs_conditional_info <- function(vs.env = NULL) {

  conditional <- NULL
  remarks <- NULL
  nremarks <- 0
  if (vs.env$nEff > 0) {
    ntoteff <- sum(vs.env$EffDel == 0) + sum(vs.env$nEffCombo > 1)

    utoteff <- rep(0, vs.env$nUserSpecPaths)
    for (i in 1:vs.env$nEff) {
      if (vs.env$EffDel[i] == 0) {
        if (i <= vs.env$uEff[1]) {
          utoteff[1] <- utoteff[1] + 1
        }
        if (vs.env$nUserSpecPaths > 1) {
          for (a in 2:vs.env$nUserSpecPaths) {
            if (i <= vs.env$uEff[a] &&  i > vs.env$uEff[a - 1]) {
              utoteff[a] <- utoteff[a] + 1
            }
          }
        }
      }
    }

    if (ntoteff > 0) {
      conditional <- as.data.frame(matrix("", ntoteff, 4))
      colnames(conditional) <- c("IV to DV", "Path", "Source", "Conditional Effect")

      # Create the conditional effect table info
      estcntr <- 0
      for (a in 1:vs.env$nUserSpecPaths) {

        # Store each pair of user specified IV and DV
        if (utoteff[a] > 0) {
          estcntr <- estcntr + 1
          conditional[estcntr, 1] <- paste0("IV = ", vs.env$Varnames[vs.env$UserSpecIV[a]], ", DV = ", vs.env$Varnames[vs.env$UserSpecDV[a]])

          # Store each path for each pair of user specified IV and DV
          if (vs.env$nEffPCombo[a] > 0) {
            for (i in 1:vs.env$nEffPCombo[a]) {
              if (i > 1) {
                estcntr <- estcntr + 1
                conditional[estcntr, 1] <- conditional[estcntr - 1, 1]
              }
              conditional[estcntr, 2] <- paste0(vs.env$Varnames[vs.env$EffPath[vs.env$EffCombo[a, i, 1, 1], vs.env$nEffPaths[vs.env$EffCombo[a, i, 1, 1]]]])
              for (j in 2:vs.env$nEffPaths[vs.env$EffCombo[a, i, 1, 1]]) {
                k <- vs.env$nEffPaths[vs.env$EffCombo[a, i, 1, 1]] - j + 1
                conditional[estcntr, 2] <- paste0(conditional[estcntr, 2], " -> ", vs.env$Varnames[vs.env$EffPath[vs.env$EffCombo[a, i, 1, 1], k]])
              }

              # Store each source for each path
              if (vs.env$nEffSCombo[a, i] > 0) {
                for (j in 1:vs.env$nEffSCombo[a, i]) {
                  if (j > 1) {
                    estcntr <- estcntr + 1
                    conditional[estcntr, 1:2] <- conditional[estcntr - 1, 1:2]
                  }
                  if (vs.env$nEffvars[vs.env$EffCombo[a, i, j, 1]] > 0) {
                    conditional[estcntr, 3] <- paste0("{", vs.env$Varnames[vs.env$Effvar[vs.env$EffCombo[a, i, j, 1], 1]])
                    if (vs.env$nEffvars[vs.env$EffCombo[a, i, j, 1]] > 1) {
                      for (k in 2:vs.env$nEffvars[vs.env$EffCombo[a, i, j, 1]]) {
                        if (vs.env$EffGroup[vs.env$EffCombo[a, i, j, 1], k] != vs.env$EffGroup[vs.env$EffCombo[a, i, j, 1], k - 1]) {
                          conditional[estcntr, 3] <- paste0(conditional[estcntr, 3], "}{")
                        } else {
                          conditional[estcntr, 3] <- paste0(conditional[estcntr, 3], " * ")
                        }
                        conditional[estcntr, 3] <- paste0(conditional[estcntr, 3], vs.env$Varnames[vs.env$Effvar[vs.env$EffCombo[a, i, j, 1], k]])
                      }
                    }
                    conditional[estcntr, 3] <- paste0(conditional[estcntr, 3], "}")
                  } else {
                    conditional[estcntr, 3] <- "{1}"
                  }

                  # Store each conditional effect for each source
                  if (vs.env$nEffCombo[a, i, j] > 0) {
                    for (k in 1:vs.env$nEffCombo[a, i, j]) {
                      if (k > 1) {
                        estcntr <- estcntr + 1
                        conditional[estcntr, 1:3] <- conditional[estcntr - 1, 1:3]
                      }
                      conditional[estcntr, 4] <- vs.env$EffPresent[vs.env$EffCombo[a, i, j, k]]
                    }

                    # If more than one conditional effect, store the total conditional effect
                    if (vs.env$nEffCombo[a, i, j] > 1) {
                      estcntr <- estcntr + 1
                      conditional[estcntr, 1:3] <- conditional[estcntr - 1, 1:3]
                      conditional[estcntr, 4] <- "Total"
                    }
                  }
                }
              }
            }
          }
        } else {
          nremarks <- nremarks + 1
          remarks[nremarks] <- paste0("* No paths between IV = ", vs.env$Varnames[vs.env$UserSpecIV[a]], " and DV = ", vs.env$Varnames[vs.env$UserSpecDV[a]])
        }
      }

      # Store the remarks for source of conditional effects
      nsource <- 0
      source <- NULL
      if (vs.env$nEffDecomposed[1] > 0) {
        for (i in 1:vs.env$nEffDecomposed[1]) {
          nsource <- nsource + 1
          source[nsource] <- paste0("F", i, " = ", vs.env$Varnames[vs.env$EffDecvar[1, i, vs.env$nEffDecvars[1, i]]], " ->")
          if (vs.env$nEffDecvars[1, i] > 1) {
            for (j in 2:vs.env$nEffDecvars[1, i]) {
              k <- vs.env$nEffDecvars[1, i] - j + 1
              source[nsource] <- paste0(source[nsource], " ", vs.env$Varnames[vs.env$EffDecvar[1, i, k]], " ->")
            }
          }
        }
      }
      if (vs.env$nEffDecomposed[2] > 0) {
        for (i in 1:vs.env$nEffDecomposed[2]) {
          nsource <- nsource + 1
          source[nsource] <- paste0("[S", i, "] = ", vs.env$Varnames[vs.env$EffDecvar[2, i, vs.env$nEffDecvars[2, i]]], " ->")
          if (vs.env$nEffDecvars[2, i] > 1) {
            for (j in 2:vs.env$nEffDecvars[2, i]) {
              k <- vs.env$nEffDecvars[2, i] - j + 1
              source[nsource] <- paste0(source[nsource], " ", vs.env$Varnames[vs.env$EffDecvar[2, i, k]], " ->")
            }
          }
        }
      }
      remarks <- c(source, remarks)
    } else {
      nremarks <- nremarks + 1
      if (vs.env$nUserSpecPaths > 0) {
        remarks[nremarks] <- paste0("No paths between all user specified pairs of IV and DV\n  No conditional effects will be displayed")
      }
    }
  }

  conditional_info <- list("Info" = conditional, "Remarks" = remarks)
  cat("Conditional effect info generated...\n")

  conditional_info
}
