# vs_check_model
#
# Check if the model specified is valid for VS

vs_check_model <- function(vs.env = NULL) {

  # check for direct invalid paths
  for (i in 1:vs.env$nPaths) {

    # Recursive paths
    if (vs.env$Pathtype[i] == 1 && vs.env$Pathfrom[i] == vs.env$Pathto[i]) {
      stop("VS ERROR: Recursive path - '", vs.env$Varnames[vs.env$Pathfrom[i]], "'\n          Please check model specification")
    } else if (vs.env$Pathtype[i] == 2) {

      # Self-moderation of endogenous variables
      if (vs.env$Pathtype[vs.env$Pathto[i]] == 1 && vs.env$Pathfrom[i] == vs.env$Pathto[vs.env$Pathto[i]]) {
        stop("VS ERROR: A variable cannot moderate any paths to itself - '", vs.env$Pathfrom[i], "'\n          Please check model specification")
      } else if (vs.env$Pathfrom[i] == vs.env$Pathfrom[vs.env$Pathto[i]] && vs.env$UseY[vs.env$Pathfrom[i]] == 1) {
        stop ("VS ERROR: An endogenous variable must not moderate any paths from itself - '", vs.env$Varnames[vs.env$Pathfrom[i]], "'\n          Please check model specification")
      }
    }
  }

  # Check for indirect recursive paths
  checkpath <- NULL
  checked <- NULL
  for (i in 1:vs.env$nPaths) {
    checknext <- 1
    cntr <- 1
    checkpath[1] <- i
    checked[1] <- 0
    while (checknext == 1) {
      checknext <- 0
      for (j in 1:cntr) {
        if (checked[j] == 0) {
          if (vs.env$Pathtype[checkpath[j]] == 1) {
            for (k in 1:vs.env$nPaths) {
              if (vs.env$Pathfrom[k] == vs.env$Pathto[checkpath[j]]) {
                if (vs.env$Pathtype[k] == 1 && vs.env$Pathto[k] == vs.env$Pathfrom[i]) {
                  stop("VS ERROR: Recursive path - '", vs.env$Varnames[vs.env$Pathfrom[i]],
                       "'\n          Please check model specification")
                } else if (vs.env$Pathtype[k] == 2 && vs.env$Pathto[k] == i) {
                  stop("VS ERROR: A variable must not moderate any paths to itself - '",
                       vs.env$Varnames[vs.env$Pathto[i]], "'\n          Please check model specification")
                } else {
                  cntr <- cntr + 1
                  checkpath[cntr] <- k
                  checked[cntr] <- 0
                  checknext <- 1
                }
              }
            }
          } else {
            for (k in 1:vs.env$nPaths) {
              if (k == vs.env$Pathto[checkpath[j]]) {
                if (vs.env$Pathtype[k] == 1 && vs.env$Pathto[k] == vs.env$Pathfrom[i]) {
                  stop("VS ERROR: Recursive path - '", vs.env$Varnames[vs.env$Pathfrom[i]],
                       "'\n          Please check model specification")
                } else if (vs.env$Pathtype[k] == 2 && vs.env$Pathto[k] == i) {
                  stop("VS ERROR: A variable must not moderate any paths to itself - '",
                       vs.env$Varnames[vs.env$Pathfrom[i]], "'\n          Please check model specification")
                } else {
                  cntr <- cntr + 1
                  checkpath[cntr] <- k
                  checked[cntr] <- 0
                  checknext <- 1
                }
              }
            }
          }
          checked[j] = 1
        }
      }
    }
  }

  # Check for indirect self-moderations of endogenous variables
  checkpath1 <- NULL
  checkpath2 <- NULL
  checked1 <- NULL
  checked2 <- NULL
  for (i in 1:length(vs.env$Varnames)) {
    if (vs.env$UseY[i] == 1) {
      cntr1 <- 0
      for (j in 1:vs.env$nPaths) {
        if (vs.env$Pathfrom[j] == i) {
          cntr1 <- cntr1 + 1
          checkpath1[cntr1] <- j
          checked1[cntr1] <- 0
        }
      }
      checknext <- 1
      while (checknext == 1) {
        checknext <- 0
        if (cntr1 > 0) {
          for (k in 1:cntr1) {
            if (checked1[k] == 0 && vs.env$Pathtype[checkpath1[k]] == 1) {
              for (l in 1:vs.env$nPaths) {
                if (vs.env$Pathfrom[l] == vs.env$Pathto[checkpath1[k]]) {
                  cntr1 <- cntr1 + 1
                  checkpath1[cntr1] <- l
                  checked1[cntr1] <- 0
                  checknext <- 1
                }
              }
            }
            checked1[k] <- 1
          }
        }
      }

      cntr2 <- 0
      for (j in 1:vs.env$nPaths) {
        if (vs.env$Pathfrom[j] == i) {
          cntr2 <- cntr2 + 1
          checkpath2[cntr2] <- j
          checked2[cntr2] <- 0
        }
      }
      checknext <- 1
      while (checknext == 1) {
        checknext <- 0
        if (cntr2 > 0) {
          for (k in 1:cntr2) {
            if (checked2[k] == 0) {
              for (l in 1:vs.env$nPaths) {
                if (vs.env$Pathtype[checkpath2[k]] == 1 && vs.env$Pathfrom[l] == vs.env$Pathto[checkpath2[k]]) {
                  cntr2 <- cntr2 + 1
                  checkpath2[cntr2] <- l
                  checked2[cntr2] <- 0
                  checknext <- 1
                } else if (vs.env$Pathtype[checkpath2[k]] == 2 && l == vs.env$Pathto[checkpath2[k]]) {
                  cntr2 <- cntr2 + 1
                  checkpath2[cntr2] <- l
                  checked2[cntr2] <- 0
                  checknext <- 1
                }
              }
            }
            checked2[k] <- 1
          }
        }
      }

      if (cntr1 > 0) {
        for (j in 1:cntr1) {
          if (cntr2 > 0) {
            for (k in 1:cntr2) {
              if (vs.env$Pathtype[checkpath2[k]] == 2 && vs.env$Pathto[checkpath2[k]] == checkpath1[j]) {
                stop ("VS ERROR: An endogenous variable cannot moderate any paths from itself - '",
                      vs.env$Varnames[i], "'\n          Please check model specification")
              }
            }
          }
        }
      }
    }
  }
  cat("Model checked...\n")
}


# vs_path_info
#
# Find IVs and DVs in conceptual model
# and assign moderation order for each each

vs_path_info <- function(vs.env = NULL) {

  # set IV and DV
  for (i in 1:vs.env$nPaths) {
    vs.env$DV[vs.env$Pathfrom[i]] <- 0
    if (vs.env$Pathtype[i] == 1) {
      vs.env$IV[vs.env$Pathto[i]] <- 0
    }
  }

  # assign order for each path
  # Original paths = 0
  # First-order moderation paths = 1
  # Second-order moderation paths = 2
  vs.env$Pathorder <- rep(0, vs.env$nPaths)
  all_assigned <- 0
  while (all_assigned == 0) {
    all_assigned <- 1
    for (i in 1:vs.env$nPaths) {
      if (vs.env$Pathtype[i] == 2 && vs.env$Pathorder[i] <= vs.env$Pathorder[vs.env$Pathto[i]]) {
        vs.env$Pathorder[i] <- vs.env$Pathorder[vs.env$Pathto[i]] + 1
        all_assigned <- 0
      } else if (vs.env$Pathtype[i] == 1) {
        for (j in 1:vs.env$nPaths) {
          if (vs.env$Pathfrom[j] == vs.env$Pathto[i] && vs.env$Pathorder[i] != vs.env$Pathorder[j]) {
            vs.env$Pathorder[i] <- vs.env$Pathorder[j]
            all_assigned <- 0
          }
        }
      }
    }
  }

  vs.env
}
