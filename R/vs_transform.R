# Transform the conceptual model to the working model

vs_transform <- function(vs.env = NULL) {

  checkpath <- NULL
  checked <- NULL
  MEMO <- NULL
  MEMOx <- NULL
  all_transformed <- 0
  idcntr <- 0
  while (all_transformed == 0) {
    found_mod <- 0

    # Search for plug
    for (i in 1:vs.env$nPaths) {
      if (vs.env$Pathtype[i] == 2) {
        transform_list <- NULL
        int_list <- NULL
        M_list <- NULL
        if (vs.env$Pathfrom[i] <= vs.env$N) {
          vs.env$isPlug[vs.env$Pathfrom[i]] <- 1
        }
        found_mod <- 1
        plug <- vs.env$Pathfrom[i]
        found_top <- 0
        ntrans <- 0
        find_next <- 1
        cntr <- 0

        # Search for 2nd plug on plug
        for (j in 1:vs.env$nPaths) {
          if (vs.env$Pathtype[j] == 2 && vs.env$Pathto[j] == i) {
            found_top <- 1
          }
        }
        if (found_top == 0) {

          # Search for predictors on plug
          for (j in 1:vs.env$nPaths) {
            if (vs.env$Pathtype[j] == 1 && vs.env$Pathto[j] == plug) {
              if (vs.env$Pathfrom[j] <= vs.env$N) {
                vs.env$isPlug[vs.env$Pathfrom[j]] <- 1
              }
              ntrans <- ntrans + 1
              transform_list[ntrans] <- j
              cntr <- cntr + 1
              checkpath[cntr] <- j
              checked[cntr] <- 0
            }
          }
        }
        while (find_next == 1) {
          find_next <- 0
          if (cntr > 0) {
            for (j in 1:cntr) {
              if (checked[j] == 0) {
                for (k in 1:vs.env$nPaths) {
                  if (vs.env$Pathtype[k] == 2 && vs.env$Pathto[k] == checkpath[j]) {
                    found_top <- 1
                    break
                  } else if (vs.env$Pathtype[k] == 1 && vs.env$Pathto[k] == vs.env$Pathfrom[checkpath[j]]) {
                    if (vs.env$Pathfrom[k] <= vs.env$N) {
                      vs.env$isPlug[vs.env$Pathfrom[k]] <- 1
                    }
                    ntrans <- ntrans + 1
                    transform_list[ntrans] <- k
                    cntr <- cntr + 1
                    checkpath[cntr] <- k
                    checked[cntr] <- 0
                    find_next <- 1
                  }
                }
              }
              checked[j] <- 1
              if (found_top == 1) {
                break
              }
            }
          }
        }

        # Change plug to path
        if (found_top == 0) {
          Y <- vs.env$Pathto[vs.env$Pathto[i]]
          X <- vs.env$Pathfrom[vs.env$Pathto[i]]
          type <- vs.env$Pathtype[vs.env$Pathto[i]]
          order <- vs.env$Pathorder[vs.env$Pathto[i]]
          vs.env$Pathtype[i] <- type
          vs.env$Pathto[i] <- Y
          sameid <- 0
          if (vs.env$Pathorder[i] == 2) {
            sameid <- 1
          }

          # Create interaction and path for plug
          if (sameid == 1) {
            vs.env <- vs_create_interactions(vs.env, i, X, Y, type, 0, 0)
            int <- vs.env$ExtraSave
          } else {
            idcntr <- idcntr + 1
            vs.env <- vs_create_interactions(vs.env, i, X, Y, type, 0, idcntr)
            int <- vs.env$ExtraSave
          }

          # Create interactions and paths for predictors on plug
          if (ntrans > 0) {
            for (j in 1:ntrans) {
              if (vs.env$PathID[transform_list[j]] == vs.env$PathID[i]) {
                if (sameid == 1) {
                  vs.env <- vs_create_interactions(vs.env, transform_list[j], X, Y, 1, 1, 0)
                  int_list[j] <- vs.env$ExtraSave
                } else {
                  vs.env <- vs_create_interactions(vs.env, transform_list[j], X, Y, 1, 1, idcntr)
                  int_list[j] <- vs.env$ExtraSave
                }
              }
            }

            # Check if the predictor on MeMo plug is the target moderator (W)
            # if yes, create paths from WX and X to M
            for (j in 1:ntrans) {
              is_W <- 1
              if (vs.env$Pathorder[transform_list[j]] > vs.env$Pathorder[i] || (vs.env$Pathorder[transform_list[j]] == vs.env$Pathorder[i] && (vs.env$PathX[transform_list[j]] == 0 || vs.env$PathX[transform_list[j]] == X))){
                for (k in 1:ntrans) {
                  if (j != k && vs.env$Pathto[transform_list[k]] == vs.env$Pathfrom[transform_list[j]]) {
                    is_W <- 0
                    break
                  }
                }
              } else {
                is_W <- 0
              }
              if (is_W == 1) {
                W <- vs.env$Pathfrom[transform_list[j]]
                M <- vs.env$Pathto[transform_list[j]]
                W_M_order <- vs.env$Pathorder[transform_list[j]]
                if (sum(MEMOx == X) == 0) {
                  checked_M <- 0
                } else {
                  checked_M <- sum(MEMO[which(MEMOx == X), ] == M)
                }
                if (checked_M == 0) {
                  M_list <- c(M_list, M)
                  WX <- int_list[j]
                  # Find MX
                  if (vs.env$Pathfrom[i] == M) {
                    MX <- int
                  } else {
                    for (k in 1:ntrans) {
                      if (vs.env$Pathfrom[transform_list[k]] == M) {
                        MX <- int_list[k]
                        break
                      }
                    }
                  }

                  # Check if paths form WX to M and from X to M exist
                  WX_M_exist <- 0
                  X_M_exist <- 0
                  for (k in 1:vs.env$nPaths) {
                    if (vs.env$Pathtype[k] == 1 && vs.env$Pathfrom[k] == WX && vs.env$Pathto[k] == M) {
                      WX_M_exist <- 1
                    } else if (vs.env$Pathtype[k] == 1 && vs.env$Pathfrom[k] == X && vs.env$Pathto[k] == M) {
                      X_M_exist <- 1
                    }
                    if (WX_M_exist == 1 && X_M_exist == 1) {
                      break
                    }
                  }

                  # If path from WX to M on plug does not exist, create it
                  if (WX_M_exist == 0) {
                    vs.env$nPaths <- vs.env$nPaths + 1
                    vs.env$Pathtype[vs.env$nPaths] <- 1
                    vs.env$Modpath[vs.env$nPaths] <- 0
                    vs.env$Pathfrom[vs.env$nPaths] <- WX
                    vs.env$Pathto[vs.env$nPaths] <- M
                    vs.env$PathX[vs.env$nPaths] <- X
                    if (sameid == 1) {
                      vs.env$PathID[vs.env$nPaths] <- 0
                    } else {
                      vs.env$PathID[vs.env$nPaths] <- idcntr
                    }
                    vs.env$Pathorder[vs.env$nPaths] <- W_M_order
                    vs.env$Pathcoef[vs.env$nPaths] <- 0
                    arr <- array(0, c(vs.env$nPaths, vs.env$new_N, vs.env$new_N))
                    arr[-vs.env$nPaths, , ] <- vs.env$Pathoutcoef
                    vs.env$Pathoutcoef <- arr
                  }

                  # If path from X to M on plug does not exist, create it
                  if (X_M_exist == 0) {
                    vs.env$nPaths <- vs.env$nPaths + 1
                    vs.env$Pathtype[vs.env$nPaths] <- 1
                    vs.env$Modpath[vs.env$nPaths] <- 0
                    vs.env$Pathfrom[vs.env$nPaths] <- X
                    vs.env$Pathto[vs.env$nPaths] <- M
                    vs.env$PathX[vs.env$nPaths] <- X
                    if (sameid == 1) {
                      vs.env$PathID[vs.env$nPaths] <- 0
                    } else {
                      vs.env$PathID[vs.env$nPaths] <- idcntr
                    }
                    vs.env$Pathorder[vs.env$nPaths] <- W_M_order
                    vs.env$Pathcoef[vs.env$nPaths] <- 0
                    arr <- array(0, c(vs.env$nPaths, vs.env$new_N, vs.env$new_N))
                    arr[-vs.env$nPaths, , ] <- vs.env$Pathoutcoef
                    vs.env$Pathoutcoef <- arr
                  }

                  # For second-order MX
                  if (vs.env$nintvars[MX-vs.env$N] == 3) {

                    # Find second-order mediator
                    M2 <- vs.env$intvar[MX-vs.env$N, 1]

                    # Check if paths form WX to M2 and from X to M2 exist
                    WX_M2_exist <- 0
                    X_M2_exist <- 0
                    for (k in 1:vs.env$nPaths) {
                      if (vs.env$Pathtype[k] == 1 && vs.env$Pathfrom[k] == WX && vs.env$Pathto[k] == M2) {
                        WX_M2_exist <- 1
                      } else if (vs.env$Pathtype[k] == 1 && vs.env$Pathfrom[k] == X && vs.env$Pathto[k] == M2) {
                        X_M2_exist <- 1
                      }
                      if (WX_M2_exist == 1 && X_M2_exist == 1) {
                        break
                      }
                    }
                    # If path from WX to M2 on plug does not exist, create it
                    if (WX_M2_exist == 0) {
                      vs.env$nPaths <- vs.env$nPaths + 1
                      vs.env$Pathtype[vs.env$nPaths] <- 1
                      vs.env$Modpath[vs.env$nPaths] <- 0
                      vs.env$Pathfrom[vs.env$nPaths] <- WX
                      vs.env$Pathto[vs.env$nPaths] <- M2
                      vs.env$PathX[vs.env$nPaths] <- X
                      if (sameid == 1) {
                        vs.env$PathID[vs.env$nPaths] <- 0
                      } else {
                        vs.env$PathID[vs.env$nPaths] <- idcntr
                      }
                      vs.env$Pathorder[vs.env$nPaths] <- W_M_order
                      vs.env$Pathcoef[vs.env$nPaths] <- 0
                      arr <- array(0, c(vs.env$nPaths, vs.env$new_N, vs.env$new_N))
                      arr[-vs.env$nPaths, , ] <- vs.env$Pathoutcoef
                      vs.env$Pathoutcoef <- arr
                    }

                    # If path from X to M2 on plug does not exist, create it
                    if (X_M2_exist == 0) {
                      vs.env$nPaths <- vs.env$nPaths + 1
                      vs.env$Pathtype[vs.env$nPaths] <- 1
                      vs.env$Modpath[vs.env$nPaths] <- 0
                      vs.env$Pathfrom[vs.env$nPaths] <- X
                      vs.env$Pathto[vs.env$nPaths] <- M2
                      vs.env$PathX[vs.env$nPaths] <- X
                      if (sameid == 1) {
                        vs.env$PathID[vs.env$nPaths] <- 0
                      } else {
                        vs.env$PathID[vs.env$nPaths] <- idcntr
                      }
                      vs.env$Pathorder[vs.env$nPaths] <- W_M_order
                      vs.env$Pathcoef[vs.env$nPaths] <- 0
                      arr <- array(0, c(vs.env$nPaths, vs.env$new_N, vs.env$new_N))
                      arr[-vs.env$nPaths, , ] <- vs.env$Pathoutcoef
                      vs.env$Pathoutcoef <- arr
                    }
                  }
                }
              }
            }
          }
          # Store the checked MEMO mediators
          if (!is.null(M_list)) {
            if (sum(MEMOx == X) == 0) {
              MEMOx <- c(MEMOx, X)
              if (is.null(MEMO)) {
                MEMO <- matrix(0, 1, length(M_list))
                MEMO[length(MEMOx), ] <- M_list
              } else {
                if (length(M_list) > ncol(MEMO)) {
                  for (k in (ncol(MEMO)+1):length(M_list)) {
                    MEMO <- cbind(MEMO, rep(0, nrow(MEMO)))
                  }
                }
                MEMO <- rbind(MEMO, rep(0, ncol(MEMO)))
                MEMO[length(MEMOx), 1:length(M_list)] <- M_list
              }
            } else {
              if (length(M_list) > ncol(MEMO)) {
                for (k in (ncol(MEMO)+1):length(M_list)) {
                  MEMO <- cbind(MEMO, rep(0, nrow(MEMO)))
                }
              }
              MEMO[which(MEMOx == X), 1:length(M_list)] <- M_list
            }
          }
        }
      }
    }

    if (found_mod == 0) {
      all_transformed <- 1
    }
  }

  cat("Transformation completed...\n")

  vs.env
}


# create interaction and interaction path of a specific moderation path for working model
# Return the interaction used in this path

vs_create_interactions <- function(vs.env = NULL, path = NULL, var = NULL, outcome = NULL,
                                   type = NULL, pred = NULL, id = NULL) {

  foundn <- 0
  foundY <- 0
  nchkints <- 0
  nchkintYs <- 0
  chkint <- NULL
  chkintY <- NULL
  vs.env$ExtraSave <- NULL

  # Assign variables in moderator into variable check list
  if (vs.env$Pathfrom[path] > vs.env$N) {
    for (i in 1:vs.env$nintvars[vs.env$Pathfrom[path] - vs.env$N]) {
      nchkints <- nchkints + 1
      chkint[nchkints] <- vs.env$intvar[vs.env$Pathfrom[path] - vs.env$N, i]
    }
  } else if (vs.env$Pathfrom[path] > 0) {
    nchkints <- nchkints + 1
    chkint[nchkints] <- vs.env$Pathfrom[path]
  }

  # Assign variables in IV into variable check list
  if (var > vs.env$N) {
    for (i in 1:vs.env$nintvars[var - vs.env$N]) {
      nchkints <- nchkints + 1
      chkint[nchkints] <- vs.env$intvar[var - vs.env$N, i]
    }
  } else if (var > 0) {
    nchkints <- nchkints + 1
    chkint[nchkints] <- var
  }

  # Check if the interaction is more than 3-way
  # If yes, stop and return error
  if (nchkints > 3) {
    stop("VS ERROR: Models are restricted to have up to second order moderations only\n          Please modify your model")
  }

  # steps for predictor on plug
  if (pred == 1) {

    # Assign variables in moderator into variable check list
    if (vs.env$Pathto[path] > vs.env$N) {
      for (i in 1:vs.env$nintvars[vs.env$Pathto[path] - vs.env$N]) {
        nchkintYs <- nchkintYs + 1
        chkintY[nchkintYs] <- vs.env$intvar[vs.env$Pathto[path] - vs.env$N, i]
      }
    } else {
      nchkintYs <- nchkintYs + 1
      chkintY[nchkintYs] <- vs.env$Pathto[path]
    }

    # Assign variables in DV into variable check list
    if (var > vs.env$N) {
      for (i in 1:vs.env$nintvars[var - vs.env$N]) {
        nchkintYs <- nchkintYs + 1
        chkintY[nchkintYs] <- vs.env$intvar[var - vs.env$N, i]
      }
    } else {
      nchkintYs <- nchkintYs + 1
      chkintY[nchkintYs] <- var
    }
  }

  # check if the interaction has been created
  if (nchkints > 1 && vs.env$nints > 0) {
    for (i in 1:vs.env$nints) {
      foundn <- 0
      if (vs.env$nintvars[i] == nchkints) {
        matched <- NULL
        for (j in 1:vs.env$nintvars[i]) {
          matched[j] <- 0
        }
        for (j in 1:nchkints) {
          for (k in 1:vs.env$nintvars[i]) {
            if (matched[k] == 0 && vs.env$intvar[i, k] == chkint[j]) {
              matched[k] <- 1
              foundn <- foundn + 1
              break
            }
          }
        }
        if (foundn == nchkints) {
          intX <- i + vs.env$N
          break
        }
      }
    }
  } else {
    foundn <- 1
    intX <- chkint[1]
  }

  # steps for predictor on plug
  if (pred == 1) {

    # find the outcome interaction term
    if (vs.env$nints > 0) {
      for (i in 1:vs.env$nints) {
        foundY <- 0
        if (vs.env$nintvars[i] == nchkintYs) {
          matched <- NULL
          for (j in 1:vs.env$nintvars[i]) {
            matched[j] <- 0
          }
          for (j in 1:nchkintYs) {
            for (k in 1:vs.env$nintvars[i]) {
              if (matched[k] == 0 && vs.env$intvar[i, k] == chkintY[j]) {
                matched[k] <- 1
                foundY <- foundY + 1
                break
              }
            }
          }
          if (foundY == nchkintYs) {
            intY <- i + vs.env$N
          }
        }
      }
    }
  }

  # if the interaction has been created, create the new paths
  if (foundn == nchkints) {
    if (pred == 1) {
      vs.env$nPaths <- vs.env$nPaths + 1
      vs.env$Pathtype[vs.env$nPaths] <- 1
      vs.env$Modpath[vs.env$nPaths] <- 0
      vs.env$Pathfrom[vs.env$nPaths] <-intX
      vs.env$Pathto[vs.env$nPaths] <- intY
      vs.env$PathX[vs.env$nPaths] <- var
      vs.env$PathID[vs.env$nPaths] <- id
      vs.env$Pathorder[vs.env$nPaths] <- vs.env$Pathorder[path]
      vs.env$UseY[intY] <- 1
    } else {
      vs.env$nPaths <- vs.env$nPaths + 1
      vs.env$Pathtype[vs.env$nPaths] <- type
      vs.env$Modpath[vs.env$nPaths] <- 0
      vs.env$Pathfrom[vs.env$nPaths] <- intX
      vs.env$Pathto[vs.env$nPaths] <- vs.env$Pathto[path]
      vs.env$PathX[vs.env$nPaths] <- var
      vs.env$PathID[vs.env$nPaths] <- id
      vs.env$Pathorder[vs.env$nPaths] <- vs.env$Pathorder[path]
    }
    vs.env$Pathcoef[vs.env$nPaths] <- vs.env$Pathcoef[path]
    arr <- array(0, c(vs.env$nPaths, vs.env$new_N, vs.env$new_N))
    arr[-vs.env$nPaths, , ] <- vs.env$Pathoutcoef
    vs.env$Pathoutcoef <- arr
    if (type == 1) {
      vs.env$Pathoutcoef[vs.env$nPaths, var, outcome] <- vs.env$Pathcoef[path]
    }
    if (vs.env$Pathfrom[path] > vs.env$N) {
      if (vs.env$nbase[vs.env$Pathfrom[path]] > 0) {
        for (j in 1: vs.env$nbase[vs.env$Pathfrom[path]]) {
          if (vs.env$nbase[intX] == ncol(vs.env$base)) {
            vs.env$base <- cbind(vs.env$base, rep(0, nrow(vs.env$base)))
          }
          vs.env$nbase[intX] <- vs.env$nbase[intX] + 1
          vs.env$base[intX, vs.env$nbase[intX]] <- vs.env$base[vs.env$Pathfrom[path], j]
        }
      }
    } else {
      if (vs.env$nbase[intX] == ncol(vs.env$base)) {
        vs.env$base <- cbind(vs.env$base, rep(0, nrow(vs.env$base)))
      }
      vs.env$nbase[intX] <- vs.env$nbase[intX] + 1
      vs.env$base[intX, vs.env$nbase[intX]] <- vs.env$Pathfrom[path]
    }
  } else {

    # if the interaction hasn't been created, create it and create the new path
    vs.env$nints <- vs.env$nints + 1
    vs.env$nintvars[vs.env$nints] <- 0
    vs.env$new_N <- vs.env$new_N + 1
    vs.env$Varnames[vs.env$new_N] <- paste0("int_", vs.env$nints)
    vs.env$IV[vs.env$new_N] <- vs.env$IV[vs.env$Pathfrom[path]]
    vs.env$DV[vs.env$new_N] <- 0
    vs.env$intvar <- rbind(vs.env$intvar, rep(0, 3))
    for (i in 1:nchkints) {
      vs.env$nintvars[vs.env$nints] <- vs.env$nintvars[vs.env$nints] + 1
      vs.env$intvar[vs.env$nints, vs.env$nintvars[vs.env$nints]] <- chkint[i]
    }
    vs.env$UseX[vs.env$new_N] <- 1
    vs.env$UseY[vs.env$new_N] <- 0
    vs.env$Usevar[vs.env$new_N] <- 1
    if (pred == 1) {
      vs.env$nPaths <- vs.env$nPaths + 1
      vs.env$Pathtype[vs.env$nPaths] <- 1
      vs.env$Modpath[vs.env$nPaths] <- 0
      vs.env$Pathfrom[vs.env$nPaths] <- vs.env$nints + vs.env$N
      vs.env$Pathto[vs.env$nPaths] <- intY
      vs.env$PathX[vs.env$nPaths] <- var
      vs.env$PathID[vs.env$nPaths] <- id
      vs.env$Pathorder[vs.env$nPaths] <- vs.env$Pathorder[path]
      vs.env$UseY[intY] <- 1
    } else {
      vs.env$nPaths <- vs.env$nPaths + 1
      vs.env$Modpath[vs.env$nPaths] <- 0
      vs.env$Pathtype[vs.env$nPaths] <- type
      vs.env$Pathfrom[vs.env$nPaths] <- vs.env$nints + vs.env$N
      vs.env$Pathto[vs.env$nPaths] <- vs.env$Pathto[path]
      vs.env$PathX[vs.env$nPaths] <- var
      vs.env$PathID[vs.env$nPaths] <- id
      vs.env$Pathorder[vs.env$nPaths] <- vs.env$Pathorder[path]
    }
    vs.env$Pathcoef[vs.env$nPaths] <- vs.env$Pathcoef[path]
    arr <- array(0, c(vs.env$nPaths, vs.env$new_N, vs.env$new_N))
    arr[-vs.env$nPaths, -vs.env$new_N, -vs.env$new_N] <- vs.env$Pathoutcoef
    vs.env$Pathoutcoef <- arr
    if (type == 1) {
      vs.env$Pathoutcoef[vs.env$nPaths, var, outcome] <- vs.env$Pathcoef[path]
    }
    vs.env$nbase[vs.env$nints + vs.env$N] <- 0
    vs.env$base <- rbind(vs.env$base, rep(0, ncol(vs.env$base)))
    if (vs.env$Pathfrom[path] > vs.env$N) {
      if (vs.env$nbase[vs.env$Pathfrom[path]] > 0) {
        for (i in 1:vs.env$nbase[vs.env$Pathfrom[path]]) {
          vs.env$nbase[vs.env$nints + vs.env$N] <- vs.env$nbase[vs.env$nints + vs.env$N] + 1
          vs.env$base[vs.env$nints + vs.env$N, vs.env$nbase[vs.env$nints + vs.env$N]] <- vs.env$base[vs.env$Pathfrom[path], i]
        }
      }
    } else {
      if (vs.env$nbase[vs.env$nints + vs.env$N] == ncol(vs.env$base)) {
        vs.env$base <- cbind(vs.env$base, rep(0, nrow(vs.env$base)))
      }
      vs.env$nbase[vs.env$nints + vs.env$N] <- vs.env$nbase[vs.env$nints + vs.env$N] + 1
      vs.env$base[vs.env$nints + vs.env$N, vs.env$nbase[vs.env$nints + vs.env$N]] <- vs.env$Pathfrom[path]
    }

    intX <- vs.env$nints + vs.env$N
  }

  # add paths from components of interactions to mediator interaction
  if (pred == 1) {
    foundX <- 0
    foundW <- 0
    for (i in 1:vs.env$nPaths) {
      if (vs.env$Pathtype[i] == 1 && vs.env$Pathfrom[i] == var && vs.env$Pathto[i] == intY) {
        foundX <- 1
      } else if (vs.env$Pathtype[i] == 1 && vs.env$Pathfrom[i] == vs.env$Pathfrom[path] && vs.env$Pathto[i] == intY) {
        foundW <- 1
      }
      if (foundX == 1 && foundW == 1) {
        break
      }
    }
    if (foundX == 0) {
      vs.env$nPaths <- vs.env$nPaths + 1
      vs.env$Pathtype[vs.env$nPaths] <- 1
      vs.env$Modpath[vs.env$nPaths] <- 0
      vs.env$Pathfrom[vs.env$nPaths] <- var
      vs.env$Pathto[vs.env$nPaths] <- intY
      vs.env$PathX[vs.env$nPaths] <- var
      vs.env$PathID[vs.env$nPaths] <- id
      vs.env$Pathorder[vs.env$nPaths] <- vs.env$Pathorder[path]
      vs.env$Pathcoef[vs.env$nPaths] <- 0
      arr <- array(0, c(vs.env$nPaths, vs.env$new_N, vs.env$new_N))
      arr[-vs.env$nPaths, , ] <- vs.env$Pathoutcoef
      vs.env$Pathoutcoef <- arr
    }
    if (foundW == 0) {
      vs.env$nPaths <- vs.env$nPaths + 1
      vs.env$Pathtype[vs.env$nPaths] <- 1
      vs.env$Modpath[vs.env$nPaths] <- 0
      vs.env$Pathfrom[vs.env$nPaths] <- vs.env$Pathfrom[path]
      vs.env$Pathto[vs.env$nPaths] <- intY
      vs.env$PathX[vs.env$nPaths] <- var
      vs.env$PathID[vs.env$nPaths] <- id
      vs.env$Pathorder[vs.env$nPaths] <- vs.env$Pathorder[path]
      vs.env$Pathcoef[vs.env$nPaths] <- 0
      arr <- array(0, c(vs.env$nPaths, vs.env$new_N, vs.env$new_N))
      arr[-vs.env$nPaths, , ] <- vs.env$Pathoutcoef
      vs.env$Pathoutcoef <- arr
    }
  }

  vs.env$ExtraSave <- intX

  vs.env
}


# Generate output data for working model
# Center the data if requested and create the interaction terms (if any)
# Variables are rearranged in the order of endogenous variables followed by exogenous variables

vs_working_data <- function(vs.env = NULL, data = NULL, scale = "raw", categorical = NULL, group = NULL) {

  catvars <- colnames(data) %in% categorical
  names(catvars) <- colnames(data)

  if (scale %in% c("center", "std")) {
    if (is.null(group)) {
      new_data <- data
      if (sum(!catvars) > 0) {

        # Centered data
        new_data[!catvars] <- as.data.frame(sapply(new_data[!catvars], function(x) scale(x, scale = FALSE)))

        # Standardized data
        if (scale == "std") {
          new_data[!catvars] <- as.data.frame(sapply(new_data[!catvars], function(x) x / sd(x)))
        }
      }
    } else {
      groupid <- unique(data[group])[, 1]
      ngroups <- length(groupid)
      groupvar <- colnames(data) == group
      groupvec <- data[groupvar]
      new_data <- data[!groupvar]
      catvars <- catvars[!groupvar]
      if (sum(!catvars) > 0) {
        for (i in 1:ngroups) {

          # Centered by group
          new_data[groupvec == groupid[i], !catvars] <- sapply(new_data[groupvec == groupid[i], !catvars], function(x) scale(x, scale=FALSE))

          # Standardized by group
          if (scale == "std") {
            new_data[groupvec == groupid[i], !catvars] <- sapply(new_data[groupvec == groupid[i], !catvars], function(x) x / sd(x))
          }
        }
      }
    }
  } else {

    # Raw data
    if (is.null(group)) {
      new_data <- data
    } else {
      groupvar <- colnames(data) == group
      groupvec <- data[groupvar]
      new_data <- data[!groupvar]
    }
  }

  # Create interactions
  int_desc <- NULL
  if (vs.env$nints > 0) {
    int_desc <- as.data.frame(matrix("", vs.env$nints, 5))
    colnames(int_desc) <- c("Label", "Nvars", "Var1", "Var2", "Var3")
    for (i in 1:vs.env$nints) {
      interaction <- rep(1, nrow(new_data))
      int_desc[i, 1] <- paste(vs.env$Varnames[vs.env$N + i], "=")
      int_desc[i, 2] <- vs.env$nintvars[i]
      for (j in 1:vs.env$nintvars[i]) {
        interaction <- interaction * new_data[vs.env$Varnames[vs.env$intvar[i, j]]]
        int_desc[i, j + 2] <- vs.env$Varnames[vs.env$intvar[i, j]]
        if (j == 1) {
          int_desc[i, 1] <- paste(int_desc[i, 1], vs.env$Varnames[vs.env$intvar[i, j]])
        } else {
          int_desc[i, 1] <- paste(int_desc[i, 1], "*", vs.env$Varnames[vs.env$intvar[i, j]])
        }
      }
      new_data[vs.env$Varnames[vs.env$N + i]] <- interaction
    }
  }
  if (!is.null(int_desc)) {
    int_desc <- as.data.frame(int_desc)
  }

  # Rearrange data by endogenous variables followed by exogenous variables
  endovars <- vs.env$Varnames[(vs.env$UseY == 1)]
  exovars <- vs.env$Varnames[(vs.env$UseX == 1 & vs.env$UseY == 0)]
  new_data <- new_data[c(endovars, exovars)]
  if (!is.null(group)) {
    new_data <- cbind(new_data, groupvec)
  }

  outlist <- list("Data" = new_data, "Interactions" = int_desc)

  outlist
}


# Generate EQUATION, VARIANCE and COVARIANCE matrices for working model

vs_working_matrices <- function(vs.env = NULL, local.dep = TRUE) {

  nPar <- 0
  vs.env$UID <- c(c(1:vs.env$new_N)[(vs.env$UseY == 1)], c(1:vs.env$new_N)[(vs.env$UseX == 1 & vs.env$UseY == 0)])
  Equation <- as.data.frame(matrix("", sum(vs.env$UseY), sum(vs.env$Usevar)))
  Order <- as.data.frame(matrix("", sum(vs.env$UseY), sum(vs.env$Usevar)))
  Covariance <- as.data.frame(matrix("", sum(vs.env$Usevar), sum(vs.env$Usevar)))
  endovars <- vs.env$Varnames[(vs.env$UseY == 1)]
  exovars <- vs.env$Varnames[(vs.env$UseX == 1 & vs.env$UseY == 0)]
  rownames(Equation) <- endovars
  colnames(Equation) <- c(endovars, exovars)
  rownames(Order) <- endovars
  colnames(Order) <- c(endovars, exovars)
  rownames(Covariance) <- c(paste0("e(", endovars, ")"), exovars)
  colnames(Covariance) <- c(paste0("e(", endovars, ")"), exovars)
  Matchcoef <- array(0, c(vs.env$N, vs.env$N, vs.env$nPaths))
  vs.env$ExtraSave <- NULL

  # Create EQUATION matrix
  for (i in 1:nrow(Equation)) {
    for (j in 1:ncol(Equation)) {
      for (k in 1:vs.env$nPaths) {
        if (vs.env$Pathtype[k] == 1 && vs.env$Pathfrom[k] == vs.env$UID[j] && vs.env$Pathto[k] == vs.env$UID[i]) {
          if (Equation[i, j] == "") {
            nPar <- nPar + 1
            Equation[i, j] <- nPar
            Order[i, j] <- vs.env$Pathorder[k]
          }

          # Create matrix for matching coefficients from conceptual model to working model
          for (l in 1:vs.env$N) {
            for (m in 1:vs.env$N) {
              if (vs.env$Pathoutcoef[k, l, m] > 0) {
                Matchcoef[l, m, vs.env$Pathoutcoef[k, l, m]] <- Equation[i, j]
              }
            }
          }
        }
      }
    }
  }

  # Create VARIANCE and COVARIANCE matrix
  for (i in 1:nrow(Covariance)) {
    for (j in 1:ncol(Covariance)) {
      if (i > sum(vs.env$UseY) && j > sum(vs.env$UseY)) {              # IV covariances
        if (i > j) {
          nPar <- nPar + 1
          Covariance[i, j] <- nPar
        }
      } else if (i <= sum(vs.env$UseY) && j <= sum(vs.env$UseY)) {     # Error covariances
        if (i > j) {
          if (vs.env$UID[i] > vs.env$N && vs.env$UID[j] > vs.env$N) {
            if (vs.env$nbase[vs.env$UID[i]] > 0) {
              for (k in 1:vs.env$nbase[vs.env$UID[i]]) {
                if (vs.env$nbase[vs.env$UID[j]] > 0) {
                  for (l in 1:vs.env$nbase[vs.env$UID[j]]) {
                    if (vs.env$base[vs.env$UID[j], l] == vs.env$base[vs.env$UID[i], k] && Covariance[i, j] == "") {
                      nPar <- nPar + 1
                      Covariance[i, j] <- nPar
                      break
                    }
                  }
                }
              }
            }
          } else if (vs.env$UID[i] > vs.env$N && vs.env$UID[j] <= vs.env$N) {
            if (local.dep == TRUE || vs.env$nintvars[vs.env$UID[i]-vs.env$N] > 2) {
              if (vs.env$nbase[vs.env$UID[i]] > 0) {
                for (k in 1:vs.env$nbase[vs.env$UID[i]]) {
                  if (vs.env$UID[j] == vs.env$base[vs.env$UID[i], k] && Covariance[i, j] == "") {
                    nPar <- nPar + 1
                    Covariance[i, j] <- nPar
                  }
                }
              }
            }
          }
        }
      } else if (i > sum(vs.env$UseY) && j <= sum(vs.env$UseY)) {      # IV-Error covariances
        if (vs.env$UID[i] > vs.env$N && vs.env$UseY[vs.env$UID[j]] == 1) {
          if (vs.env$UID[j] <= vs.env$N) {
            if (local.dep == TRUE || vs.env$nintvars[vs.env$UID[i]-vs.env$N] > 2) {
              for (k in 1:vs.env$nintvars[vs.env$UID[i] - vs.env$N]) {
                if (vs.env$intvar[vs.env$UID[i] - vs.env$N, k] == vs.env$UID[j] && Covariance[i, j] == "" && Equation[j, i] == "") {
                  nPar <- nPar + 1
                  Covariance[i, j] <- nPar
                }
              }
            }
          } else {
            if (local.dep == TRUE || vs.env$nintvars[vs.env$UID[i]-vs.env$N] + vs.env$nintvars[vs.env$UID[j]-vs.env$N] >= 4) {
              for (k in 1:vs.env$nintvars[vs.env$UID[i] - vs.env$N]) {
                for (l in 1:vs.env$nbase[vs.env$UID[j]]) {
                  if (vs.env$intvar[vs.env$UID[i] - vs.env$N, k] == vs.env$base[vs.env$UID[j], l] && Covariance[i, j] == "" && Equation[j, i] == "") {
                    nPar <- nPar + 1
                    Covariance[i, j] <- nPar
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  for (i in 1:nrow(Covariance)) {
    nPar <- nPar + 1
    Covariance[i, i] <- nPar
  }

  outlist <- list("Equation" = Equation, "Covariance" = Covariance, "Order" = Order, "Matchcoef" = Matchcoef)

  vs.env$ExtraSave <- outlist

  vs.env
}
