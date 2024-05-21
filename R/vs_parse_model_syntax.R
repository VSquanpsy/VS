# Parse the model syntax into separated paths

vs_parse_model_syntax <- function(vs.env = NULL, data = NULL, model.syntax = NULL) {

  # check for empty syntax
  if(nchar(model.syntax) == 0) {
    stop("VS ERROR: empty model syntax")
  }

  # remove comments prior to split:
  # match from comment character to newline, but don't eliminate newline
  model.syntax <- gsub("[#!].*(?=\n)", "", model.syntax, perl = TRUE)
  # remove all whitespace prior to split
  model.syntax <- gsub("[ \t]+", "", model.syntax, perl = TRUE)
  # remove any occurrence of >= 2 consecutive newlines to eliminate blank statements
  model.syntax <- gsub("\n{2,}", "\n", model.syntax, perl = TRUE)

  # check for empty syntax
  if(nchar(model.syntax) == 0) {
    stop("VS ERROR: empty model syntax")
  }

  # break up in lines
  model <- unlist(strsplit(model.syntax, "\n"))
  # remove the first syntax if it is an empty line
  if (model[1] == "") {
    model <- model[-1]
  }

  for (i in 1:length(model)) {
    equal.sign <- regexpr("=", model[i])
    if (equal.sign == -1) {
      stop("VS ERROR: Invalid model syntax: ", model[i])
    } else {
      lhs <- substr(model[i], 1, equal.sign-1)
      # check if lhs is empty
      # If yes, stop and return error
      if (nchar(lhs) == 0L) {
        stop("VS ERROR: Invalid model syntax: ", model[i])
      } else {
        # check if lhs is a path
        # If yes, type = 2
        # If no, type = 1
        if (grepl("->", lhs, fixed = TRUE)) {
          type <- 2
        } else {
          type <- 1
          if (!is.null(vs.env$Pathname)) {
            for (j in vs.env$Pathname) {
              # check if lhs is a path name
              # If yes, type = 3
              if (lhs == j) {
                type <- 3
              }
            }
          }
        }
      }
      rhs <- substr(model[i], equal.sign+1, nchar(model[i]))
      # check if rhs is empty
      # If yes, stop and return error
      if (nchar(rhs) == 0L) {
        stop("VS ERROR: Invalid model sytnax:", model[i])
      } else {
        RHS <- strsplit(rhs, split = "+", fixed = TRUE)[[1]]

        # check if there are path names specified at the end of the IV list
        pathlist <- NULL
        if (grepl("\\(", RHS[length(RHS)])) {
          parenthesis <- regexpr("\\(", RHS[length(RHS)])

          # check if user forgot the close parenthesis
          if (grepl("\\)", RHS[length(RHS)])) {
            paths <- substr(RHS[length(RHS)], parenthesis+1, nchar(RHS[length(RHS)])-1)
          } else {
            paths <- substr(RHS[length(RHS)], parenthesis+1, nchar(RHS[length(RHS)]))
          }

          pathlist <- strsplit(paths, split = ",", fixed = TRUE)[[1]]
          if (!is.null(pathlist)) {
            for (j in pathlist) {
              # Check if the length of the path names <= 5
              if (nchar(j) > 5L) {
                stop("VS ERROR: Path name is too long: ", j, "\n          Maximum length of path names = 5")
              } else {
                if (!is.null(vs.env$Pathname)) {
                  for (k in vs.env$Pathname) {
                    # Check for any duplicated path names
                    if (j == k) {
                      stop("VS ERROR: Duplicated path names: ", j)
                    }
                  }
                }
              }
            }
          }
          RHS[length(RHS)] <- substr(RHS[length(RHS)], 1, parenthesis-1)
        }
      }

      if (type == 1) {
        vs.env <- vs_store_dv(vs.env, lhs)
        Y <- vs.env$ExtraSave
        if (length(RHS) > 0) {
          for (j in 1:length(RHS)) {
            vs.env <- vs_store_iv(vs.env, RHS[j])
            X <- vs.env$ExtraSave
            same.path <- 0

            # Check if the path has already been defined
            if (vs.env$nPaths > 0) {
              for (k in 1:vs.env$nPaths) {
                if (vs.env$Pathtype[k] == 1 && vs.env$Pathfrom[k] == X && vs.env$Pathto[k] == Y) {
                  same.path <- 1
                }
              }
            }
            # If the path has not been defined, add new path
            if (same.path == 0) {
              vs.env$nPaths <- vs.env$nPaths + 1
              vs.env$Pathtype[vs.env$nPaths] <- 1
              vs.env$Modpath[vs.env$nPaths] <- 0
              vs.env$Pathcoef[vs.env$nPaths] <- vs.env$nPaths
              vs.env$Pathfrom[vs.env$nPaths] <- X
              vs.env$Pathto[vs.env$nPaths] <- Y
              vs.env$PathX[vs.env$nPaths] <- 0
              if (vs.env$nPaths > 1) {
                arr <- array(0, c(vs.env$nPaths, vs.env$N, vs.env$N))
                arr[-vs.env$nPaths, , ] <- vs.env$Pathoutcoef
                vs.env$Pathoutcoef <- arr
              }
              vs.env$Pathoutcoef[vs.env$nPaths, vs.env$Pathfrom[vs.env$nPaths], Y] <- vs.env$nPaths
              if (length(pathlist) >= j) {
                vs.env$Pathname[vs.env$nPaths] <- pathlist[j]
              } else {
                vs.env$Pathname[vs.env$nPaths] <- ""
              }
            }
          }
        }
      }
      else {
        vs.env <- vs_store_path(vs.env, type, lhs)
        XY <- vs.env$ExtraSave
        if (length(RHS) > 0) {
          for (j in 1:length(RHS)) {
            vs.env <- vs_store_iv(vs.env, RHS[j])
            X <- vs.env$ExtraSave
            same.path <- 0
            # Check if the path has already been defined
            if (vs.env$nPaths > 0) {
              for (k in 1:vs.env$nPaths) {
                if(vs.env$Pathtype[k] == 2 && vs.env$Pathfrom[k] == X && vs.env$Pathto[k] == XY) {
                  same.path <- 1
                }
              }
            }
            # If the path has not been defined, add new path
            if (same.path == 0) {
              vs.env$nPaths <- vs.env$nPaths + 1
              vs.env$Pathtype[vs.env$nPaths] <- 2
              vs.env$Modpath[vs.env$nPaths] <- 0
              vs.env$Pathcoef[vs.env$nPaths] <- vs.env$nPaths
              vs.env$Pathfrom[vs.env$nPaths] <- X
              vs.env$Pathto[vs.env$nPaths] <- XY
              vs.env$PathX[vs.env$nPaths] <- 0
              if (vs.env$nPaths > 1) {
                arr <- array(0, c(vs.env$nPaths, vs.env$N, vs.env$N))
                arr[-vs.env$nPaths, , ] <- vs.env$Pathoutcoef
                vs.env$Pathoutcoef <- arr
              }

              if (length(pathlist) >= j) {
                vs.env$Pathname[vs.env$nPaths] <- pathlist[j]
              } else {
                vs.env$Pathname[vs.env$nPaths] <- ""
              }
            }
          }
        }
      }
    }
  }

  if (vs.env$nPaths == 0) {
    stop("VS ERROR: No paths specified")
  }

  # set original path identifiers
  vs.env$PathID <- rep(0, vs.env$nPaths)
  vs.env$Pathadded <- rep(0, vs.env$nPaths)

  # copy and save the conceptual model
  vs.env$CptPathtype <- vs.env$Pathtype
  vs.env$CptPathfrom <- vs.env$Pathfrom
  vs.env$CptPathto <- vs.env$Pathto
  vs.env$CptPathadded <- vs.env$Pathadded
  vs.env$CptPathname <- vs.env$Pathname
  vs.env$nCptPaths <- vs.env$nPaths

  vs.env$CptUseX <- vs.env$UseX
  vs.env$CptUseY <- vs.env$UseY
  vs.env$CptUsePathY <- vs.env$UsePathY
  vs.env$CptUsePathX <- vs.env$UsePathX
  vs.env$nCptUsePaths <- vs.env$nUsePaths

  # copy and save the equation structure
  vs.env$EqnPathtype <- vs.env$Pathtype
  vs.env$EqnPathfrom <- vs.env$Pathfrom
  vs.env$EqnPathto <- vs.env$Pathto
  vs.env$EqnPathnvars <- rep(0, vs.env$nPaths)
  vs.env$EqnPathvar <- matrix(0, vs.env$nPaths, 3)

  cat("Model syntax read...\n")

  vs.env
}


# Find and store the DV used

vs_store_dv <- function(vs.env = NULL, check_dv = NULL) {

  founddv <- 0
  vs.env$ExtraSave <- NULL

  if (length(vs.env$Varnames) > 0) {
    for (i in 1:length(vs.env$Varnames)) {
      if (vs.env$Varnames[i] == check_dv) {
        founddv <- 1
        vs.env$Usevar[i] <- 1
        vs.env$UseY[i] <- 1

        vs.env$ExtraSave <- i
      }
    }
  }
  # If DV name not found in data variable names, stop and return error
  if (!founddv) {
    stop("VS ERROR: DV name not found: ", check_dv)
  }

  vs.env
}


# Find and store the IV used

vs_store_iv <- function(vs.env = NULL, check_iv = NULL) {

  foundiv <- 0
  vs.env$ExtraSave <- NULL

  if (length(vs.env$Varnames) > 0) {
    for (i in 1:length(vs.env$Varnames)) {
      if (vs.env$Varnames[i] == check_iv) {
        foundiv <- 1
        vs.env$Usevar[i] <- 1
        vs.env$UseX[i] <- 1

        vs.env$ExtraSave <- i
      }
    }
  }

  # If IV name not found in data variable names, stop and return error
  if (!foundiv) {
    stop("VS ERROR: IV name not found: ", check_iv)
  }

  vs.env
}


# Find and store the path used

vs_store_path <- function(vs.env = NULL, type = NULL, check_path = NULL) {

  foundp <- 0
  foundpath <- 0
  vs.env$ExtraSave <- NULL

  if (type == 2) {
    path.vars <- strsplit(check_path, "->", fixed = TRUE)[[1]]

    # Check if the path is specified before
    if (vs.env$nPaths > 0) {
      for (i in 1:vs.env$nPaths) {
        if (vs.env$Varnames[vs.env$Pathfrom[i]] == path.vars[1] && vs.env$Varnames[vs.env$Pathto[i]] == path.vars[2]) {

          # Check if the path has been created
          if (vs.env$nUsePaths > 0) {
            for (j in 1:length(vs.env$nUsePaths)) {
              if (vs.env$UsePathX[j] == path.vars[1] && vs.env$UsePathY[j] == path.vars[2]) {
                foundpath <- 1
                break
              }
            }
          }

          # If path has not been created, create the path
          if (!foundpath) {
            vs.env$nUsePaths <- vs.env$nUsePaths + 1
            vs.env$UsePathX[vs.env$nUsePaths] <- path.vars[1]
            vs.env$UsePathY[vs.env$nUsePaths] <- path.vars[2]
          }
          foundp <- 1
          vs.env$Modpath[i] <- 1

          vs.env$ExtraSave <- i
        }
      }
    }
  } else {

    # Check if the path name is specified before
    if (vs.env$nPaths > 0) {
      for (i in 1:vs.env$nPaths) {
        if (vs.env$Pathname[i] == check_path) {

          # Check if the path has been created
          if (vs.env$nUsePaths > 0) {
            for (j in 1:length(vs.env$nUsePaths)) {
              if (vs.env$UsePathX[j] == "" && vs.env$UsePathY[j] == check_path) {
                foundpath <- 1
                break
              }
            }
          }

          # If path has not been created, create the path
          if (!foundpath) {
            vs.env$nUsePaths <- vs.env$nUsePaths + 1
            vs.env$UsePathX[vs.env$nUsePaths] <- ""
            vs.env$UsePathY[vs.env$nUsePaths] <- check_path
          }
          foundp <- 1
          vs.env$Modpath[i] <- 1

          vs.env$ExtraSave <- i
        }
      }
    }
  }

  # If path or path name is not specified before, stop and return error
  if (!foundp) {
    stop("VS ERROR: Path not found: ", check_path)
  }

  vs.env
}

