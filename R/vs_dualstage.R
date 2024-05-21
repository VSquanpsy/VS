# Check if model is a dual stage moderated mediation model
# if yes, store the model structure

vs_check_dualstage <- function(vs.env = NULL) {

  dualstageb <- TRUE
  X <- NULL
  M <- NULL
  Y <- NULL
  W <- NULL
  Z <- NULL
  dualstagepath <- NULL
  if (sum(vs.env$Pathorder == 1) == 2) {
    medtype <- vs.env$Pathtype[vs.env$Pathorder == 1]
    if (sum(medtype == 2) == 2) {
      medfrom <- vs.env$Pathfrom[vs.env$Pathorder == 1]
      medto <- vs.env$Pathto[vs.env$Pathorder == 1]
      if (medto[1] != medto[2]) {
        if (sum(vs.env$Pathorder == 0) == 2 || sum(vs.env$Pathorder == 0) == 3) {
          id <- seq(1:vs.env$nPaths)[vs.env$Pathorder == 0]
          from <- vs.env$Pathfrom[vs.env$Pathorder == 0]
          to <- vs.env$Pathto[vs.env$Pathorder == 0]

          # Confirm the structure of the ordinary paths (X->M->Y)
          if (sum(vs.env$Pathorder == 0) == 2) {
            if (from[1] == to[2]) {
              first <- 2
              second <- 1
            } else {
              if (from[2] == to[1]) {
                first <- 1
                second <- 2
              } else {
                dualstageb <- FALSE
              }
            }

            # Confirm the structure of the ordinary paths (X->M->Y & X->Y)
          } else {
            if (from[1] == to[2] && from[2] == from[3] && to[1] == to[3]) {
              first <- 2
              second <- 1
            } else {
              if (from[2] == to[1] && from[1] == from[3] && to[2] == to[3]) {
                first <- 1
                second <- 2
              } else {
                if (from[1] == to[3] && from[3] == from[2] && to[1] == to[2]) {
                  first <- 3
                  second <- 1
                } else {
                  if (from[3] == to[1] && from[1] == from[2] && to[3] == to[2]) {
                    first <- 1
                    second <- 3
                  } else {
                    if (from[2] == to[3] && from[3] == from[1] && to[2] == to[1]) {
                      first <- 3
                      second <- 2
                    } else {
                      if (from[3] == to[2] && from[2] == from[1] && to[3] == to[1]) {
                        first <- 2
                        second <- 3
                      } else {
                        dualstageb <- FALSE
                      }
                    }
                  }
                }
              }
            }
          }
          if (dualstageb == TRUE) {
            X <- vs.env$Varnames[from[first]]
            M <- vs.env$Varnames[to[first]]
            Y <- vs.env$Varnames[to[second]]
            if (medto[1] == id[first] && medto[2] == id[second]) {
              W <- vs.env$Varnames[medfrom[1]]
              Z <- vs.env$Varnames[medfrom[2]]
            } else {
              if (medto[1] == id[second] && medto[2] == id[first]) {
                W <- vs.env$Varnames[medfrom[2]]
                Z <- vs.env$Varnames[medfrom[1]]
              } else {
                dualstageb <- FALSE
              }
            }
          }
        } else {
          dualstageb <- FALSE
        }
      } else {
        dualstageb <- FALSE
      }
    } else {
      dualstageb <- FALSE
    }
  } else {
    dualstageb <- FALSE
  }

  if (dualstageb == TRUE) {
    dualstagepath <- paste(X, "->", M, "->", Y)

  }

  dualstage <- list("Boolean" = dualstageb,
                    "Path" = dualstagepath,
                    "X" = X,
                    "M" = M,
                    "Y" = Y,
                    "W" = W,
                    "Z" = Z,
                    "WX" = NULL,
                    "ZM" = NULL)

  dualstage
}


# Find and store the interaction terms for WX and ZM in dual stage moderated mediation model

vs_dualstage_int <- function(dualstage = NULL, int = NULL) {

  if (!is.null(int)) {
    for (i in 1:nrow(int)) {
      if (sum(c(dualstage$W, dualstage$X) %in% int[i, 3:4]) == 2) {
        dualstage$WX <- paste0("int_", i)
      } else {
        if (sum(c(dualstage$Z, dualstage$M) %in% int[i, 3:4]) == 2) {
          dualstage$ZM <- paste0("int_", i)
        }
      }
    }
  }

  dualstage
}
