# Parse the effect syntax

vs_parse_effect_syntax <- function(vs.env = NULL, data = NULL, effect.syntax = NULL) {

  # check for empty syntax
  if(nchar(effect.syntax) == 0) {
    stop("VS ERROR: empty effect syntax")
  }

  # remove comments prior to split:
  # match from comment character to newline, but don't eliminate newline
  effect.syntax <- gsub("[#!].*(?=\n)", "", effect.syntax, perl = TRUE)

  # remove all whitespace prior to split
  effect.syntax <- gsub("[ \t]+", "", effect.syntax, perl = TRUE)

  # remove any occurrence of >= 2 consecutive newlines to eliminate blank statements
  effect.syntax <- gsub("\n{2,}", "\n", effect.syntax, perl = TRUE)

  # break up in lines
  effect <- unlist(strsplit(effect.syntax, "\n"))

  # remove the first syntax if it is an empty line
  if (effect[1] == "") {
    effect <- effect[-1]
  }

  for (a in 1:length(effect)) {
    equal.sign <- regexpr("=", effect[a])
    if (equal.sign == -1) {
      stop("VS ERROR: Invalid effect syntax: ", effect[a])
    } else {
      lhs <- substr(effect[a], 1, equal.sign-1)
      rhs <- substr(effect[a], equal.sign+1, nchar(effect[a]))

      # check if lhs is empty
      # If yes, stop and return error
      if (nchar(lhs) <= 2L) {
        stop("VS ERROR: Invalid effect syntax: ", effect[a])
      } else {
        if (toupper(substr(lhs, 1, 2)) == "DV") {
          found <- 0
          if (vs.env$nUserSpecPaths > 0) {
            for (i in 1:vs.env$nUserSpecPaths) {
              if (vs.env$UserSpecID[i] == substr(lhs, 3, nchar(lhs))) {
                found <- 1
                if (vs.env$UserSpecDV[i] == 0) {
                  foundV <- 0
                  for (j in 1:length(vs.env$Varnames)) {
                    if (vs.env$Varnames[j] == rhs) {
                      vs.env$UserSpecDV[i] <- j
                      foundV <- 1
                      break
                    }
                  }
                  if (foundV == 0) {
                    stop("VS ERROR: Underfined variable in effect syntax: ", effect[a])
                  } else if (vs.env$DV[vs.env$UserSpecDV[i]] == 0) {
                    stop("VS ERROR: The effect DV specification must not be an IV or a mediator: ", effect[a])
                  }
                } else {
                  stop("VS ERROR: Duplicated effect DV specification: ", lhs)
                }
                break
              }
            }
          }
          if (found == 0) {
            foundV <- 0
            for (j in 1:length(vs.env$Varnames)) {
              if (vs.env$Varnames[j] == rhs) {
                vs.env$nUserSpecPaths <- vs.env$nUserSpecPaths + 1
                vs.env$UserSpecDV[vs.env$nUserSpecPaths] <- j
                vs.env$UserSpecIV[vs.env$nUserSpecPaths] <- 0
                vs.env$UserSpecID[vs.env$nUserSpecPaths] <- substr(lhs, 3, nchar(lhs))
                foundV <- 1
                break
              }
            }
            if (foundV == 0) {
              stop("VS ERROR: Underfined variable in effect syntax: ", effect[a])
            } else if (vs.env$DV[vs.env$UserSpecDV[vs.env$nUserSpecPaths]] == 0) {
              stop("VS ERROR: The effect DV specification must not be an IV or a mediator: ", effect[a])
            }
          }
        } else if (toupper(substr(lhs, 1, 2)) == "IV") {
          found <- 0
          if (vs.env$nUserSpecPaths > 0) {
            for (i in 1:vs.env$nUserSpecPaths) {
              if (vs.env$UserSpecID[i] == substr(lhs, 3, nchar(lhs))) {
                found <- 1
                if (vs.env$UserSpecIV[i] == 0) {
                  foundV <- 0
                  for (j in 1:length(vs.env$Varnames)) {
                    if (vs.env$Varnames[j] == rhs) {
                      vs.env$UserSpecIV[i] <- j
                      foundV <- 1
                      break
                    }
                  }
                  if (foundV == 0) {
                    stop("VS ERROR: Underfined variable in effect syntax: ", effect[a])
                  } else if (vs.env$IV[vs.env$UserSpecIV[i]] == 0) {
                    stop("VS ERROR: The effect IV specification must not be a DV or a mediator: ", effect[a])
                  }
                } else {
                  stop("VS ERROR: Duplicated effect IV specification: ", lhs)
                }
                break
              }
            }
          }
          if (found == 0) {
            foundV <- 0
            for (j in 1:length(vs.env$Varnames)) {
              if (vs.env$Varnames[j] == rhs) {
                vs.env$nUserSpecPaths <- vs.env$nUserSpecPaths + 1
                vs.env$UserSpecIV[vs.env$nUserSpecPaths] <- j
                vs.env$UserSpecDV[vs.env$nUserSpecPaths] <- 0
                vs.env$UserSpecID[vs.env$nUserSpecPaths] <- substr(lhs, 3, nchar(lhs))
                foundV <- 1
                break
              }
            }
            if (foundV == 0) {
              stop("VS ERROR: Underfined variable in effect syntax: ", effect[a])
            } else if (vs.env$IV[vs.env$UserSpecIV[vs.env$nUserSpecPaths]] == 0) {
              stop("VS ERROR: The effect IV specification must not be a DV or a mediator: ", effect[a])
            }
          }
        } else {
          stop("VS ERROR: Invalid effect syntax: ", effect[a])
        }
      }
    }
  }

  # Check whether the IV and DV in effect syntax are well matched
  if (vs.env$nUserSpecPaths > 0) {
    for (i in 1:length(vs.env$nUserSpecPaths)) {
      if (vs.env$UserSpecIV[i] == 0) {
        stop("VS ERROR: Missing IV in effect syntax: IV", vs.env$UserSpecID[i])
      } else if (vs.env$UserSpecDV[i] == 0) {
        stop("VS ERROR: Missing DV in effect syntax: DV", vs.env$UserSpecID[i])
      }
    }
    vs.env$uEff <- rep(0, vs.env$nUserSpecPaths)
    vs.env$uDirEff <- rep(0, vs.env$nUserSpecPaths)
  }

  cat("Effect syntax read...\n")

  vs.env
}
