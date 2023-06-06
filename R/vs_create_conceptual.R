# vs_conceptual_model
#
# Create the conceptual model matrix

vs_create_conceptual <- function(vs.env = NULL, data = NULL) {

  conceptual <- as.data.frame(matrix("", sum(vs.env$CptUseY) + vs.env$nCptUsePaths, sum(vs.env$CptUseX)))
  colnames(conceptual) <- c(colnames(data)[vs.env$CptUseX == 1])
  if (sum(vs.env$CptUseY) == 1) {
    rownames(conceptual)[1] <- c(colnames(data)[vs.env$CptUseY == 1])
  } else {
    rownames(conceptual)[1:sum(vs.env$CptUseY)] <- c(colnames(data)[vs.env$CptUseY == 1])
  }
  if (vs.env$nCptUsePaths > 0) {
    cntr <- 1
    if(sum(vs.env$CptUsePathX != "") > 0) {
      for (i in 1:vs.env$nCptUsePaths) {
        if (vs.env$CptUsePathX[i] != "") {
          rownames(conceptual)[sum(vs.env$CptUseY) + cntr] <- paste(vs.env$CptUsePathX[i], "->", vs.env$CptUsePathY[i])
          cntr <- cntr + 1
        }
      }
    }
    if(sum(vs.env$CptUsePathX == "") > 0) {
      for (i in 1:vs.env$nCptUsePaths) {
        if (vs.env$CptUsePathX[i] == "") {
          rownames(conceptual)[sum(vs.env$CptUseY) + cntr] <- vs.env$CptUsePathY[i]
          cntr <- cntr + 1
        }
      }
    }
  }

  for (i in 1:vs.env$nCptPaths) {
    mark <- "*"
    if (vs.env$CptPathname[i] != "") {
      mark <- paste(mark, "(", vs.env$CptPathname[i], ")")
    }
    if (vs.env$CptPathtype[i] == 1) {
      conceptual[vs.env$Varnames[vs.env$CptPathto[i]], vs.env$Varnames[vs.env$CptPathfrom[i]]] <- mark
    } else {
      if (vs.env$CptPathname[vs.env$CptPathto[i]] == "") {
        rowindex <- paste(vs.env$Varnames[vs.env$CptPathfrom[vs.env$CptPathto[i]]], "->", vs.env$Varnames[vs.env$CptPathto[vs.env$CptPathto[i]]])
      } else {
        rowindex <- vs.env$CptPathname[vs.env$CptPathto[i]]
      }
      conceptual[rowindex, vs.env$Varnames[vs.env$CptPathfrom[i]]] <- mark
    }
  }

  conceptual
}

