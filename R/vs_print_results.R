#' Print information of input data
#'
#' Print the number of variables, list of variables, and number of observations of the \code{VS}
#' input data.
#'
#' @param output A \code{VS} list object created by \code{\link{VS}} or
#'   \code{\link{VS_groupEqual}}.
#'
#' @examples
#' modelspec <- '
#'  PV1MATH = MATHEFF
#'  MATHEFF->PV1MATH = ESCS + HEDRES
#'  HEDRES = ESCS
#'  '
#' effectspec <- '
#'  IV1 = MATHEFF
#'  DV1 = PV1MATH
#'  '
#' VS_model <- VS(PISA2012HK, model = modelspec, effect = effectspec, scale = "center")
#'
#' VS_input_summary(VS_model)
#'
#' @export

# Print information of input data

VS_input_summary <- function(output = NULL) {

  cat("INPUT DATA\n\n")
  cat("Number of Variables:", ncol(output$Input), "\n")
  cat("Variables are:\n")
  variables <- ""
  for (i in 1:ncol(output$Input)) {
    if (nchar(variables) + nchar(colnames(output$Input))[i] > 80) {
      cat(variables, "\n")
      variables <- ""
    }
    variables <- paste(variables, colnames(output$Input)[i])
  }
  cat(variables, "\n")
  cat("Number of Observations:", nrow(output$Input), "\n")
}


#' Print VS conceptual model
#'
#' Print the \code{VS} conceptual model in matrix form.
#'
#' @param output A \code{VS} list object created by \code{\link{VS}} or
#'   \code{\link{VS_groupEqual}}.
#'
#' @examples
#' modelspec <- '
#'  PV1MATH = MATHEFF
#'  MATHEFF->PV1MATH = ESCS + HEDRES
#'  HEDRES = ESCS
#'  '
#' effectspec <- '
#'  IV1 = MATHEFF
#'  DV1 = PV1MATH
#'  '
#' VS_model <- VS(PISA2012HK, model = modelspec, effect = effectspec, scale = "center")
#'
#' VS_printConceptual(VS_model)
#'
#' @export

# Print the conceptual model in matrix form

VS_printConceptual <- function(output = NULL) {

  cat("CONCEPTUAL MODEL\n\n")
  nprints <- ceiling((ncol(output$Conceptual)/6))
  for (i in 1:nprints) {
    printmatrix <- as.matrix(output$Conceptual[, ((i-1)*6+1):min(ncol(output$Conceptual),((i-1)*6+6))])
    rownames(printmatrix) <- rownames(output$Conceptual)
    colnames(printmatrix) <- colnames(output$Conceptual)[((i-1)*6+1):min(ncol(output$Conceptual),((i-1)*6+6))]
    printrow <- "                        "
    printline <- "                        "
    for (j in 1:ncol(printmatrix)) {
      printrow <- paste0(printrow, vs_align_center(colnames(printmatrix)[j], 9))
      printline <- paste0(printline, "---------")
    }
    cat(printrow, "\n")
    cat(printline, "\n")
    for (j in 1:nrow(printmatrix)) {
      if (sum(printmatrix[j, ] != "") > 0) {
        printrow <- paste0(sprintf("%20s", rownames(printmatrix)[j]), "    ")
        printline <- "                        "
        for (k in 1:ncol(printmatrix)) {
          mark <- printmatrix[j, k]
          if (mark != "") {
            printrow <- paste0(printrow, vs_align_center(mark, 9))
          } else {
            printrow <- paste0(printrow, "         ")
          }
          printline <- paste0(printline, "---------")
        }
        cat(printrow, "\n")
        cat(printline, "\n")
      }
    }
    if (i < nprints) {
      cat("\n\n")
    }
  }
}


#' Print information of output data
#'
#' Print the number of variables, number of valid cases, data type, list of variables,
#' and description of interactions created of the \code{VS} output data.
#'
#' @param output A \code{VS} list object created by \code{\link{VS}} or
#'   \code{\link{VS_groupEqual}}.
#'
#' @examples
#' modelspec <- '
#'  PV1MATH = MATHEFF
#'  MATHEFF->PV1MATH = ESCS + HEDRES
#'  HEDRES = ESCS
#'  '
#' effectspec <- '
#'  IV1 = MATHEFF
#'  DV1 = PV1MATH
#'  '
#' VS_model <- VS(PISA2012HK, model = modelspec, effect = effectspec, scale = "center")
#'
#' VS_output_summary(VS_model)
#'
#' @export

# Print information of output data

VS_output_summary <- function(output = NULL) {

  cat("OUTPUT DATA\n\n")
  cat("Number of Variables:", ncol(output$Output), "\n")
  if (output$Multisample == TRUE) {
    cat(paste0("Number of Valid Cases by ", output$Group, ":\n"))
    group <- paste0(output$Group, " = ", output$GroupID, ":")
    length_group <- max(nchar(group))
    obs <- NULL
    for (i in 1:output$N_groups) {
      obs[i] <- nrow(output$Output[output$Output[output$Group] == i, ])
    }
    length_obs <- max(nchar(obs))
    for (i in 1:output$N_groups) {
      printobs <-
        cat(paste0(" ", vs_align_left(group[i], length_group), " ",
                   vs_align_right(obs[i], length_obs), "\n"))
    }
  } else {
    cat("Number of Valid Cases:", nrow(output$Output),"\n")
  }
  cat("Data type:", output$Scale, "\n")
  cat("Variables are:\n")
  variables <- ""
  for (i in 1:ncol(output$Output)) {
    if (nchar(variables) + nchar(colnames(output$Output)[i]) > 80) {
      cat(variables, "\n")
      variables <- ""
    }
    variables <- paste(variables, colnames(output$Output)[i])
  }
  cat(variables, "\n")
  if (!is.null(output$Categorical)) {
    cat("Categorical are:\n")
    variables <- ""
    for (i in 1:length(output$Categorical)) {
      if (nchar(variables) + nchar(output$Categorical[i]) > 80) {
        cat(variables, "\n")
        variables <- ""
      }
      variables <- paste(variables, output$Categorical[i])
    }
    cat(variables, "\n")
  }
  cat("\n")

  if (!is.null(output$Int_labels)) {
    cat("Interaction Terms Created:\n")
    VS_printInt(output)
  }
}


#' Print interactions
#'
#' If any, print the description for interactions created in the \code{VS} model.
#'
#' @param output A \code{VS} list object created by \code{\link{VS}} or
#'   \code{\link{VS_groupEqual}}.
#'
#' @examples
#' modelspec <- '
#'  PV1MATH = MATHEFF
#'  MATHEFF->PV1MATH = ESCS + HEDRES
#'  HEDRES = ESCS
#'  '
#' effectspec <- '
#'  IV1 = MATHEFF
#'  DV1 = PV1MATH
#'  '
#' VS_model <- VS(PISA2012HK, model = modelspec, effect = effectspec, scale = "center")
#'
#' VS_printInt(VS_model)
#'
#' @export

# Print the description for interactions created in VS model

VS_printInt <- function(output = NULL) {

  if (!is.null(output$Int_labels)) {
    for (i in 1:nrow(output$Int_labels)) {
      cat("", output$Int_labels[i, 1], "\n")
    }
  } else {
    cat("No interactions created in your VS model")
  }
}


#' Print covariance matrix
#'
#' Print the covariance matrix of the \code{VS} output data in \code{VS} style.
#'
#' @param output A \code{VS} list object created by \code{\link{VS}} or
#'   \code{\link{VS_groupEqual}}.
#' @param groupid A numerical or character variable indicating any group ID for the multi-sample
#'   \code{\link{VS}} output list. If \code{"printall"} (default), a list of covariance matrices
#'   for all groups will be returned. If \code{Multisample} = \code{FALSE}, ignore this argument.
#'
#' @examples
#' modelspec <- '
#'  PV1MATH = MATHEFF
#'  MATHEFF->PV1MATH = ESCS + HEDRES
#'  HEDRES = ESCS
#'  '
#' effectspec <- '
#'  IV1 = MATHEFF
#'  DV1 = PV1MATH
#'  '
#' # VS model with centered data
#' VS_model <- VS(PISA2012HK, model = modelspec, effect = effectspec, scale = "center")
#' # VS multi-sample model with centered data
#' VS_group <- VS(PISA2012HK, model = modelspec, effect = effectspec, group = "IMMIG",
#'                scale = "center")
#' # VS multi-sample model constrainted by group with centered data
#' VS_group_equal <- VS_groupEqual(VS_group, effect.equal = 2)
#'
#' # print the covariance matrix for a VS model
#' VS_printCov(VS_model)
#'
#' # print the covariance matrix for a VS multi-sample model with IMMIG = 2
#' VS_printCov(VS_group, 2)
#'
#' # print the covariance matrices of all groups for a VS multi-sample model
#' VS_printCov(VS_group_equal)
#'
#' @export

# Print the covariance matrix in VS format

VS_printCov <- function(output = NULL, groupid = "printall") {

  covariance <- VS_getCov(output, groupid)

  if (output$Multisample == TRUE && groupid == "printall") {
    for (g in 1:length(covariance)) {
      matrix <- covariance[[g]]
      nprints <- ceiling((ncol(matrix)/6))
      cat(paste0("  GROUP ", g, ": ", output$Group, " = ", output$GroupID[g], "\n\n"))
      for (i in 1:nprints) {
        printmatrix <- as.matrix(matrix[((i-1)*6+1):nrow(matrix),
                                        ((i-1)*6+1):min(ncol(matrix),((i-1)*6+6))])
        rownames(printmatrix) <- rownames(matrix)[((i-1)*6+1):nrow(matrix)]
        colnames(printmatrix) <- colnames(matrix)[((i-1)*6+1):min(ncol(matrix),((i-1)*6+6))]
        cat("         Covariance Matrix\n\n")
        printrow <- "         "
        printline <- "         "
        for (j in 1:ncol(printmatrix)) {
          printrow <- paste0(printrow, sprintf("%11s", colnames(printmatrix)[j]))
          printline <- paste0(printline, "   --------")
        }
        cat(printrow, "\n")
        cat(printline, "\n")
        for (j in 1:nrow(printmatrix)) {
          printrow <- sprintf("%9s", rownames(printmatrix)[j])
          for (k in 1:ncol(printmatrix)) {
            if (j >= k) {
              printrow <- paste0(printrow, " ", vs_change_digits(printmatrix[j, k]))
            }
          }
          cat(printrow, "\n")
        }
        if (i < nprints) {
          cat("\n")
        }
      }
      if (g < length(covariance)) {
        cat("\n\n")
      }
    }
  } else {
    matrix <- covariance
    nprints <- ceiling((ncol(matrix)/6))
    for (i in 1:nprints) {
      printmatrix <- as.matrix(matrix[((i-1)*6+1):nrow(matrix),
                                      ((i-1)*6+1):min(ncol(matrix),((i-1)*6+6))])
      rownames(printmatrix) <- rownames(matrix)[((i-1)*6+1):nrow(matrix)]
      colnames(printmatrix) <- colnames(matrix)[((i-1)*6+1):min(ncol(matrix),((i-1)*6+6))]
      cat("         Covariance Matrix\n\n")
      printrow <- "         "
      printline <- "         "
      for (j in 1:ncol(printmatrix)) {
        printrow <- paste0(printrow, sprintf("%11s", colnames(printmatrix)[j]))
        printline <- paste0(printline, "   --------")
      }
      cat(printrow, "\n")
      cat(printline, "\n")
      for (j in 1:nrow(printmatrix)) {
        printrow <- sprintf("%9s", rownames(printmatrix)[j])
        for (k in 1:ncol(printmatrix)) {
          if (j >= k) {
            printrow <- paste0(printrow, " ", vs_change_digits(printmatrix[j, k]))
          }
        }
        cat(printrow, "\n")
      }
      if (i < nprints) {
        cat("\n")
      }
    }
  }
}


#' Print correlation matrix
#'
#' Print the correlation matrix of the \code{VS} output data in \code{VS} style.
#'
#' @param output A \code{VS} list object created by \code{\link{VS}} or
#'   \code{\link{VS_groupEqual}}.
#' @param groupid A numerical or character variable indicating any group ID for the multi-sample
#'   \code{\link{VS}} output list. If \code{"printall"} (default), a list of correlation matrices
#'   for all groups will be returned. If \code{Multisample} = \code{FALSE}, ignore this argument.
#'
#' @examples
#' modelspec <- '
#'  PV1MATH = MATHEFF
#'  MATHEFF->PV1MATH = ESCS + HEDRES
#'  HEDRES = ESCS
#'  '
#' effectspec <- '
#'  IV1 = MATHEFF
#'  DV1 = PV1MATH
#'  '
#' # VS model with centered data
#' VS_model <- VS(PISA2012HK, model = modelspec, effect = effectspec, scale = "center")
#' # VS multi-sample model with centered data
#' VS_group <- VS(PISA2012HK, model = modelspec, effect = effectspec, group = "IMMIG",
#'                scale = "center")
#' # VS multi-sample model constrainted by group with centered data
#' VS_group_equal <- VS_groupEqual(VS_group, effect.equal = 2)
#'
#' # print the correlation matrix for a VS model
#' VS_printCor(VS_model)
#'
#' # print the correlation matrix for a VS multi-sample model with IMMIG = 2
#' VS_printCov(VS_group, 2)
#'
#' # print the correlation matrices of all groups for a VS multi-sample model
#' VS_printCor(VS_group_equal)
#'
#' @export

# Print the correlation matrix in VS format

VS_printCor <- function(output = NULL, groupid = "printall") {

  correlation <- VS_getCor(output, groupid)

  if (output$Multisample == TRUE && groupid == "printall") {
    for (g in 1:length(correlation)) {
      matrix <- correlation[[g]]
      nprints <- ceiling((ncol(matrix)/6))
      cat(paste0("  GROUP ", g, ": ", output$Group, " = ", output$GroupID[g], "\n\n"))
      for (i in 1:nprints) {
        printmatrix <- as.matrix(matrix[((i-1)*6+1):nrow(matrix),
                                        ((i-1)*6+1):min(ncol(matrix),((i-1)*6+6))])
        rownames(printmatrix) <- rownames(matrix)[((i-1)*6+1):nrow(matrix)]
        colnames(printmatrix) <- colnames(matrix)[((i-1)*6+1):min(ncol(matrix),((i-1)*6+6))]
        cat("         Correlation Matrix\n\n")
        printrow <- "         "
        printline <- "         "
        for (j in 1:ncol(printmatrix)) {
          printrow <- paste0(printrow, sprintf("%11s", colnames(printmatrix)[j]))
          printline <- paste0(printline, "   --------")
        }
        cat(printrow, "\n")
        cat(printline, "\n")
        for (j in 1:nrow(printmatrix)) {
          printrow <- sprintf("%9s", rownames(printmatrix)[j])
          for (k in 1:ncol(printmatrix)) {
            if (j >= k) {
              printrow <- paste0(printrow, " ", vs_change_digits(printmatrix[j, k]))
            }
          }
          cat(printrow, "\n")
        }
        if (i < nprints) {
          cat("\n")
        }
      }
      if (g < length(correlation)) {
        cat("\n\n")
      }
    }
  } else {
    matrix <- correlation
    nprints <- ceiling((ncol(matrix)/6))
    for (i in 1:nprints) {
      printmatrix <- as.matrix(matrix[((i-1)*6+1):nrow(matrix),
                                      ((i-1)*6+1):min(ncol(matrix),((i-1)*6+6))])
      rownames(printmatrix) <- rownames(matrix)[((i-1)*6+1):nrow(matrix)]
      colnames(printmatrix) <- colnames(matrix)[((i-1)*6+1):min(ncol(matrix),((i-1)*6+6))]
      cat("         Correlation Matrix\n\n")
      printrow <- "         "
      printline <- "         "
      for (j in 1:ncol(printmatrix)) {
        printrow <- paste0(printrow, sprintf("%11s", colnames(printmatrix)[j]))
        printline <- paste0(printline, "   --------")
      }
      cat(printrow, "\n")
      cat(printline, "\n")
      for (j in 1:nrow(printmatrix)) {
        printrow <- sprintf("%9s", rownames(printmatrix)[j])
        for (k in 1:ncol(printmatrix)) {
          if (j >= k) {
            printrow <- paste0(printrow, " ", vs_change_digits(printmatrix[j, k]))
          }
        }
        cat(printrow, "\n")
      }
      if (i < nprints) {
        cat("\n")
      }
    }
  }
}


#' Print mean vector
#'
#' Print the mean vector of the \code{VS} output data in \code{VS} style.
#'
#' @param output A \code{VS} list object created by \code{\link{VS}} or
#'   \code{\link{VS_groupEqual}}.
#' @param groupid A numerical or character variable indicating any group ID for the multi-sample
#'   \code{\link{VS}} output list. If \code{"printall"} (default), a list of mean vectors
#'   for all groups will be returned. If \code{Multisample} = \code{FALSE}, ignore this argument.
#'
#' @examples
#' modelspec <- '
#'  PV1MATH = MATHEFF
#'  MATHEFF->PV1MATH = ESCS + HEDRES
#'  HEDRES = ESCS
#'  '
#' effectspec <- '
#'  IV1 = MATHEFF
#'  DV1 = PV1MATH
#'  '
#' # VS model with centered data
#' VS_model <- VS(PISA2012HK, model = modelspec, effect = effectspec, scale = "center")
#' # VS multi-sample model with centered data
#' VS_group <- VS(PISA2012HK, model = modelspec, effect = effectspec, group = "IMMIG",
#'                scale = "center")
#' # VS multi-sample model constrainted by group with centered data
#' VS_group_equal <- VS_groupEqual(VS_group, effect.equal = 2)
#'
#' # print the mean vector for a VS model
#' VS_printMean(VS_model)
#'
#' # print the mean vector for a VS multi-sample model with IMMIG = 2
#' VS_printMean(VS_group, 2)
#'
#' # print the mean vectors of all groups for a VS multi-sample model
#' VS_printMean(VS_group_equal)
#'
#' @export

# Print the mean vector in VS format

VS_printMean <- function(output = NULL, groupid = "printall") {

  mean <- VS_getMean(output, groupid)

  if (output$Multisample == TRUE && groupid == "printall") {
    for (g in 1:length(mean)) {
      vector <- mean[[g]]
      nprints <- ceiling((length(vector)/6))
      cat(paste0("  GROUP ", g, ": ", output$Group, " = ", output$GroupID[g], "\n\n"))
      for (i in 1:nprints) {
        printvector <- vector[((i-1)*6+1):min(length(vector), ((i-1)*6+6))]
        names(printvector) <- names(vector)[((i-1)*6+1):min(length(vector), ((i-1)*6+6))]
        cat("         Means\n\n")
        printrow <- "         "
        printline <- "         "
        for (j in 1:length(printvector)) {
          printrow <- paste0(printrow, sprintf("%11s", names(printvector)[j]))
          printline <- paste0(printline, "   --------")
        }
        cat(printrow, "\n")
        cat(printline, "\n")
        printrow <- "         "
        for (j in 1:length(printvector)) {
          printrow <- paste0(printrow, " ", vs_change_digits(printvector[j]))
        }
        cat(printrow, "\n")
        if (i < nprints) {
          cat("\n")
        }
      }
      if (g < length(mean)) {
        cat("\n\n")
      }
    }
  } else {
    vector <- mean
    nprints <- ceiling((length(vector)/6))
    for (i in 1:nprints) {
      printvector <- vector[((i-1)*6+1):min(length(vector), ((i-1)*6+6))]
      names(printvector) <- names(vector)[((i-1)*6+1):min(length(vector), ((i-1)*6+6))]
      cat("         Means\n\n")
      printrow <- "         "
      printline <- "         "
      for (j in 1:length(printvector)) {
        printrow <- paste0(printrow, sprintf("%11s", names(printvector)[j]))
        printline <- paste0(printline, "   --------")
      }
      cat(printrow, "\n")
      cat(printline, "\n")
      printrow <- "         "
      for (j in 1:length(printvector)) {
        printrow <- paste0(printrow, " ", vs_change_digits(printvector[j]))
      }
      cat(printrow, "\n")
      if (i < nprints) {
        cat("\n")
      }
    }
  }
}


#' Print VS working model
#'
#' Print the equation specification and covariance specification of the \code{VS} working
#' model in matrix form.
#'
#' @param output A \code{VS} list object created by \code{\link{VS}} or
#'   \code{\link{VS_groupEqual}}.
#'
#' @examples
#' modelspec <- '
#'  PV1MATH = MATHEFF
#'  MATHEFF->PV1MATH = ESCS + HEDRES
#'  HEDRES = ESCS
#'  '
#' effectspec <- '
#'  IV1 = MATHEFF
#'  DV1 = PV1MATH
#'  '
#' VS_model <- VS(PISA2012HK, model = modelspec, effect = effectspec, scale = "center")
#'
#' VS_printWorking(VS_model)
#'
#' @export

# Print model specification of working model
# (key for interactions, equation specification, covariance specification)

VS_printWorking <- function(output = NULL) {

  cat("WORKING MODEL\n\n")
  cat("Model Specification (corresponding to Lavaan output)\n\n")
  if (output$Bootstrap > 0) {
    cat("Bootstrap =", output$Bootstrap, "\n\n")
  }

  # Print description for interactions created
  if (!is.null(output$Int_labels)) {
    VS_printInt(output)
  }
  cat("\n\n")

  # Print equation specification
  nprints <- ceiling((ncol(output$Eq_spec)/6))
  for (i in 1:nprints) {
    printmatrix <- as.matrix(output$Eq_spec[, ((i-1)*6+1):min(ncol(output$Eq_spec), ((i-1)*6+6))])
    rownames(printmatrix) <- rownames(output$Eq_spec)
    colnames(printmatrix) <- colnames(output$Eq_spec)[((i-1)*6+1):min(ncol(output$Eq_spec), ((i-1)*6+6))]
    cat ("              EQUATION\n\n")
    printrow <- "              "
    printline <- "              "
    for (j in 1:ncol(printmatrix)) {
      printrow <- paste0(printrow, vs_align_center(colnames(printmatrix)[j], 13))
      printline <- paste0(printline, "-------------")
    }
    cat(printrow, "\n")
    cat(printline, "\n")
    for (j in 1:nrow(printmatrix)) {
      printrow <- paste0(sprintf("%13s", rownames(printmatrix)[j]), " ")
      printline <- "              "
      for (k in 1:ncol(printmatrix)) {
        if (printmatrix[j, k] != "") {
          printrow <- paste0(printrow, vs_align_center(paste0("a", printmatrix[j, k]), 13))
        } else {
          printrow <- paste0(printrow, "             ")
        }
        printline <- paste0(printline, "-------------")
      }
      cat(printrow, "\n")
      cat(printline, "\n")
    }
    if (i < nprints) {
      cat("\n")
    }
  }
  cat("\n\n")

  # Print variance and covariance specification
  nprints <- ceiling((ncol(output$Cov_spec)/6))
  for (i in 1:nprints) {
    printmatrix <- as.matrix(output$Cov_spec[((i-1)*6+1):nrow(output$Cov_spec),
                                             ((i-1)*6+1):min(ncol(output$Cov_spec), ((i-1)*6+6))])
    rownames(printmatrix) <- rownames(output$Cov_spec)[((i-1)*6+1):nrow(output$Cov_spec)]
    colnames(printmatrix) <- colnames(output$Cov_spec)[((i-1)*6+1):min(ncol(output$Cov_spec), ((i-1)*6+6))]
    cat ("              VARIANCE AND COVARIANCE\n\n")
    printrow <- "              "
    printline <- "              "
    for (j in 1:ncol(printmatrix)) {
      printrow <- paste0(printrow, vs_align_center(colnames(printmatrix)[j], 13))
      printline <- paste0(printline, "-------------")
    }
    cat(printrow, "\n")
    cat(printline, "\n")
    for (j in 1:nrow(printmatrix)) {
      printrow <- paste0(sprintf("%13s", rownames(printmatrix)[j]), " ")
      printline <- "              "
      for (k in 1:ncol(printmatrix)) {
        if (printmatrix[j, k] != "") {
          if (j == k) {
            printrow <- paste0(printrow, vs_align_center(paste0("v", printmatrix[j, k]), 13))
          } else {
            printrow <- paste0(printrow, vs_align_center(paste0("c", printmatrix[j, k]), 13))
          }
        } else {
          printrow <- paste0(printrow, "             ")
        }
        printline <- paste0(printline, "-------------")
      }
      cat(printrow, "\n")
      cat(printline, "\n")
    }
    if (i < nprints) {
      cat("\n")
    }
  }
}


#' Print conditional effect table
#'
#' Print the conditional effect table of the \code{VS} output data in \code{VS} style.
#'
#' @param output A \code{VS} list object created by \code{\link{VS}} or
#'   \code{\link{VS_groupEqual}}.
#' @param groupid A numerical or character variable indicating any group ID for the multi-sample
#'   \code{VS} output list. If \code{"printall"} (default), a list of conditional effect tables
#'   for all groups will be returned. If \code{Multisample} = \code{FALSE}, ignore this argument.
#'
#' @examples
#' modelspec <- '
#'  PV1MATH = MATHEFF
#'  MATHEFF->PV1MATH = ESCS + HEDRES
#'  HEDRES = ESCS
#'  '
#' effectspec <- '
#'  IV1 = MATHEFF
#'  DV1 = PV1MATH
#'  '
#' # VS model with centered data
#' VS_model <- VS(PISA2012HK, model = modelspec, effect = effectspec, scale = "center")
#' # VS multi-sample model with centered data
#' VS_group <- VS(PISA2012HK, model = modelspec, effect = effectspec, group = "IMMIG",
#'                scale = "center")
#' # VS multi-sample model constrainted by group with centered data
#' VS_group_equal <- VS_groupEqual(VS_group, effect.equal = 2)
#'
#' # print the conditional effect table for a VS model
#' VS_printEffects(VS_model)
#'
#' # print the conditional effect table for a VS multi-sample model with IMMIG = 2
#' VS_printEffects(VS_group, 2)
#'
#' # print the conditional effect tables of all groups for a VS multi-sample model
#' VS_printEffects(VS_group_equal)
#'
#' @export

# Print the conditional effect table in VS format

VS_printEffects <- function(output = NULL, groupid = "printall") {

  conditional <- VS_getEffects(output, groupid)

  cat("CONDITIONAL EFFECTS OF IV(s) ON DV(s):\n")
  if (!is.null(conditional)) {
    if (output$Multisample == TRUE && groupid == "printall") {
      for (g in 1:length(conditional)) {
        table <- conditional[[g]]
        cat(paste0("\n  GROUP ", g, ": ", output$Group, " = ", output$GroupID[g], "\n"))
        for (i in 1:nrow(table)) {
          if (i == 1) {
            cat("\n", " ", vs_align_center(table[i, 1], 92))
            cat("\n", " Path:", table[i, 2], "\n")
            cat("  Source:", table[i, 3], "\n")
            if (output$Bootstrap > 0) {
              cat("  --------------------------------------------------------------------------------------------------\n")
              cat("   Eff   Conditional Effect         Est.       S.E.    Z-value         p      95% bootstrap C.I.    \n")
              cat("  --------------------------------------------------------------------------------------------------\n")
            } else {
              cat("  --------------------------------------------------------------------------------------------------\n")
              cat("   Eff   Conditional Effect         Est.       S.E.    Z-value         p           95% C.I.         \n")
              cat("  --------------------------------------------------------------------------------------------------\n")
            }
          } else {
            if (table[i, 1] != table[i - 1, 1]) {
              cat("\n", " ", vs_align_center(table[i, 1], 92), "\n")
            }
            if (table[i, 2] != table[i - 1, 2]) {
              cat("\n", " Path:", table[i, 2], "\n")
            }
            if (table[i, 2] != table[i - 1, 2] || table[i, 3] != table[i - 1, 3]) {
              cat("  Source:", table[i, 3], "\n")
            }
            if (table[i, 1] != table[i - 1, 1] || table[i, 2] != table[i - 1, 2] || table[i, 3] != table[i - 1, 3]) {
              if (output$Bootstrap > 0) {
                cat("  --------------------------------------------------------------------------------------------------\n")
                cat("   Eff   Conditional Effect         Est.       S.E.    Z-value         p      95% bootstrap C.I.    \n")
                cat("  --------------------------------------------------------------------------------------------------\n")
              } else {
                cat("  --------------------------------------------------------------------------------------------------\n")
                cat("   Eff   Conditional Effect         Est.       S.E.    Z-value         p           95% C.I.         \n")
                cat("  --------------------------------------------------------------------------------------------------\n")
              }
            }
          }
          printrow <- paste0("   ", vs_align_center(rownames(table)[i], 3))
          printrow <- paste0(printrow, " ", vs_align_center(table[i, 4], 22))
          printrow <- paste0(printrow, " ", vs_change_digits(as.numeric(table[i, 5])))
          printrow <- paste0(printrow, " ", vs_change_digits(as.numeric(table[i, 6])))
          printrow <- paste0(printrow, " ", vs_change_digits(as.numeric(table[i, 7])))
          printrow <- paste0(printrow, vs_change_digits(as.numeric(table[i, 8]), small = FALSE))
          printrow <- paste0(printrow, "   (", vs_change_digits(as.numeric(table[i, 9])))
          printrow <- paste0(printrow, ",", vs_change_digits(as.numeric(table[i, 10])), ") ")
          cat(printrow, "\n")
          cat("  --------------------------------------------------------------------------------------------------\n")
          if (ncol(table) == 11) {
            if (!is.na(table[i, 11])) {
              cat("  I_wz = ", vs_change_digits(as.numeric(table[i, 11])), "\n")
            }
          }
        }
        if (output$Bootstrap > 0) {
          if (output$CItype == "norm") {
            cat("  Bootstrap C.I. by first order normal approximation\n")
          } else if (output$CItype == "basic") {
            cat("  Bootstrap C.I. by basic bootstrap interval\n")
          } else if (output$CItype == "perc") {
            cat("  Bootstrap C.I. by bootstrap percentile interval\n")
          } else {
            cat("  Bootstrap C.I. by adjusted bootstrap percentile (BCa) interval with no correction for acceleration\n")
          }
        }
        if (!is.null(output$CondRemarks)) {
          for (i in 1:length(output$CondRemarks)) {
            cat(" ", output$CondRemarks[i], "\n")
          }
        }
        if (g < length(table)) {
          cat("\n")
        }
      }
    } else {
      table <- conditional
      for (i in 1:nrow(table)) {
        if (i == 1) {
          cat("\n", " ", vs_align_center(table[i, 1], 92))
          cat("\n", " Path:", table[i, 2], "\n")
          cat("  Source:", table[i, 3], "\n")
          if (output$Bootstrap > 0) {
            cat("  --------------------------------------------------------------------------------------------------\n")
            cat("   Eff   Conditional Effect         Est.       S.E.    Z-value         p      95% bootstrap C.I.    \n")
            cat("  --------------------------------------------------------------------------------------------------\n")
          } else {
            cat("  --------------------------------------------------------------------------------------------------\n")
            cat("   Eff   Conditional Effect         Est.       S.E.    Z-value         p           95% C.I.         \n")
            cat("  --------------------------------------------------------------------------------------------------\n")
          }
        } else {
          if (table[i, 1] != table[i - 1, 1]) {
            cat("\n", " ", vs_align_center(table[i, 1], 92))
          }
          if (table[i, 2] != table[i - 1, 2]) {
            cat("\n", " Path:", table[i, 2], "\n")
          }
          if (table[i, 2] != table[i - 1, 2] || table[i, 3] != table[i - 1, 3]) {
            cat("  Source:", table[i, 3], "\n")
          }
          if (table[i, 1] != table[i - 1, 1] || table[i, 2] != table[i - 1, 2] || table[i, 3] != table[i - 1, 3]) {
            if (output$Bootstrap > 0) {
              cat("  --------------------------------------------------------------------------------------------------\n")
              cat("   Eff   Conditional Effect         Est.       S.E.    Z-value         p      95% bootstrap C.I.    \n")
              cat("  --------------------------------------------------------------------------------------------------\n")
            } else {
              cat("  --------------------------------------------------------------------------------------------------\n")
              cat("   Eff   Conditional Effect         Est.       S.E.    Z-value         p           95% C.I.         \n")
              cat("  --------------------------------------------------------------------------------------------------\n")
            }
          }
        }
        printrow <- paste0("   ", vs_align_center(rownames(table)[i], 3))
        printrow <- paste0(printrow, " ", vs_align_center(table[i, 4], 22))
        printrow <- paste0(printrow, " ", vs_change_digits(as.numeric(table[i, 5])))
        printrow <- paste0(printrow, " ", vs_change_digits(as.numeric(table[i, 6])))
        printrow <- paste0(printrow, " ", vs_change_digits(as.numeric(table[i, 7])))
        printrow <- paste0(printrow, vs_change_digits(as.numeric(table[i, 8]), small = FALSE))
        printrow <- paste0(printrow, "   (", vs_change_digits(as.numeric(table[i, 9])))
        printrow <- paste0(printrow, ",", vs_change_digits(as.numeric(table[i, 10])), ") ")
        cat(printrow, "\n")
        cat("  --------------------------------------------------------------------------------------------------\n")
        if (ncol(table) == 11) {
          if (!is.na(table[i, 11])) {
            cat("  I_wz = ", vs_change_digits(as.numeric(table[i, 11])), "\n")
          }
        }
      }
      if (output$Bootstrap > 0) {
        if (output$CItype == "norm") {
          cat("  Bootstrap C.I. by first order normal approximation\n")
        } else if (output$CItype == "basic") {
          cat("  Bootstrap C.I. by basic bootstrap interval\n")
        } else if (output$CItype == "perc") {
          cat("  Bootstrap C.I. by bootstrap percentile interval\n")
        } else {
          cat("  Bootstrap C.I. by adjusted bootstrap percentile (BCa) interval with no correction for acceleration\n")
        }
      }
      if (!is.null(output$CondRemarks)) {
        VS_printCondPaths(output)
      }
    }
  } else if (is.null(output$CondRemarks)) {
    cat("\n  No EFFECT specification\n  No conditional effects will be displayed\n")
  } else {
    cat("\n")
    VS_printCondPaths(output)
  }
}


#' Print table labels and remarks
#'
#' If any, print the labels of the conditional effect paths and the remarks for the
#'   \code{VS} conditional effect table.
#'
#' @param output A \code{VS} list object created by \code{\link{VS}} or
#'   \code{\link{VS_groupEqual}}.
#'
#' @examples
#' modelspec <- '
#'  PV1MATH = MATHEFF
#'  MATHEFF->PV1MATH = ESCS + HEDRES
#'  HEDRES = ESCS
#'  '
#' effectspec <- '
#'  IV1 = MATHEFF
#'  DV1 = PV1MATH
#'  '
#' VS_model <- VS(PISA2012HK, model = modelspec, effect = effectspec, scale = "center")
#'
#' VS_printCondPaths(VS_model)
#'
#' @export

# Print the labels of the conditional effect paths and any remarks for the conditional effect table

VS_printCondPaths <- function(output = NULL) {

  if (!is.null(output$CondRemarks)) {
    for (i in 1:length(output$CondRemarks)) {
      cat(" ", output$CondRemarks[i], "\n")
    }
  } else {
    cat("No conditional paths and remarks for your VS model\n")
  }
}


#' Print VS parameter estimates
#'
#' Print the parameter estimates and confidence intervals of the \code{VS} model.
#'
#' @param output A \code{VS} list object created by \code{\link{VS}} or
#' \code{\link{VS_groupEqual}}.
#' @param boot.ci.type A character variable containing the type of bootstrapping confidence
#'   interval required. Can be \code{"norm"} for first order normal approximation,
#'   \code{"basic"} for basic bootstrap interval, \code{"perc"} for bootstrap percentile
#'   interval (default), or \code{"bca.simple"} for adjusted bootstrap percentile (BCa)
#'   interval with no correction for acceleration (only for bias).
#'
#' @examples
#' modelspec <- '
#'  PV1MATH = MATHEFF
#'  MATHEFF->PV1MATH = ESCS + HEDRES
#'  HEDRES = ESCS
#'  '
#' effectspec <- '
#'  IV1 = MATHEFF
#'  DV1 = PV1MATH
#'  '
#' # VS model with centered data with first order normal approximation bootstrapping C.I.
#' VS_boot <- VS(PISA2012HK, model = modelspec, effect = effectspec, scale = "center",
#'               bootstrap = 100, boot.ci.type = "norm")
#'
#' # print the parameter estimates with model specified C.I.
#' VS_printParameters(VS_boot)
#'
#' # print the parameter estimates bootstrap percentile interval
#' VS_printParameters(VS_boot, "perc")
#'
#' @import lavaan
#'
#' @export

# Print the parameter estimates and confidence intervals

VS_printParameters <- function(output = NULL, boot.ci.type = NULL) {

  cat("95% C.I. for parameter estimates:\n")
  if (output$Bootstrap > 0) {
    if (is.null(boot.ci.type)) {
      type <- output$CItype
    } else {
      type <- boot.ci.type
    }
    if (type == "norm") {
      cat("  (C.I. by first order normal approximation)\n")
    } else if (type == "basic") {
      cat("  (C.I. by basic bootstrap interval)\n")
    } else if (type == "perc") {
      cat("  (C.I. by bootstrap percentile interval)\n")
    } else {
      cat("  (C.I. by adjusted bootstrap percentile (BCa) interval with no correction for acceleration)\n")
    }
    cat("  ---------------------------------------------------------------------------------\n")
    cat("    VS label       Est.       S.E.    Z-value         p      95% bootstrap C.I.    \n")
    cat("  ---------------------------------------------------------------------------------\n")
    par <- lavaan::parameterEstimates(output$Model, boot.ci.type = type)
  } else {
    cat("  ---------------------------------------------------------------------------------\n")
    cat("    VS label       Est.       S.E.    Z-value         p           95% C.I.         \n")
    cat("  ---------------------------------------------------------------------------------\n")
    par <- lavaan::parameterEstimates(output$Model)
  }
  par <- par[c("label", "est", "se", "z", "pvalue", "ci.lower", "ci.upper")]
  for (i in 1:nrow(par)) {
    printrow <- paste0("    ", vs_align_center(par[i, 1], 8))
    printrow <- paste0(printrow, " ", vs_change_digits(as.numeric(par[i, 2])))
    printrow <- paste0(printrow, " ", vs_change_digits(as.numeric(par[i, 3])))
    printrow <- paste0(printrow, " ", vs_change_digits(as.numeric(par[i, 4])))
    printrow <- paste0(printrow, vs_change_digits(as.numeric(par[i, 5]), small = FALSE))
    printrow <- paste0(printrow, "   (", vs_change_digits(as.numeric(par[i, 6])))
    printrow <- paste0(printrow, ",", vs_change_digits(as.numeric(par[i, 7])), ") ")
    cat(printrow, "\n")
  }
  cat("  ---------------------------------------------------------------------------------\n")
}


#' Print summary of VS model
#'
#' Print the summary of the \code{VS} model results include the input data summary,
#' conceptual model, output data summary, covariance matrix, correlation matrix,
#' mean vector, working model, conditional effect table, \code{lavaan} model summary,
#' and parameter estimates with confidence intervals.
#'
#' @param output A \code{VS} list object created by \code{\link{VS}} or
#' \code{\link{VS_groupEqual}}.
#'
#' @examples
#' modelspec <- '
#'  PV1MATH = MATHEFF
#'  MATHEFF->PV1MATH = ESCS + HEDRES
#'  HEDRES = ESCS
#'  '
#' effectspec <- '
#'  IV1 = MATHEFF
#'  DV1 = PV1MATH
#'  '
#' VS_model <- VS(PISA2012HK, model = modelspec, effect = effectspec, scale = "center")
#'
#' VS_summary(VS_model)
#'
#' @import lavaan
#'
#' @export

# Print VS summary of results

VS_summary <- function(output = NULL) {

  cat("VS", output$Version, "\nBy W. Chan, J. L.-Y. Kwan, Y.-T. Choi, & J. C.-K. Ng\n",
      " Chan, W., Kwan, J. L.-Y., Choi, Y.-T., & Ng, J. C.-K. (2024).\n",
      " Variable System (VS). R package version", paste0(output$Version,".\n"),
      " https://vsquanpsy.wixsite.com/home\n\n")

  # Print information of input data
  VS_input_summary(output)
  cat("\n\n")

  # Print conceptual model
  VS_printConceptual(output)
  cat("\n\n")

  # Print information of output data
  VS_output_summary(output)
  cat("\n\n")

  # Print summary statistics
  cat("SUMMARY STATISTICS\n\n")

  if (output$Multisample == TRUE) {
    for (g in 1:output$N_groups) {
      cat(paste0("Group ", g, ": ", output$Group, " = ", output$GroupID[g], "\n\n"))

      # Print covariance matrix
      VS_printCov(output, output$GroupID[g])
      cat("\n\n")

      # Print correlation matrix
      VS_printCor(output, output$GroupID[g])
      cat("\n\n")

      # Print means
      VS_printMean(output, output$GroupID[g])
      if (g < output$N_groups) {
        cat("\n\n")
      }
    }
  } else {

    # Print covariance matrix
    VS_printCov(output)
    cat("\n\n")

    # Print correlation matrix
    VS_printCor(output)
    cat("\n\n")

    # Print means
    VS_printMean(output)
  }
  cat("\n\n\n")

  # Print working model specification
  VS_printWorking(output)
  cat("\n\n\n")

  # Print conditional effects
  VS_printEffects(output)
  cat("\n\n")

  # Print Lavaan model summary
  cat("LAVAAN MODEL SUMMARY\n\n")
  summary <- lavaan::summary(output$Model, fit.measures = TRUE, rsquare = TRUE)
  print(summary)
  cat("\n")

  # Print parameter estimates
  VS_printParameters(output)
}


# vs_change_digits
#
# If -99999.999 < number < 999999.999, return number with 3 digits
# If number > 999999.999 or number < -99999.999, return number with scientific notation
# [optional] If abs(number) < 0.001, return number with scientific notation

vs_change_digits <- function (x, small = TRUE) {

  if (!is.na(x)) {
    if (round(x, 3) > 999999.999 || round(x, 3) < -99999.999) {
      new_x <- sprintf("%10.3e", x)
    } else if (small == TRUE && abs(x) < 0.001) {
      new_x <- sprintf("%10.3e", x)
    } else {
      new_x <- sprintf("%10.3f", x)
    }
  } else {
    new_x <- vs_align_right(x, 10)
  }

  new_x
}


# vs_align_center
#
# Align the text to center

vs_align_center <- function (x, len) {

  if (is.na(x)) {
    old_x <- "NA"
    if (len < 2) {
      old_x <- substr("NA", 1, len)
    }
  } else {
    old_x <- x
    if (nchar(x) > len) {
      old_x <- substr(x, 1, len)
    }
  }
  new_x <- ""
  if (len %% 2 == 0) {
    startlen <- len / 2
    endlen <- len / 2
  } else {
    startlen <- floor(len / 2)
    endlen <- ceiling(len / 2)
  }
  if (startlen-floor(nchar(old_x)/2) > 0) {
    for (i in 1:(startlen-floor(nchar(old_x)/2))) {
      new_x <- paste0(new_x, " ")
    }
  }
  new_x <- paste0(new_x, old_x)
  if (nchar(old_x) %% 2 == 0) {
    space <- endlen - nchar(old_x) / 2
  } else {
    space <- endlen - ceiling(nchar(old_x) / 2)
  }
  if (space > 0) {
    for (i in 1:space) {
      new_x <- paste0(new_x, " ")
    }
  }

  new_x
}


# vs_align_left
#
# Align the text to left

vs_align_left <- function (x, len) {

  if (is.na(x)) {
    old_x <- "NA"
    if (len < 2) {
      old_x <- substr("NA", 1, len)
    }
  } else {
    old_x <- x
    if (nchar(x) > len) {
      old_x <- substr(x, 1, len)
    }
  }
  new_x <- old_x
  if (len > nchar(old_x)) {
    for (i in 1:(len - nchar(old_x))) {
      new_x <- paste0(new_x, " ")
    }
  }

  new_x
}


# vs_align_right
#
# Align the text to right

vs_align_right <- function (x, len) {

  if (is.na(x)) {
    old_x <- "NA"
    if (len < 2) {
      old_x <- substr("NA", 1, len)
    }
  } else {
    old_x <- x
    if (nchar(x) > len) {
      old_x <- substr(x, 1, len)
    }
  }
  new_x <- ""
  if (len > nchar(old_x)) {
    for (i in 1:(len - nchar(old_x))) {
      new_x <- paste0(new_x, " ")
    }
  }
  new_x <- paste0(new_x, old_x)

  new_x
}

