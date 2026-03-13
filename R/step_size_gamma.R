#' Compute step size (gamma) in LAR
#'
#' Determines how far to move along the equiangular direction before
#' the next predictor is added to the active set in Least Angle Regression (LAR).
#'
#' This function computes candidate step sizes for inactivated variables
#' and selects the smallest positive step, ensuring the next variable reaches
#' the maximum correlation with the current residual.
#'
#' @param X Numeric predictor matrix (standardized, n x p).
#' @param u Equiangular direction vector.
#' @param correlations Numeric vector of current correlations (X^T r).
#' @param active_indices Integer vector of currently active predictor indices.
#'
#' @return A list with components:
#' \describe{
#' \item{gamma}{Step size along u}
#' \item{next_index}{Index of next variable going into the active set}
#' }
step_size_gamma <- function(X, u, correlations, active_indices) {
  inactive <- setdiff(seq_len(ncol(X)), active_indices) #Spalten die noch nicht im aktiven Set sind
  if(length(inactive) == 0) return(list(gamma = 0, next_index = NA)) #alle Variablen aktiv
  C <- max(abs(correlations[active_indices])) #Maximum der Korrelation aller Variablen

  X_inactive <- X[, inactive, drop = FALSE]
  a <- drop(crossprod(X_inactive, u)) #aj=Xj^Tu (Matrixeigenschaft fallen lassen)
  c_inactive <- correlations[inactive]

  #Berechnen der möglichen Schrittweiten (wähle dann die kleinste positive weite)
  if(length(active_indices) == 1) {#eine aktive Variable
    gamma_candidates <- c((C - c_inactive), (C + c_inactive))
  }else {#mehrere aktive Variablen
    gamma_candidates <- c((C - c_inactive)/(1 - a), (C + c_inactive)/(1 + a))
  }
  gamma_candidates <- gamma_candidates[gamma_candidates > 0 & is.finite(gamma_candidates)] #nur positive endliche Kandidaten

  if(length(gamma_candidates) == 0) {
    gamma <- 0
    next_index <- NA
  } else {
    gamma <- min(gamma_candidates)
    #den Index der nächsten Variable bestimmen
    if(length(active_indices) == 1){
      next_index <- inactive[which.min(pmin(C - c_inactive, C + c_inactive))]
    } else {
      candidate_matrix <- cbind((C - c_inactive)/(1 - a), (C + c_inactive)/(1 + a))
      gamma_idx <- which(candidate_matrix == gamma, arr.ind = TRUE)[1,1]
      next_index <- inactive[gamma_idx]
    }
  }

  #Ausgabe
  list(gamma = gamma, next_index = next_index)
}
