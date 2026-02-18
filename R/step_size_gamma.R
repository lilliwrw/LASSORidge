#' Compute step size (gamma) in LAR
#'
#' Determines how far to move along the equiangular direction before
#' the next predictor gets put in the active set.
#'
#' @param X Numeric predictor matrix (standardized)
#' @param u Equiangular direction vector
#' @param correlations Numeric vector of current correlations
#' @param active_indices Integer vector of currently active predictor indices
#'
#' @return A list with components:
#' \describe{
#' \item{gamma}{Step size along u}
#' \item{next_index}{Index of next variable going into the active set}
#' }
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20), 5, 4)
#' active <- c(2)
#' eq <- equangular_direction(X, active)
#' corrs <- compute_correlations(X, rnorm(5))
#' step_size_gamma(X, eq$u, corrs, active)
step_size_gamma <- function(X, u, correlations, active_indices) {
  inactive <- setdiff(seq_len(ncol(X)), active_indices) #Spalten die noch nicht im aktiven Set sind
  C <- max(abs(correlations[active_indices])) #Maximum der Korrelation der aktiven Variablen

  a <- drop(crossprod(X[, inactive, drop=FALSE], u)) #aj=Xj^Tu (Matrixeigenschaft fallen lassen)
  c_inactive <- correlations[inactive]

  #Berechnen der möglichen Schrittweiten (wähle dann die kleinste positive weite)
  gamma_candidates <- c((C - c_inactive) / (1 - a),
                        (C + c_inactive) / (1 + a))

  gamma_candidates <- gamma_candidates[gamma_candidates > 0]

  gamma <- min(gamma_candidates)
  next_index <- inactive[which.min(pmin((C - c_inactive)/(1 - a), (C + c_inactive)/(1 + a)))]

  list(gamma = gamma, next_index = next_index)
}
