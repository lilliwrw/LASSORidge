#' Update coefficients and residual in LAR
#'
#' Updates the coefficient vector and residual after moving along the equiangular
#' direction by step size \code{gamma} in Least Angle Regression (LAR).
#'
#' @param beta Numeric vector. Current coefficient vector (length p).
#' @param active_indices Integer vector of active predictor indices.
#' @param w Numeric vector. Weights of active predictors (from \code{equiangular_direction}).
#' @param gamma Numeric scalar. Step size along the equiangular direction.
#' @param r Numeric vector. Current residual vector.
#' @param u Numeric vector. Equiangular direction vector.
#'
#' @return A list with updated:
#' \describe{
#' \item{beta}{Updated coefficient vector.}
#' \item{r}{Updated residual vector.}
#' }
update_coefficients <- function(beta, active_indices, w, gamma, r, u) {
  if(length(w) != length(active_indices)) stop("length of w does not match active indices")
  gamma <- as.numeric(gamma)
  w <- drop(as.matrix(w))      #drop() sodass W und u Vektoren sind
  u <- drop(as.matrix(u))
  beta[active_indices] <- beta[active_indices] + (gamma * w) #Update der Koeffizienten
  r <- r - (gamma * u) #und der Residuen

  #Ausgabe
  list(beta = beta, r = r)
}
