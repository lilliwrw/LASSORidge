#' Update coefficients and residual in LAR
#'
#' Updates the coefficient vector and residual after doing the
#' equiangular direction by step size gamma.
#'
#' @param beta Current coefficient vector (length p)
#' @param active_indices Integer vector of active predictor indices
#' @param w Weights of active predictors (from equangular_direction)
#' @param gamma Step size
#' @param r residual vector
#' @param u Equiangular direction vector
#'
#' @return A list with updated:
#' \describe{
#' \item{beta}{Updated coefficient vector}
#' \item{r}{Updated residual vector}
#' }
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20), 5, 4)
#' beta <- rep(0, 4)
#' r <- rnorm(5)
#' active <- c(2)
#' eq <- equangular_direction(X, active)
#' out <- update_coefficients(beta, active, eq$w, 0.1, r, eq$u)
#' out$beta
#' out$r
update_coefficients <- function(beta, active_indices, w, gamma, r, u) {
  gamma <- as.numeric(gamma)
  w <- drop(as.matrix(w))      #drop() sodass W und u Vektoren sind
  u <- drop(as.matrix(u))
  beta[active_indices] <- beta[active_indices] + gamma * w
  r <- r - gamma * u

  list(beta = beta, r = r)
}
