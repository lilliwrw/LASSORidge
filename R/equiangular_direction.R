#' Compute equiangular direction for active predictors
#'
#' Computes the equiangular direction in Least Angle Regression (LAR) for
#' the current active set of predictors.
#'
#' @param X Numeric predictor matrix (standardized).
#' @param active_indices Integer vector of column indices of active predictors.
#' @param c_vec Numeric vector of current correlations X^T r.
#'
#' @return A list with components:
#' \describe{
#' \item{u}{Equiangular direction vector (n × 1).}
#' \item{w}{Weights of active predictors.}
#' \item{s}{Numeric vector. Sign vector of active correlations.}
#' }
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20 * 3), 20, 3)
#' y <- rnorm(20)
#' beta <- rep(0, 3)
#' r <- y - X %*% beta
#' c_vec <- as.vector(crossprod(X, r))
#' active <- which.max(abs(c_vec))
#' equiangular_direction(X, active, c_vec)
equiangular_direction <- function(X, active_indices,c_vec) {
  if(length(active_indices) == 0) stop("Active set is empty!")

  X_A <- X[, active_indices, drop = FALSE] #nur aktive Variablen/ Spalten
  s<- sign(c_vec[active_indices]) #Sign der aktiven
  eps <- 1e-12

  if(length(active_indices) == 1) {
    # Spezialfall 1 aktive Variable
    w <- s
  } else {
    # ≥2 aktive Variablen: numerisch stabil
    G_A <- crossprod(X_A) #Matrix X_A^T X_A
    w <- solve(G_A + diag(eps, nrow(G_A)), s)
    w <- w / sqrt(sum(s * w))
  }
  #u in alle Richtungen gleichwinklig, d.h. Korrelation von u mit allen aktiven Variablen ist gleich
  u <- X_A %*% w

  stopifnot(length(u) == nrow(X))
  stopifnot(length(w) == length(active_indices))

  list(u = drop(u), #Vektor der gleichwinkligen (equiangular)  in alle Richtungen
       w = drop(w), #Gewichte
       s = drop(s))
}
