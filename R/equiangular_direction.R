#' Compute equiangular direction for active predictors
#'
#' Computes the equiangular direction in Least Angle Regression (LAR) for
#' the current active set of predictors.
#'
#' @param X Numeric predictor matrix (standardized).
#' @param active_indices Integer vector of column indices of active predictors.
#'
#' @return A list with components:
#' \describe{
#' \item{u}{Equiangular direction vector (n × 1).}
#' \item{w}{Weights of active predictors.}
#' \item{A}{Normalization factor.}
#' }
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20), 5, 4)
#' active <- c(2,3)
#' equi <- equiangular_direction(X, active)
#' equi$u
#' equi$w
#' equi$A
equiangular_direction <- function(X, active_indices) {
  X_A <- X[, active_indices, drop = FALSE] #nur aktive Variablen/ Spalten
  G_A <- crossprod(X_A) #Matrix X_A^T X_A
  one_vec <- rep(1, length(active_indices)) #Gewichtung

  w_vorerst <- solve(G_A, one_vec)
  A <- 1 / sqrt(sum(one_vec * w_vorerst))  #sum(1 * G_A^{-1} 1) = 1^T G^-1 1 (normierung)
  w <- w_vorerst * A  #normierte Gewichte
  u <- X_A %*% w  #u in alle Richtungen gleichwinklig, d.h. Korrelation von u mit allen aktiven Variablen ist gleich

  list(u = drop(u), #Vektor der gleichwinkligen (equiangular)  in alle Richtungen
       w = drop(w), #Gewichte
       A = as.numeric(A)) #Normierungsfaktor
}
