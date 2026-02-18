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
#' equi <- equangular_direction(X, active)
#' equi$u
#' equi$w
#' equi$A
equangular_direction <- function(X, active_indices) {
  X_A <- X[, active_indices, drop = FALSE] #nur aktive Variablen/ Spalten
  G_A <- crossprod(X_A) #Matrix X_A^T X_A
  one_vec <- rep(1, length(active_indices)) #Gewichtung
  w <- solve(G_A, one_vec)
  A <- drop(1 / sqrt(sum(w))) #Normierungsfaktor, damit U Länge 1 haben wird
  u <- X_A %*% w * A #u in alle Richtungen gleichwinklig, d.h. Korrelation von u mit allen aktiven Variablen ist gleich

  list(u = u, #Vektor der gleichwinkligen (equiangular)  in alle Richtungen
       w = w, #Gewichte
       A = A) #Normierungsfaktor
}
