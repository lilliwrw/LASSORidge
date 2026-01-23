#' LASSO through Coordinate Descent
#'
#' Fits a LASSO regression model using coordinate descent.
#'
#' @param X Numeric matrix of predictors (n x p), should be standardized.
#' @param y Numeric response vector of length n, should be centered.
#' @param lambda Non-negative regularization parameter.
#' @param tol Convergence tolerance, default 1e-6.
#' @param max_iter Maximum number of iterations, default 1000.
#'
#' @returns A list with:
#' \describe{
#'   \item{beta}{Estimated coefficients (p x 1).}
#'   \item{iterations}{Number of iterations performed.}
#'   \item{lambda}{The regularization parameter used.}
#' }
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20*5), nrow=20)
#' beta <- c(1.5, -2, 1, -0.5, 0)
#' y <- X %*% beta + rnorm(20, 0, 0.1)
#' std <- standardize_data(X, y)
#' fit <- lasso_cd(std$X, std$y, lambda = 0.1)
#' fit$beta #smaller numbers then beta
lasso_cd <- function(X, y, lambda, tol = 1e-6, max_iter = 1000) {
  if(lambda < 0) stop('lambda must be non-negative')

  n <- nrow(X)
  p <- ncol(X)
  beta <- rep(0, p)

  for(iter in 1:max_iter) {
    beta_old <- beta

    for(j in 1:p) {#Spalte j von X enspricht Regressionskoeffizient beta_j
      #Rest berechnen (j-te Koordinate)
      r_j <- y - X %*% beta + X[, j] * beta[j]

      z_j <- sum(X[, j] * r_j) / n
      #beta_j mittels soft_threshold berecnen
      beta[j] <- soft_threshold(z_j, lambda)
    }
    #auf Konvergenz des Verfahrens prüfen, falls die größte Änderung kleiner ist als die bestimmte Toleranz
    if(max(abs(beta - beta_old)) < tol) break
  }
  #Ausgabe
  list(beta = beta,
    iterations = iter,
    lambda = lambda)
}
