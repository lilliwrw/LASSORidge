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
lasso_cd <- function(X, y, lambda, tol = 1e-6, max_iter = 1000) {
  #Input check
  if(lambda < 0) stop('lambda must be non-negative')

  #Initialisierung (beta=0)
  n <- nrow(X)
  p <- ncol(X)
  beta <- rep(0, p)

  for(iter in 1:max_iter) { #loop bis max_iter oder Konvergenz
    beta_old <- beta #alte Werte zum Prüfen der Änderung

    for(j in 1:p) {#Spalte j von X enspricht Regressionskoeffizient beta_j
      #Rest berechnen (j-te Koordinate), rj= y- Sum{k!=j} Xk betak
      r_j <- y - X %*% beta + X[, j] * beta[j]
      #Update Schritt, zj=1/n Xj^T rj
      z_j <- sum(X[, j] * r_j) / n
      #beta_j mittels soft_threshold berechnen
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
