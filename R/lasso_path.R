#' LASSO Path
#'
#' Computes LASSO estimates for a sequence of lambda values.
#'
#' @param X Numeric matrix of predictors (n x p), standardized.
#' @param y Numeric response vector, centered.
#' @param n_lambda Number of lambda values to compute, default 100.
#' @param lambda_min_ratio Smallest lambda / largest lambda, default 0.01.
#' @param tol Convergence tolerance for coordinate descent, default 1e-6.
#' @param max_iter Maximum iterations for coordinate descent, default 1000.
#'
#' @returns A list with:
#' \describe{
#'   \item{lambda_seq}{A numeric vector containing the sequence of λ values used in the LASSO path.}
#'   \item{beta}{A numeric matrix of dimension p x n_lambda containing the estimated coefficients.
#'               Each column corresponds to one λ in \code{lambda_seq}, each row corresponds to a predictor.}
#' }
lasso_path <- function(X, y, n_lambda=100, lambda_min_ratio=0.01, tol=1e-6, max_iter=1000) {
  lambda_seq <- lambda_sequence(X, y, n_lambda, lambda_min_ratio) #Lambda-Sequenz erzeugen
  #Initialisierung
  p <- ncol(X)
  n_lam <- length(lambda_seq)
  beta_mat <- matrix(0, nrow=p, ncol=n_lam) #Koeffizienten sollen in Matrix beta_mat gespeichert werden

  for(i in seq_along(lambda_seq)){ #beta durch lasso_cd berechnen und in beta_mat speichern
    lambda <- lambda_seq[i]
    est <- lasso_cd(X, y, lambda, tol, max_iter)
    b <- est$beta
    #Safty checks
    if(!is.numeric(b)) stop("lasso_cd returned non-numeric beta")
    if(length(b) != p) stop('lasso_cd returned beta with wrong length')
    beta_mat[, i] <- as.numeric(est$beta) #Beta als Spaltenvektor
  }
  if(n_lam == 1) { #Sicherstellen, das beta Matrix bleibt
    beta_mat <- matrix(beta_mat, nrow = p, ncol = 1)
  }

  #Ausgabe
  list(lambda_seq = lambda_seq,
       beta = beta_mat)
}
