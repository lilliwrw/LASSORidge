#' Fit LASSO Model (User-Friendly)
#'
#' Fits a LASSO regression using coordinate descent for a sequence of lambda values.
#' Standardizes X and centers y internally by default.
#'
#' @param X Numeric predictor matrix (n x p).
#' @param y Numeric response vector of length n.
#' @param n_lambda Number of lambda values to compute (default 100).
#' @param lambda_min_ratio Smallest lambda / largest lambda (default 0.01).
#' @param tol Convergence tolerance for coordinate descent (default 1e-6).
#' @param max_iter Maximum iterations for coordinate descent (default 1000).
#' @param standardize Logical; if TRUE (default), X is standardized
#'                    and y is centered before fitting.
#'
#' @returns A \code{lasso_model} object with:
#' \describe{
#'   \item{beta}{Numeric matrix of dimension p x n_lambda with estimated coefficients. Each column corresponds to one lambda.}
#'   \item{lambda_seq}{Numeric vector of lambda values used in the path.}
#' }
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(50*5), 50, 5)
#' y <- rnorm(50)
#' fit <- lasso(X, y, n_lambda=10)
lasso <- function(X, y, n_lambda=100, lambda_min_ratio=0.01,
                      tol=1e-6, max_iter=1000, standardize=TRUE) {
  #Standartisieren, falls standardize=TRUE
  if(standardize) {
    std <- standardize_data(X, y)
    X_use <- std$X
    y_use <- std$y
  } else {
    X_use <- X
    y_use <- y
  }

  #Lasso_path
  path <- lasso_path(X_use, y_use, n_lambda, lambda_min_ratio, tol, max_iter)

  #Sicherstellen, dass beta Matrix ist
  beta_mat <- path$beta
  if(!is.matrix(beta_mat)) beta_mat <- matrix(beta_mat, nrow=ncol(X_use), ncol=length(path$lambda_seq))

  #Ausgabe
  structure(
    list(beta = beta_mat,
      lambda_seq = path$lambda_seq),
    class = "lasso_model")
}
