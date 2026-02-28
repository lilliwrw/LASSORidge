#' Fit Least Angle Regression (LAR) Model
#'
#' Computes the full LAR path for predictors X and response y.
#' Standardization is applied internally.
#'
#' @param X Numeric predictor matrix
#' @param y Numeric response vector
#' @param max_steps Maximum number of LAR steps (default = number of predictors)
#' @param standardize Logical, whether to standardize X and y (default TRUE)
#'
#' @return S3 object of class 'lar_model' containing:
#' \describe{
#'   \item{beta}{matrix of coefficients, columns = steps along the path}
#'   \item{active_sets}{list of active variables at each step}
#' }
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(30*5), 30, 5)
#' beta_true <- c(2, -1.5, 0, 0, 0)
#' y <- X %*% beta_true + rnorm(30, sd=0.5)
#' fit <- lar(X, y)
#' fit$beta
#' fit$active_sets
#'
#' @export
lar <- function(X, y, max_steps = ncol(X), standardize = TRUE) {

  #Standartisierung falls standardize = TRUE
  if (standardize==TRUE) {
    std <- standardize_lar(X, y)
    X <- std$X
    y <- std$y
  }

  #LAR path
  path <- lar_path(X, y, max_steps = max_steps)

  #Sicherstellen, dass beta Matrix ist
  beta_mat <- path$beta_path
  if(!is.matrix(beta_mat)) beta_mat <- matrix(beta_mat, nrow = ncol(X), ncol = ncol(beta_mat))

  #Ausgabe
  structure(
    list(
      beta = beta_mat,
      active_sets = path$active_sets
    ),
    class = "lar_model"
  )
}
