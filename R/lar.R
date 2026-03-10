#' Fit Least Angle Regression (LAR) Model
#'
#' Computes the full LAR coefficient path for a predictor matrix \eqn{X} and response vector \eqn{y}.
#' Optionally standardizes predictors and centers the response internally.
#'
#' @param X Numeric predictor matrix of dimension \eqn{n \times p}.
#' @param y Numeric response vector of length \eqn{n}.
#' @param max_steps Maximum number of LAR steps (default = number of predictors)
#' @param standardize Logical, whether to standardize X and y (default TRUE)
#'
#' @return S3 object of class 'lar_model' containing:
#' \describe{
#'   \item{beta}{Matrix of coefficients (p × (K+1)), columns correspond to LAR steps. First column = 0.}
#'   \item{active_sets}{List of active variable indices at each step.}
#'   \item{standartization}{standardized data, if standardized=TRUE, else NULL.}
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
    list(beta = beta_mat,
      active_sets = path$active_sets,
      standardization = if(standardize) std else NULL),
    class = "lar_model"
  )
}
