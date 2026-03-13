#' Generate a sequence of lambda values for Ridge regression
#'
#' Creates a logarithmically spaced sequence of regularization parameters for
#' Ridge regression.
#'
#' @param X Numeric predictor matrix of dimension n x p. The matrix is assumed
#'   to be centered and standardized.
#' @param n_lambda Positive integer giving the number of lambda values to
#'   generate. Default is 100.
#' @param lambda_min_ratio Positive numeric scalar giving the ratio of
#'   \code{lambda_min} to \code{lambda_max}. Default is \code{1e-4}.
#'
#' @returns Numeric vector of length \code{n_lambda}, sorted in decreasing order
#'   from \code{lambda_max} to \code{lambda_min}.
#'
lambda_sequence_ridge <- function(X, n_lambda = 100, lambda_min_ratio = 1e-4) {

  if (!is.matrix(X)) stop("X must be a matrix.")
  if (!is.numeric(X)) stop("X must be numeric.")
  if (anyNA(X)) stop("X must not contain missing values.")

  if (length(n_lambda) != 1 || !is.numeric(n_lambda) || is.na(n_lambda) ||
      n_lambda <= 0 || n_lambda != as.integer(n_lambda)) {
    stop("n_lambda must be a positive integer.")
  }
  n_lambda <- as.integer(n_lambda)

  if (length(lambda_min_ratio) != 1 || !is.numeric(lambda_min_ratio) ||
      is.na(lambda_min_ratio) || lambda_min_ratio <= 0) {
    stop("lambda_min_ratio must be a positive number.")
  }

  # Largest eigenvalue of X'X
  lambda_max <- max(eigen(t(X) %*% X, symmetric = TRUE)$values)
  lambda_max <- max(lambda_max, 1e-4)

  # Smallest lambda in the sequence
  lambda_min <- max(lambda_max * lambda_min_ratio, 1e-4)

  lambda_seq <- exp(seq(log(lambda_max), log(lambda_min), length.out = n_lambda))

  lambda_seq
}
