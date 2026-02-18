#' Generate a sequence of lambda values (for LASSO)
#'
#' Creates a logarithmically spaced sequence of regularization parameters.
#'
#' @param X Numeric matrix of predictors (n x p), centered.
#' @param y Numeric response vector of length n, centered.
#' @param n_lambda Number of lambda values to generate, default 100.
#' @param lambda_min_ratio Ratio of min lambda to max lambda, default 0.01.
#'
#' @returns Numeric vector of length n_lambda (descending from lambda_max to lambda_min)
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20*5), nrow=20)
#' y <- rnorm(20)
#' std <- standardize_data(X, y)
#' lambda_seq <- lambda_sequence(std$X, std$y)
lambda_sequence <- function(X, y, n_lambda = 100, lambda_min_ratio = 0.01) {
  if(!is.matrix(X)) stop('X must be a matrix')
  if(length(y) != nrow(X)) stop('y length must be nrow(X)')
  n <- nrow(X)

  #max lambda
  lambda_max <- max(abs(crossprod(X,y)))/n
  lambda_max <- max(lambda_max, 1e-4)

  #min lambda
  lambda_min <- max(lambda_max*lambda_min_ratio, 1e-4)

  if(lambda_max <= lambda_min) lambda_min <- lambda_max * 0.5

  #Sequenz (logarithmisch absteigend)
  exp(seq(log(lambda_max), log(lambda_min), length.out = n_lambda))
}
