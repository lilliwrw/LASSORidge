#' Generate a sequence of lambda values (for Ridge)
#'
#' Creates a logarithmically spaced sequence of regularization parameters.
#'
#' @param X Numeric matrix of predictors (n x p), centered and standardized.
#' @param n_lambda Number of lambda values to generate, default 100.
#' @param lambda_min_ratio Ratio of min lambda to max lambda, default 1e-4.
#'
#' @returns Numeric vector of length n_lambda (descending from lambda_max to lambda_min)
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20*5), nrow=20)
#' y <- rnorm(20)
#' std <- standardize_data(X, y)
#' lambda_seq <- lambda_sequence_ridge(std$X)
lambda_sequence_ridge <- function(X, n_lambda = 100, lambda_min_ratio = 1e-4) {
  if(!is.matrix(X)) stop('X must be a matrix')

  #maximum lambda depending on the eigenvalues
  lambda_max <- max(eigen(t(X) %*% X, symmetric = TRUE)$values)
  lambda_max <- max(lambda_max, 1e-4)

  #minimum lambda
  lambda_min <- max(lambda_max * lambda_min_ratio, 1e-4)

  if(lambda_max <= lambda_min) { lambda_min <- lambda_max * 0.5}

  #Logarithmically distributed from largest to smallest
  lambda_seq <- exp(seq(log(lambda_max), log(lambda_min), length.out = n_lambda))

  return(lambda_seq)
}
