#' Generate a sequence of lambda values (for LASSO)
#'
#' Creates a logarithmically spaced sequence of regularization parameters.
#'
#' @param X Numeric matrix of predictors (n x p).
#' @param y Numeric response vector of length n.
#' @param n_lambda Number of lambda values to generate, default 100.
#' @param lambda_min_ratio Ratio of min lambda to max lambda, default 0.01.
#'
#' @returns Numeric vector of length \code{n_lambda} containing
#' the sequence of lambda values for the LASSO regularization path.
#' The values are logarithmically spaced and descend from \code{lambda_max}
#' to \code{lambda_min}.
lambda_sequence <- function(X, y, n_lambda = 100, lambda_min_ratio = 0.01) {
  #Input checks
  if(!is.matrix(X)) stop('X must be a matrix')
  if(length(y) != nrow(X)) stop('y length must be nrow(X)')
  n <- nrow(X)

  #max lambda
  lambda_max <- max(abs(crossprod(X,y)))/n # Max der X^Ty (Korrelation jeder Spalte von X mit y)
  lambda_max <- max(lambda_max, 1e-4) #vermeidet lambda_max=0

  #min lambda
  lambda_min <- max(lambda_max*lambda_min_ratio, 1e-4)

  #lambda_min darf nicht größer als lambda_max sein
  if(lambda_max <= lambda_min) lambda_min <- lambda_max * 0.5

  #Sequenz (logarithmisch absteigend, gleichmäßig von lambda_max nach lambda_min)
  exp(seq(log(lambda_max), log(lambda_min), length.out = n_lambda))
}
