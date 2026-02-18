#' Compute predictor correlations with residual
#'
#' Computes inner products between each predictor column and a residual
#' vector. In Least Angle Regression (LAR), these correlations determine
#' which predictors enter the active set.
#'
#' Mathematically:
#' c = X^T r
#'
#' This function assumes valid numeric inputs and is intended for internal
#' use inside the LAR algorithm.
#'
#' @param X Numeric predictor matrix (standardized).
#' @param r Numeric residual vector.
#'
#' @return Numeric vector of correlations.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20), 5, 4)
#' r <- rnorm(5)
#' compute_correlations(X, r)
compute_correlations <- function(X, r) drop(crossprod(X, r))
