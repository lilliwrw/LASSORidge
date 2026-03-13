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
#' @param X Numeric predictor matrix of dimension \eqn{n \times p} (standardized).
#' @param r Numeric residual vector of length \eqn{n}.
#'
#' @return Numeric vector of length \eqn{p} containing
#'   the predictor–residual correlations.
compute_correlations <- function(X, r) drop(crossprod(X, r))
