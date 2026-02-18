#' Select predictors with maximal correlation
#'
#' Determines which predictors have the largest absolute correlation with the
#' current residual, optionally excluding already active predictors (default integer(0))
#'
#' @param correlations Numeric vector of correlations for each predictor
#' @param already_active Optional integer vector of indices already in the active set.
#'
#' @return Integer vector of predictor indices to enter the active set.
#'
#' @export
#'
#' @examples
#' corrs <- c(0.5, -0.8, 0.3)
#' active_set(corrs)
#' active_set(corrs, already_active = 2)
active_set <- function(correlations, already_active = integer(0)) {
  max_corr <- max(abs(correlations))
  candidates <- which(abs(correlations) == max_corr)
  setdiff(candidates, already_active)
}
