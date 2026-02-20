#' Standardize Design Matrix and Response
#'
#' Standardizes the design matrix \code{X} column-wise and centers the response vector \code{y}.
#'
#' @param X A numeric design matrix of dimension n x d.
#' @param y A numeric response vector of length n.
#'
#' @returns  A list containing:
#' \itemize{
#'   \item \code{Xs} — standardized design matrix
#'   \item \code{ys} — centered response vector
#'   \item \code{x_means} — column means of \code{X}
#'   \item \code{x_sds} — column standard deviations of \code{X}
#'   \item \code{y_mean} — mean of \code{y}
#' }
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20), 10, 2)
#' y <- rnorm(10)
#' ridge_standardizeData(X, y)
ridge_standardizeData <- function(X, y){
  x_means <- colMeans(X)
  x_sds <- apply(X, 2, sd)
  Xs <- scale(X)

  y_mean <- mean(y)
  ys <- y-y_mean
  #TODO y auch normieren?

  list(
    Xs = Xs,
    ys = ys,
    x_means = x_means,
    x_sds = x_sds,
    y_mean = y_mean
  )
}
