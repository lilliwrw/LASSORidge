#' Standardize Design Matrix and Response
#'
#' Standardizes the design matrix \code{X} column-wise and centers the response vector \code{y}.
#'
#' @param X A numeric design matrix of dimension \eqn{n \times d}.
#' @param y A numeric response vector of length \eqn{n}.
#'
#' @returns  A list containing:
#' \itemize{
#'   \item \code{Xs} — standardized design matrix
#'   \item \code{ys} — centered response vector
#'   \item \code{X_means} — column means of \code{X}
#'   \item \code{X_sds} — column standard deviations of \code{X}
#'   \item \code{y_mean} — mean of \code{y}
#' }
#'
#' @importFrom stats sd
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20), 10, 2)
#' y <- rnorm(10)
#' ridge_standardizeData(X, y)
ridge_standardizeData <- function(X, y){
  #Standardize X
  X_means <- colMeans(X)
  X_sds <- apply(X, 2, sd)

  # Avoid division by zero
  zero_sd <- X_sds == 0
  if (any(zero_sd)) {
    warning("Some columns have zero standard deviation and were not scaled.")
    X_sds[zero_sd] <- 1
  }

  Xs <- scale(X, center = X_means, scale = X_sds)

  #Center y
  y_mean <- mean(y)
  ys <- y-y_mean

  list(
    Xs = Xs,
    ys = ys,
    X_means = X_means,
    X_sds = X_sds,
    y_mean = y_mean
  )
}
