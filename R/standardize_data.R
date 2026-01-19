#' Standardize data for LASSO / regression
#'
#' Centers and optionally scales the columns of a predictor matrix X and the response vector y.
#' This is a necessary preprocessing step for LASSO and other regularized regression methods.
#'
#' @param X A numeric matrix of predictors (n x p).
#' @param y A numeric response vector of length n.
#' @param center Logical, default TRUE. If TRUE, center the columns of X and y by subtracting the mean.
#' @param scale Logical, default TRUE. If TRUE, scale the columns of X to have unit ℓ₂-norm after centering.
#'
#' @returns A list containing:
#' \describe{
#'   \item{X}{The standardized predictor matrix.}
#'   \item{y}{The centered response vector.}
#'   \item{X_mean}{The means of the original columns of X.}
#'   \item{X_scale}{The scaling factors used for each column of X.}
#'   \item{y_mean}{The mean of the original response vector y.}
#' }
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(20*5), nrow = 20, ncol = 5)
#' y <- rnorm(20)
#' std <- standardize_data(X, y)
#' colMeans(std$X)       # Should be ~0
#' colSums(std$X^2)      # Should be ~1
#' mean(std$y)           # Should be ~0
standardize_data <- function(X, y, center = TRUE, scale = TRUE){
  if(!is.matrix(X))X <- as.matrix(X)
  n <- nrow(X)  # Anzahl der Beobachtungen
  p <- ncol(X)
  if (length(y) != n)stop('Length of y must match number of rows of X')

  #Zentrieren der Eingaben, falls center=TRUE (default)
  if(center){
    X_means <- colMeans(X)
    y_mean <- mean(y)
  }
  else {
    X_means <- rep(0,p)
    y_mean <- 0
  }
  X_centered <- sweep(X, 2, X_means, FUN = "-")
  y_centered <- y - y_mean

  #Skalieren der Eingaben, falls scale=TRUE (default)
  if (scale) {
    X_scales <- sqrt(colSums(X_centered^2))
    X_scales[X_scales == 0] <- 1 # Norm von konstanten Spalten auf 1 setzen um Division durch 0 zu vermeiden
    X_scaled <- sweep(X_centered, 2, X_scales, FUN = "/")
  } else {
    X_scales <- rep(1, p)
    X_scaled <- X_centered
  }

  #Ausgabe
  list(
    X = X_scaled,
    y = y_centered,
    X_mean = X_means,
    X_scale = X_scales,
    y_mean = y_mean
  )
}
