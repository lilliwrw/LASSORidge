#' Standardize Data for LAR (L2-Norm Version)
#'
#' This function standardizes the predictor matrix `X` by centering
#' each column (mean = 0) and scaling it to have L2 norm equal to 1,
#' and centers the response vector `y` (mean = 0), exactly as done
#' in the \code{lars} package when using
#' \code{normalize = TRUE, intercept = TRUE}.
#'
#' Note: Scaling is performed using the Euclidean (L2) norm,
#' not the standard deviation.
#'
#' @param X Numeric matrix of predictors (observations in rows, variables in columns)
#' @param y Numeric vector of response
#'
#' @return A list with components:
#' \describe{
#'   \item{X}{Standardized predictor matrix (centered, L2 norm = 1 per column)}
#'   \item{y}{Centered response vector}
#'   \item{X_means}{Column means of original X}
#'   \item{X_scales}{L2 norms of centered X columns}
#'   \item{y_mean}{Mean of original y}
#' }
#'
#' @export
#'
#' @examples
#' set.seed(123)
#' X <- matrix(rnorm(20), 5, 4)
#' y <- rnorm(5)
#' std <- standardize_lar(X, y)
#' str(std)
#'
#' #X columns are centered
#' colMeans(std$X) #about 0
#'
#' #X columns have L2 norm 1
#' sqrt(colSums(std$X^2)) #1
#'
#' #y is centered
#' mean(std$y) #0
standardize_lar <- function(X, y) {

  if (!is.matrix(X)) X <- as.matrix(X)
  if (!is.numeric(X)) stop("X must be numeric")
  if (!is.numeric(y)) stop("y must be numeric")
  if (length(y) != nrow(X)) stop("Length of y must match number of rows of X")

  n <- nrow(X)
  p <- ncol(X)

  #Zentrieren
  X_means <- colMeans(X)
  y_mean <- mean(y)

  Xc <- sweep(X, 2, X_means, FUN = "-")
  yc <- y - y_mean

  #L2 Norm auf 1
  X_scales <- sqrt(colSums(Xc^2))
  X_scales[X_scales == 0] <- 1  #vermeidet durch 0 teilen

  Xs <- sweep(Xc, 2, X_scales, FUN = "/")

  list(
    X = Xs,
    y = yc,
    X_means = X_means,
    X_scales = X_scales,
    y_mean = y_mean
  )
}
