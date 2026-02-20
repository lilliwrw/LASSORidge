#' Transform Coefficients to Original Scale
#'
#' Recovers ridge regression coefficients obtained from
#' standardized data back on the original data scale
#' and computes the corresponding intercept.
#'
#' @param beta_scaled A numeric vector of ridge coefficients
#' estimated from standardized data.
#' @param x_means A numeric vector of column means of the
#' original design matrix \code{X}.
#' @param x_sds A numeric vector of column standard deviations
#' of the original design matrix \code{X}.
#' @param y_mean The mean of the original response vector \code{y}.
#'
#' @returns A list containing:
#' \itemize{
#'   \item \code{beta} — coefficients on the original scale
#'   \item \code{intercept} — intercept on the original scale
#' }
#' @export
#'
#' @examples
#' beta_scaled <- c(0.5, -1.2)
#' x_means <- c(2, 3)
#' x_sds <- c(1, 2)
#' y_mean <- 5
#' ridge_inverseTransform(beta_scaled, x_means, x_sds, y_mean)
ridge_inverseTransform <- function(beta_scaled,
                                   x_means,
                                   x_sds,
                                   y_mean){
  beta <- beta_scaled / x_sds

  intercept <- y_mean - sum(beta * x_means)

  list(
    beta = beta,
    intercept = intercept
  )
}
