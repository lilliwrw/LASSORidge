#' Transform Coefficients to Original Scale
#'
#' Internal helper function. Recovers ridge regression coefficients obtained from
#' standardized data back on the original data scale
#' and computes the corresponding intercept.
#'
#' @param beta_scaled A numeric vector of ridge coefficients
#' estimated from standardized data.
#' @param X_means A numeric vector of column means of the
#' original design matrix \code{X}.
#' @param X_sds A numeric vector of column standard deviations
#' of the original design matrix \code{X}.
#' @param y_mean The mean of the original response vector \code{y}.
#'
#' @returns A list containing:
#' \itemize{
#'   \item \code{beta} — coefficients on the original scale
#'   \item \code{intercept} — intercept on the original scale
#' }
ridge_inverseTransform <- function(beta_scaled,
                                   X_means,
                                   X_sds,
                                   y_mean){
  #Assume checked, standardized data and compatibility
  #Back-transformation
  beta <- beta_scaled / X_sds

  intercept <- y_mean - sum(beta * X_means)

  list(
    beta = beta,
    intercept = intercept
  )
}
