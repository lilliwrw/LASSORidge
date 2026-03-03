#' Extract coefficients from a LAR model
#'
#' S3 method to extract coefficients from an object of class `"lar_model"`.
#' The function returns the coefficient path computed by the Least Angle
#' Regression (LAR) algorithm. The coefficient matrix has dimension
#' \eqn{p \times K}, where \eqn{p} is the number of predictors and
#' \eqn{K} is the number of LAR steps. Each column corresponds to one
#' step along the solution path.
#'
#' @param object An object of class `"lar_model"`.
#' @param step Optional integer specifying which step to return.
#'   If `NULL` (default), returns the full coefficient matrix.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Numeric matrix (p x K) containing the full coefficient path if `step = NULL`,
#'   or numeric vector of length p containing the coefficients at that step, if a specific `step` is given.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(30 * 5), 30, 5)
#' beta_true <- c(2, -1.5, 0, 0, 0)
#' y <- X %*% beta_true + rnorm(30, sd = 0.5)
#' fit <- lar(X, y)
#'
#' #Full coefficient path
#' coef(fit)
#'
#' #Coefficients at step 3
#' coef(fit, step = 3)
coef.lar_model <- function(object, step = NULL, ...) {
  beta <- object$beta #Dimension: px(max_steps+1(LAR Steps))

  # Wenn step angegeben: gültigen Bereich prüfen
  if (!is.null(step)) {
    if (step < 1 || step > ncol(beta)) {
      stop("step must be between 1 and the number of steps in your path")
    }
    if(length(step)!=1)stop("step must be a single integer")
    return(beta[,step])
  }

  #ganze Koeffizientenmatrix zurückgeben
  return(beta)
}
