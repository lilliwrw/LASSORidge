#' Extract coefficients from a LAR model
#'
#' S3 method to extract coefficients from an object of class `"lar_model"`.
#' Returns the coefficient matrix or a specific step.
#'
#' @param object An object of class `"lar_model"`.
#' @param step Optional integer specifying which step to return.
#'   If `NULL` (default), returns the full coefficient matrix.
#' @param ... Additional arguments
#'
#' @return Numeric matrix (p x K) of coefficients if `step = NULL`,
#'   or numeric vector of length p if a specific `step` is given.
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
  beta <- object$beta #Dimendion: px(max_steps+1)

  # Wenn step angegeben: gültigen Bereich prüfen
  if (!is.null(step)) {
    if (step < 1 || step > ncol(beta)) {
      stop("step must be between 1 and the number of steps in your path")
    }
    return(beta[,step])
  }

  #ganze Koeffizientenmatrix zurückgeben
  return(beta)
}
