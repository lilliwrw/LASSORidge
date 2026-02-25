#' Extract Coefficients from Ridge Path
#'
#' Returns the estimated coefficients including the intercepts.
#'
#' @param object An object of class \code{"ridge_path"}.
#' @param step Optional lambda value or index.
#' @param ... Additional arguments (ignored).
#'
#' @returns If \code{step} is NULL, the full coefficient matrix.
#' Otherwise, a coefficient vector for the selected lambda.
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(50), 10, 5)
#' y <- rnorm(10)
#' path <- ridge_path(X, y, lambda = c(0.1, 1))
#' coef(path)
coef.ridge_path <- function(object, step = NULL, ...){
  if(is.null(step)){
    return(object$coefficients)
  }

  if (length(step) == 1 && step %in% seq_along(object$lambda)) {
    return(object$coefficients[, step])
  }

  idx <- which(object$lambda == step)

  if (length(idx) == 0) {
    stop("Requested lambda not found.")
  }

  object$coefficients[, idx, drop = FALSE]
}
