#' Print Method for LAR Model Objects
#'
#' S3 method to print a concise summary of a Least Angle Regression (LAR) model.
#' Displays the number of predictors, number of steps, a preview of the coefficient matrix,
#' and the first steps of the active sets.
#'
#' @param x An object of class \code{"lar_model"}.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns the object \code{x}.
#'
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(30*5), 30, 5)
#' beta_true <- c(2, -1.5, 0, 0, 0)
#' y <- X %*% beta_true + rnorm(30, sd=0.5)
#' fit <- lar(X, y)
#' print(fit)
print.lar_model <- function(x, ...) {
  cat("LAR Model\n")
  cat("Number of predictors:", nrow(x$beta), "\n")
  cat("Number of steps:", ncol(x$beta), "\n\n")

  cat("Coefficient Matrix (showing first 6 rows and columns if large):\n")
  coef_mat <- x$beta
  max_rows <- min(6, nrow(coef_mat))
  max_cols <- min(6, ncol(coef_mat))
  print(coef_mat[1:max_rows, 1:max_cols, drop = FALSE])

  cat("\nActive Sets (first 6 steps shown if large):\n")
  max_steps <- min(6, length(x$active_sets))
  for(i in 1:max_steps) {
    cat("Step", i, ":", paste(x$active_sets[[i]], collapse = ", "), "\n")
  }

  invisible(x)
}
