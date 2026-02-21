#' Plot method for LAR models
#'
#' S3 plot method for objects of class `"lar_model"`.
#' Internally calls `plot_lar_path()`.
#'
#' @param x An object of class `"lar_model"`.
#' @param x_axis Character, either "l1" (default) for L1-norm of beta or "step" for step index.
#' @param main Plot title (default "LAR Coefficient Path").
#' @param ... Additional arguments passed to `plot_lar_path()`.
#'
#' @return Invisibly returns NULL
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
#' # Plot using default L1-norm
#' plot(fit)
#'
#' # Plot using step index
#' plot(fit, x_axis = "step")
plot.lar_model <- function(x,x_axis = c("l1", "step"),
                           main = "LAR Coefficient Path", ...) {
  plot_lar_path(beta = x$beta, x_axis = x_axis,main = main, ...)
}
