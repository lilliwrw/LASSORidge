#' Generic plot function for LASSO models
#'
#' This creates the generic `plot()` S3 method for `lasso_model` objects.
#' Users can then call `plot(fit)` directly.
#'
#' @param x A lasso_model object
#' @param ... Additional arguments passed to plotting function
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(50*5), 50, 5)
#' y <- rnorm(50)
#' fit <- lasso_fit(X, y, n_lambda = 10)
#'
#' # Basic plot using S3-method
#' plot(fit)
#'
#' # Customized title and colors
#' plot(fit, main = "Mein LASSO Pfad", col = rainbow(ncol(fit$beta)))
plot.lasso_model <- function(x, ...) {
  if(!inherits(x, "lasso_model")) stop("x must be a lasso_model object")

  fit$beta <- as.matrix(fit$beta)
  fit$lambda_seq <- as.numeric(fit$lambda_seq)
  #bestehende plot_lasso_path Funktion aufrufen
  plot_lasso_path(beta = x$beta, lambda_seq = x$lambda_seq, ...)
}
