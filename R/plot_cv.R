#' Plot cross-validation results
#'
#' Plots the cross-validation error for a sequence of lambda values and marks the optimal
#' lambda.
#'
#' @param lambda_seq Numeric vector of lambda values used in cross-validation.
#' @param cv_values Numeric vector of CV errors corresponding to each lambda.
#' @param lambda_opt Numeric scalar of the lambda value with minimal CV error.
#' @param log_scale Logical, whether to plot log(lambda) on x-axis. Default
#'   TRUE.
#' @param main Character; main title of the plot. Default is "Cross Validation".
#'
#' @importFrom graphics plot abline points
#'
#' @return None (invisible NULL). Produces a plot.
#' @export
#'
#' @examples
#' set.seed(123)
#' n <- 100; p <- 4
#' X <- matrix(rnorm(n*p), nrow=n, ncol=p)
#' y <- rnorm(n)
#' cv_result <- lambda_cv(X, y, method = "lasso")
#' plot_cv(cv_result$lambda_seq, cv_result$cv_values, cv_result$lambda_opt)
#'
plot_cv <- function(lambda_seq, cv_values, lambda_opt,
                    log_scale = TRUE,
                    main = "Cross-validation") {

  # Input checks
  if (!is.numeric(lambda_seq) || !is.numeric(cv_values) || !is.numeric(lambda_opt)) {
    stop("lambda_seq, cv_values, and lambda_opt must be numeric.")
  }
  if (length(lambda_seq) != length(cv_values)) {
    stop("lambda_seq and cv_values must have the same length.")
  }
  if (length(lambda_opt) != 1) {
    stop("lambda_opt must be a single numeric value.")
  }
  if (anyNA(lambda_seq) || anyNA(cv_values) || is.na(lambda_opt)) {
    stop("lambda_seq, cv_values, and lambda_opt must not contain NA.")
  }
  if (log_scale && (any(lambda_seq <= 0) || lambda_opt <= 0)) {
    stop("For log_scale = TRUE, all lambda values must be > 0.")
  }

  # Sort by lambda
  o <- order(lambda_seq)
  lambda_seq <- lambda_seq[o]
  cv_values  <- cv_values[o]

  # X values
  x_vals <- if (log_scale) log10(lambda_seq) else lambda_seq
  x_opt  <- if (log_scale) log10(lambda_opt) else lambda_opt
  y_vals <- cv_values

  # Approximate y-value at lambda_opt for highlighting
  y_opt <- stats::approx(x = x_vals, y = y_vals, xout = x_opt, rule = 2)$y

  # Axis labels
  x_lab <- if (log_scale) expression(log[10](lambda)) else expression(lambda)

  # Plot
  plot(
    x_vals, y_vals,
    type = "b",
    pch = 19,
    col = "blue",
    xlab = x_lab,
    ylab = "CV error",
    main = main,
    sub = paste0("Selected lambda = ", signif(lambda_opt, 4)),
    col.sub = "red"
  )

  # Mark optimal lambda
  abline(v = x_opt, col = "red", lty = 2, lwd = 1.5)
  points(x_opt, y_opt, pch = 19, col = "red", cex = 1.2)

  invisible(NULL)
}
