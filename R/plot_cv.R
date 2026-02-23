#' Plot Cross Validation
#'
#' Plots the cv_error for a sequence of lambda values and marks the optimal lambda.
#'
#' @param cv_result A list containing the following elements:
#'   \describe{
#'     \item{lambda_seq}{Numeric vector of lambda values used in cross-validation.}
#'     \item{cv_values}{Numeric vector of CV errors corresponding to each lambda.}
#'     \item{lambda_opt}{Numeric scalar of the lambda value with minimal CV error.}
#' @param log_scale Logical, whether to plot log(lambda) on x-axis. Default TRUE.
#' @param main Character; main title of the plot. Default is "Cross Validation".
#'
#' @return None (invisible NULL). Produces a plot.
#' @export
#'
#' @examples
#' set.seed(123)
#' n <- 100; p <- 4
#' X <- matrix(rnorm(n*p), nrow=n, ncol=p)
#' y <- rnorm(n)
#' cv_result <- lambda_CV(X, y, method = "lasso")
#' plot_cv(cv_result)
plot_cv <- function(cv_result, log_scale = TRUE, main = "Cross Validation") {

  # x axis lambda_seq and y axis cv_values for the lambda values
  x_vals <- if (log_scale) log10(cv_result$lambda_seq) else cv_result$lambda_seq
  y_vals <- cv_result$cv_values

  # Basisplot
  plot(x_vals, y_vals, type = "b", pch = 19, col = "blue",
       xlab = ifelse(log_scale, "log10(lambda)", "lambda"),
       ylab = "CV-Error",
       main = main)

  # Marking the optimal lambda with a red vertical line
  lambda_opt <- cv_result$lambda_opt
  abline(v = if (log_scale) log10(lambda_opt) else lambda_opt, col = "red", lty =43)

  # Label indicating the optimal value in the top left corner
  usr <- par("usr")  # c(x1, x2, y1, y2)
  x_text <- usr[1] + 0.02 * (usr[2]-usr[1])
  y_text <- usr[4] - 0.08 * (usr[4]-usr[3])
  text(x_text, y_text,
       labels = paste("best lambda =", round(lambda_opt, 4)),
       adj = c(0,1),
       pos = 4, col = "red")
}

