#' Plot Ridge Coefficient Path
#'
#' Plots the ridge regression coefficient paths as a function
#' of decreasing lambda or log(lambda).
#'
#' @param x An object of class \code{"ridge_path"}.
#' @param log.lambda Logical; if \code{TRUE} (default), the horizontal
#' axis shows \code{log(lambda)}.
#' @param legend Logical; if \code{TRUE} (default), a legend is shown in the top right.
#' @param intercept Logical; if \code{FALSE} (default), the intercept is not plotted.
#' @param type Line type for plotting (default: "l").
#' @param lty Line type specification (default: 1).
#' @param col Optional vector of colors.
#' @param xlab Label for x-axis. If \code{NULL}, a default is chosen.
#' @param ylab Label for y-axis. If \code{NULL}, default is "Coefficients".
#' @param main Plot title (default: "Ridge Path")
#' @param ... Additional graphical parameters passed to \code{plot} (ignored).
#'
#' @returns Produces a plot and returns \code{NULL} invisibly.
#'
#' @importFrom grDevices rainbow
#'
#' @export
#'
#' @examples
#'  set.seed(1)
#'  X <- matrix(rnorm(100), 20, 5)
#'  y <- rnorm(20)
#'  lambda_seq <- exp(seq(-2, 2, length.out = 10))
#'  #Fit the ridge model
#'  path <- ridge_path(X, y, lambda_seq)
#'  #Plot the coefficient path
#'  plot(path)
plot.ridge_path <- function(x,
                            log.lambda = TRUE,
                            legend = TRUE,
                            intercept = FALSE,
                            type = "l",
                            lty = 1,
                            col = NULL,
                            xlab = NULL,
                            ylab = NULL,
                            main = "Ridge Path",
                            ...){
  # Lambda values for x-axis
  lambda_vals <- if (log.lambda) log(x$lambda) else x$lambda
  if (log.lambda && any(x$lambda <= 0)) {
    stop("Cannot use log scale when lambda contains non-positive values.")
  }

  coef_mat <- x$coefficients

  # Remove intercept if requested
  if (!intercept) {
    if ("(Intercept)" %in% rownames(coef_mat)) {
      coef_mat <- coef_mat[rownames(coef_mat) != "(Intercept)", , drop = FALSE]
    }
  }

  #Set defaults
  if (is.null(col)) {
    col <- rainbow(nrow(coef_mat))
  }

  if (is.null(xlab)) {
    xlab <- if (log.lambda) "log(lambda)" else "lambda"
  }

  if (is.null(ylab)) {
    ylab <- "Coefficients"
  }

  #Plot first line
  plot(lambda_vals,
       coef_mat[1, ],
       type = type,
       lty = lty,
       col = col[1],
       xlab = xlab,
       ylab = ylab,
       main = main,
       xlim = rev(range(lambda_vals)),
       ylim = range(coef_mat),
       ...)

  # Add horizontal reference line at 0
  abline(h = 0, lty = 2, col = "grey50")

  #Plot other lines
  if (nrow(coef_mat) > 1) {
    for (i in 2:nrow(coef_mat)) {
      lines(lambda_vals,
            coef_mat[i, ],
            type = type,
            lty = lty,
            col = col[i])
    }
  }
  if(legend){
    #Add legend
    legend("topright",
           legend = rownames(coef_mat),
           col = col,
           lty = lty,
           cex = 0.8)

  }

  invisible(NULL)
}
