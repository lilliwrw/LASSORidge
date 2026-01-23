#' Plot LASSO Path
#'
#' Plots the LASSO coefficient paths for a sequence of lambda values.
#'
#' @param beta Matrix of coefficients (p x n_lambda) from lasso_path().
#' @param lambda_seq Vector of lambda values corresponding to columns of beta.
#' @param log_x Logical, whether to plot log(lambda) on x-axis. Default TRUE.
#' @param main Plot title, default "LASSO Path".
#' @param xlab Label for x-axis, default "log(lambda)".
#' @param ylab Label for y-axis, default "Coefficients".
#' @param col Color vector for the lines. Default NULL (R default palette).
#' @export
#'
#' @examples
#' set.seed(1)
#' X <- matrix(rnorm(50*5), nrow=50)
#' y <- rnorm(50)
#' std <- standardize_data(X, y)
#' path <- lasso_path(std$X, std$y, n_lambda=10)
#' plot_lasso_path(path$beta, path$lambda_seq)
plot_lasso_path <- function(beta, lambda_seq, log_x=TRUE, main="LASSO Path",xlab="log(lambda)",
                            ylab="Coefficients", col=NULL) {
  p <- nrow(beta)
  if(is.null(col)) col <- rainbow(p) #jede Linie bekommt eigene Farbe
  x_vals <- if(log_x) log(lambda_seq) else lambda_seq #erlaubt lineare Darstellung, falls lox_x FALSE

  plot(x_vals, beta[1,], type="n",xlab=xlab, ylab=ylab, main=main, #"leerer" Plot
       ylim=range(beta), xlim=rev(range(x_vals))) #invertieren der Achsen
  abline(h=0, col="black", lty=2) #schwarze Linie bei 0
  for(j in 1:p) lines(x_vals, beta[j,], col=col[j], lwd=2) #Linien für alle beta j einzeln hinzufügen
}
