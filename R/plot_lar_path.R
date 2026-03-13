#' Plot LAR Coefficient Path
#'
#' Plots the coefficient path of a Least Angle Regression (LAR) model.
#' Each line represents one predictor. The x-axis can be either
#' the L1-norm of coefficients (classical LAR representation) or the step index.
#'
#' @param beta Numeric matrix of coefficients (p × K). Rows represent variables,
#'   columns represent steps along the LAR path.
#' @param x_axis Character, either "l1" (default) for L1-norm of beta or "step" for step index.
#' @param main Plot title (default "LAR Coefficient Path").
#' @param xlab Label for x-axis. If NULL, automatically set based on `x_axis`.
#' @param ylab Label for y-axis (default "Coefficients").
#' @param col Optional vector of colors, one per variable. Default uses `rainbow()`.
#'
#' @return Invisibly returns NULL
plot_lar_path <- function(beta, x_axis = c("l1", "step"),main = "LAR Coefficient Path",
                          xlab = NULL, ylab = "Coefficients", col = NULL) {

  x_axis <- match.arg(x_axis) #Fehlermeldung bei unsinnigen x_axis Eingaben
  p <- nrow(beta)
  K <- ncol(beta)
  if (is.null(col)) col <- rainbow(p) #jede Linie soll eigene Farbe bekommen

  if (x_axis == "l1") {
    x_vals <- colSums(abs(beta)) #Summe der Koeffs jeder Spalte (absoulut)
    if (is.null(xlab)) xlab <- expression("L1 Norm of" ~ beta) #Beschriftung x-Axe
  } else { #bei Schrittindex
    x_vals <- seq_len(K)
    if (is.null(xlab)) xlab <- "Step"
  }

  plot(x_vals, beta[1, ], type = "n", xlab = xlab, ylab = ylab, main = main,
       ylim = range(beta)) #leerer Plot um Linien hinzuzufügen

  abline(h = 0, col = "black", lty = 2) #Linie bei Null in schwarz

  for (j in 1:p) { #für jede Variable eine Linie einfügen
    lines(x_vals, beta[j, ],col = col[j], lwd = 2)
  }

  invisible(NULL)
}
