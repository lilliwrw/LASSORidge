plot_cv <- function(cv_result, X, y, method, log_scale = TRUE, main_title = "Cross Validation") {

  # cv_result: Liste aus lambda_CV(), enthält lambda_seq und cv_values

  x_vals <- if (log_scale) log10(cv_result$lambda_seq) else cv_result$lambda_seq
  y_vals <- cv_result$cv_values

  # Basisplot
  plot(x_vals, y_vals, type = "b", pch = 19, col = "blue",
       xlab = ifelse(log_scale, "log10(lambda)", "lambda"),
       ylab = "CV-MSE",
       main = main_title)

  # Lambda mit minimalem Fehler markieren
  lambda_min <- cv_result$lambda_opt
  abline(v = if (log_scale) log10(lambda_min) else lambda_min, col = "red", lty = 2)
  usr <- par("usr")  # c(x1, x2, y1, y2)
  x_text <- usr[1] + 0.02 * (usr[2]-usr[1])
  y_text <- usr[4] - 0.08 * (usr[4]-usr[3])
  text(x_text, y_text,
       labels = paste("lambda min =", round(lambda_min, 4)),
       adj = c(0,1),
       pos = 4, col = "red")
}
