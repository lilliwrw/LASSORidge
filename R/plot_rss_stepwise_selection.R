#' Plotting residual sum of squares
#'
#' \code{plot_rss_stepwise_selection} plots the RSS (sum of the squared residuals)
#' for each number of coefficients in the stepwise selection.
#'
#' @param selection A list of multiple \code{lm}-objects
#'
#' @importFrom graphics plot
#'
#' @export
#'
#' @examples
#' set.seed(18645)
#' linear_coefficients <- 10^(1:10) * rnorm(10, 1, 0.1)
#' data <- matrix(runif(1000, 0, 100), ncol=10)
#' colnames(data) <- letters[1:10]
#' data <- as.data.frame(data)
#' # Synthesize test outputs
#' data$output <- rowSums(t(t(data)*linear_coefficients))
#' data$output <- data$output * rnorm(100, 1, 0.00001)
#' # Calculate which coefficients can be dropped with least influence on residual square sum
#' res <- forward_stepwise_selection(data, input=letters[1:10], output="output", return_lm = TRUE)
#' # Plot the result
#' plot_rss_stepwise_selection(res)
#'
plot_rss_stepwise_selection <- function(selection) {
  ##############################################################################
  # Input validation                                                           #
  ##############################################################################
  stopifnot("'selection' must be of type 'list'" = typeof(selection) == "list",
            "'selection' must have length greater than 1" = length(selection)>1)
  for (model in selection) {
    stopifnot("'selection' may only contain lm-onjects" = "lm" %in% class(model))
  }

  ##############################################################################
  # Compute rss                                                                #
  ##############################################################################
  number_of_coefficients <- numeric(length(selection))
  rss <- numeric(length(selection))
  for (i in 1:length(selection)) {
    model <- selection[[i]]
    number_of_coefficients[i] <- sum(unique(model$assign)>0)
    rss[i] <- sum(stats::weighted.residuals(model)^2)
  }

  ##############################################################################
  # Plot                                                                       #
  ##############################################################################
  plot(number_of_coefficients, rss, type="b", main="Plot of the RSS for stepwise selection",
       xlab="number of coefficients", ylab="RSS (logarithmic scale)", log="y")

}
