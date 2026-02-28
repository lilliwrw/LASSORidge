#' Soft-thresholding operator
#'
#' Applies the soft-thresholding operator used in LASSO.
#'
#' @param z Numeric scalar or vector.
#' @param lambda Non-negative regularization parameter.
#'
#' @returns Numeric value or vector after soft-thresholding.
#' @export
#'
#' @examples
#' soft_threshold(3, 1)
#' soft_threshold(c(-2, -0.5, 0.5, 2), 1)
soft_threshold <- function(z, lambda) {
  #Safty check
  if (lambda < 0) stop('lambda must be non-negative')
  sign(z) * pmax(abs(z) - lambda, 0) #Elementweises Maximum, um Vektoreingaben zu ermöglichen
}
