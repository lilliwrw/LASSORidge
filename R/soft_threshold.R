#' Soft-thresholding operator
#'
#' Applies the soft-thresholding operator used in LASSO.
#'
#' @param z Numeric scalar or vector.
#' @param lambda Non-negative regularization parameter.
#'
#' @returns Numeric value or vector after soft-thresholding.
soft_threshold <- function(z, lambda) {
  #Safty check
  if (lambda < 0) stop('lambda must be non-negative')
  sign(z) * pmax(abs(z) - lambda, 0) #Elementweises Maximum, um Vektoreingaben zu ermöglichen
}
