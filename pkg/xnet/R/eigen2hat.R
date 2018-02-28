#' Calculate the hat matrix from an eigen decomposition
#'
#' This function calculates the hat matrix for a two-step kernel ridge
#' regression based on the eigen decomposition of the kernel matrix.
#'
#' @param eigen a matrix with the eigenvectors.
#' @param vec an numeric vector with the eigen values.
#' @param lambda a single numeric value for the hyperparameter lambda
#'
#' @return a numeric matrix representing the hat matrix
#'
#' @export
eigen2hat <- function(eigen, vec , lambda){

  # Sigma is a diagonal matrix, so Sigma %*% Sigma + lambdaI can
  # be calculated based on the vecs as vec * 1/(vec + lambda)
  # This can be calculated first due to associativity of the
  # matrix multiplication.

  mid <- vec / (vec + lambda)

  # Use recycling to calculate the right side first
  # thanks to associativity of the matrix multiplication.

  return(eigen %*% (mid * t(eigen)))
}