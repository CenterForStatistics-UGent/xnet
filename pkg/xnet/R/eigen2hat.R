#' Calculate the hat matrix from an eigen decomposition
#'
#' This functions calculate either the hat matrix, the mapping matrix or
#' the original (kernel) matrix for a two-step kernel ridge regression,
#' based on the eigen decomposition of the kernel matrix.
#'
#' For the hat matrix, this boils down to:
#'
#' \deqn{U\Sigma(\Sigma + \lambda I)^{-1} U^{T}}
#'
#' For the map matrix, this is :
#'
#' \deqn{U(\Sigma + \lambda I)^{-1} U^{T}}
#'
#' with \eqn{U} the matrix with eigenvectors, \eqn{\Sigma} a diagonal matrix
#' with the eigen values on the diagonal, \eqn{I} the identity matrix and
#' \eqn{\lambda} the hyperparameter linked to this kernel.
#' The internal calculation is optimized to avoid having to invert
#' a matrix. This is done using the fact that \eqn{\Sigma} is a
#' diagonal matrix.
#'
#' @param eigen a matrix with the eigenvectors.
#' @param vec an numeric vector with the eigen values.
#' @param lambda a single numeric value for the hyperparameter lambda
#'
#' @return a numeric matrix representing either the hat matrix
#' (\code{eigen2hat}), the map matrix (\code{eigen2map}) or
#' the original matrix (\code{eigen2matrix})
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

#' @rdname eigen2hat
#' @export
eigen2map <- function(eigen, vec, lambda){

  mid <- 1 / (vec + lambda)

  # Use recycling to calculate the right side first
  # thanks to associativity of the matrix multiplication.

  return(eigen %*% (mid * t(eigen)))
}

#' @rdname eigen2hat
#' @export
eigen2matrix <- function(eigen, vec){
  return(eigen %*% (vec*t(eigen)) )
}