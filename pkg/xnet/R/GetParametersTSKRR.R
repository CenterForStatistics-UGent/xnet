#' Calculate the weights
#'
#' This function returns the weight matrix for two-step kernel ridge
#' regression given eigenvalue decomposition of the Gram matrices.
#'
#' @param Y a matrix with the vertices (i.e. observed links)
#' @param U a matrix with eigenvectors for the rows
#' @param Sigma a vector with eigenvalues for the rows
#' @param V a matrix with eigenvectors for the columns
#' @param S a vector with eigenvalues for the columns
#' @param lambda.u a numeric value giving the lambda parameter for the rows
#' @param lambda.v a numeric value giginv the lambda parameter for the columns
#'
#' @section Warning:
#' This is initial test code. It is currently NOT guaranteed to give
#' meaningful results.
#'
#' @return a matrix with the weights
#'
#' @export

GetParametersTSKRR <- function(Y, U, Sigma, V, S, lambda.u, lambda.v){
  # return the weight matrix for two-step kernel ridge regression
  # given the eigenvalue decomposition of the Gram matrices
  W <- ReconstructFromEigen(U, 1 / (Sigma + lambda.u)) %*% Y %*% ReconstructFromEigen(V, 1 / (S + lambda.v))
  return(W)
}
