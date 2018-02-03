#' Train a two-step kernel ridge regression
#'
#' This function trains the two step kernel ridge regression
#'
#' @param Y a matrix with the observations
#' @param K a matrix - Gram matrix for the rows
#' @param G a matrix - Gram matrix for the columns
#' @param lambda.u a numeric value for the lambda parameter for the rows
#' @param lambda.v a numeric value for the lambda parameter for the columns
#'
#' @return a list with the model components. Currently classless
#'
#' @section Warning:
#' This is initial test code. It is currently NOT guaranteed to give
#' meaningful results.
#'
#' @export
TrainTSKRR <- function(Y, K, G, lambda.u=1, lambda.v=1){
  # trains two-step kernel ridge regression by performing
  # eigenvalue decomposition of the two gram matrices
  eigen.decomp.K <- eigen(K, symmetric = TRUE)
  eigen.decomp.G <- eigen(G, symmetric = TRUE)
  W <- GetParametersTSKRR(Y=Y, U=eigen.decomp.K$vectors, Sigma=eigen.decomp.K$values,
                          V=eigen.decomp.G$vectors, S=eigen.decomp.G$values,
                          lambda.u=lambda.u, lambda.v=lambda.v)
  model <- list(
    Y = Y,
    lambda.u = lambda.u,
    lambda.v = lambda.v,
    K = K,
    G = G,
    U = eigen.decomp.K$vectors,
    V = eigen.decomp.G$vectors,
    Sigma = eigen.decomp.K$values,
    S = eigen.decomp.G$values,
    # make weights
    W = W,
    # make corresponding prediction matrix
    F = K %*% W %*% G
  )
  return(model)
}
