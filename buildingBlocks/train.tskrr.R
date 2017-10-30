# reconstruct matrix from eigenvalue decomposition
# JORIS: I think this can implemented much more efficiently

ReconstructFromEigen <- function(vectors, values){
  return(vectors %*% diag(values) %*% t(vectors))  # don't like the 'diag'
  # as it is wasteful, can you circumvent this?
}

GetParametersTSKRR <- function(Y, U, Sigma, V, S, lambda.u, lambda.v){
  # return the weight matrix for two-step kernel ridge regression
  # given the eigenvalue decomposition of the Gram matrices
  W <- ReconstructFromEigen(U, 1 / (Sigma + lambda.u)) %*% Y %*% ReconstructFromEigen(V, 1 / (S + lambda.v))
  return(W)
}

# generic model to train two-step kernel ridge regression
# input: labels, K, G, lambda.u, lambda.v

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

# predict using two-step krr

predict.tskrr <- function(model, k, g){
  # make a prediction, given two vectors
  # or matrices containing the corresponding kernel
  # values
  return(k %*% model$W %*% t(g))
}

# to do:
#   - change lambda's
#   - homogeneous networks