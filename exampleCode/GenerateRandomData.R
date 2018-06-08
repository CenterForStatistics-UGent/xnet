# Create simulated data
library(Matrix)
randKernelMatrix <- function(lim = c(0,1), n = 4){

  notOK <- TRUE
  nsim <- sum(seq_len(n-1))
  out <- matrix(lim[2],ncol=n, nrow = n)

  repeat {
    val <- runif(nsim, lim[1], lim[2])
    out[upper.tri(out)] <- val
    out <- forceSymmetric(out)

    eig <- eigen(out)$values
    if(!is.complex(eig)) break
  }

  return(as.matrix(out))
}

K <- randKernelMatrix(n = 4)
G <- randKernelMatrix(n = 5)

randInteractionMatrix <- function(K, G, n, bias = 0.1){
  rdim <- nrow(K)
  cdim <- nrow(G)

  out <- matrix(0, nrow = rdim, ncol = cdim)

  counter <- 1
  repeat{
    i <- sample(rdim, 1)
    j <- sample(cdim, 1)

    out[i,j] <- 1


    l <- which.max(K[i, -i] + runif(rdim - 1, -bias, bias))
    m <- which.max(G[j, -j] + runif(cdim - 1, -bias, bias))

    out[l,m] <- 1


    counter <- counter + 1

    if(counter >= n) break
  }
  return(out)
}