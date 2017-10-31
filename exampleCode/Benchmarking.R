##############################################
# Benchmarking for speed-up calculations
# author : Joris Meys
# date last modified : 2017/10/31
library(rbenchmark)

read.matrix <- function(file, sep){
  X <- strsplit(readLines(file), sep)
  out <- do.call(rbind,
                 lapply(X[-1], function(i) as.numeric(i[-1]))
  )
  colnames(out) <- X[[1]][-1]
  return(out)
}

#####
# Calculate UVt(U) with V being a diagonal matrix

# Get function and data
source("buildingBlocks/train.tskrr.R")
K <- read.matrix("data/e_simmat_dc.txt", sep = "\t")
decomp <- eigen(K)

# rewrite the function ReconstructFromEigen
RFEopt <- function(vectors, values){
  V2 <- vectors * rep(values,each = nrow(vectors))
  return(tcrossprod(V2, vectors))
}


# Benchmarking
sol1 <- ReconstructFromEigen(decomp$vectors, decomp$values)
sol2 <- RFEopt(decomp$vectors, decomp$values)
all.equal(sol1, sol2)
benchmark(
  ReconstructFromEigen(decomp$vectors, decomp$values),
  RFEopt(decomp$vectors, decomp$values)
)

#####
# Parameter estimation TSKRR
GPOpt <- function(Y,U,Sigma,V,S,lambda.u, lambda.v){

}