#' Fitting a two step kernel ridge regression
#'
#' \code{tskrr} is the main function for fitting a two step kernel
#' ridge regression. It can be used for both homogenous and heterogenous
#' networks.
#'
#' TO DO:
#'
#' - add symmetry checks (for skewed or symmetric or none)
#'
#' @param y a response matrix
#' @param k a kernel matrix for the rows
#' @param g an optional kernel matrix for the columns
#' @param lambda a numeric vector with one or two values for the
#' hyperparameter lambda. If two values are given, the first one is
#' used for the k matrix and the second for the g matrix.
#' @param homogenous a logical value indicating whether the fitting should
#' be done for a homogenous network. Normally this is obvious from the
#' input (i.e. a lacking \code{g} matrix indicates the network is homogenous)
#' @param testdim a logical value indicating whether symmetry
#' and the dimensions of the kernel(s) should be tested.
#' Defaults to \code{TRUE}, but for large matrices
#' putting this to \code{FALSE} will speed up the function.
#'
#' @return a \code{\link[xnet:tskrr-class]{tskrr}} object
#'
#' @examples
#' data(drugtarget)
#'
#' mod <- tskrr(drugTargetInteraction, targetSim, drugSim)
#' heatmap(fitted(mod))
#'
#' @export
tskrr <- function(y,k,g = NULL,
                  lambda = 1e-4,
                  homogenous = is.null(g),
                  testdim = TRUE
                  ){

  # TESTS INPUT
  if( !(is.matrix(y) && is.numeric(y)) )
    stop("y should be a matrix.")

  if( !(is.matrix(k) && is.numeric(k)) )
    stop("k should be a matrix.")

  if(!is.numeric(lambda))
    stop("lambda should be numeric.")

  if(!homogenous){
    if( !(is.matrix(g) && is.numeric(g)) )
      stop("g should be a matrix.")

    nl <- length(lambda)
    if(nl < 1 || nl > 2)
      stop("lambda should contain one or two values. See ?tskrr")

  } else {
    if(length(lambda) != 1)
      stop("lambda should be a single value. See ?tskrr")
  }

  # TEST KERNELS
  if(testdim){
    if(!isSymmetric(k))
      stop("k should be a symmetric matrix.")

    if(!homogenous && !isSymmetric(g))
      stop("g should be a symmetric matrix.")

    if(!valid_dimensions(y,k,g))
      stop(paste("The dimensions of the matrices don't match.",
                 "Did you switch the k and g matrices?",
                 sep = "\n"))
  }

  # SET LAMBDAS
  lambda.k <- lambda[1]
  lambda.g <- if(!homogenous){
    if(nl == 1) lambda else lambda[2]
  } else NULL

  # CALCULATE EIGEN DECOMPOSITION
  k.eigen <- eigen(k, symmetric = TRUE)
  g.eigen <- if(!homogenous) eigen(g, symmetric = TRUE) else NULL

  res <- tskrr.fit(y,
                   k.eigen,
                   g.eigen,
                   lambda.k,
                   lambda.g)

  # CREATE OUTPUT
  if(homogenous){
    out <- new("tskrrHomogenous",
               y = y,
               k = k.eigen,
               lambda.k = lambda.k,
               pred = res$pred,
               symmetric = "symmetric")
  } else {
    out <- new("tskrrHeterogenous",
               y = y,
               k = k.eigen,
               g = g.eigen,
               lambda.k = lambda.k,
               lambda.g = lambda.g,
               pred = res$pred)
  }
  return(out)
}