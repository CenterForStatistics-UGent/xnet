#' Fitting a two step kernel ridge regression
#'
#' \code{tskrr} is the main function for fitting a two step kernel
#' ridge regression. It can be used for both homogenous and heterogenous
#' networks.
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
#' @param testlabels a logical value indicating wether the row- and column
#' names of the matrices have to be checked for consistency. Defaults to
#' \code{TRUE}, but for large matrices putting this to \code{FALSE} will
#' speed up the function.
#' @param symmetry a character value with the possibilities
#' "auto", "symmetric" or "skewed". In case of a homogenous fit, you
#' can either specify whether the adjacency matrix is symmetric or
#' skewed, or you can let the function decide (option "auto").
#' @param keep a logical value indicating whether the kernel hat
#' matrices should be stored in the model object. Doing so makes the
#' model object quite larger, but can speed up predictions in
#' some cases. Defaults to \code{FALSE}.
#'
#' @return a \code{\link[xnet:tskrr-class]{tskrr}} object
#'
#' @seealso \code{\link{response}}, \code{\link{fitted}},
#' \code{\link{get_eigen}}, \code{\link{eigen2hat}}
#' @examples
#'
#' # Heterogenous network
#'
#' data(drugtarget)
#'
#' mod <- tskrr(drugTargetInteraction, targetSim, drugSim)
#'
#' Y <- response(mod)
#' pred <- fitted(mod)
#'
#' # Homogenous network
#'
#' data(proteinInteraction)
#'
#' modh <- tskrr(proteinInteraction, Kmat_y2h_sc)
#'
#' Yh <- response(modh)
#' pred <- fitted(modh)
#'
#' @export
tskrr <- function(y,k,g = NULL,
                  lambda = 1e-4,
                  homogenous = is.null(g),
                  testdim = TRUE,
                  testlabels = TRUE,
                  symmetry = c("auto","symmetric","skewed"),
                  keep = FALSE
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

  if(any(is.na(y)))
    stop(paste("Missing values in the y matrix are not allowed. You can",
               "use the function impute_tskrr for imputations."))

  # TEST KERNELS
  if(testdim){
    if(!is_symmetric(k))
      stop("k should be a symmetric matrix.")

    if(!homogenous && !is_symmetric(g))
      stop("g should be a symmetric matrix.")

    if(!valid_dimensions(y,k,g))
      stop(paste("The dimensions of the matrices don't match.",
                 "Did you maybe switch the k and g matrices?",
                 sep = "\n"))
  }
  if(testlabels){

    valid_labels(y,k,g) # Generates errors if something's wrong

    rk <- rownames(k) # not when there's no row/-colnames
    if(!is.null(rk)){
      y <- match_labels(y,rk,colnames(g))
    }

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

  # Create labels
  rn <- rownames(y)
  cn <- colnames(y)
  if(is.null(rn)) rn <- NA_character_
  if(is.null(cn)) cn <- NA_character_

  # CREATE OUTPUT
  if(homogenous){

    # Test symmetry if required.
    symmetry <- match.arg(symmetry)
    if(symmetry == "auto"){
      symmetry <- test_symmetry(y)
    }

    out <- new("tskrrHomogenous",
               y = y,
               k = k.eigen,
               lambda.k = lambda.k,
               pred = res$pred,
               symmetry = symmetry,
               has.hat = keep,
               Hk = if(keep) res$k else matrix(0),
               labels = list(k=rn, g = NA_character_))
  } else {
    out <- new("tskrrHeterogenous",
               y = y,
               k = k.eigen,
               g = g.eigen,
               lambda.k = lambda.k,
               lambda.g = lambda.g,
               pred = res$pred,
               has.hat = keep,
               Hk = if(keep) res$k else matrix(0),
               Hg = if(keep) res$g else matrix(0),
               labels = list(k=rn, g=cn))
  }
  return(out)
}

