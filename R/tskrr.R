#' Fitting a two step kernel ridge regression
#'
#' \code{tskrr} is the primary function for fitting a two-step kernel
#' ridge regression model. It can be used for both homogenous and heterogeneous
#' networks.
#'
#' @param y a label matrix
#' @param k a kernel matrix for the rows
#' @param g an optional kernel matrix for the columns
#' @param lambda a numeric vector with one or two values for the
#' hyperparameter lambda. If two values are given, the first one is
#' used for the k matrix and the second for the g matrix.
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
#' can either specify whether the label matrix is symmetric or
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
                  testdim = TRUE,
                  testlabels = TRUE,
                  symmetry = c("auto","symmetric","skewed"),
                  keep = FALSE
                  ){

  iptest <- .test_input(y,k,g,lambda,testdim,testlabels)
  homogenous <- iptest$homogenous
  lambda.k <- iptest$lambda.k
  lambda.g <- iptest$lambda.g

  # Rearrange matrices if necessary
  rk <- rownames(k) # not when there's no row/-colnames

  if(!is.null(rk)){
    if(homogenous){
      ck <- rownames(k)
      if(any(rownames(y) !=rk))
        y <- match_labels(y,rk,ck)
    } else {
      cg <- colnames(g)
      if(any(rownames(y) != rk) || any(colnames(y) !=cg) )
        y <- match_labels(y,rk,cg)
    }
  }

  # Test whether Y is symmetric
  if(homogenous){
    # Test symmetry if required.
    symmetry <- match.arg(symmetry)
    if(symmetry == "auto"){
      symmetry <- test_symmetry(y)
      if(symmetry == "none")
        stop(paste("The Y matrix is not symmetric or skewed symmetric.",
                   "You need a kernel matrix for rows and columns."))
    }
  }

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
