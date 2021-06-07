#' Fitting a two step kernel ridge regression
#'
#' \code{tskrr} is the primary function for fitting a two-step kernel
#' ridge regression model. It can be used for both homogeneous and heterogeneous
#' networks.
#'
#' @param y an object that can be converted to an adjacency matrix. See details.
#' @param k an object that can be converted to \code{\link{gramData}}. See details.
#' @param g an optional object that can be converted to \code{\link{gramData}}. See details.
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
#' "auto", "symmetric" or "skewed". In case of a homogeneous fit, you
#' can either specify whether the label matrix is symmetric or
#' skewed, or you can let the function decide (option "auto").
#' @param keep a logical value indicating whether the kernel hat
#' matrices should be stored in the model object. Doing so makes the
#' model object quite larger, but can speed up predictions in
#' some cases. Defaults to \code{FALSE}.
#' @param ... arguments passed to other methods.
#'
#' @return a \code{\link[xnet:tskrr-class]{tskrr}} object.
#'
#' @seealso \code{\link{response}}, \code{\link{fitted}},
#' \code{\link{get_eigen}}, \code{\link{eigen2hat}}
#'
#' @examples
#'
#' # Heterogeneous network
#'
#' data(drugtarget)
#'
#' mod <- tskrr(drugTargetInteraction, targetSim, drugSim)
#'
#' Y <- response(mod)
#' pred <- fitted(mod)
#'
#' # Homogeneous network
#'
#' data(proteinInteraction)
#'
#' modh <- tskrr(proteinInteraction, Kmat_y2h_sc)
#'
#' Yh <- response(modh)
#' pred <- fitted(modh)
#'
#' @rdname tskrr
#' @name tskrr
NULL # leave here to avoid adding internal funs to usage
     # by roxygen2.

#-------------------------------
# internal functions
.tskrr_hetero <- function(y,k,g,
                          lambda = 1e-4,
                          testdim = TRUE,
                          testlabels = TRUE,
                          keep = FALSE,
                          ...
){
  # TEST INPUT
  iptest <- .test_input(y,k,g,lambda,testdim,testlabels)
  lambda.k <- iptest$lambda.k
  lambda.g <- iptest$lambda.g

  # Rearrange matrices if necessary
  rk <- rownames(k) # not when there's no row/-colnames

  if(!is.null(rk)){
    cg <- colnames(g)
    if(any(rownames(y) != rk) || any(colnames(y) !=cg) )
      y <- match_labels(y,rk,cg)
  }

  # CALCULATE EIGEN DECOMPOSITION
  k.eigen <- eigen(k, symmetric = TRUE)
  g.eigen <- eigen(g, symmetric = TRUE)

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
  out <- new("tskrrHeterogeneous",
             y = y,
             k = k.eigen,
             g = g.eigen,
             lambda.k = lambda.k,
             lambda.g = lambda.g,
             pred = res$pred,
             has.hat = keep,
             Hk = if(keep) res$k else matrix(0),
             Hg = if(keep) res$g else matrix(0))
  return(out)
}


.tskrr_homo <- function(y,k,
                        lambda = 1e-4,
                        testdim = TRUE,
                        testlabels = TRUE,
                        symmetry = c("auto","symmetric","skewed"),
                        keep = FALSE,
                        ...
){

  iptest <- .test_input(y,k,g = NULL,lambda,testdim,testlabels)
  lambda.k <- iptest$lambda.k
  lambda.g <- iptest$lambda.g

  # Rearrange matrices if necessary
  rk <- rownames(k) # not when there's no row/-colnames

  if(!is.null(rk)){
    ck <- rownames(k)
    if(any(rownames(y) !=rk))
      y <- match_labels(y,rk,ck)
  }

  # Test whether Y is symmetric
  symmetry <- match.arg(symmetry)
  if(symmetry == "auto"){
    symmetry <- test_symmetry(y)
    if(symmetry == "none")
      stop(paste("The Y matrix is not symmetric or skewed symmetric.",
                 "You need a kernel matrix for rows and columns."))
  }

  # CALCULATE EIGEN DECOMPOSITION
  k.eigen <- eigen(k, symmetric = TRUE)

  res <- tskrr.fit(y,
                   k.eigen,
                   NULL,
                   lambda.k,
                   lambda.g)

  # Create labels
  rn <- rownames(y)
  cn <- colnames(y)
  if(is.null(rn)) rn <- NA_character_
  if(is.null(cn)) cn <- NA_character_

  # CREATE OUTPUT
  out <- new("tskrrHomogeneous",
             y = y,
             k = k.eigen,
             lambda.k = lambda.k,
             pred = res$pred,
             symmetry = symmetry,
             has.hat = keep,
             Hk = if(keep) res$k else matrix(0))
  return(out)
}

#------------------------------------------
# METHODS

#' @rdname tskrr
#' @export
setMethod(tskrr,
          signature = c("matrix","matrix","matrix"),
          function(y, k, g, ...){
            k <- gramData(k)
            g <- gramData(g)
            .tskrr_hetero(y, k, g, ...)
          })


#' @rdname tskrr
#' @export
setMethod(tskrr,
          signature = c("matrix","matrix","missing"),
          .tskrr_homo)

#' @rdname tskrr
#' @export
setMethod(tskrr,
          signature = c("matrix","matrix","NULL"),
          function(y,k,g, ...){
            .tskrr_homo(y,k, ...)
            })
