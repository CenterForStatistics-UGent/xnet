#' Getters for tskrr objects
#'
#' The functions described here are convenience functions to get
#' information out of a \code{\link[xnet:tskrr-class]{tskrr}} object.
#'
#'
#' @param x a \code{\link[xnet:tskrr-class]{tskrr}} object or an
#' object inheriting from \code{tskrr}.
#' @param ... arguments passed to other methods.
#'
#' @examples
#' data(drugtarget)
#'
#' mod <- tskrr(drugTargetInteraction, targetSim, drugSim)
#' is_homogeneous(mod)
#'
#' EigR <- get_eigen(mod)
#' EigC <- get_eigen(mod, which = 'column')
#' lambda(mod)
#'
#' @include all_generics.R
#' @rdname getters-tskrr
#' @name getters-tskrr
#' @aliases response response,tskrr-method
#' @export
#' @return For \code{response}: the original label matrix
setMethod("response",
          "tskrr",
          function(x, ...){
            x@y
          })

#' @rdname getters-tskrr
#' @return For \code{lambda}: a named numeric vector with one resp both lambda
#' values used in the model. The names are "k" and "g" respectively.
#' @export
setMethod("lambda",
          "tskrrHomogeneous",
          function(x){
            c(k = x@lambda.k)
          })

#' @rdname getters-tskrr
#' @aliases lambda
#' @export
setMethod("lambda",
          "tskrrHeterogeneous",
          function(x){
            c(k = x@lambda.k, g = x@lambda.g)
          })

#' @rdname getters-tskrr
#' @aliases is_tskrr
#' @return For \code{is_tskrr} a logical value indicating whether the
#' object is a \code{tskrr} object
#' @export
is_tskrr <- function(x){
  inherits(x, "tskrr")
}

#' @rdname getters-tskrr
#' @aliases is_homogeneous
#' @return For \code{is_homogeneous} a logical value indicating whether the
#' tskrr model is a homogeneous one.
#' @export
is_homogeneous <- function(x){
  if(!inherits(x, "tskrr")) stop("x should be a tskrr model.")
  inherits(x, "tskrrHomogeneous")
}

#' @rdname getters-tskrr
#' @aliases is_heterogeneous
#' @return For \code{is_heterogeneous} a logical value indicating whether the
#' tskrr model is a heterogeneous one.
#' @export
is_heterogeneous <- function(x){
  if(!inherits(x, "tskrr")) stop("x should be a tskrr model.")
  inherits(x, "tskrrHeterogeneous")
}

#' @rdname getters-tskrr
#' @aliases symmetry
#' @return For \code{symmetry} a character value indicating the symmetry
#' for a \code{\link[xnet:tskrrHomogeneous-class]{homogeneous model}}. If
#' the model is not homogeneous, \code{NA} is returned.
#' @export
symmetry <- function(x){
  if(!is_homogeneous(x))
    NA else
  x@symmetry
}

#' @rdname getters-tskrr
#' @param which a character value indicating whether the eigen decomposition
#' for the row kernel matrix or the column kernel matrix should be returned.
#' @return For \code{get_eigen} the eigen decomposition of the requested
#' kernel matrix.
#' @export
setMethod("get_eigen",
          "tskrr",
          function(x, which = c('row', 'column')){
  if(is_homogeneous(x)){
    get_eigen(x@k)
  } else {
    which <- match.arg(which)
    if(which == 'row')
      get_eigen(x@k)
    else
      get_eigen(x@g)
  }
})

#' @rdname getters-tskrr
#' @aliases get_kernelmatrix
#' @return For \code{get_kernelmatrix} the original kernel matrix
#' for the rows or columns.
#' @export
setMethod("get_kernelmatrix",
          "tskrr",
          function(x, which = c('row','column')){

  which <- match.arg(which)

  tmp <- if(is_homogeneous(x) || which == 'row'){
    x@k
  } else {
    x@g
  }
  if(has_gram(tmp)){
    get_gram(tmp)
  } else {
    tmp <- get_eigen(tmp)
    eigen2matrix(tmp$vectors, tmp$values)
  }
})

#' @rdname getters-tskrr
#' @aliases has_hat
#' @return For \code{has_hat} a logical value indicating whether
#' the tskrr model contains the kernel hat matrices.
#' @export
has_hat <- function(x){
  if(!inherits(x, 'tskrr'))
    stop("x needs to be a tskrr model.")

  x@has.hat
}
