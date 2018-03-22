#' Getters for tskrr objects
#'
#' The functions described here are convenience functions to get
#' information out of a \code{\link[xnet:tskrr-class]{tskrr}} object.
#'
#' @param x a \code{\link[xnet:tskrr-class]{tskrr}} object or an
#' object inheriting from \code{tskrr}.
#' @param ... arguments passed to other methods.
#'
#' @examples
#' data(drugtarget)
#'
#' mod <- tskrr(drugTargetInteraction, targetSim, drugSim)
#' is_homogenous(mod)
#'
#' EigR <- get_eigen(mod)
#' EigC <- get_eigen(mod, which = 'column')
#' lambda(mod)
#'
#' @include all_generics.R
#' @rdname getters-tskrr
#' @aliases response
#' @export
#' @return For \code{response}: the original response matrix
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
          "tskrr",
          function(x){
            c(k = x@lambda.k)
          })

#' @rdname getters-tskrr
#' @aliases lambda
#' @export
setMethod("lambda",
          "tskrrHeterogenous",
          function(x){
            c(k = x@lambda.k, g = x@lambda.g)
          })

#' @rdname getters-tskrr
#' @aliases is_homogenous
#' @return For \code{is_homogenous} a logical value indicating whether the
#' tskrr model is a homogenous one.
#' @export
is_homogenous <- function(x){
  if(!inherits(x, "tskrr")) stop("x should be a tskrr model.")
  inherits(x, "tskrrHomogenous")
}

#' @rdname getters-tskrr
#' @aliases symmetry
#' @return For \code{symmetry} a character value indicating the symmetry
#' for a \code{\link[xnet:tskrrHomogenous-class]{homogenous model}}. If
#' the model is not homogenous, \code{NA} is returned.
#' @export
symmetry <- function(x){
  if(!is_homogenous(x))
    NA else
  x@symmetry
}

#' @rdname getters-tskrr
#' @aliases get_eigen
#' @param which a character value indicating whether the eigen decomposition
#' for the row kernel matrix or the column kernel matrix should be returned.
#' @return For \code{get_eigen} the eigen decomposition of the requested
#' kernel matrix.
#' @export
get_eigen <- function(x, which = c('row', 'column')){
  if(is_homogenous(x)){
    x@k
  } else {
    which <- match.arg(which)
    if(which == 'row')
      x@k
    else
      x@g
  }
}

#' @rdname getters-tskrr
#' @aliases get_kernelmatrix
#' @return For \code{get_kernelmatrix} the original kernel matrix
#' for the rows or columns.
get_kernel <- function(x, which = c('row','column')){

  which <- match.arg(which)

  if(is_homogenous(x) || which == 'row'){

    if(x@has.orig) x@k.orig else eigen2matrix(x@k$vectors, x@k$values)

  } else{

    if(x@has.orig) x@g.orig else eigen2matrix(x@g$vectors, x@g$values)

  }
}

#' @rdname getters-tskrr
#' @aliases has_original
#' @return For \code{has_original} a logical value indicating whether
#' the tskrr model contains the original kernel matrices.
has_original <- function(x){
  if(!inherits(x, 'tskrr'))
    stop("x needs to be a tskrr model.")

  x@has.orig
}

