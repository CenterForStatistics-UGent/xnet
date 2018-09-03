#' Return the hat matrix of a tskrr model
#'
#' This function returns the hat matrix or hat matrices of
#' a tskrr model. \code{xnet} creates an S4 generic for \code{hat}
#' and links the default method to the \code{\link[=influence.measures]{hat}} function
#' of \code{stats}
#'
#' @param x a tskrr model
#' @param which a character value with possible values "row" or
#' "column" to indicate which should be returned. For homogenous
#' models, this parameter is ignored.
#' @param ... arguments passed to other methods.
#'
#' @return the requested hat matrix of the model.
#' @rdname hat
#' @export
setMethod("hat",
          "tskrrHeterogenous",
          function(x, which = c('row','column')){

            which <- match.arg(which)

            eig <- if(which == 'row') x@k else x@g
            l <- if(which == 'row') x@lambda.k else x@lambda.g

            eigen2hat(eig$vectors, eig$values, l)
          })

#' @rdname hat
#' @export
setMethod("hat",
          "tskrrHomogenous",
          function(x, ...){
            eig <- x@k
            l <- x@lambda.k

            eigen2hat(eig$vectors, eig$values, l)
          })