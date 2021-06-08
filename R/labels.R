#' Extract labels from a tskrr object
#'
#' These functions allow you to extract the labels from a
#' \code{\link{tskrr}} object. The function \code{labels} and the
#' function \code{dimnames} are aliases and do the exact same
#' thing. The functions \code{rownames} and \code{colnames} work like
#' you would expect. Note that contrary to the latter two, \code{labels}
#' will never return \code{NULL}. If no labels are found, it will construct
#' labels using the prefixes defined in the argument \code{prefix}.
#'
#' @section Warning:
#' If the original data didn't contain row- or column names for the
#' label matrix, \code{rownames} and \code{colnames} will return
#' \code{NULL}. Other functions will extract the automatically generated
#' labels, so don't count on \code{rownames} and \code{colnames} if you
#' want to predict output from other functions!
#'
#' @param x a \code{\link{tskrr}} object, \code{\link{gramData}} object or a matrix.
#' @param object a \code{\link{tskrr}} object
#' @param do.NULL logical. If \code{FALSE} and labels are \code{NULL},
#' labels are created. If \code{TRUE}, the function returns \code{NULL} in
#' the absence of labels.
#' @param prefix a prefix used for construction of the labels in case
#' none are available. For \code{label}, a character vector of length 1 for
#' homogeneous networks or of length 2 for heterogeneous networks.
#' In case two values are given, the first is used for the rows and the second
#' for the columns. Otherwise the only value is used for both. In the case of
#' \code{rownames} and \code{colnames}, a single value.
#' See also \code{\link[=colnames]{row+colnames}}
#' @param ... arguments passed to/from other methods.
#'
#' @return for \code{labels} and \code{dimnames}: a list with two elements \code{k} and
#' \code{g}
#'
#' @examples
#' amat <- matrix(1:10, ncol = 2)
#' labels(amat)
#' labels(amat, do.NULL = TRUE)
#' labels(amat, prefix = c("Sp.","Drug"))
#'
#' data(drugtarget)
#' mod <- tskrr(drugTargetInteraction,
#'              targetSim, drugSim)
#' labels(mod)
#' colnames(mod)
#'
#' @rdname labels
#' @name labels
NULL

# internal functions
.makelabels <- function(n, prefix=NULL){
  if(!is.null(prefix) &&
     (!is.character(prefix) || length(prefix) !=1) )
    stop("Prefix should be a single character value.")
  paste0(prefix, seq_len(n))
}

labels.tskrr <- function(object,
                         prefix = if(is_homogeneous(object)) "row" else c("row","col"), ...){

  homogeneous <- is_homogeneous(object)

  # Check labels
  if(!is.character(prefix))
    stop("Prefix should be a character vector.")
  if(homogeneous){
    if(length(prefix) != 1)
      stop("Prefix should be a single character value for a homogeneous network.")
  } else {
    if(length(prefix) != 2 )
      stop("Prefix should contain 2 character values for a heterogeneous network.")
  }

  if(homogeneous)
    prefix <- rep(prefix,2)
  labs <- labels(response(object), prefix, ...)

  return(labs)
}

#' @rdname labels
#' @export
setMethod("labels",
          "gramData",
          function(object, prefix = NULL, do.NULL = FALSE){
            lb <- object@labels
            if(length(lb) == 0 ){
              lb <- if(do.NULL) NULL else .makelabels(dim(object), prefix)
            }
            return(lb)
          })

#' @rdname labels
#' @export
setMethod("labels",
          "matrix",
          function(object, prefix = NULL, do.NULL = FALSE){
            lb <- dimnames(object)
            if(is.null(lb) && !do.NULL){
              if(is.null(prefix)) prefix <- rep("",2)
              if(!is.vector(prefix) && length(prefix) != 2)
                stop("prefix should be a character vector with 2 values: one for the rows and one for the columns")
              lb <- list(
                k = .makelabels(nrow(object),prefix[1]),
                g = .makelabels(ncol(object), prefix[2])
              )
            } else if(!is.null(lb)){
              names(lb) <- c("k","g")
            }
            return(lb)
          })

#' @rdname labels
#' @export
setMethod("labels",
          "tskrr",
          labels.tskrr)


#' @rdname labels
#' @aliases dimnames.tskrr
#' @export
setMethod("dimnames",
          "tskrr",
          function(x) labels(x))

#' @rdname labels
#' @export
setMethod("rownames",
          "tskrr",
          function(x, do.NULL = TRUE, prefix = "row"){
            labels(x@k, do.NULL = do.NULL,
                         prefix = prefix)
          })

#' @rdname labels
#' @export
setMethod("colnames",
          "tskrr",
          function(x, do.NULL = TRUE, prefix = "col"){
            obj <- if(is_homogeneous(x))
              x@k
            else
              x@g
            labels(obj, do.NULL = do.NULL,
                   prefix = prefix)
          })

#' @rdname labels
#' @export
setMethod("rownames",
          "gramData",
          function(x,do.NULL = TRUE, prefix = NULL){
            labels(x, do.NULL = do.NULL,
                   prefix = prefix)
          })

#' @rdname labels
#' @export
setMethod("colnames",
          "gramData",
          function(x,do.NULL = TRUE, prefix = NULL){
            labels(x, do.NULL = do.NULL,
                   prefix = prefix)
          })