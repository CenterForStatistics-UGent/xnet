#' Extract labels from a tskrr object
#'
#' These functions allow you to extract the labels from a
#' \code{\link{tskrr}} object. The function \code{labels} and the
#' function \code{dimnames} are aliases and do the exact same
#' thing.
#'
#' The functions \code{colnames} and \code{rownames} work like you would
#' expect, with one difference: the rownames or colnames for a \code{tskrr}
#' object can never be \code{NULL}.
#'
#' @param x a \code{\link{tskrr}} object
#' @param object a \code{\link{tskrr}} object
#' @param prefix a prefix used for construction of the labels in case
#' none are available. For \code{label}, a character vector of length 1 or 2.
#' In case two values are given, the first is used for the rows and the second
#' for the columns. Otherwise the only value is used for both. In the case of
#' \code{rownames} and \code{colnames}, a single value.
#' See also \code{\link[=row+colnames]{row+colnames}}
#' @param ... arguments passed to/from other methods.
#'
#' @return for \code{labels} and \code{dimnames}: a list with two elements \code{k} and
#' \code{g}
#'
#' @rdname labels
#' @method labels tskrr
#' @export
labels.tskrr <- function(object,
                         prefix = if(is_homogenous(object)) "row" else c("row","col"), ...){

  labs <- object@labels

  homogenous <- is_homogenous(object)


  # Process the prefixes
  if(!is.character(prefix) && !is.vector(prefix))
    stop("prefix should be a character vector with maximum 2 values.")
  nref <- length(prefix)
  if(nref == 1 && !homogenous)
    prefix <- rep(prefix, 2)
  else if(nref > 2 || nref < 1)
    stop("prefix should contain maximum 2 values.")
  else if(nref == 2 && homogenous)
    warning(paste("Two prefixes were given for a homogenous model.",
                  "The second value", prefix[2],"is ignored."))

  # Generate the labels if no are available
  if(length(labs$k) == 1 && is.na(labs$k)){
    labs$k <- paste0(prefix[1], seq_len(nrow(object@y)))
  }

  if(homogenous)
    labs$g <- labs$k
  else if(length(labs$g) == 1 && is.na(labs$g))
    labs$g <- paste0(prefix[2], seq_len(ncol(object@y)))

  return(labs)
}

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
          function(x, do.NULL, prefix){
            rn <- x@labels$k

            nolabels <- length(rn) == 1 && is.na(rn)

            if(do.NULL && nolabels)
              return(NULL)
            else if(nolabels)
              rn <- paste0(prefix, seq_len(nrow(object@y)))

            return(rn)
          })

#' @rdname labels
#' @export
setMethod("colnames",
          "tskrr",
          function(x, do.NULL, prefix){
            rn <- x@labels$g

            nolabels <- length(rn) == 1 && is.na(rn)

            if(do.NULL && nolabels)
              return(NULL)
            else if(nolabels)
              rn <- paste0(prefix, seq_len(nrow(object@y)))

            return(rn)
          })
