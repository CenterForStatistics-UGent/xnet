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
#' @param x a \code{\link{tskrr}} object
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
#' @rdname labels
#' @method labels tskrr
#' @export
labels.tskrr <- function(object,
                         prefix = if(is_homogeneous(object)) "row" else c("row","col"), ...){

  labs <- object@labels

  homogeneous <- is_homogeneous(object)


  # Process the prefixes
  if(!is.character(prefix) || !is.vector(prefix))
    stop("prefix should be a character vector with maximum 2 values.")
  nref <- length(prefix)
  if(nref == 1 && !homogeneous)
    stop("A heterogeneous network needs 2 values for prefix.")
  else if(nref > 2 || nref < 1)
    stop("prefix should contain 1 or 2 values. See also ?labels.")
  else if(nref == 2 && homogeneous)
    warning(paste("Two prefixes were given for a homogeneous model.",
                  "The second value", prefix[2],"is ignored."))

  # Generate the labels if no are available
  if(length(labs$k) == 1 && is.na(labs$k)){
    labs$k <- paste0(prefix[1], seq_len(nrow(object@y)))
  }

  if(homogeneous)
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
          function(x, do.NULL = TRUE, prefix = "row"){
            rn <- x@labels$k

            nolabels <- length(rn) == 1 && is.na(rn)

            if(do.NULL && nolabels)
              return(NULL)
            else if(nolabels){
              if(length(prefix) > 1 || !is.character(prefix))
                stop("prefix should be a single character value.")
              rn <- paste0(prefix, seq_len(nrow(x@y)))
            }

            return(rn)
          })

#' @rdname labels
#' @export
setMethod("colnames",
          "tskrr",
          function(x, do.NULL = TRUE, prefix = "col"){
            rn <- if(is_homogeneous(x)) x@labels$k else  x@labels$g

            nolabels <- length(rn) == 1 && is.na(rn)

            if(do.NULL && nolabels)
              return(NULL)
            else if(nolabels){
              if(length(prefix) > 1 || !is.character(prefix))
                stop("prefix should be a single character value.")
              rn <- paste0(prefix, seq_len(ncol(x@y)))
            }

            return(rn)
          })
