#' Getters for linearFilter objects
#'
#' These functions allow you to extract slots from objects of the
#' class \code{\link{linearFilter}}.
#'
#' @param x a \code{linearFilter} object
#' @param ... arguments passed to or from other methods.
#'
#' @return for \code{mean}: the mean of the original matrix
#'
#' @examples
#' data(drugtarget)
#' lf <- linear_filter(drugTargetInteraction, alpha = 0.25)
#' alpha(lf)
#' mean(lf)
#' colMeans(lf)
#' na_removed(lf)
#'
#' @rdname getters_linearFilter
#' @name getters_linearFilter
#'
#' @method mean linearFilter
#' @export
mean.linearFilter <- function(x,...){
  x@mean
}

#' @rdname getters_linearFilter
#' @export
setMethod("mean", "linearFilter", mean.linearFilter)

#' @rdname getters_linearFilter
#' @return for \code{colMeans}: a numeric vector with the column means
#' @export
setMethod("colMeans", "linearFilter",
          function(x) x@colmeans)

#' @rdname getters_linearFilter
#' @return for \code{rowMeans}: a numeric vector with the row means
#' @export
setMethod("rowMeans", "linearFilter",
          function(x) x@rowmeans)

#' @rdname getters_linearFilter
#' @return for \code{get_alpha}: a numeric vector of length 4 with the alpha
#' values.
#' @export
setMethod("get_alpha", "linearFilter",
          function(x) x@alpha)

#' @rdname getters_linearFilter
#' @return for \code{na_removed}: a logical value indicating whether
#' missing values were removed prior to the fitting of the filter.
#' @export
setMethod("na_removed", "linearFilter",
          function(x) x@na.rm)
