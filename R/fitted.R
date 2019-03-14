#' extract the predictions
#'
#' This functions extracts the fitted predictions from a
#' \code{\link[xnet:tskrr-class]{tskrr}} object. The \code{xnet}
#' package provides a generic for the \code{stats} function
#' \code{\link[=fitted]{fitted}} and a method for the
#' \code{\link[xnet:tskrr-class]{tskrr}} objects.
#'
#' @param object an object for which the extraction of model fitted values
#' is meaningful.
#' @param ... arguments passed to or from other methods.
#' @param labels a logical value indicating whether the labels should
#' be shown. Defaults to TRUE
#'
#' @return a numeric matrix with the predictions
#'
#' @examples
#'
#' data(drugtarget)
#'
#' mod <- tskrr(drugTargetInteraction, targetSim, drugSim)
#' pred <- fitted(mod)
#'
#' @include all_generics.R
#'
#'
#' @rdname fitted
#' @method fitted tskrr
#' @export
fitted.tskrr <- function(object, labels = TRUE, ...){
  out <- object@pred
  if(labels){
    l <- labels(object)
    rownames(out) <- l$k
    colnames(out) <- l$g
  }
  out
}

#' @rdname fitted
#' @method fitted linearFilter
#' @export
fitted.linearFilter <- function(object, ...){
  object@pred
}


#' @rdname fitted
#' @export
setMethod("fitted",
          "tskrr",
          fitted.tskrr)

#' @rdname fitted
#' @export
setMethod("fitted",
          "linearFilter",
          fitted.linearFilter)
