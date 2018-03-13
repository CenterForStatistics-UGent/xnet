#' extract the predictions
#'
#' This functions extracts the fitted predictions from a
#' \code{\link[xnet:tskrr-class]{tskrr}} object. The \code{xnet}
#' package provides a generic for the \code{stats} function
#' \code{\link[stats]{fitted}} and a method for the
#' \code{\link[xnet:tskrr-class]{tskrr}} objects.
#'
#' @param object an object for which the extraction of model fitted values
#' is meaningful.
#' @param ... other arguments.
#' @include all_generics.R
#' @rdname fitted
#' @export
setMethod("fitted",
          "tskrr",
          function(object, ...){
            object@pred
          })