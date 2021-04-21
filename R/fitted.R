#' extract the predictions
#'
#' This functions extracts the fitted predictions from a
#' \code{\link[xnet:tskrr-class]{tskrr}} object or an object
#' inheriting from that class. The \code{xnet}
#' package provides an S4 generic for the function
#' \code{\link[=fitted]{fitted}}  from the package \code{stats},
#' and a method for \code{\link[xnet:tskrr-class]{tskrr}} objects.
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
#' @rdname fitted
#' @export
setMethod("fitted",
          "tskrr",
          function(object, labels = TRUE, ...){
            out <- object@pred
            if(labels){
              l <- labels(object)
              rownames(out) <- l$k
              colnames(out) <- l$g
            }
            out
          })

#' @rdname fitted
#' @export
setMethod("fitted",
          "linearFilter",
          function(object, ...){
            object@pred
          })
