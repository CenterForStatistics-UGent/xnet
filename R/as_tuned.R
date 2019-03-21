#' convert tskrr model to a tuned model
#'
#' This function allows to convert a \code{\link[xnet:tskrr-class]{tskrr}}
#' model to a \code{\link[xnet:tskrrTune-class]{tskrrTune}} model of
#' the same type. The arguments take the values of the slots needed
#' for the conversion. More information on these slots can be found
#' at the help page of \code{\link[xnet:tskrrTune-class]{tskrrTune}}. This function is not exported.
#'
#' @section \bold{Warning}:
#' This function does NOT tune a model. It is used to
#' construct the appropriate object by \code{\link{tune}}.
#'
#' @seealso
#' * \code{\link{tune}} for actually tuning a model.
#' * \code{\link[xnet:tskrrTune-class]{tskrrTune}} for
#' names and possible values of the slots passed through
#' \dots
#' @md
#'
#' @param x a model of class \code{\link[xnet:tskrr-class]{tskrr}}
#' @param ... values for the extra slots defined by
#' the class \code{\link[xnet:tskrrTune-class]{tskrrTune}}
#'
#' @return a \code{\link[xnet:tskrrTune-class]{tskrrTune}} object of
#' the proper class (homogenous or heterogenous)
#'
#' @include all_generics.R
#' @rdname as_tuned
#' @method as_tuned tskrrHomogenous
setMethod("as_tuned",
          "tskrrHomogenous",
          function(x, ...){

            x <- as(x, "tskrrTuneHomogenous")
            initialize(x, ...)
          })

#' @rdname as_tuned
#' @method as_tuned tskrrHeterogenous
setMethod("as_tuned",
          "tskrrHeterogenous",
          function(x, ...){

            x <- as(x, "tskrrTuneHeterogenous")
            initialize(x, ...)
          })
