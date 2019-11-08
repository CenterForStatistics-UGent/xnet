#' convert tskrr models
#'
#' These functions allow converting models that inherit from the
#' \code{\link[xnet:tskrr-class]{tskrr}} and
#' \code{\link[xnet:tskrrTune-class]{tskrrTune}} class into each other,
#' keeping track of whether the model is homogenous or heterogeneous.
#' The dots argument allows specifying values for possible extra slots
#' when converting from \code{tskrr} to \code{tskrrTune}.
#' More information on these slots can be found
#' on the help page of \code{\link[xnet:tskrrTune-class]{tskrrTune}}.
#' **These functions are not exported.**
#'
#' @section \bold{Warning}:
#' This functions do NOT tune a model. they are used internally to
#' make the connection between both types in the methods.
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
#' @return For \code{as_tuned}:
#' a \code{\link[xnet:tskrrTune-class]{tskrrTune}} object of
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

#' @rdname as_tuned
#' @return For \code{as_tskrr}: an object of class
#' \code{\link[xnet:tskrrHomogenous-class]{tskrrHomogenous}} or
#' \code{\link[xnet:tskrrHeterogenous-class]{tskrrHeterogenous}} depending
#' on whether the original object was homogenous or heterogenous.
#'
#' @method as_tskrr tskrrTune
setMethod("as_tskrr",
          "tskrrTune",
          function(x){
            if(is_homogenous(x))
              as(x, "tskrrHomogenous")
            else
              as(x, "tskrrHeterogenous")
          })

#' @rdname as_tuned
#' @method as_tskrr tskrrImpute
setMethod("as_tskrr",
          "tskrrImpute",
          function(x){
            if(is_homogenous(x))
              as(x, "tskrrHomogenous")
            else
              as(x, "tskrrHeterogenous")
          })

#' @rdname as_tuned
#' @method as_tskrr tskrr
setMethod("as_tskrr",
          "tskrr",
          function(x){
            return(x)
          })
