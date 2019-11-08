#' convert tskrr models
#'
#' These functions allow converting models that inherit from the
#' \code{\link[xnet:tskrr-class]{tskrr}} and
#' \code{\link[xnet:tskrrTune-class]{tskrrTune}} class into each other,
#' keeping track of whether the model is homogeneous or heterogeneous.
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
#' the proper class (homogeneous or heterogeneous)
#'
#' @include all_generics.R
#' @rdname as_tuned
#' @method as_tuned tskrrHomogeneous
setMethod("as_tuned",
          "tskrrHomogeneous",
          function(x, ...){

            x <- as(x, "tskrrTuneHomogeneous")
            initialize(x, ...)
          })

#' @rdname as_tuned
#' @method as_tuned tskrrHeterogeneous
setMethod("as_tuned",
          "tskrrHeterogeneous",
          function(x, ...){

            x <- as(x, "tskrrTuneHeterogeneous")
            initialize(x, ...)
          })

#' @rdname as_tuned
#' @return For \code{as_tskrr}: an object of class
#' \code{\link[xnet:tskrrHomogeneous-class]{tskrrHomogeneous}} or
#' \code{\link[xnet:tskrrHeterogeneous-class]{tskrrHeterogeneous}} depending
#' on whether the original object was homogeneous or heterogeneous.
#'
#' @method as_tskrr tskrrTune
setMethod("as_tskrr",
          "tskrrTune",
          function(x){
            if(is_homogeneous(x))
              as(x, "tskrrHomogeneous")
            else
              as(x, "tskrrHeterogeneous")
          })

#' @rdname as_tuned
#' @method as_tskrr tskrrImpute
setMethod("as_tskrr",
          "tskrrImpute",
          function(x){
            if(is_homogeneous(x))
              as(x, "tskrrHomogeneous")
            else
              as(x, "tskrrHeterogeneous")
          })

#' @rdname as_tuned
#' @method as_tskrr tskrr
setMethod("as_tskrr",
          "tskrr",
          function(x){
            return(x)
          })
