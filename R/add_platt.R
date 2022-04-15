#' Add/get platt scaling coefs to/from a tskrr object
#'
#' This function adds Platt scaling coefficients to any object inheriting from tskrr.
#'
#' @param x an object inheriting from \code{\link[xnet:tskrr-class]{tskrr}}
#'
#' @return the same object, but with the platt scaling coefficients filled in.
#'
#' @rdname add_platt
#' @name add_platt
#' @export
setMethod("add_platt",
          "tskrr",
          function(x){
  pr <- fitted(x)
  o <- response(x)

  x@platt <- .generate_coefs(o, pr)
  return(x)
          })

#' @rdname add_platt
#' @export
setMethod("get_plattcoef",
          "tskrr",
          function(x) x@platt)