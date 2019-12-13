#' calculate residuals from a tskrr model
#'
#' This function returns the residuals for
#' an object inheriting from class \code{\link[xnet:tskrr-class]{tskrr}}
#'
#' @param object a tskrr model
#' @param method a character value indicating whether the
#' residuals should be based on the predictions or on a
#' leave-one-out crossvalidation.
#' @inheritParams loo
#' @param ... arguments passed from/to other methods.
#'
#' @inherit loo details
#'
#' @return a matrix(!) with the requested residuals
#'
#' @examples
#'
#' data(drugtarget)
#' mod <- tskrr(drugTargetInteraction, targetSim, drugSim,
#'              lambda = c(0.01,0.01))
#' delta <- response(mod) - loo(mod, exclusion = "both")
#' resid <- residuals(mod, method = "loo", exclusion = "both")
#' all.equal(delta, resid)
#'
#' @rdname residuals.tskrr
#' @method residuals tskrr
#' @export
residuals.tskrr <- function(object,
                            method = c("predictions","loo"),
                            exclusion = c("interaction","row",
                                          "column", "both"),
                            replaceby0 = FALSE,
                            ...){

  method <- match.arg(method)
  exclusion <- match.arg(exclusion)

  obs <- response(object)
  preds <- if(method == "predictions"){
    fitted(object)
  } else {
    loo(object, exclusion, replaceby0)
  }
  obs - preds
}

#' @rdname residuals.tskrr
#' @export
setMethod("residuals",
          "tskrr",
          residuals.tskrr)
