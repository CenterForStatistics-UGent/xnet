#' Calculate or extract the loss of a tskrr model
#'
#' This function allows calculating the loss of a tskrr model using
#' either one of the functions defined in \code{\link{loss_functions}}
#' or a custom user function. If the model inherits from class
#' \code{\link[xnet:tskrrTune-class]{tskrrTune}} and no additional arguments
#' are given, the loss is returned for the settings used when tuning.
#' The function can also be used to extract the original loss from a
#' \code{\link[xnet:permtest-class]{permtest}} object.
#'
#' The settings for arguments \code{exclusion} and \code{replaceby0} are passed on to the function \code{\link{loo}}. Exclusion defaults to \code{"interaction"}, but this isn't necessarily the most useful setting. Much depends on the context of the analysis.
#'
#' @param x a model that inherits from class
#' \code{\link[xnet:tskrr-class]{tskrr}}
#' @param fun a function to be used for calculating the loss. This
#' can also be a character value giving the name of one of the loss
#' functions provided in the package
#' @param predictions a logical value to indicate whether the
#' predictions should be used instead of leave one out crossvalidation.
#' If set to \code{TRUE}, the other arguments are ignored.
#' @inheritParams loo
#' @param ... extra arguments passed to the loss function in \code{fun}.
#'
#' @return a numeric value with the calculated loss
#'
#' @seealso
#' * \code{\link{loss_functions}} for possible loss functions
#' * \code{\link{tune}} for tuning a model based on loss functions
#' * \code{\link{loo}} for extra information on the Leave-One-Out options
#' @md
#'
#' @examples
#' data(drugtarget)
#'
#' mod <- tskrr(drugTargetInteraction, targetSim, drugSim)
#'
#' loss(mod, fun = loss_auc)
#'
#' tuned <- tune(mod, fun = loss_auc)
#'
#' loss(tuned)
#' loss(tuned, fun = loss_mse)
#'
#' @rdname loss
#' @export
setMethod("loss",
          "tskrr",
          function(x,
                   fun = loss_mse,
                   exclusion = "interaction",
                   replaceby0 = FALSE,
                   predictions = FALSE,
                   ...){

            fun <- match.fun(fun)
                # needed to make this work for homogeneous models!
            loo <- if(predictions){
              fitted(x)
            } else {
              loo(x, exclusion, replaceby0)
            }
            fun(response(x), loo, ...)
          })

#' @rdname loss
#' @export
setMethod("loss",
          "tskrrTune",
          function(x,
                   fun = loss_mse,
                   exclusion = "interaction",
                   replaceby0 = FALSE,
                   predictions = FALSE,
                   ...){

            # When no arguments are given, return the loss from object
            if(missing(fun) && missing(predictions) &&
               missing(exclusion) && missing(replaceby0))
              return(x@best_loss)
            else
              callGeneric(as_tskrr(x),
                          fun,
                          exclusion,
                          replaceby0,
                          predictions,
                          ...)

          })

#' @rdname loss
#' @export
setMethod("loss",
          "permtest",
          function(x,
                   ...){
            x@orig_loss
          })
