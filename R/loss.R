#' Calculate or extract the loss of a tskrr model
#'
#' This function allows to calculate the loss of a tskrr model using
#' either one of the functions defined in \code{\link{loss_functions}}
#' or a custom user function.If the model inherits from class
#' \code{\link[xnet:tskrrTune-class]{tskrrTune}} and no additional arguments
#' are given, the loss is returned for the settings used when tuning.
#'
#' @param x a model that inherits from class
#' \code{\link[xnet:tskrr-class]{tskrr}}
#' @param fun a function to be used for calculating the loss. This
#' can also be a character value giving the name of one of the loss
#' functions provided in the package
#' @param exclusion a character value with possible values "interaction",
#' "row", "column" or "both".
#' See also \code{\link{loo}} for more information.
#' @param replaceby0 a logical value indicating whether the interaction
#' should be simply removed (\code{FALSE}) or replaced by 0 (\code{TRUE}).
#' @param predictions a logical value to indicate whether the
#' predictions should be used instead of leave one out crossvalidation.
#' If set to \code{TRUE}, the other arguments are ignored.
#' @param ... extra arguments passed to the loss function in \code{fun}.
#'
#' @return a numeric value with the calculated loss
#'
#' @seealso
#' * \code{\link{loss_functions}} for possible loss functions
#' * \code{\link{tune}} for tuning a model based on loss functions
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
                   exclusion = c("interaction","row","column","both"),
                   replaceby0 = FALSE,
                   predictions = FALSE,
                   ...){

            fun <- match.fun(fun)
            exclusion <- match.arg(exclusion)
                # needed to make this work for homogenous models!
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
                   exclusion = c("interaction","row","column","both"),
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
