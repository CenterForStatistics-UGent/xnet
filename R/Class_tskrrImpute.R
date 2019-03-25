#' Class tskrrImpute
#'
#' The class \code{tskrrImpute} is a virtual class that represents a
#' \code{\link[xnet:tskrr-class]{tskrr}} model with imputed values in
#' the adjacency matrix Y. Apart from the model, it contains extra
#' information on the imputed values.
#'
#' @slot imputeid a vector with integer values indicating which of
#' the values in \code{y} are imputed
#' @slot imputemethod a character value with either \code{"predictions"} or
#' \code{"loo"} indicating whether the predictions or the leave-one-out
#' crossvalidation values were used in the imputation.
#' @slot loo_settings in case \code{imputemethod = "loo"}, a list with
#' two elements named \code{exclusion} and \code{replaceby0}. These
#' indicate the arguments used in \code{\link{get_loo_fun}}.
#' @slot niter an integer value gving the number of iterations used
#' @slot tol a numeric value with the tolerance used
#'
#' @rdname tskrrImpute-class
#' @aliases tskrrImpute
#' @exportClass tskrrImpute
setClass("tskrrImpute",
         slots = c(imputeid = "integer",
                   imputemethod = "character",
                   loo_settings = "list",
                   niter = "integer",
                   tol = "numeric")
         )

validTskrrImpute <- function(object){
  impmethod <- object@imputemethod

  if(length(impmethod) != 1)
    return("imputemethod should be a character vector of length 1.")

  if(match(impmethod, c("predictions","loo"), nomatch = 0L) == 0L)
    return("imputemethod should be one of 'predictions' or 'loo'.")

  if(impmethod == "loo"){
    # Check the loo_settings
    looset <- object@loo_settings
    if(names(looset) != c("exclusion", "replaceby0"))
      return("loo_settings should have elements 'exclusion' and 'replaceby0'")
    if(!inherits(looset$exclusion, "character") ||
       !inherits(looset$replaceby0, "logical") ||
       length(looset$exclusion) != 1 ||
       length(looset$replaceby0) != 1)
      return(paste("loo_settings needs a single character value for",
                   "the element 'exclusion' and a single logical value",
                   "for the element 'replaceby0'."))

  }

}

setValidity("tskrrImpute", validTskrrImpute)

setMethod("show",
          "tskrrImpute",
          function(object){

            # Information on imputation
            is_loo <- object@imputemethod == "loo"

            if(is_loo){
              impmethod <- paste("leave one out ( exclusion:",
                                 object@loo_settings$exclusion)
              if(object@loo_settings$replaceby0)
                impmethod <- paste(impmethod, "- values replaced by 0 )")
              else
                impmethod <- paste(impmethod, ")")
            } else {
              impmethod <- "predictions"
            }

            cat("Two-step kernel ridge regression with imputation\n")

            cat("\nImputation information:\n")
            cat("-------------------\n")
            cat("  method:",impmethod,"\n")
            cat("  iterations:", object@niter,"\n")
            cat("  tolerance:", signif(object@tol, 4),"\n")

            cat("\nModel:\n")
            show(as_tskrr(object))

          })