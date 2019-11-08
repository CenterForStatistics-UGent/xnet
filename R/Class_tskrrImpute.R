#' Class tskrrImpute
#'
#' The class \code{tskrrImpute} is a virtual class that represents a
#' \code{\link[xnet:tskrr-class]{tskrr}} model with imputed values in
#' the label matrix Y. Apart from the model, it contains the
#' following extra information on the imputed values.
#'
#' @slot imputeid a vector with integer values indicating which of
#' the values in \code{y} are imputed
#' @slot niter an integer value gving the number of iterations used
#' @slot tol a numeric value with the tolerance used
#'
#' @rdname tskrrImpute-class
#' @aliases tskrrImpute
#' @exportClass tskrrImpute
setClass("tskrrImpute",
         slots = c(imputeid = "integer",
                   niter = "integer",
                   tol = "numeric"),
         prototype = prototype(
           niter = 0L,
           tol = 0L
         )
         )

validTskrrImpute <- function(object){
  if(length(object@niter) != 1)
    return("niter should contain a single integer value")
  if(length(object@tol) != 1)
    return("tol should contain a single numeric value")
}

setValidity("tskrrImpute", validTskrrImpute)

setMethod("show",
          "tskrrImpute",
          function(object){

            ishomog <- is_homogenous(object)
            type <- ifelse(ishomog,"Homogenous","Heterogenous")
            tl   <- ifelse(ishomog,"----------","------------")
            cat(paste(type,"two-step kernel ridge regression with imputation"),
                paste(tl,"------------------------------------------------",sep="-"),
                sep = "\n")
            .show_tskrr(object, ishomog)

            cat("\nImputation information:\n")
            cat("-----------------------\n")
            cat("iterations:", object@niter,"\n")
            cat("tolerance:", signif(object@tol, 4),"\n")


          })
