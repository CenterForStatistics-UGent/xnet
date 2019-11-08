#' Class tskrrTune
#'
#' The class tskrrTune represents a tuned \code{\link[xnet:tskrr-class]{tskrr}}
#' model, and is the output of the function \code{\link{tune}}. Apart from
#' the model, it contains extra information on the tuning procedure. This is
#' a virtual class only.
#'
#' @slot lambda_grid a list object with the elements \code{k} and possibly
#' \code{g} indicating the tested lambda values for the row kernel \code{K}
#' and - if applicable - the column kernel \code{G}. Both elements have
#' to be numeric.
#' @slot best_loss a numeric value with the loss associated with the
#' best lambdas
#' @slot loss_values a matrix with the loss results from the searched grid.
#' The rows form the X dimension (related to the first lambda), the columns
#' form the Y dimension (related to the second lambda if applicable)
#' @slot loss_function the used loss function
#' @slot exclusion a character value describing the exclusion used
#' @slot replaceby0 a logical value indicating whether or not the cross
#' validation replaced the excluded values by zero
#' @slot onedim a logical value indicating whether the grid search
#' was done in one dimension. For homogeneous networks, this is
#' true by default.
#'
#' @seealso
#'  * the function \code{tune} for the tuning itself
#'  * the class \code{\link{tskrrTuneHomogeneous}} and
#'  \code{tskrrTuneHeterogeneous} for the actual classes.
#' @md
#'
#' @rdname tskrrTune-class
#' @name tskrrTune-class
#' @aliases tskrrTune
#' @exportClass tskrrTune
setClass("tskrrTune",
         slots = c(lambda_grid = "list",
                   best_loss = "numeric",
                   loss_values = "matrix",
                   loss_function = "function",
                   exclusion = "character",
                   replaceby0 = "logical",
                   onedim = "logical"))

validTskrrTune <- function(object){

  lossval <- object@loss_values
  lgrid <- object@lambda_grid
  excl <- object@exclusion

  # General tests
  if(!all(sapply(lgrid, is.numeric)))
    return("lambda_grid should have only numeric elements.")

  if(length(object@best_loss) != 1)
    return("best_loss should be a single value.")

  if(length(excl) != 1)
    return("exclusion should be a single character value.")

  if(object@replaceby0 && excl != "interaction")
    return("replaceby0 can only be used with interaction exclusion")

  if(length(object@onedim) != 1)
    return("onedim should be a single logical value.")
  else
    return(TRUE)
}

setValidity("tskrrTune", validTskrrTune)

setMethod("show",
          "tskrrTune",
          function(object){

            # HEADER

            ishomog <- is_homogeneous(object)
            type <- ifelse(ishomog,"homogeneous","heterogeneous")
            tl   <- ifelse(ishomog,"----------","------------")
            cat(paste("Tuned",type,"two-step kernel ridge regression"),
                  paste("-----",tl,"--------------------------------",sep="-"),
                sep = "\n")
            .show_tskrr(object, ishomog)

            # Information on tuning
            excl <- object@exclusion
            if(object@replaceby0) excl <- paste(excl,"(values replaced by 0)")

            if(identical(object@loss_function, loss_mse))
              loss_name <- "Mean Squared Error (loss_mse)"
            else if(identical(object@loss_function, loss_auc))
              loss_name <- "Area under curve (loss_auc)"
            else
              loss_name <- "custom function by user"

            cat("\nTuning information:\n")
            cat("-------------------\n")
            cat("exclusion setting:",object@exclusion,"\n")
            cat("loss value:", object@best_loss,"\n")
            cat("loss function:", loss_name,"\n")
            if(object@onedim && is_heterogeneous(object))
              cat("Grid search done in one dimension.\n")

          })


