#' Class tskrrTune
#'
#' The class tskrrTune represents a tuned \code{\link[xnet:tskrr-class]{tskrr}}
#' model, and is the output of the function \code{\link{tune}}. Apart from
#' the model, it contains extra information on the tuning procedure.
#'
#' @slot model an object of class \code{\link[xnet:tskrr-class]{tskrr}} with
#' the tuned model.
#' @slot lambda_grid a list object with the elements \code{k} and possibly
#' \code{g} indicating the tested lambda values for the row kernel \code{K}
#' and - if applicable - the column kernel \code{G}. Both elements have
#' to be numeric.
#' @slot best_lambda a numeric vector with the lambda value(s) that
#' minimize the loss function
#' @slot best_loss a numeric value with the loss associated with the
#' best lambdas
#' @slot loss_values a matrix with the loss results from the searched grid
#' @slot loss_function the used loss function
#' @slot exclusion a character value describing the exclusion used
#' @slot replaceby0 a logical value indicating whether or not the cross
#' validation replaced the excluded values by zero
#'
#' @seealso
#'  * classes \code{\link{tskrrHomogenous}} and
#' \code{\link{tskrrHeterogenous}} for the model classes
#'  * the function \code{tune} for the tuning itself
#' @md
#'
#' @rdname tskrrTune-class
#' @name tskrrTune-class
#' @aliases tskrrTune
#' @exportClass tskrrTune
setClass("tskrrTune",
         slots = c(model = "tskrr",
                   lambda_grid = "list",
                   best_lambda = "numeric",
                   best_loss = "numeric",
                   loss_values = "matrix",
                   loss_function = "function",
                   exclusion = "character",
                   replaceby0 = "logical"))

validTskrrTune <- function(object){

  homogenous <- is_homogenous(object@model)
  lossval <- object@loss_values
  lgrid <- object@lambda_grid
  excl <- object@exclusion

  if(homogenous){
    # All tests for homogenous models
    if(names(lgrid) != "k")
      return("lambda_grid should be a list with one element named k for homogenous networks.")

    if(length(object@best_lambda) != 1)
      return("best_lambda should contain a single value for homogenous networks.")

    if(nrow(lossval) != 1 ||
       ncol(lossval) != length(lgrid$k))
      return(paste("Loss values should have 1 row and",length(lgrid$k),"columns to match the lambda grid."))

  } else {
    # All tests for heterogenous models
    if(names(lgrid) != c("k","g"))
      return("lambda grid should be a list with two elements named k and g (in that order) for heterogenous networks")

    if(length(object@best_lambda) != 2)
      return("best_lambda should contain two values for heterogenous networks.")

    if(nrow(lossval) != length(lgrid$g) ||
       ncol(lossval) != length(lgrid$k))
      return(paste("Loss values should have",length(lgrid$g),"rows and",length(lgrid$k),"columns to match the lambda grid."))
  }

  # General tests
  if(lambda(object@mod) != object@best_lambda)
    return("best_lambda is not the one used for fitting.")

  if(!all(sapply(lgrid, is.numeric)))
    return("lambda_grid should have only numeric elements.")

  if(length(object@best_loss != 1))
    return("best_loss should be a single value.")

  if(length(excl) != 1)
    return("exclusion should be a single character value.")

  exclmatch <- match(excl, c("interaction","row","column","both"),
                  nomatch = 0L)
  if(exclmatch == 0)
    return("exclusion should be one of 'interaction', 'row', 'column' or 'both'")

  if(object@replaceby0 && excl != "interaction")
    return("replaceby0 can only be used with interaction exclusion")
  else
    return(TRUE)
}

setValidity("tskrrTune", validTskrrTune)
