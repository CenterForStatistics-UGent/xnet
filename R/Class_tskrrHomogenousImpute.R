#' Class tskrrHomogenousImpute
#'
#' The class \code{tskrrHomogenousImpute} is a subclass of the
#' class \code{\link[xnet:tskrrHomogenous-class]{tskrrHomogenous}}
#' specifically for homogenous networks with imputed values. It is
#' the result of the function \code{\link{impute_tskrr}} or
#' the function \code{\link{impute_loo}}.
#'
#' @slot y the matrix with responses
#' @slot k the eigen decomposition of the kernel matrix for the rows
#' @slot lambda.k the lambda value used for k
#' @slot pred the matrix with the predictions
#' @slot g the eigen decomposition of the kernel matrix for the columns
#' @slot lambda.g the lambda value used for g
#' @slot has.hat a logical value indicating whether the kernel hat matrices
#' are stored in the object.
#' @slot Hk the kernel hat matrix for the rows.
#' @slot Hg the kernel hat matrix for the columns.
#' @slot labels a list with elements \code{k} and \code{g} (see
#' \code{\link{tskrr-class}}).
#'  If any element is \code{NA}, the labels used
#' are integers indicating the row resp column number.
#' @slot imputeid a vector with integer values indicating which of
#' the values in \code{y} are imputed
#' @slot imputemethod a character value with either \code{"predictions"} or
#' "loo" indicating whether the predictions or the leave-one-out
#' crossvalidation values were used in the imputation.
#' @slot loofun if imputemethod is "loo", this slot contains the loo
#' function used during imputation.
#'
#' @include Class_tskrrHomogenous.R
#' @rdname tskrrHomogenousImpute-class
#' @aliases tskrrHomogenousImpute
#' @exportClass tskrrHomogenousImpute
setClass("tskrrHomogenousImpute",
         contains = "tskrrHomogenous",
         slots = c(imputeid = "integer",
                   imputemethod = "character",
                   loofun = "function"),
         prototype = list(
           imputeid = integer(0),
           imputemethod = "",
           loofun = function(){}
         ))
