#' Class tskrrImputeHeterogeneous
#'
#' The class \code{tskrrImputeHeterogeneous} is a subclass of the
#' class \code{\link[xnet:tskrrHeterogeneous-class]{tskrrHeterogeneous}} and
#' \code{\link[xnet:tskrrImpute-class]{tskrrImpute}}
#' specifically for heterogeneous networks with imputed values. It is
#' the result of the function \code{\link{impute_tskrr}}.
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
#' @slot niter an integer value gving the number of iterations used
#' @slot tol a numeric value with the tolerance used
#'
#' @include Class_tskrrHeterogeneous.R Class_tskrrImpute.R
#' @rdname tskrrImputeHeterogeneous-class
#' @aliases tskrrImputeHeterogeneous
#' @exportClass tskrrImputeHeterogeneous
setClass("tskrrImputeHeterogeneous",
         contains = c("tskrrImpute", "tskrrHeterogeneous")
         )
