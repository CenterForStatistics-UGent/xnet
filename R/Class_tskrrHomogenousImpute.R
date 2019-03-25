#' Class tskrrHomogenousImpute
#'
#' The class \code{tskrrHomogenousImpute} is a subclass of the
#' class \code{\link[xnet:tskrrHomogenous-class]{tskrrHomogenous}} and
#' \code{\link[xnet:tskrrImpute-class]{tskrrImpute}}
#' specifically for homogenous networks with imputed values. It is
#' the result of the function \code{\link{impute_tskrr}} or
#' the function \code{\link{impute_loo}}.
#'
#' @slot y the matrix with responses
#' @slot k the eigen decomposition of the kernel matrix for the rows
#' @slot lambda.k the lambda value used for k
#' @slot pred the matrix with the predictions
#' @slot symmetry a character value that can have the possible values
#' \code{"symmetric"}, \code{"skewed"} or \code{"not"}. It indicates
#' whether the \code{y} matrix is symmetric, skewed-symmetric or not
#' symmetric.
#' @slot has.hat a logical value indicating whether the kernel hat matrices
#' are stored in the object.
#' @slot Hk the kernel hat matrix for the rows.
#' @slot labels a list with elements \code{k} and \code{g} (see
#' \code{\link{tskrr-class}}). For homogenous networks, \code{g}
#' is always \code{NA}. If \code{k} is \code{NA}, the labels used
#' are integers indicating the row resp column number.
#' @slot imputeid a vector with integer values indicating which of
#' the values in \code{y} are imputed
#' @slot imputemethod a character value with either \code{"predictions"} or
#' "loo" indicating whether the predictions or the leave-one-out
#' crossvalidation values were used in the imputation.
#' @slot loo_settings in case \code{imputemethod = "loo"}, a list with
#' two elements named \code{exclusion} and \code{replaceby0}. These
#' indicate the arguments used in \code{\link{get_loo_fun}}.
#' @slot niter an integer value gving the number of iterations used
#' @slot tol a numeric value with the tolerance used
#'
#' @include Class_tskrrHomogenous.R Class_tskrrImpute.R
#' @rdname tskrrHomogenousImpute-class
#' @aliases tskrrHomogenousImpute
#' @exportClass tskrrHomogenousImpute
setClass("tskrrHomogenousImpute",
         contains = c("tskrrImpute", "tskrrHomogenous")
         )
