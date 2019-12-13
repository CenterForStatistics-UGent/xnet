#' Class tskrrImputeHomogeneous
#'
#' The class \code{tskrrImputeHomogeneous} is a subclass of the
#' class \code{\link[xnet:tskrrHomogeneous-class]{tskrrHomogeneous}} and
#' \code{\link[xnet:tskrrImpute-class]{tskrrImpute}}
#' specifically for homogeneous networks with imputed values. It is
#' the result of the function \code{\link{impute_tskrr}} on a
#' homogeneous network model.
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
#' \code{\link{tskrr-class}}). For homogeneous networks, \code{g}
#' is always \code{NA}. If \code{k} is \code{NA}, the labels used
#' are integers indicating the row resp column number.
#' @slot imputeid a vector with integer values indicating which of
#' the values in \code{y} are imputed
#' @slot niter an integer value gving the number of iterations used
#' @slot tol a numeric value with the tolerance used
#'
#' @include Class_tskrrHomogeneous.R Class_tskrrImpute.R
#' @rdname tskrrImputeHomogeneous-class
#' @aliases tskrrImputeHomogeneous
#' @exportClass tskrrImputeHomogeneous
setClass("tskrrImputeHomogeneous",
         contains = c("tskrrImpute", "tskrrHomogeneous")
         )
