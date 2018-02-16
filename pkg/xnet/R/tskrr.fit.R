#' Carry out a two-step kernel ridge regression
#'
#' This function provides an interface for two-step kernel ridge regression.
#' To use this function, you need at least one kernel matrix and one
#' interaction matrix.
#'
#' @section TO DO:
#' \itemize{
#'   \item Add automatic tuning (not implemented yet)
#'   \item Add mechanism to allow for different boundaries for lambda.k
#'   and lambda.g
#' }
#'
#' @param y a matrix with the response values
#' @param k an object of class \code{\link{eigen}} containing the eigen
#' decomposition of the first kernel matrix.
#' @param g an optional object of class \code{\link{eigen}} containing
#' the eigen decomposition of the second kernel matrix. If \code{NULL},
#' the network is considered to be homogenous.
#' @param lambda.k an optional numeric value for the lambda parameter tied
#' to the first kernel. If \code{NULL}, the parameters are automatically
#' tuned
#' @param lambda.g an optional numeric value for the lambda parameter tied
#' to the second kernel. If \code{NULL}, the model is fit using the same
#' value for \code{lambda.k} and \code{lambda.g}
#' @param lim a numeric vector with 2 values indicating the boundaries of
#' the space in which lambda is defined.
#' @param ... arguments passed to other functions. Currently ignored.
#'
#' @return TO BE DETERMINED
#'
#' @export
tskrr.fit <- function(y, k, g, lambda.k = NULL, lambda.g = NULL,
                      lim = c(1e-7,1e7), ...){
  NULL
}
