#' Carry out a two-step kernel ridge regression
#'
#' This function provides an interface for two-step kernel ridge regression.
#' To use this function, you need at least one kernel matrix and one
#' interaction matrix.
#'
#' @param y a matrix representing the links between the nodes of both
#' networks.
#' @param k an object of class \code{\link{eigen}} containing the eigen
#' decomposition of the first kernel matrix.
#' @param g an optional object of class \code{\link{eigen}} containing
#' the eigen decomposition of the second kernel matrix. If \code{NULL},
#' the network is considered to be homogenous.
#' @param lambda.k a numeric value for the lambda parameter tied
#' to the first kernel.
#' @param lambda.g a numeric value for the lambda parameter tied
#' to the second kernel. If \code{NULL}, the model is fit using the same
#' value for \code{lambda.k} and \code{lambda.g}
#' @param ... arguments passed to other functions. Currently ignored.
#'
#' @return TO BE DETERMINED
#'
#' @export
tskrr.fit <- function(y, k, g = NULL, lambda.k = NULL, lambda.g = NULL,
                      ...){

  # Set flags
  homogenous <- is.null(g)

  # process input
  if(is.null(lambda.g)) lambda.g <- lambda.k

  # get hat matrics
  Hk <- eigen2hat(k$vectors, k$values, lambda.k)
  Hg <- if(!homogenous) eigen2hat(g$vectors, g$values, lambda.g) else NULL

  # Create predictions
  pred <- if(!homogenous)
    Hk %*% y %*% Hg
  else
    Hk %*% y %*% Hk

  return(list(k = Hk,g = Hg, pred = pred))

}
