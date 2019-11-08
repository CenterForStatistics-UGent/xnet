#' Carry out a two-step kernel ridge regression
#'
#' This function provides an interface for two-step kernel ridge regression.
#' To use this function, you need at least one kernel matrix and one
#' label matrix. It's the internal engine used by the function
#' \code{\link{tskrr}}.
#'
#' This function is mostly available for internal use. In most cases, it
#' makes much more sense to use \code{\link{tskrr}}, as that function
#' returns an object one can work with. The function
#' \code{tskrr.fit} could be useful when doing simulations or
#' fitting algorithms, as the information returned from this function
#' is enough to use the functions returned by \code{\link{get_loo_fun}}.
#'
#' @param y a matrix representing the links between the nodes of both
#' networks.
#' @param k an object of class \code{\link{eigen}} containing the eigen
#' decomposition of the first kernel matrix.
#' @param g an optional object of class \code{\link{eigen}} containing
#' the eigen decomposition of the second kernel matrix. If \code{NULL},
#' the network is considered to be homogeneous.
#' @param lambda.k a numeric value for the lambda parameter tied
#' to the first kernel.
#' @param lambda.g a numeric value for the lambda parameter tied
#' to the second kernel. If \code{NULL}, the model is fit using the same
#' value for \code{lambda.k} and \code{lambda.g}
#' @param ... arguments passed to other functions. Currently ignored.
#'
#' @return a list with three elements:
#' \itemize{
#'    \item k : the hat matrix for the rows
#'    \item g : the hat matrix for the columns (or \code{NULL})
#'    for homogeneous networks.
#'    \item pred : the predictions
#' }
#'
#' @examples
#'
#' data(drugtarget)
#'
#' K <- eigen(targetSim)
#' G <- eigen(drugSim)
#'
#' res <- tskrr.fit(drugTargetInteraction,K,G,
#'                  lambda.k = 0.01, lambda.g = 0.05)
#'
#' @export
tskrr.fit <- function(y, k, g = NULL, lambda.k = NULL, lambda.g = NULL,
                      ...){

  # Set flags
  homogeneous <- is.null(g)

  # process input
  if(is.null(lambda.g)) lambda.g <- lambda.k

  # get hat matrics
  Hk <- eigen2hat(k$vectors, k$values, lambda.k)
  Hg <- if(!homogeneous) eigen2hat(g$vectors, g$values, lambda.g) else NULL

  # Create predictions
  pred <- if(!homogeneous)
    Hk %*% y %*% Hg
  else
    Hk %*% y %*% Hk

  return(list(k = Hk,g = Hg, pred = pred))

}
