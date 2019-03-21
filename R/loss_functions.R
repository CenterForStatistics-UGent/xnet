#' loss functions
#'
#' The functions can be used in the \code{tune} as loss functions.
#' Currently two functions are provided: a function calculating the
#' classic mean squared error (\code{loss_mse}) and a function
#' calculating 1 - AUC (\code{loss_auc}).
#'
#' The AUC is calculated by sorting the \code{Y} matrix based on
#' the order of the values in the \code{LOO} matrix. The false and true
#' positive rate are calculated solely based on that ordering, which
#' allows for values in \code{LOO} outside the range [0,1]. It's
#' a naive implementation which is good enough for tuning, but
#' shouldn't be used as a correct value for 1 - auc in case the
#' values in \code{LOO} are outside the range [0,1].
#'
#' @section Note:
#' The function \code{loss_auc} should only be used for a \code{Y}
#' matrix that contains solely the values 0 and 1.
#'
#' @param Y the adjacency matrix with observed responses
#' @param LOO the leave-one-out crossvalidation (or predictions if you
#' must). This one can be calculated by the function \code{loo}.
#' @param na.rm a logical value
#'
#' @seealso \code{\link{tune}} for application of the loss function
#'
#' @examples
#'
#' x <- c(1,0,0,1,0,0,1,0,1)
#' y <- c(0.8,-0.1,0.2,0.2,0.4,0.01,1.12,0.9,0.9)
#' loss_mse(x,y)
#' loss_auc(x,y)
#'
#' @rdname loss_functions
#' @name loss_functions
#' @aliases loss_mse loss_auc
#' @export
loss_mse <- function(Y, LOO, na.rm = FALSE){
  mean((Y - LOO)^2, na.rm = na.rm)
}

#' @rdname loss_functions
#' @export
loss_auc <- function(Y, LOO){

  id <- order(LOO)
  roc_y <- Y[id]

  # Calculate total number positives, negatives and y values
  np <- sum(roc_y)
  ny <- length(roc_y)
  nn <- ny - np

  # tpr and fpr
  fpr <- cumsum(roc_y)/np
  tpr <- cumsum(roc_y == 0) / nn

  dtpr <- diff(tpr)
  dfpr <- diff(fpr)
  auc <- sum( dfpr * tpr[2:ny] - (dfpr * dtpr)/2  )
  return(1 - auc)
}