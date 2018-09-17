#' Fit a linear filter over an adjacency matrix
#'
#' This function fits a linear filter over an adjacency matrix.
#'
#' NEEDS DIFFERENT OUTPUT AND ALPHA CONTROL
#'
#' @param y an adjacency matrix
#' @param alpha a vector with 4 alpha values, or a single alpha value
#' which then is used for all 4 alphas.
#'
#' @return an object of class \code{\link[class-linearFilter]{linearFilter}}
#'
#' @examples
#' data(drugtarget)
#' linear_filter(drugTargetInteraction, alpha = 0.25)
#' linear_filter(drugTargetInteraction, alpha = c(0.1,0.1,0.4,0.4))
#'
#' @export
linear_filter <- function(y, alpha=0.25){

  stopifnot(is.matrix(y),
            is.numeric(alpha),
            is.atomic(alpha))

  if(length(alpha) == 1)
    alpha <- rep(alpha,4)
  else if(length(alpha) !=4)
    stop("alpha should be a single number of 4 numbers.")

  # simple matrix filter
  model = list(
    Y = Y,
    alpha.1 = alpha.1,
    alpha.2 = alpha.2,
    alpha.3 = alpha.3,
    alpha.4 = alpha.4,
    global.mean = mean(Y),
    col.means = c(colMeans(Y)),
    row.means = c(rowMeans(Y))
  )
  model$F <- alpha.1 * model$Y + alpha.2 * rep(model$col.means, each=nrow(Y)) +
    alpha.3 * rep(model$row.means, times=ncol(Y)) +
    alpha.4 * model$global.mean
  return(model)
}
