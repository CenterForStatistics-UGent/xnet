#' Impute values based on a two-step kernel ridge regression
#'
#' This function provides an interface for the imputation of values
#' based on a \code{\link{tskrr}} model and is the internal function
#' used by \code{\link{impute_tskrr}}.
#'
#' This function is mostly available for internal use. In most cases,
#' it makes much more sense to use \code{\link{impute_tskrr}}, as that
#' function returns an object one can work with. The function
#' \code{impute_tskrr.fit} could be useful when doing simulations or
#' creating fitting algorithms.
#'
#' @param y a label matrix
#' @param Hk a hat matrix for the rows (see also \code{\link{eigen2hat}}
#' on how to calculate them from an eigen decomposition)
#' @param Hg a hat matrix for the columns. For homogenous networks, this
#' should be Hk again.
#' @param naid an optional index with the values that have to be imputed,
#' i.e. at which positions you find a \code{NA} value. It can be a vector
#' with integers or a matrix with \code{TRUE}/\code{FALSE} values.
#' @inheritParams impute_tskrr
#'
#' @return a list with two elements:
#'  * a matrix \code{y} with the imputed values filled in.
#'  * a numeric value \code{niter} with the amount of iterations
#'
#' @seealso
#'  * \code{\link{impute_tskrr}} for the user-level function, and
#'  * \code{\link{eigen2hat}} for conversion of a eigen decomposition to
#'  a hat matrix.
#' @md
#'
#' @examples
#'
#' data(drugtarget)
#'
#' K <- eigen(targetSim)
#' G <- eigen(drugSim)
#'
#' Hk <- eigen2hat(K$vectors, K$values, lambda = 0.01)
#' Hg <- eigen2hat(G$vectors, G$values, lambda = 0.05)
#'
#' drugTargetInteraction[c(3,17,123)] <- NA
#'
#' res <- impute_tskrr.fit(drugTargetInteraction, Hk, Hg,
#'                         niter = 1000, tol = 10e-10,
#'                         start = 0, verbose = FALSE)
#'
#' @export
impute_tskrr.fit <- function(y,Hk,Hg,naid = NULL,
                             niter,tol, start, verbose){

  if(is.null(naid)) naid <- is.na(y)
  if(!any(naid)){
    warning("The matrix didn't contain missing values")
    return(list(y = y,
                niter = 0L))
  }

  # Replace values
  y[naid] <- start
  prev <- y[naid]
  div <- TRUE
  # Loop
  iter <- 0
  showsteps <- verbose > 1
  showres <- verbose > 0
  while(iter < niter && div > tol){

    iter <- iter + 1

    pred <- Hk %*% y %*% Hg
    y[naid] <- pred[naid]

    div <- sum((prev - y[naid])^2)
    if(showsteps){
      if(iter %% 10 == 0) message("iteration: ",iter," - Deviation: ",div,"\n")
    }
    prev <- y[naid]

  }
  if(showres){
    message("Nr. of iterations: ", iter, " - Deviation:",div,"\n")
  }
  return(list(y = y,
              niter = iter))
}
