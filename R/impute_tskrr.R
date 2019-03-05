#' Impute missing values in a adjacency matrix
#'
#' This function implements an optimization algorithm that allows to
#' impute missing values in the adjacency matrix while fitting a
#' \code{tskrr} model.
#'
#' @inheritParams tskrr
#' @param niter an integer giving the maximum number of iterations
#' @param tol a numeric value indicating the tolerance for convergence of
#' the algorithm. It is the maximum sum of squared differences between
#' to iteration steps.
#' @param start a numeric value indicating the value with which NA's are
#' replaced in the first step of the algorithm. Defaults to 0.
#' @param verbose either a logical value, 1 or 2. \code{1} means "show the number
#' of iterations and the final deviation", \code{2} means "show the deviation
#' every 10 iterations". A value \code{TRUE} is read as \code{1}.
#' @param useloo a logical value indicating whether leave-one-out
#' crossvalidation should be used to do the imputations.
#' @inheritParams get_loo_fun
#'
#' @return a matrix with the missing values replaced by the imputations
#'
#' @export
impute_tskrr <- function(y,
                   k,
                   g = NULL,
                   lambda = 1e-04,
                   testdim = TRUE,
                   testlabels = TRUE,
                   niter = 1e4,
                   tol = sqrt(.Machine$double.eps),
                   start = mean(y, na.rm = TRUE),
                   verbose = FALSE,
                   useloo = FALSE,
                   exclusion = c("interaction","row","column","both"),
                   replaceby0 = FALSE
){

  # Set flags
  homogenous <- is.null(g)
  stop("This function needs reworking.")

  # TESTS INPUT
  if( !(is.matrix(y) && is.numeric(y)) )
    stop("y should be a matrix.")

  if( !(is.matrix(k) && is.numeric(k)) )
    stop("k should be a matrix.")

  if(!is.numeric(lambda))
    stop("lambda should be numeric.")

  if(!is.numeric(tol) || length(tol) != 1)
    stop("tol should be a single numeric value.")

  if(!is.numeric(niter) || length(niter) != 1 || niter %% 1 != 0)
    stop("niter should be a single integer value.")

  if(!homogenous){
    if( !(is.matrix(g) && is.numeric(g)) )
      stop("g should be a matrix.")

    nl <- length(lambda)
    if(nl < 1 || nl > 2)
      stop("lambda should contain one or two values. See ?tskrr")

  } else {
    if(length(lambda) != 1)
      stop("lambda should be a single value. See ?tskrr")
  }

  # TEST KERNELS
  if(testdim){
    if(!is_symmetric(k))
      stop("k should be a symmetric matrix.")

    if(!homogenous && !is_symmetric(g))
      stop("g should be a symmetric matrix.")

    if(!valid_dimensions(y,k,g))
      stop(paste("The dimensions of the matrices don't match.",
                 "Did you maybe switch the k and g matrices?",
                 sep = "\n"))
  }
  if(testlabels){

    valid_labels(y,k,g) # Generates errors if something's wrong

    rk <- rownames(k) # not when there's no row/-colnames
    if(!is.null(rk)){
      y <- match_labels(y,rk,colnames(g))
    }

  }

  # SET LAMBDAS
  lambda.k <- lambda[1]
  lambda.g <- if(!homogenous){
    if(nl == 1) lambda else lambda[2]
  } else NULL

  # CALCULATE EIGEN DECOMPOSITION
  k.eigen <- eigen(k, symmetric = TRUE)
  g.eigen <- if(!homogenous) eigen(g, symmetric = TRUE) else NULL

  # CALCULATE HAT MATRICES
  Hk <- eigen2hat(k.eigen$vectors, k.eigen$values, lambda.k)

  Hg <- if(!homogenous)
    eigen2hat(g.eigen$vectors, g.eigen$values, lambda.g)
  else
    Hk

  if(useloo){
    exclusion <- match.arg(exclusion)
    whatfun <- if(homogenous)"tskrrHomogenous" else "tskrrHeterogenous"
    loofun <- get_loo_fun(whatfun,
                          exclusion = exclusion,
                          replaceby0 = replaceby0)
    .impute_loo(y, Hk, Hg, loofun, niter, tol, start, verbose)
  } else {
    .impute_pred(y,Hk,Hg,niter,tol,start, verbose)
  }

}
# Workhorse function
.impute_pred <- function(x,Hk,Hg,niter,tol, start, verbose){

  naid <- is.na(x)
  if(!any(naid)){
    warning("The matrix didn't contain missing values")
    return(x)
  }

  # Replace values
  x[naid] <- start
  prev <- x[naid]
  div <- TRUE
  # Loop
  iter <- 0
  showsteps <- verbose > 1
  showres <- verbose > 0
  while(iter <= niter && div > tol){

    iter <- iter + 1

    pred <- Hk %*% x %*% Hg
    x[naid] <- pred[naid]

    div <- sum((prev - x[naid])^2)
    if(showsteps){
      if(iter %% 10 == 0) cat("iteration: ",iter," - deviation: ",div,"\n")
    }
    prev <- x[naid]

  }
  if(showres){
    cat("Nr. of iterations:", iter, " - Deviation:",div,"\n")
  }
  return(x)
}

# Workhorse function
.impute_loo <- function(x,Hk,Hg, loofun, niter,tol, start, verbose){

  naid <- is.na(x)
  if(!any(naid)){
    warning("The matrix didn't contain missing values")
    return(x)
  }

  # Replace values
  x[naid] <- start
  prev <- x[naid]
  div <- TRUE
  # Loop
  iter <- 0
  showsteps <- verbose > 1
  showres <- verbose > 0
  while(iter <= niter && div > tol){

    iter <- iter + 1

    pred <- Hk %*% x %*% Hg
    x[naid] <- pred[naid]

    div <- sum((prev - x[naid])^2)
    if(showsteps){
      if(iter %% 10 == 0) cat("iteration: ",iter," - deviation: ",div,"\n")
    }
    prev <- x[naid]

  }
  if(showres){
    cat("Nr. of iterations:", iter, " - Deviation:",div,"\n")
  }

  return(x)
}
