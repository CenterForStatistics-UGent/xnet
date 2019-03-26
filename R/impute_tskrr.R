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
#'
#' @return A \code{tskrr} model of the class \code{\link{tskrrHeterogenousImpute}} or \code{\link{tskrrHomogenousImpute}} depending on whether or
#' not \code{g} has a value.
#'
#' @examples
#'
#' data(drugtarget)
#'
#' naid <- sample(length(drugTargetInteraction), 30)
#' drugTargetInteraction[naid] <- NA
#'
#' impute_tskrr(drugTargetInteraction, targetSim, drugSim)
#'
#' @rdname impute_tskrr
#' @name impute_tskrr
#' @aliases impute_tskrr
#' @export
impute_tskrr <- function(y,
                   k,
                   g = NULL,
                   lambda = 1e-04,
                   testdim = TRUE,
                   testlabels = TRUE,
                   symmetry = c("auto","symmetric","skewed"),
                   keep = FALSE,
                   niter = 1e4,
                   tol = sqrt(.Machine$double.eps),
                   start = mean(y, na.rm = TRUE),
                   verbose = FALSE
){

  iptest <- .test_input(y,k,g,lambda,testdim,testlabels,
                        checkna = FALSE)
  homogenous <- iptest$homogenous
  lambda.k <- iptest$lambda.k
  lambda.g <- iptest$lambda.g

  # Rearrange matrices if necessary
  rk <- rownames(k) # not when there's no row/-colnames
  ck <- colnames(k)

  if(!is.null(rk)){
    cg <- colnames(g)
    if(!all(rownames(y) == rk))
      y <- match_labels(y,rk,cg)
    if(!all(rk == ck))
      k <- match_labels(k,rk,rk)
    if(!homogenous){
      rg <- rownames(g)
      if(!all(cg == rg))
        g <- match_labels(g,cg,cg)
    }

  }


  # CALCULATE EIGEN DECOMPOSITION
  k.eigen <- eigen(k, symmetric = TRUE)
  g.eigen <- if(!homogenous) eigen(g, symmetric = TRUE) else NULL

  # CALCULATE HAT MATRICES
  Hk <- eigen2hat(k.eigen$vectors, k.eigen$values, lambda.k)

  Hg <- if(!homogenous)
    eigen2hat(g.eigen$vectors, g.eigen$values, lambda.g)
  else
    Hk

  if(homogenous){
    # Test symmetry if required.
    symmetry <- match.arg(symmetry)
    if(symmetry == "auto"){
      symmetry <- test_symmetry(y)
      if(symmetry == "none")
        stop(paste("The Y matrix is not symmetric or skewed symmetric.",
                   "You need a kernel matrix for rows and columns."))
    }
  }

  naid <- is.na(y)
  pred <- .impute_pred(y,Hk,Hg,naid,niter,tol, start, verbose)

  # Create labels
  rn <- rownames(y)
  cn <- colnames(y)
  if(is.null(rn)) rn <- NA_character_
  if(is.null(cn)) cn <- NA_character_

  # CREATE OUTPUT
  if(homogenous){

    out <- new("tskrrHomogenousImpute",
               y = y,
               k = k.eigen,
               lambda.k = lambda.k,
               pred = pred,
               symmetry = symmetry,
               has.hat = keep,
               Hk = if(keep) Hk else matrix(0),
               labels = list(k=rn, g = NA_character_),
               imputeid = which(naid),
               niter = as.integer(niter),
               tol = tol
               )
  } else {
    out <- new("tskrrHeterogenousImpute",
               y = y,
               k = k.eigen,
               g = g.eigen,
               lambda.k = lambda.k,
               lambda.g = lambda.g,
               pred = pred,
               has.hat = keep,
               Hk = if(keep) Hk else matrix(0),
               Hg = if(keep) Hg else matrix(0),
               labels = list(k=rn, g = cn),
               imputeid = which(naid),
               niter = as.integer(niter),
               tol = tol
    )
  }
  return(out)
}
# Workhorse function
.impute_pred <- function(x,Hk,Hg,naid, niter,tol, start, verbose){

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

