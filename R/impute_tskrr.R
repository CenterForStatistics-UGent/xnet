#' Impute missing values in a label matrix
#'
#' This function implements an optimization algorithm that allows
#' imputing missing values in the label matrix while fitting a
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
#' @param ... arguments passed to other methods.
#'
#' @return A \code{tskrr} model of the class \code{\link{tskrrImputeHeterogeneous}} or \code{\link{tskrrImputeHomogeneous}} depending on whether or
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
NULL
# Internal function
.impute_tskrr <- function(y,
                   k,
                   g = NULL,
                   lambda = 1e-02,
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
  homogeneous <- iptest$homogeneous
  lambda.k <- iptest$lambda.k
  lambda.g <- iptest$lambda.g

  # Rearrange matrices if necessary
  rk <- rownames(k) # not when there's no row/-colnames

  if(!is.null(rk)){
    cg <- if(homogeneous) rk else colnames(g)
    if(!all(rownames(y) == rk) || ! all(colnames(y) == cg))
      y <- match_labels(y,rk,cg)

  }


  # CALCULATE EIGEN DECOMPOSITION
  k.eigen <- get_eigen(k)
  g.eigen <- if(!homogeneous) get_eigen(g) else NULL

  # CALCULATE HAT MATRICES
  Hk <- eigen2hat(k.eigen$vectors, k.eigen$values, lambda.k)

  Hg <- if(!homogeneous)
    eigen2hat(g.eigen$vectors, g.eigen$values, lambda.g)
  else
    Hk

  naid <- is.na(y)

  if(homogeneous){
    # Test symmetry if required.
    symmetry <- match.arg(symmetry)
    if(symmetry == "auto"){
      ynona <- y
      ynona[naid] <- 0
      symmetry <- test_symmetry(ynona)
      if(symmetry == "none")
        stop(paste("The Y matrix is not symmetric or skewed symmetric.",
                   "You need a kernel matrix for rows and columns."))
    }
  }


  imp <- impute_tskrr.fit(y,Hk,Hg,naid,niter,tol, start, verbose)
  pred <- Hk %*% imp$y %*% Hg

  # CREATE OUTPUT
  if(homogeneous){

    out <- new("tskrrImputeHomogeneous",
               y = imp$y,
               k = k,
               lambda.k = lambda.k,
               pred = pred,
               symmetry = symmetry,
               has.hat = keep,
               Hk = if(keep) Hk else matrix(0),
               imputeid = which(naid),
               niter = as.integer(imp$niter),
               tol = tol
               )
  } else {
    out <- new("tskrrImputeHeterogeneous",
               y = imp$y,
               k = k,
               g = g,
               lambda.k = lambda.k,
               lambda.g = lambda.g,
               pred = pred,
               has.hat = keep,
               Hk = if(keep) Hk else matrix(0),
               Hg = if(keep) Hg else matrix(0),
               imputeid = which(naid),
               niter = as.integer(imp$niter),
               tol = tol
    )
  }
  return(out)
}

#----------------------------
# Methods

#' @rdname impute_tskrr
#' @export
setMethod("impute_tskrr",
          c("matrix","gramData","gramData"),
          .impute_tskrr)

#' @rdname impute_tskrr
#' @export
setMethod("impute_tskrr",
          c("matrix","matrix","matrix"),
          function(y,k,g, ...){
            k <- gramData(k)
            g <- gramData(g)
            .impute_tskrr(y,k,g,...)
          })

#' @rdname impute_tskrr
#' @export
setMethod("impute_tskrr",
          c("matrix","matrix","ANY"),
          function(y, k, g, ...){
            if(!missing(g) && !is.null(g))
              stop(paste("No method for an object of class",
                         class(g)[1],"for argument g."))
            k <- gramData(k)
            .impute_tskrr(y, k, g = NULL, ...)
          })
