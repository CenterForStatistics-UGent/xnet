#' tune the lambda parameters for a tskrr
#'
#' This function lets you tune the lambda parameter(s) of a two-step
#' kernel ridge regression model for optimal performance. You can either
#' tune a previously fitted \code{\link{tskrr}} model, or pass the
#' label matrix and kernel matrices to fit and tune a model in
#' one go.
#'
#' This function currently only performs a simple grid search for all
#' (combinations of) lambda values. If no specific lambda values are
#' provided, then the function uses \code{\link{create_grid}} to
#' create an evenly spaced (on a logarithmic scale) grid.
#'
#' In the case of a heterogeneous network, you can specify different values
#' for the two parameters that need tuning. To do so, you need to
#' provide a list with the settings for every parameter to the arguments
#' \code{lim}, \code{ngrid} and/or \code{lambda}. If you
#' try this for a homogeneous network, the function will return an error.
#'
#' Alternatively, you can speed up the grid search by searching in a
#' single dimension. When \code{onedim = TRUE}, the search for a
#' heterogeneous network will only consider cases where both lambda values
#' are equal.
#'
#' The arguments \code{exclusion} and \code{replaceby0} are used by
#' the function \code{\link{get_loo_fun}} to find the correct
#' leave-one-out function.
#'
#' By default, the function uses standard mean squared error based on
#' the cross-validation results as a measure for optimization. However, you
#' can provide a custom function if needed, as long as it takes
#' two matrices as input: \code{Y} being the observed interactions and
#' \code{LOO} being the result of the chosen cross-validation.
#'
#' @param x a \code{\link{tskrr}} object representing a two step
#' kernel ridge regression model.
#' @param lim a vector with 2 values that give the boundaries for the domain
#' in which lambda is searched, or possibly a list with 2 elements. See details
#' @param ngrid a single numeric value giving the number of points
#' in a single dimension of the grid, or possibly a list with 2 elements.
#' See details.
#' @param lambda a vector with the lambdas that need checking for
#' homogeneous networks, or possibly a list with two elements for
#' heterogeneous networks. See Details. Defaults to
#' \code{NULL}, which means that the function constructs the search grid
#' from the other arguments.
#' @param fun a loss function that takes the label matrix Y and the
#' result of the crossvalidation LOO as input. The function name can
#' be passed as a character string as well.
#' @param onedim a logical value indicating whether the search should be
#' done in a single dimension. See details.
#' @inheritParams get_loo_fun
#' @inheritParams tskrr
#' @param ... arguments to be passed to the loss function
#'
#' @return a model of class \code{\link[xnet:tskrrTune-class]{tskrrTune}}
#'
#' @seealso
#' * \code{\link{loo}}, \code{\link{loo_internal}} and
#' \code{\link{get_loo_fun}} for more information on how leave one out
#' validation works.
#' * \code{\link{tskrr}} for fitting a twostep kernel ridge regression.
#' * \code{\link{loss_functions}} for different loss functions.
#' @md
#'
#' @examples
#' data(drugtarget)
#'
#' mod <- tskrr(drugTargetInteraction, targetSim, drugSim)
#' tuned <- tune(mod, lim = c(0.1,1), ngrid = list(5,10),
#'               fun = loss_auc)
#'
#' \dontrun{
#'
#' # This is just some visualization of the matrix
#' # It can be run safely.
#' gridvals <- get_grid(tuned)
#' z <- get_loss_values(tuned)        # loss values
#'
#' image(gridvals$k,gridvals$g,z, log = 'xy',
#'       xlab = "lambda k", ylab = "lambda g",
#'       col = rev(heat.colors(20)))
#'
#' }
#' @rdname tune
#' @name tune
NULL

#' @rdname tune
#' @export
setMethod("tune",
          "tskrrHomogeneous",
          function(x,
                   lim = c(1e-4,1),
                   ngrid = 10,
                   lambda = NULL,
                   fun = loss_mse,
                   exclusion = 'edges',
                   replaceby0 = FALSE,
                   onedim = TRUE,
                   ...){

            # Translate edges and vertices
            if(exclusion %in% c("interaction","both"))
              exclusion <- switch(exclusion,
                                  interaction = "edges",
                                  both = "vertices")

            if(!onedim)
              warning("Only one-dimensional search is possible for homogeneous networks.")
            fun <- match.fun(fun)
            lambda <- .prepare_lambdas(lim, ngrid, lambda, homogeneous = TRUE)

            loofun <- .getloo_homogeneous(exclusion = exclusion,
                                         symmetry = symmetry(x),
                                         replaceby0 = replaceby0)

            decomp <- get_eigen(x)

            loss <- function(lambda){
              Hr <- eigen2hat(decomp$vectors,
                              decomp$values,
                              lambda)
              pred <- Hr %*% x@y %*% Hr
              fun(x@y, loofun(x@y, Hr, pred), ...)
            }

            lval <- vapply(lambda$k,loss, numeric(1))
            best <- which.min(lval)

            best_loss <- lval[best]
            best_lambda <- lambda$k[best]

            newx <- update(x, best_lambda)

            as_tuned(newx,
                lambda_grid = lambda,
                lambda.k = best_lambda,
                best_loss = best_loss,
                loss_values = matrix(lval, ncol = 1),
                loss_function = fun,
                exclusion = exclusion,
                replaceby0 = replaceby0,
                onedim = TRUE
                )

          })

#' @rdname tune
#' @export
setMethod("tune",
          "tskrrHeterogeneous",
          function(x,
                   lim = c(1e-4,1),
                   ngrid = 10,
                   lambda = NULL,
                   fun = loss_mse,
                   exclusion = 'interaction',
                   replaceby0 = FALSE,
                   onedim = FALSE,
                   ...){

            fun <- match.fun(fun)
            lambda <- .prepare_lambdas(lim, ngrid, lambda,
                                       homogeneous = FALSE,
                                       onedim = onedim)

            # Prepare objects
            decompr <- get_eigen(x, 'row')
            decompc <- get_eigen(x, 'column')

            loofun <- .getloo_heterogeneous(exclusion = exclusion,
                                           replaceby0 = replaceby0)

            loss <- function(l1, l2){
              Hr <- eigen2hat(decompr$vectors,
                              decompr$values,
                              l1)
              Hc <- eigen2hat(decompc$vectors,
                              decompc$values,
                              l2)
              pred <- Hr %*% x@y %*% Hc
              fun(x@y, loofun(x@y, Hr, Hc, pred), ...)
            }

            if(onedim){
              lval <- vapply(lambda$k,
                             function(i) loss(i,i), numeric(1))
              best <- which.min(lval)

              best_loss <- lval[best]
              best_lambda <- rep(lambda$k[best],2)

              # Add correct data for heterogeneous
              lval <- matrix(lval, ncol = 1) # must be a matrix
            } else {
              lval <- vapply(lambda$g,
                             function(l2){
                               vapply(lambda$k,
                                      loss,
                                      numeric(1),
                                      l2)
                             },
                             numeric(length(lambda$k)))

              best <- find_min_pos(lval)

              best_lambda <- c(lambda$k[best[1]], lambda$g[best[2]])
              best_loss <- lval[best[1],best[2]]
            }

            newx <- update(x, best_lambda)

            as_tuned(newx,
                lambda_grid = lambda,
                lambda.k = best_lambda[1],
                lambda.g = best_lambda[2],
                best_loss = best_loss,
                loss_values = lval,
                loss_function = fun,
                exclusion = exclusion,
                replaceby0 = replaceby0,
                onedim = onedim
            )
          })

#' @rdname tune
#' @export
setMethod("tune",
          "matrix",
          function(x,
                   k,
                   g = NULL,
                   lim = c(1e-4,1),
                   ngrid = 10,
                   lambda = NULL,
                   fun = loss_mse,
                   exclusion = 'interaction',
                   replaceby0 = FALSE,
                   testdim = TRUE,
                   testlabels = TRUE,
                   symmetry = c("auto","symmetric","skewed"),
                   keep = FALSE,
                   onedim = is.null(g),
                   ...){

            homogeneous <- is.null(g)
            fun <- match.fun(fun)
            # get initial lambdas
            lambda <- .prepare_lambdas(lim, ngrid, lambda,
                                       homogeneous = homogeneous,
                                       onedim = onedim)
            if(homogeneous){
              init_lambda <- lambda$k[1]
            } else {
              init_lambda <- c(lambda$k[1], lambda$g[1])
            }

            # Fit initial model
            mod <- tskrr(x,k,g, lambda = init_lambda,
                         testdim = testdim,
                         testlabels = testlabels,
                         symmetry = symmetry,
                         keep = keep)

            # Carry out the tuning
            callGeneric(mod,
                        lambda = lambda,
                        fun = fun,
                        exclusion = exclusion,
                        replaceby0 = replaceby0,
                        onedim = onedim,
                        ...)
          })

# Helper function find_best_lambda

find_min_pos <- function(x){

  id <- which.min(x)
  nr <- nrow(x)
  rowid <- id %% nr
  if(rowid == 0) rowid <- nr
  colid <- id %/% nr + 1*(rowid != nr)
  return(c(rowid,colid))
}
