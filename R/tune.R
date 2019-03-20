#' tune the lambda parameters for a tskrr
#'
#' This function lets you tune the lambda parameter(s) of a two-step
#' kernel ridge regression for optimal performance. The objective
#' function used for tuning is the squared norm based on leave-one-out
#' crossvalidation.
#'
#' This function currently only performs a naive grid search for all
#' (combinations of) lambda values. If no specific lambda values are
#' provided, then the function uses \code{\link{create_grid}} to
#' create a grid that's evenly spaced on a logarithmic scale.
#'
#' In the case of a heterogenous network, you can specify different values
#' for the two parameters that need tuning. In order to do so, you need to
#' provide a list with the settings for every parameter to the arguments
#' \code{lim}, \code{ngrid} and/or \code{lambda}. If you
#' try this for a homogenous network, the function will return an error.
#'
#' The arguments \code{exclusion} and \code{replaceby0} are used by
#' the function \code{\link{get_loo_fun}} to find the correct
#' leave-one-out function.
#'
#' By default the function uses classic mean squared error based on
#' the crossvalidation results as a measure for optimization. But you
#' can provide your own function if needed, as long as it takes
#' two matrices as input: \code{Y} being the observed interactions and
#' \code{LOO} being the result of the chosen crossvalidation.
#'
#' @param x a \code{\link{tskrr}} object representing a two step
#' kernel ridge regression model.
#' @param lim a vector with 2 values that give the boundaries for the domain
#' in which lambda is searched, or possibly a list with 2 elements. See details
#' @param ngrid a single numeric value giving the number of points
#' in a single dimension of the grid, or possibly a list with 2 elements.
#' See details.
#' @param lambda a vector with the lambdas that need checking for
#' homogenous networks, or possibly a list with two elements for
#' heterogenous networks. See Details. Defaults to
#' \code{NULL}, which means that the function constructs the search grid
#' from the other arguments.
#' @param fun a loss function that takes the adjacency matrix Y and the
#' result of the crossvalidation LOO as input.
#' @inheritParams get_loo_fun
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
#' tuned <- tune(mod, ngrid = 10)
#'
#' \dontrun{
#'
#' # This is just some visualization of the matrix
#' # It can be run safely.
#'
#' x <- tuned$lambda[[1]] #lambdas for rows
#' y <- tuned$lambda[[2]] #lambdas for columns
#' z <- tuned$loss        # loss values
#'
#' image(x,y,log(z), log = 'xy')
#'
#' }
#' @rdname tune
#' @name tune
NULL

#' @rdname tune
#' @export
setMethod("tune",
          "tskrrHomogenous",
          function(x,
                   lim = c(1e-4,1),
                   ngrid = 10,
                   lambda = NULL,
                   fun = loss_mse,
                   exclusion = 'interaction',
                   replaceby0 = FALSE,
                   ...){

            lambda <- .prepare_lambdas(lim, ngrid, lambda, homogenous = TRUE)

            loofun <- .getloo_homogenous(exclusion = exclusion,
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

            best_lambda <- lambda$k[best]
            best_loss <- lval[best]

            newx <- update(x, best_lambda)

            new("tskrrTune",
                model = newx,
                lambda_grid = lambda,
                best_lambda = best_lambda,
                best_loss = best_loss,
                loss_values = matrix(lval, ncol = 1),
                loss_function = fun,
                exclusion = exclusion,
                replaceby0 = replaceby0
                )

          })

#' @rdname tune
#' @export
setMethod("tune",
          "tskrrHeterogenous",
          function(x,
                   lim = c(1e-4,1),
                   ngrid = 10,
                   lambda = NULL,
                   fun = loss_mse,
                   exclusion = 'interaction',
                   replaceby0 = FALSE,
                   ...){

            lambda <- .prepare_lambdas(lim, ngrid, lambda, homogenous = FALSE)

            # Prepare objects
            decompr <- get_eigen(x, 'row')
            decompc <- get_eigen(x, 'column')

            loofun <- .getloo_heterogenous(exclusion = exclusion,
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

            newx <- update(x, best_lambda)

            new("tskrrTune",
                model = newx,
                lambda_grid = lambda,
                best_lambda = best_lambda,
                best_loss = best_loss,
                loss_values = lval,
                loss_function = fun,
                exclusion = exclusion,
                replaceby0 = replaceby0
            )
          })

#' @rdname tune
#' @export
setMethod("tune",
          "tskrrTune",
          function(x, ...){
            tune(x@model, ...)
          })

# Helper function find_best_lambda

find_min_pos <- function(x){

  id <- which.min(x)
  nr <- nrow(x)
  rowid <- id %% nr
  colid <- id %/% nr + 1
  if(rowid == 0) rowid <- nr
  return(c(rowid,colid))
}
