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
#'
#' @rdname tune
#' @name tune
NULL

#' @rdname tune
#' @export
setMethod("tune",
          "tskrrHomogenous",
          function(x,
                   lim = c(1e-4,1e4),
                   ngrid = 10,
                   lambda = NULL){

            if(is.null(lambda))
              lambda <- create_grid(lim, base, ngrid)

            loofun <- get_loo_fun(exclusion = 'interaction',
                                  homogenous = TRUE,
                                  symmetry = symmetry(x))

            decomp <- get_eigen(x)

            loss <- function(lambda){
              Hr <- eigen2hat(decomp$vectors,
                              decomp$values,
                              lambda)
              sqrt((loofun(x@y, Hk, x@pred) - x@y)^2)
            }

            lval <- lapply(lambda,loss)

            return(list(SSE = lval, lambda = lambda))

          })

#' @rdname tune
#' @export
setMethod("tune",
          "tskrrHeterogenous",
          function(x,
                   lim = c(1e-4,1e4),
                   ngrid = 10,
                   lambda = NULL){

            twol <- FALSE #check for two separate lambda vectors
            nolambda <- is.null(lambda)

            if(!nolambda){
              # Processing lambda
              if(is.list(lambda) && length(lambda) == 2){
                twol <- TRUE
                lambda1 <- lambda[[1]]
                lambda2 <- lambda[[2]]
                if(!all(is.numeric(lambda1), is.numeric(lambda2)))
                  stop(paste0("lambda should be either a list with two numeric",
                              " vectors or a single numeric vector."))
              } else if(!is.numeric(lambda)) {
                stop(paste0("lambda should be either a list with two numeric",
                            " vectors or a single numeric vector."))
              }
            } else {

              if(is.list(lim) && length(lim) == 2){
                twol <- TRUE
                onelim <- FALSE
                lim1 <- lim[[1]]
                lim2 <- lim[[2]]
                # Check lim values
                if(length(lim1) != 2 || length(lim2) != 2 ||
                   !is.numeric(lim1) || !is.numeric(lim2))
                  stop("If lim is a list, both elements should contain 2 numeric values")
              } else if(is.numeric(lim) && length(lim) == 2) {
                onelim <- TRUE
              } else {
                stop(paste0("lim should be either a list with two elements ",
                            "or a numeric vector with 2 values."))
              }

              if(is.list(ngrid) && length(ngrid) == 2){
                twol <- TRUE
                onegrid <- FALSE
                ngrid1 <- ngrid[[1]]
                ngrid2 <- ngrid[[2]]
              } else if(is.numeric(ngrid) && length(ngrid) == 1){
                onegrid <- TRUE
              } else {
                stop(paste0("ngrid should be either a list with two elements ",
                            "or a numeric vector with 1 value."))
              }


            }

          })