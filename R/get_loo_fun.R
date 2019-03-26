#' Retrieve a loo function
#'
#' This function returns the correct function needed to perform
#' one of the leave one out crossvalidations. It primarily meant
#' for internal use but can be handy when doing simulations.
#'
#' This function can be used to select the correct loo function in
#' a simulation or tuning algorithm, based on the model object you
#' created. Depending on its class, the returned functions will have
#' different arguments, so you should only use this if you know
#' what you're doing and after you checked the actual possibly returned
#' functions in \code{\link{loo_internal}}.
#'
#' Using \code{replaceby0} only makes sense if you only remove the interaction.
#' In all other cases, this argument is ignored.
#'
#' For the class \code{tskrrHomogenous}, it doesn't make sense to only
#' remove rows or columns. If you try it anyway, you'll be met with an error.
#' For the class \code{linearFilter} it only makes sense to exclude the
#' interaction (i.e. a single cell). Therefor you do not have an argument
#' \code{exclusion} for that method.
#'
#' For the classes \code{tskrrTune} and \code{tskrrImpute},
#' not specifying \code{exclusion} or \code{replaceby0} returns the used
#' loo function. If you specify either of them,
#' it will use the method for the appropriate model and return
#' a new loo function.
#'
#' @inheritParams loo
#' @param x a character value with the class or a \code{\link{tskrr}}
#' or \code{\link{linearFilter}} object.
#' @param ... arguments passed to or from other methods.
#'
#' @return a function taking the arguments y, and possibly pred
#' for calculating the leave one out crossvalidation. For class
#' \code{tskrrHeterogenous}, the returned function also
#' has an argument Hk and Hg, representing the hat matrix for the rows
#' and the columns respectively. For class \code{tskrrHomogenous},
#' only the extra argument Hk is available. For class \code{linearFilter},
#' the extra argument is called \code{alpha} and takes the alpha vector
#' of that model.
#'
#' @seealso \code{\link{loo}} for carrying out a leave on out crossvalidation,
#' and \code{\link{loo_internal}} for more information on the internal
#' functions one retrieves with this one.
#'
#' @rdname get_loo_fun
#' @export
setMethod("get_loo_fun",
          "tskrrHeterogenous",
          function(x,
                   exclusion = c("interaction","row","column","both"),
                   replaceby0 = FALSE
                   ){

            exclusion <- match.arg(exclusion)
            .getloo_heterogenous(exclusion, replaceby0)
          })

#' @rdname get_loo_fun
#' @export
setMethod("get_loo_fun",
          "tskrrHomogenous",
          function(x,
                   exclusion = c("interaction","both"),
                   replaceby0 = FALSE
                   ){
            exclusion <- match.arg(exclusion)
            symmetry <- symmetry(x)
            .getloo_homogenous(exclusion,replaceby0, symmetry)

          })

#' @rdname get_loo_fun
#' @export
setMethod("get_loo_fun",
          "linearFilter",
          function(x,
                   replaceby0 = FALSE){
            .getloo_linearfilter(replaceby0)
          })

#' @rdname get_loo_fun
#' @export
setMethod("get_loo_fun",
          "character",
          function(x = c("tskrrHeterogenous","tskrrHomogenous","linearFilter"),
                   ...){
            x <- match.arg(x)

            fun <- switch(x,
                          tskrrHeterogenous = .getloo_heterogenous,
                          tskrrHomogenous = .getloo_homogenous,
                          linearFilter = .getloo_linearfilter)
            fun(...)
          })

#' @rdname get_loo_fun
#' @export
setMethod("get_loo_fun",
          "tskrrTune",
          function(x, ... ){
            dots <- list(...)
            class <- if(is_homogenous(x)) "tskrrHomogenous" else "tskrrHeterogenous"
            if(length(dots))
              do.call(get_loo_fun,
                      c(as(x, class), dots))
            else
              get_loo_fun(as(x, class),
                          exclusion = x@exclusion,
                          replaceby0 = x@replaceby0)
          })
