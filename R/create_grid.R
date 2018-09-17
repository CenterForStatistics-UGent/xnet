#' Create a grid of values for tuning tskrr
#'
#' This function creates an grid of values for
#' tuning a \code{\link{tskrr}} model. The grid is equally spaced on
#' a logarithmic
#'
#' The \code{lim} argument sets the boundaries of the domain in which
#' the lambdas are sought. The function creates a grid based onbases
#' of the value of \code{base}. So if \code{base = 2} then the
#' lambda values at which the function is evaluated, is calculated as:
#'
#' \code{exp(seq(log(1e-4), log(1e4), length.out = ngrid))}

#'
#' @param lim a numeric vector with 2 values giving the lower and upper limit
#' for the grid.
#' @param ngrid the number of values that have to be produced. If this
#' number is not integer, it is truncated. The value should be 2 or
#' larger.
#'
#' @return a numeric vector with values evenly spaced on a
#' logarithmic scale.
#'
#' @export
create_grid <- function(lim = c(1e-4, 1e4),
                        ngrid = 10){

  if(!length(lim) == 2 || !is.numeric(lim))
    stop("The argument lim needs 2 numeric values.")
  if(!length(ngrid) == 1 || !is.numeric(ngrid))
    stop("The argument ngrid should be a single numeric value.")

  #Optimized for speed
  llim <- log(lim)
  by <- (llim[2] - llim[1])/(ngrid - 1L)
  exp(c(llim[1], llim[1] + seq_len(ngrid - 2L) * by, llim[2]))
}