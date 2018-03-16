#' Retrieve a loo function
#'
#' This function returns the correct function needed to perform
#' one of the leave one out crossvalidations. It primarily meant
#' for internal use but can be handy when doing simulations.
#'
#' This function can be used to select the correct loo function in
#' a simulation or tuning algorithm.
#'
#' @inheritParams loo
#' @param homogenous a logical value indicating whether
#' this should be for a homogenous model or not.
#' @param symmetry a character value with possible values
#' \code{"symmetric"}, \code{"skewed"} or \code{"not"}. If
#' \code{homogenous = FALSE} this argument is ignored. See also
#' \code{\link{symmetry}} and the
#' \code{\link[xnet:tskrrHomogenous-class]{tskrrHomogenous class}}
#'
#' @return a function taking the arguments y, Hr, and possibly pred
#' for calculating the leave one out crossvalidation. When
#' \code{homogenous = FALSE}, the returned function also
#' has an argument Hc.
#'
#' @seealso \code{\link{loo}} for carrying out a leave on out crossvalidation,
#' and \code{\link{loo_internal}} for more information on the internal
#' functions one retrieves with this one.
#'
#' @rdname get_loo_fun
#' @name get_loo_fun
#' @export
get_loo_fun <- function(exclusion,
                        homogenous,
                        symmetry,
                        replaceby0){
  if(homogenous){
    if(exclusion == "interaction"){
      if(symmetry == "symmetric"){
        if(replaceby0) loo.e0.sym else loo.e.sym
      } else if(symmetry == "skewed"){
        if(replaceby0) loo.e0.skew else loo.e.skew
      } else {
        stop("No loo optimization for homogenous networks that aren't symmetric or skewed.")
      }
    } else if(exclusion %in% c('row','column','both')) { # exclusion is not interaction
      loo.v
    } else {
      stop("Exclusion should be one of interaction, row,column or both")
    }
  } else { # Heterogenous network
    if(exclusion == "interaction"){
      if(replaceby0) loo.i0 else loo.i
    } else if(exclusion == "row"){
      loo.r
    } else if(exclusion == "column"){
      loo.c
    } else if(exclusion == "both"){
      loo.b
    } else {
      stop("Exclusion should be one of interaction, row, column or both.")
    }
  }
}