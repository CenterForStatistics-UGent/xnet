#' Find correct loo function
#'
#' This function is a wrapper that looks for the correct internal
#' function to carry out a leave one out crossvalidation. The
#' function is not exported.
#'
#' @details The parameter \code{exclusion} defines what exactly is left out.
#' The value "interaction" means that a single interaction is removed.
#' In the case of a homogenous model this can be interpreted as the
#' removal of the interaction between two edges. The values "row" and
#' "column" mean that all interactions for a row edge resp. a column
#' edge are removed. The value "both" removes all interactions for
#' a row and a column edge.
#'
#' In the case of a homogenous model, "row" and "column" don't make sense
#' and will be replaced by "both". This can be interpreted
#' as removing vertices, i.e. all interactions between one edge and
#' all other edges. For more information, see Stock et al PAPER TO BE ADDED.
#'
#' @inheritParams loo
#' @param homogenous a logical value indicating whether or
#' not the network is homogenous
#' @param symmetry a character value with the possible values
#'
#' @return the function needed to calculate the loo predictions
#'
#' @rdname get_loo_fun
get_loo_fun <- function(exclusion,
                        homogenous,
                        symmetry,
                        replaceby0 = FALSE){

}