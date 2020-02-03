#' Getters for tskrrTune objects
#'
#' The functions described here are convenience functions to get
#' information out of a \code{\link[xnet:tskrrTune-class]{tskrrTune}}
#' object.
#'
#' @param x a \code{\link[xnet:tskrrTune-class]{tskrrTune}} object or an
#' object inheriting from \code{tskrrTune}.
#'
#' @return For \code{is_tuned}: a logical value indicating whether the
#' model is tuned.
#'
#' @examples
#'
#' data(drugtarget)
#'
#' mod <- tskrr(drugTargetInteraction, targetSim, drugSim)
#' tuned <- tune(mod, ngrid = 10)
#'
#' is_tuned(mod)
#' is_tuned(tuned)
#'
#' # Basic visualization of the grid.
#'
#' gridvals <- get_grid(tuned)
#' z <- get_loss_values(tuned)
#'
#' \dontrun{
#' image(gridvals$k,gridvals$g,log(z), log = 'xy',
#' xlab = "lambda k", ylab = "lambda g")
#' }

#'
#' @include all_generics.R
#' @rdname getters-tskrrTune
#' @aliases is_tuned
#' @export
is_tuned <- function(x){
  if(!inherits(x, "tskrr")) stop("x should be a tskrr model.")
  inherits(x, "tskrrTune")
}

#' @return For \code{get_grid} a list with the elements \code{k} and
#' possibly \code{g}, each containing the different lambdas tried in
#' the tuning for the row and column kernel matrices respectively.
#' @rdname getters-tskrrTune
#' @aliases get_grid
#' @export
get_grid <- function(x){
  if(!inherits(x, "tskrrTune")) stop("x should be a tuned model.")
  x@lambda_grid
}

#' @return For \code{get_loss_values} a matrix with the calculated
#' loss values. Note that each row represents the result for one
#' lambda value related to the row kernel matrix K. For heterogeneous
#' models, every column represents the result for one lambda related
#' to the column kernel matrix G.
#' @rdname getters-tskrrTune
#' @aliases get_loss_values
#' @export
get_loss_values <- function(x){
  if(!inherits(x, "tskrrTune")) stop("x should be a tuned model.")
  x@loss_values
}

#' @return for \code{is_onedim} a single logical value telling whether the
#' grid search in the object was onedimensional.
#' @rdname getters-tskrrTune
#' @aliases has_onedim
has_onedim <- function(x){
  if(!inherits(x, "tskrrTune")) stop("x should be a tuned model.")
  x@onedim
}
