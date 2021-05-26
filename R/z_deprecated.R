#' Deprecated functions
#'
#' The functions mentioned here will be removed in
#' future versions of the package.
#'
#' @param x an object.
#'
#' @return See the actual functions
#'
#' @seealso
#'  for \code{alpha}: \code{\link{get_alpha}}
#'
#' @rdname deprecated
#' @name deprecated
#' @aliases alpha
#' @export
alpha <- function(x){
  warning("alpha() is deprecated. Use get_alpha() instead.")
  get_alpha(x)
}
