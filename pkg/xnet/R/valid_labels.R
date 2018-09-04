#' Test the correctness of the labels.
#'
#' This function checks whether the labels between the Y, K and G
#' matrices make sense. This means that all the labels found as
#' rownames for \code{y} can be found in \code{k}, and all the
#' labels for \code{g} can be found in \code{g}. This is a non-
#' exported convenience function.
#'
#' Compatible labels means that it is unequivocally clear which
#' rows and columns can be linked throughout the model. In case none
#' of the matrices have row- or colnames, the labels are considered
#' compatible. In all other cases, all matrices should have both row
#' and column names. They should fulfill the following conditions:
#'
#' \itemize{
#'   \item the row and column names of a kernel matrix must contain
#'   the same values.
#'   \item the rownames of \code{y} should correspond to the rownames
#'   of \code{k}
#'   \item the colnames of \code{y} should correspond to the colnames
#'   of \code{g} if it is supplied, or the colnames of \code{k} in
#'   case \code{g} is \code{NULL}
#' }
#'
#' @param y the adjacency matrix
#' @param k the kernel matrix for the rows
#' @param g the kernel matrix for the columns (optional). If not available,
#' it takes the value \code{NULL}
#'
#' @return \code{TRUE} if all labels are compatible, an error otherwise.
#'
#' @rdname valid_labels
valid_labels <- function(y, k, g = NULL){

  if(!valid_dimensions(y, k, g))
    stop("Dimensions are incompatible.")

  rny <- rownames(y)
  cny <- colnames(y)
  rnk <- rownames(k)
  cnk <- colnames(k)

  checkg <- !is.null(g)

  # Check for NULL
  rynull <- is.null(rny)
  rknull <- is.null(rnk)
  cynull <- is.null(cny)
  cknull <- is.null(cnk)

  if(checkg){
    rng <- rownames(g)
    cng <- colnames(g)
    rgnull <- is.null(rng)
    cgnull <- is.null(cng)

    if(all(rynull,rknull,rgnull,cynull,cknull,cgnull))
      return(TRUE)
    else if(any(rynull,rknull,rgnull,cynull,cknull,cgnull))
      stop("Not all row labels and col labels could be found.")
  } else {
    if(all(rynull,rknull,cynull,cknull))
      return(TRUE)
    else if(any(rynull,rknull,cynull,cknull))
      stop("Not all row labels and col labels could be found.")
  }

  out <- all(match(rny,rnk,0L) > 0L)

  if(!out)
    stop("rownames of y and k are not matching.")

  cnk <- colnames(k)
  out <- all(match(rnk, cnk, 0L) > 0L)

  if(!out)
    return("Different row- and colnames found for k.")

  if(checkg){
    # When there is g, check against g
    cng <- colnames(g)
    rng <- rownames(g)
    out <- all(match(cny,cng,0L) > 0L)

    if(!out)
      stop("colnames of y and g are not matching.")

    out <- all(match(rng,cng, 0L) > 0L)

    if(!out)
      stop("Different row- and colnames found for g.")

  } else {
    # No g, so check against k again
    out <- all(match(cny, cnk,0L) > 0L)

    if(!out)
      stop("colnames of y and k are not matching.")
  }
  return(out)
}