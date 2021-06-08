#' Test the correctness of the labels.
#'
#' This function checks whether the labels of the adjacency matrix and the gramData objects are compatible. This means that all the labels found as
#' rownames for \code{y} can be found as rownames \emph{and} column
#' names of \code{k}, and all the colnames for \code{y} can be found
#' as rownames \emph{and} colnames of \code{g} (if provided).
#'
#' Compatible labels mean that it is unequivocally clear which
#' rows and columns can be linked throughout the model. In case none
#' of the matrices have row- or colnames, the labels are considered
#' compatible. In all other cases, all matrices should have both row
#' and column names. They should fulfill the following conditions:
#'
#' \itemize{
#'   \item the row- and column names of a kernel matrix must contain
#'   the same values in the same order. Otherwise, the matrix can't
#'   be symmetric.
#'   \item the rownames of \code{y} should correspond to the rownames
#'   of \code{k}
#'   \item the colnames of \code{y} should correspond to the colnames
#'   of \code{g} if it is supplied, or the colnames of \code{k} in
#'   case \code{g} is \code{NULL}
#' }
#'
#' @param y an adjacency matrix.
#' @param k an object of class \code{\link{gramData}} for the rows.
#' @param g an optional object of class \code{\link{gramData}}. If not available,
#' it takes the value \code{NULL}
#'
#' @note This is a non-exported convenience function. Note that it assumes the \code{\link{gramData}} objects are formed properly and hence have the same row- and columnames.
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

  checkg <- !is.null(g)

  # Check for NULL
  rynull <- is.null(rny)
  rknull <- is.null(rnk)
  cynull <- is.null(cny)

  if(checkg){
    cng <- colnames(g)
    cgnull <- is.null(cng)

    if(all(rynull,rknull,cynull,cgnull))
      return(TRUE)
    else if(any(rynull,rknull,cynull,cgnull))
      stop(paste("Not all row labels and col labels could be found.",
                 "You need to have compatible row and column labels",
                 "for all matrices. See also ?valid_labels."))
  } else {
    if(all(rynull,rknull,cynull))
      return(TRUE)
    else if(any(rynull,rknull,cynull))
      stop(paste("Not all row labels and col labels could be found.",
                 "You need to have compatible row and column labels",
                 "for all matrices. See also ?valid_labels."))
  }

  out <- all(match(rny,rnk,0L) > 0L)

  if(!out)
    stop(paste("rownames of y and k are not matching.",
               "See also ?valid_labels."))

  if(checkg){
    # When there is g, check against g
   out <- out && all(match(cny,cng,0L) > 0L)

    if(!out)
      stop(paste("colnames of y and g are not matching.",
                 "See also ?valid_labels."))

  } else {
    # No g, so check against k again
    out <- all(match(cny, rnk,0L) > 0L)

    if(!out)
      stop(paste("colnames of y and k are not matching.",
                 "See also ?valid_labels."))
  }
  return(out)
}
