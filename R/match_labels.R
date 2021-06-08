#' Reorder the label matrix
#'
#' Reorders the label matrix based on the labels of the kernel matrices.
#' In case there are no labels, the original label matrix is returned,
#' but with the labels in \code{rows} and \code{cols} as rownames and
#' column names respectively.
#'
#' @param y a matrix representing the label matrix.
#' @param rows a character vector with the labels for the rows or a matrix
#' with rownames that will be used as labels.
#' @param cols a character vector with the labels for the cols or a matrix
#' with colnames that will be used as labels. If \code{NULL}, \code{rows} will be
#' used for both row and column labels.
#'
#' @return a matrix with the rows and columns reordered.
#'
#' @examples
#' mat <- matrix(1:6, ncol = 2,
#'               dimnames = list(c("b", "a", "d"),
#'                               c("ca", "cb"))
#'              )
#'
#' match_labels(mat, c("a","b", "d"), c("ca","cb"))
#'
#' #Using matrices
#' data(drugtarget)
#' out <- match_labels(drugTargetInteraction, targetSim, drugSim)
#'
#' @rdname match_labels
#' @name match_labels
#' @export
match_labels <- function(y,rows,cols = NULL){

  if(!is.matrix(y))
    stop("y has to be a matrix.")

  if(is.matrix(rows)){
    rows <- rownames(rows)
    if(is.null(rows))
      stop("There are no rownames for rows.")
  } else if(!is.character(rows)) {
    stop("rows should be a matrix with rownames or a character vector.")
  }

  if(is.null(cols)){
    cols <- rows
  } else if(is.matrix(cols)){
    cols <- colnames(cols)
    if(is.null(cols))
      stop("There are no colnames for cols.")
  } else if(!is.character(cols)){
    stop("cols should be a matrix with colnames or a character vector.")
  }

  nr <- length(rows)
  nc <- length(cols)

  if(nrow(y) != nr)
    stop("row labels not of the correct length.")
  if(ncol(y) != nc)
    stop("col labels not of the correct length.")

  if(is.null(dn <- dimnames(y))){
    dimnames(y) <- list(rows,cols)
    return(y)
  }

  rmatch <- match(rows, dn[[1]], 0L)
  if(any(rmatch == 0L ))
    stop("row labels not compatible with rownames y")

  cmatch <- match(cols, dn[[2]], 0L)
  if(any(cmatch == 0L))
    stop("col labels not compatible with colnames y")

  return(y[rmatch,cmatch])
}
