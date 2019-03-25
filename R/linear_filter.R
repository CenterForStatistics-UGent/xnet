#' Fit a linear filter over an adjacency matrix
#'
#' This function fits a linear filter over an adjacency matrix. It calculates
#' the row, column and total means, and uses those to construct the linear
#' filter.
#'
#' If there are missing values and they are removed before calculating the
#' means, a warning is issued. If \code{na.rm = FALSE} and there are
#' missing values present, the outcome is by definition a matrix filled
#' with NA values.
#'
#'
#'
#' @param y an adjacency matrix
#' @param alpha a vector with 4 alpha values, or a single alpha value
#' which then is used for all 4 alphas.
#' @param na.rm a logical value indicating whether missing values should
#' be removed before calculating the row-, column- and total means.
#'
#' @return an object of class \code{\link[=linearFilter-class]{linearFilter}}
#'
#' @examples
#' data(drugtarget)
#' linear_filter(drugTargetInteraction, alpha = 0.25)
#' linear_filter(drugTargetInteraction, alpha = c(0.1,0.1,0.4,0.4))
#'
#' @export
linear_filter <- function(y, alpha=0.25, na.rm = FALSE){

  stopifnot(is.matrix(y),
            is.numeric(alpha),
            is.atomic(alpha))

  if(length(alpha) == 1)
    alpha <- rep(alpha,4)
  else if(length(alpha) !=4)
    stop("alpha should be a numeric vector with either 1 or 4 values.")
  if(sum(alpha) != 1 )
    stop("alpha values should add up to 1.")

  cm <- colMeans(y, na.rm = na.rm)
  rm <- rowMeans(y, na.rm = na.rm)
  m <- mean(y, na.rm = na.rm)
  nc <- ncol(y)
  nr <- nrow(y)

  if(any(is.na(y))){
    if(na.rm){
      warning("NAs removed before fitting the linear filter.")
    } else {
      # Return the empty matrix for now.
      res <- new("linearFilter",
                 y = y,
                 alpha = alpha,
                 pred = matrix(NA_real_,
                               nrow = nrow(y),ncol = ncol(y)),
                 mean = NA_real_,
                 colmeans = cm,
                 rowmeans = rm,
                 na.rm = na.rm)
    }

  }

  pred <- .linear_filter(y,alpha,cm,rm,m,nr,nc)


  # simple matrix filter
  new("linearFilter",
      y = y,
      alpha = alpha,
      pred = pred,
      mean = m,
      colmeans = cm,
      rowmeans = rm,
      na.rm = na.rm)
}

# Function .linear_filter allows for optimization algorithms.
# Input: cm is column mean, rm is row mean, m is global mean, nc is
# number of columns
.linear_filter <- function(y, alpha, cm, rm, m, nr, nc){
  alpha[1]*y + rep(alpha[2]*cm, each = nr) +
    rep(alpha[3]*rm, times = nc) + alpha[4] * m
}