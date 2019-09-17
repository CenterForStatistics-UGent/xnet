#' Calculate the relative importance of the edges
#'
#' This function does a permutation-based evaluation of the impact of
#' different edges on the final result.
#'
#' @param x either a \code{\link{tskrr-class}} or a
#' \code{\link{tskrrTune-class}} object
#'
#' @return a matrix with the differences in loss
#'
#' @rdname importance
#' @export
setMethod("importance","tskrr",
          function(x,
                   permutation = c("both","row","column"),
                   n = 100,
                   exclusion = c("interaction","row","column","both")){
            NULL
          })

#' @rdname importance
#' @export
#' setMethod("importance","tskrr",
setMethod("importance",
          "tskrrTune",
          function(x,
                   permutation = c("both","row","column"),
                   n = 100){
            NULL
          })
