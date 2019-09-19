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
#' @rdname permtest
#' @export
setMethod("permtest","tskrr",
          function(x,
                   permutation = c("both","row","column"),
                   n = 100,
                   exclusion = c("interaction","row","column","both")){
            NULL
          })

#' @rdname importance
#' @export
setMethod("permtest",
          "tskrrTune",
          function(x,
                   permutation = c("both","row","column"),
                   n = 100){
            NULL
          })

# Internal permtest function.

.permtest <- function(Y,K,G,n,lossmod,lossfun, loofun,
                      permutation,exclusion,lambda){
  NULL
}