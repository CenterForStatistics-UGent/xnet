#' Getters for permtest objects
#'
#' The functions described here are convenience functions to get
#' information out of a \code{\link[xnet:permtest-class]{permtest}}
#' object.
#'
#' @param x a \code{\link[xnet:permtest-class]{permtest}} object
#' @param i either a numeric vector, a logical vector or a character
#' vector with the elements that need extraction.
#'
#' @return the requested values
#'
#' @seealso \code{\link{loss}} to extract the original loss value.
#'
#' @examples
#'
#' data(drugtarget)
#'
#' mod <- tskrr(drugTargetInteraction, targetSim, drugSim)
#' ptest <- permtest(mod, fun = loss_auc)
#'
#' loss(ptest)
#' ptest[c(2,3)]
#' permutations(ptest)
#'
#' @rdname getters-permtest
#' @aliases Extract-permtest permutations
#' @export
permutations <- function(x){
  if(!inherits(x, "permtest"))
    stop("x has to be of class permtest")
  x@perm_losses
}

#' @rdname getters-permtest
#' @export
setMethod("[",
          c("permtest","ANY"),
          function(x,i){

            res <- .make_res_table(x@perm_losses,
                                   x@orig_loss,
                                   x@pval)
            tryCatch(res[,i],
                     error = function(e){
                       stop("Could not find requested element(s).",
                            call. = FALSE)
                     })
          })
