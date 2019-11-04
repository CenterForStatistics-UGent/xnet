#' Class permtest
#'
#' This class represents the permutation test outcomes. See also
#' the function \code{\link{permtest}}.
#'
#' @slot orig_loss a numeric value with the original loss of
#' the model.
#' @slot perm_losses a numeric vector with the losses of the
#' different permutations.
#' @slot n the number of permutations
#' @slot loss_function the function used to calculate the losses.
#' @slot exclusion a character value indicating the exclusion
#' setting used for the test
#' @slot replaceby0 a locigal value that indicates whether the
#' exclusion was done by replacing with zero. See also
#' \code{\link{loo}}.
#' @slot permutation a character value that indicats in which
#' kernel matrices were permuted.
#' @slot pval a p value indicating how likely it is to find a
#' smaller loss than the one of the model based on a normal
#' approximation.
#'
#' @seealso
#'  * the function \code{\link{permtest}} for the actual test.
#'  * the function \code{\link{loo}} for the leave one out
#'  procedures
#'  * the function \code{\link{t.test}} for the actual test
#' @md
#'
#' @include all_generics.R
#'
#' @rdname permtest-class
#' @name permtest-class
#' @exportClass permtest
setClass("permtest",
         slots = c(orig_loss = "numeric",
                   perm_losses = "numeric",
                   n = "numeric",
                   loss_function = "function",
                   exclusion = "character",
                   replaceby0 = "logical",
                   permutation = "character",
                   pval = "numeric"))

# Validity testing
validPermtest <- function(object){
  if(length(object@orig_loss) != 1)
    return("orig_loss should be a single value.")
  if(length(object@pval) != 1)
    return("pval should be a single value.")
  if(length(object@perm_losses) != object@n)
    return("perm_losses doesn't have a length of n.")

}

setValidity("permtest", validPermtest)

# Show method
print.permtest <- function(x){
  cat("\n")
  cat(strwrap("Permutation test for a tskrr model", prefix = "\t"))
  cat("\n")
}

setMethod("show",
          "permtest",
          function(object){
            print.permtest(object)
          })