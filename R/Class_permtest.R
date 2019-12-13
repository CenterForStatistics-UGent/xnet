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
#' @slot exact a logical value indicating whether the P value was
#' calculated exactly or approximated by the normal distribution.
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
                   pval = "numeric",
                   exact = "logical"))

# Validity testing
validPermtest <- function(object){
  if(length(object@orig_loss) != 1)
    return("orig_loss should be a single value.")
  if(length(object@pval) != 1)
    return("pval should be a single value.")
  if(length(object@perm_losses) != object@n)
    return("perm_losses doesn't have a length of n.")
  if(length(object@exact)!= 1)
    return("exact should be a single value.")

}

setValidity("permtest", validPermtest)

# internal

.make_res_table <- function(perm_losses, orig_loss, pval){
  avg <- mean(perm_losses)
  sd <- sd(perm_losses)
  # results
  res <- matrix(
    c(orig_loss, avg, sd, pval),
    nrow = 1,
    dimnames = list(
      " ",
      c("Loss", "Avg. loss", "St.dev", "Pr(X < loss)")
    )
  )
}

# Show method
#' @param digits the number of digits shown in the output
#' @rdname permtest
#' @export
print.permtest <- function(x,
                           digits = max(3L, getOption("digits") - 3),
                           ...){

  if(identical(x@loss_function, loss_mse))
    loss_name <- "Mean Squared Error (loss_mse)"
  else if(identical(x@loss_function, loss_auc))
    loss_name <- "Area under curve (loss_auc)"
  else
    loss_name <- "custom function by user"

  excl <- x@exclusion
  if(x@replaceby0) excl <- paste(excl,"(values replaced by 0)")

  loss_name <- paste("  Loss function:",loss_name,"\n")
  excl <- paste("  Exclusion:", excl, "\n")
  perm <- paste0("  Permutations: ", x@n," (direction: ",x@permutation,")\n")

  res <- .make_res_table(x@perm_losses,
                         x@orig_loss,
                         x@pval)

  cat("\n")
  cat(strwrap("Permutation test for a tskrr model", prefix = "\t"))
  cat("\n")
  cat("Using:\n")
  cat(loss_name)
  cat(excl)
  cat(perm)
  cat("\n")
  printCoefmat(res, digits = digits)
  cat("\n")
  if(!x@exact)
    cat("P value is approximated based on a normal distribution.\n\n")
  invisible(res)

}

setMethod("show",
          "permtest",
          function(object){
            print.permtest(object)
          })
