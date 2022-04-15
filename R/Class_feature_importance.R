#' Class feature_importance
#'
#' This class represents the feature importance outcomes. See also the function \code{\link{feature_importance}}.
#'
#' @slot orig_loss a numeric value with the original loss of the model.
#' @slot n the number of bootstrap samples
#' @slot loss_function the function used to calculate the losses.
#' @slot exclusion a character value indicating the exclusion setting used for the test.
#' @slot replaceby0 a logical value that indicates whether the excluseion was done by replacing with zero. See also \code{\link{loo}}
#' @slot features the labels for the features that were checked.
#' @slot simloss a matrix with the simulated losses for the bootstrapped samples
#'
#' @seealso
#'  * the function \code{\link{feature_importance}} for the actual test
#'  * the function \code{\link{loo}} for the leave one out procedures
#' @md
#'
#' @include all_generics.R
#'
#' @importFrom stats fivenum
#'
#' @rdname feature_importance-class
#' @name feature_importance-class
#' @exportClass feature_importance
setClass("feature_importance",
         slots = c(orig_loss = "numeric",
                   simloss = "matrix",
                   n = "numeric",
                   loss_function = "function",
                   exclusion = "character",
                   replaceby0 = "logical",
                   features = "character"))

validFeatureImportance <- function(object){
  if(length(object@orig_loss) != 1)
    return("orig_loss should be a single value.")
  if(length(object@n) != 1)
    return("n should be a single value")
  if(ncol(object@simloss) != length(object@features))
    return("Number of columns in simloss doesn't match the length of features.")
  if(nrow(object@simloss) != object@n)
    return("Number of rows in simloss doesn't match n.")

  return(TRUE)
}

setValidity("feature_importance",
            validFeatureImportance)

# Show method
#' @param digits the number of digits shown in the output
#' @rdname feature_importance
#' @export
print.feature_importance <- function(x,
                                     digits = 3, ...){
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
  perm <- paste0("  Bootstrap samples: ", x@n)

  diffs <- (x@simloss - x@orig_loss) / x@orig_loss

  summ <- t(apply(diffs, 2, fivenum))
  summ <- round(summ, digits = digits)
  rownames(summ) <- x@features
  colnames(summ) <- c("Min", "1st Qu","Median", "3rd Qu", "Max")
  summ <- summ[order(summ[,3], decreasing = TRUE),]

  cat("\n")
  cat(strwrap("Feature importance for a tskrr model", prefix = "\t"))
  cat("\n")
  cat("Using:\n")
  cat(loss_name)
  cat(excl)
  cat(perm)
  cat("\n\n")
  cat("Relative increase of the original loss value of", signif(x@orig_loss, digits = digits),":\n" )
  print(summ)
  invisible(summ)

}

setMethod("show",
          "feature_importance",
          function(object){
            print.feature_importance(object)
          })