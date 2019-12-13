#' Class tskrrTuneHomogeneous
#'
#' The class tskrrTuneHomogeneous represents a tuned homogeneous
#' \code{\link[xnet:tskrr-class]{tskrr}} model. It inherits from
#' the classes \code{\link[xnet:tskrrHomogeneous-class]{tskrrHomogeneous}}
#' and \code{\link[xnet:tskrrTune-class]{tskrrTune}}.
#'
#' @rdname tskrrTuneHomogeneous-class
#' @name tskrrTuneHomogeneous-class
#' @aliases tskrrTuneHomogeneous
#' @exportClass tskrrTuneHomogeneous
setClass("tskrrTuneHomogeneous",
         contains = c("tskrrTune", "tskrrHomogeneous"))

validTskrrTuneHomogeneous <- function(object){

  lossval <- object@loss_values
  lgrid <- object@lambda_grid
  excl <- object@exclusion

  if(names(lgrid) != "k")
    return("lambda_grid should be a list with one element named k for homogeneous networks.")

  if(ncol(lossval) != 1 ||
     nrow(lossval) != length(lgrid$k))
    return(paste("Loss values should have 1 row and",length(lgrid$k),"columns to match the lambda grid."))

  exclmatch <- match(excl, c("edges","vertices"),
                     nomatch = 0L)
  if(exclmatch == 0)
    return("exclusion should be either 'interaction' or 'both' for homogeneous networks.")

  else if(!object@onedim)
    return("grid search can only be done in one dimension for a homogeneous network.")

  if(object@replaceby0 && excl != "edges")
    return("replaceby0 can only be used with edges exclusion")
  else
    return(TRUE)


}

setValidity("tskrrTuneHomogeneous", validTskrrTuneHomogeneous)
