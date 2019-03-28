#' Class tskrrTuneHeterogenous
#'
#' The class tskrrTuneHeterogenous represents a tuned Heterogenous
#' \code{\link[xnet:tskrr-class]{tskrr}} model. It inherits from
#' the classes \code{\link[xnet:tskrrHeterogenous-class]{tskrrHeterogenous}}
#' and \code{\link[xnet:tskrrTune-class]{tskrrTune}}.
#'
#' @rdname tskrrTuneHeterogenous-class
#' @name tskrrTuneHeterogenous-class
#' @aliases tskrrTuneHeterogenous
#' @exportClass tskrrTuneHeterogenous
setClass("tskrrTuneHeterogenous",
         contains = c("tskrrTune", "tskrrHeterogenous"))

validTskrrTuneHeterogenous <- function(object){

  lossval <- object@loss_values
  lgrid <- object@lambda_grid
  excl <- object@exclusion
  onedim <- object@onedim

  if(!onedim && any(names(lgrid) != c("k","g")))
    return("lambda grid should be a list with two elements named k and g (in that order) for heterogenous networks")
  else if(onedim && any(names(lgrid) != "k") && length(lgrid) > 1)
    return("in a one-dimensional search there should only be a single element named k in the lambda grid.")

  if(nrow(lossval) != length(lgrid$k))
    return(paste("Loss values should have",length(lgrid$k),"rows to match the lambda grid."))

  if(!onedim && ncol(lossval) != length(lgrid$g))
    return(paste("Loss values should have",length(lgrid$g),"columns to match the lambda grid."))
  else if(onedim && ncol(lossval) != 1)
    return(paste("Loss values should have one column in case of one-dimensional search."))

  exclmatch <- match(excl, c("interaction","row","column","both"),
                     nomatch = 0L)
  if(exclmatch == 0)
    return("exclusion should be one of 'interaction', 'row', 'column' or 'both'")
  else
    return(TRUE)

}

setValidity("tskrrTuneHeterogenous", validTskrrTuneHeterogenous)
