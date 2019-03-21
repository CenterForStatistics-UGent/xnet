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

  if(any(names(lgrid) != c("k","g")))
    return("lambda grid should be a list with two elements named k and g (in that order) for heterogenous networks")

  if(nrow(lossval) != length(lgrid$k) ||
     ncol(lossval) != length(lgrid$g))
    return(paste("Loss values should have",length(lgrid$k),"rows and",length(lgrid$g),"columns to match the lambda grid."))

  exclmatch <- match(excl, c("interaction","row","column","both"),
                     nomatch = 0L)
  if(exclmatch == 0)
    return("exclusion should be one of 'interaction', 'row', 'column' or 'both'")

}

setValidity("tskrrTuneHeterogenous", validTskrrTuneHeterogenous)
