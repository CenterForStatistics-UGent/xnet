#' Carry out conversions
#'
#' Convert an object using the conversion stored in one of
#' the \code{\link[=dataClasses]{data classes}}.
#'
#' @param x the object to be converted
#' @param using the \code{\link{gramData}} or \code{\link{adjacencyData}} object to be used.
#' @param args a named list with extra arguments to be passed to the conversion function.
#' @param ... extra parameters passed to other methods.
#'
#' @return the result of the conversion.
#'
#' @rdname convert
#' @name convert
NULL

# Function can be used for every defined class.
# S4 dispatch makes it unnecessary to check arg types.
.convert <- function(x, using, args = NULL){
  conv <- using@conversion
  conv.args <- using@conv.args

  # Check whether args are double
  if(!is.null(args)){
    nargs <- names(args)
    if(any(nargs %in% names(conv.args))){
      change <- intersect(names(args),
                          names(conv.args))
      conv.args[change] <- NULL
    }
  }

  arglist <- c(list(x),
               conv.args,
               args)

  do.call(conv, arglist)
}

#' @rdname convert
#' @aliases convert,matrix,adjacencyDataMatrix-method
#' @export
setMethod("convert",
          c("matrix","adjacencyDataMatrix"),
          .convert)

#' @rdname convert
#' @export
setMethod("convert",
          c("igraph","adjacencyDataIgraph"),
          .convert)

#' @rdname convert
#' @export
setMethod("convert",
          c("matrix","gramDataMatrix"),
          .convert)

#' @rdname convert
#' @export
setMethod("convert",
          c("data.frame","gramDataFrame"),
          .convert)
