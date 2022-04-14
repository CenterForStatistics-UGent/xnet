#' Carry out conversions
#'
#' Convert an object using the conversion stored in one of
#' the \code{\link[=dataClasses]{data classes}}. With this function you can
#' calculate the gram matrix after applying the same kernel as used by the
#' \code{\link{gramData}} object passed to the \code{using} argument.
#' This function is useful for predictions, as the returned objects are used
#' by the function \code{\link[=predict.tskrr]{predict}}
#' to calculate the predictions.
#'
#' @param x the object to be converted
#' @param using the \code{\link{gramData}} object to be used.
#' @param args a named list with extra arguments to be passed to the conversion function.
#' @param ... extra parameters passed to other methods.
#'
#' @return an object inheriting from \code{matrix} with the gram matrix where the rows present the data in \code{x} and the columns the data in the original object.
#'
#' @rdname convert
#' @name convert
NULL

# Function can be used for every defined class.
# S4 dispatch makes it unnecessary to check arg types.
.convert <- function(x, using, args = NULL){
  conv <- using@conversion
  conv.args <- using@conv.args

  nr <- seq_len(nrow(x))

  # Check whether args are double
  if(!is.null(args)){
    nargs <- names(args)
    if(any(nargs %in% names(conv.args))){
      change <- intersect(names(args),
                          names(conv.args))
      conv.args[change] <- NULL
    }
  }

  arglist <- c(list(rbind(using@orig,x)),
               conv.args,
               args)

  do.call(conv, arglist)[nr, -nr]
}

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
