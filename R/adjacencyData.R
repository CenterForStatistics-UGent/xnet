#' Constructors for adjacencyData objects
#'
#' These functions can be used to manually construct objects from the \code{\link[=dataClasses]{gramData classes}} for use in \code{\link{tskrr}} objects.
#'
#' @include all_generics.R
#'
#' @rdname adjacencyData
#' @export
setMethod("adjacencyData",
          "matrix",
          function(x){
            NULL
          })
