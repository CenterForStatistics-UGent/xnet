#' Getters for the classes adjacencyData and gramData
#'
#' These functions allow you to get slots from the classes \code{\link[xnet:dataClasses]{adjacencyData}} and \code{\link[xnet:dataclasses]{gramData}}.
#'
#' @param x an object of any of those classes or classes that inherit from those classes.
#' @param ... arguments passed to other methods
#'
#' @rdname getters-data
#' @name getters-data
#' @aliases has_orig,adjacencyData-method
#' @export
setMethod("has_orig",
          "adjacencyData",
          function(x){
            x@hasorig
          })

#' @rdname getters-data
#' @export
setMethod("has_orig",
          "gramData",
          function(x){
            x@hasorig
          })



#' @rdname getters-data
#' @export
setMethod("get_orig",
          "adjacencyData",
          function(x){
            if(has_orig(x)){
              x@orig
            } else {
              warning("No original data in this object.")
              NULL
            }
          })

#' @rdname getters-data
#' @export
setMethod("get_orig",
          "gramData",
          function(x){
            if(has_orig(x)){
              x@orig
            } else {
              warning("No original data in this object.")
              NULL
            }
          })
