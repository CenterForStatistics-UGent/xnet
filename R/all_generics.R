# All Generics

## For tskrr
setGeneric("response",
           function(x, ...) standardGeneric("response"))
setGeneric("lambda",
           function(x, ...) standardGeneric("lambda"))
setGeneric("tune",
           function(x, ...) standardGeneric("tune"))

# For the hat matrices
#' @rdname hat
setGeneric("hat",
           function(x, ...) standardGeneric("hat"))
setMethod("hat",
          "ANY",
          stats::hat)

# For the labels
setGeneric("labels")
setGeneric("rownames")
setGeneric("colnames")
setGeneric("dimnames")

# For the dimensions
setGeneric("dim")

# For the linearFilter
setGeneric("mean")