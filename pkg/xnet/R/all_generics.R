# All Generics

## For tskrr
setGeneric("response",
           function(x, ...) standardGeneric("response"))
setGeneric("lambda",
           function(x, ...) standardGeneric("lambda"))

setGeneric("fitted")

# For the hat matrices
setGeneric("hat",
           function(x, ...) standardGeneric("hat"))
setMethod("hat",
          "ANY",
          stats::hat)