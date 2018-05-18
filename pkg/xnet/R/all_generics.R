# All Generics

## For tskrr
setGeneric("response",
           function(x, ...) standardGeneric("response"))
setGeneric("lambda",
           function(x, ...) standardGeneric("lambda"))
setGeneric("tune",
           function(x, ...) standardGeneric("tune"))

## From stats
setGeneric("fitted")
setGeneric("weights")
setGeneric("fitted")
setGeneric("predict")


# For the hat matrices
setGeneric("hat",
           function(x, ...) standardGeneric("hat"))
setMethod("hat",
          "ANY",
          stats::hat)