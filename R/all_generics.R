# Classes needed for generics
setOldClass("htest")
setOldClass("igraph") # To allow for igraphs to be read as
                      # adjacency matrices

# Generics from S3 methods

setGeneric("update", useAsDefault = stats::update)
setGeneric("as.matrix")

# For the labels
setGeneric("labels")
setGeneric("rownames")
setGeneric("colnames")
setGeneric("dimnames")

# For the dimensions
setGeneric("dim")

# For the linearFilter
setGeneric("mean")
setGeneric("colMeans")
setGeneric("rowMeans")

# Generics for data classes
#' @rdname convert
#' @export
setGeneric("convert",
           function(x, using, ...) standardGeneric("convert"))

# Generics for tskrr methods
#' @rdname tskrr
#' @export
setGeneric("tskrr",
           function(y, k, g, ...) standardGeneric("tskrr"))

# Generics for data constructors
#' @rdname adjacencyData
#' @export
setGeneric("adjacencyData",
           function(x, conversion, ...) standardGeneric("adjacencyData"))

#' @rdname gramData
#' @export
setGeneric("gramData",
           function(x, conversion, ...) standardGeneric("gramData"))

# All Generics

#' @rdname get_loo_fun
#' @export
setGeneric("get_loo_fun",
           function(x, ...) standardGeneric("get_loo_fun"))

#' @rdname loo
#' @export
setGeneric("loo",
           function(x, ...) standardGeneric("loo"))

## For the data classes

#' @rdname getters-data
#' @export
setGeneric("has_orig",
           function(x, ...) standardGeneric("has_orig"))

#' @rdname getters-data
#' @export
setGeneric("get_orig",
           function(x, ...) standardGeneric("get_orig"))

## For tskrr
setGeneric("response",
           function(x, ...) standardGeneric("response"))
setGeneric("lambda",
           function(x, ...) standardGeneric("lambda"))
setGeneric("tune",
           function(x, ...) standardGeneric("tune"))

#' @rdname permtest
#' @export
setGeneric("permtest",
           function(x, ...) standardGeneric("permtest"))




# For the hat matrices
# Needed to do it this way, as the original has
# other arguments that cannot be used to dispatch on.
#' @rdname hat
setGeneric("hat",
           function(x, ...) standardGeneric("hat"))
setGenericImplicit("hat")

#' @rdname getters_linearFilter
#' @export
setGeneric("get_alpha", function(x) standardGeneric("get_alpha"))

#' @rdname getters_linearFilter
#' @export
setGeneric("na_removed", function(x) standardGeneric("na_removed"))

# For the tune

#' @rdname as_tuned
setGeneric("as_tuned", function(x, ...) standardGeneric("as_tuned"))

#' @rdname as_tuned
setGeneric("as_tskrr", function(x, ...) standardGeneric("as_tskrr"))

#' @rdname loss
#' @export
setGeneric("loss", function(x, ...) standardGeneric("loss"))

#' @rdname residuals.tskrr
#' @export
setGeneric("residuals")
