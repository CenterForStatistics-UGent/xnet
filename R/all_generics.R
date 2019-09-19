# All Generics

#' @rdname get_loo_fun
#' @export
setGeneric("get_loo_fun",
           function(x, ...) standardGeneric("get_loo_fun"))

#' @rdname loo
#' @export
setGeneric("loo",
           function(x, ...) standardGeneric("loo"))

## For tskrr
setGeneric("response",
           function(x, ...) standardGeneric("response"))
setGeneric("lambda",
           function(x, ...) standardGeneric("lambda"))
setGeneric("tune",
           function(x, ...) standardGeneric("tune"))
setGeneric("permtest",
           function(x, ...) standardGeneric("permtest"))

#' @rdname update
#' @export
setGeneric("update")

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
setGeneric("colMeans")
setGeneric("rowMeans")

#' @rdname getters_linearFilter
#' @export
setGeneric("alpha", function(x) standardGeneric("alpha"))

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
