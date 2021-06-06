#' Constructors for adjacencyData objects
#'
#' These functions can be used to manually construct objects from the \code{\link[xnet:dataClasses]{adjacencyData classes}} for use in \code{\link{tskrr}} objects.
#'
#' For a \code{\link[igraph:igraph-package]{igraph}} object, the original data is stored as this is needed for further analysis.
#'
#' @param x an object to be converted to an \code{adjacencyData} class.
#' @param conversion an optional function used to convert the input to an adjacency matrix.
#' @param conv.args a named list with extra arguments passed to the conversion function.
#' @param symmetric a logical value indicating whether or not the resulting adjacency matrix is symmetric. If \code{NULL} this is checked by \code{\link[base]{isSymmetric}}. Specifying a value overrides this check. Do this at your own risk.
#' @param ... arguments passed to other methods.
#'
#' @include all_generics.R
#'
#' @importFrom igraph is.bipartite as_adjacency_matrix as_incidence_matrix
#'
#' @rdname adjacencyData
#' @name adjacencyData
#' @aliases adjacencyData,matrix,function-method
#' @export
setMethod("adjacencyData",
          c("matrix", "function"),
          function(x, conversion, conv.args = list(),
                   symmetric = NULL){
            new("adjacencyDataMatrix",
                orig = x,
                conversion = conversion,
                conv.args = conv.args,
                symmetric = symmetric)
          })

#' @rdname adjacencyData
#' @export
setMethod("adjacencyData",
          c("matrix","missing"),
          function(x, conversion, symmetric = NULL){
            new("adjacencyData",
                adjacency = x,
                symmetric = symmetric)
          })

#' @rdname adjacencyData
#' @export
setMethod("adjacencyData",
          c("igraph","function"),
          function(x, conversion,
                   conv.args = list(),
                   symmetric = NULL){
            new("adjacencyDataIgraph",
                orig = x,
                conversion = conversion,
                conv.args = conv.args,
                symmetric = symmetric)
          })

#' @rdname adjacencyData
#' @export
setMethod("adjacencyData",
          c("igraph","missing"),
          function(x, conversion){
              bip <- is.bipartite(x)
              convfun <- if(bip)
                as_incidence_matrix else
                  as_adjacency_matrix
              conv.args <- list(sparse = FALSE)

              new("adjacencyDataIgraph",
                  orig = x,
                  conversion = convfun,
                  conv.args = conv.args)
          })

#' @rdname adjacencyData
#' @export
setMethod("adjacencyData",
          c("ANY", "function"),
          function(x, conversion, symmetric = NULL){
            adj <- conversion(x)
            if(!is.matrix(adj))
              stop("Conversion of x didn't result in a matrix")

            new("adjacencyData",
                adjacency = adj,
                symmetric = symmetric)
          })