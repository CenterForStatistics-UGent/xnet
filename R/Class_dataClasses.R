#' Classes containing the original data
#'
#' The classes defined here serve as objects to store and
#' manipulate the original data used for creating a
#' \code{\link{tskrr}} model. They contain the original
#' data structure and the functions to transform the
#' original data to an adjacency matrix or a gram matrix.
#'
#' The classes \code{adjacencyData} and \code{gramData} are virtual classes, and shouldn't be used directly.
#'
#' The classes inheriting from \code{adjacencyData} can store the original data used to create an adjacency matrix. They also contain the matrix itself, and the conversion function used to construct it. The following classes inherit from \code{adjacencyData}:
#'
#' \describe{
#'   \item{adjacencyDataMatrix}{the slot \code{adjacency} contains an adjacency matrix. The \code{conversion} functions is the \code{\link[base]{identity}} function by default.}
#'   \item{adjacencyDataIgraph}{the slot \code{orig} contains an \code{\link[igraph]{igraph}} object. The \code{conversion} function is either \code{\link[igraph]{as_adjacency_matrix}} or \code{\link[igraph]{as_incidence_matrix}}, depending on whether it's a homogeneous or heterogeneous network.}
#' }
#'
#' The following classes inherit from \code{gramData}:
#'
#' \describe{
#'   \item{gramDataMatrix}{BLAHBLAHBLAH}
#' }
#'
#' @slot orig The original data. For adjacency objects, this
#' can be an \code{\link{igraph}} object. For gram objects,
#' this can be a matrix or a data frame.
#' @slot adjacency The adjacency matrix used in the model (only for classes inheriting from \code{adjacencyData})
#' @slot conversion A function used for the conversion of
#' @slot hasorig a logical value, indicating whether a \code{DataMatrix} objects contain an original matrix.
#' In case the original matrix is already an adjacency or gram matrix,
#' this is set to \code{FALSE} and the slot \code{adjacency} or \code{gram} is  used as the original instead.
#'
#' @slot eigen The eigen-decomposition of the calculated
#' gram matrix (only for classes inheriting from \code{gramData})
#' @slot gram The calculated gram matrix if \code{hasgram} is \code{TRUE} (only for classes inheriting from \code{gramData}).
#' the original data object to the adjacency or gram matrix.
#' @slot hasgram a logical value indicating whether the gram matrix
#' is contained in the object or not.  (only for classes inheriting from \code{gramData}).
#'
#' @seealso \code{\link{gramData}} for constructing \code{gramData} objects.
#'
#' @rdname dataClasses
#' @name dataClasses
#' @aliases adjacencyData-class  adjacencyDataIgraph-class adjacencyDataMatrix-class
#' @exportClass adjacencyData
setOldClass("igraph")
setOldClass("eigen")

setClass("adjacencyData",
         slots = c(adjacency = "matrix",
                   conversion = "function"))

#' @rdname dataClasses
#' @exportClass adjacencyDataIgraph
setClass("adjacencyDataIgraph",
         slots = c(orig  = "igraph"),
         contains = "adjacencyData")

#' @rdname dataClasses
#' @exportClass adjacencyDataMatrix
setClass("adjacencyDataMatrix",
         slots = c(orig = "matrix",
                   hasorig = "logical"),
         contains = "adjacencyData")

#' @rdname dataClasses
#' @aliases gramData-class
#' @exportClass gramData
setClass("gramData",
         slots = c(eigen = "eigen",
                   gram = "matrix",
                   conversion = "function",
                   hasgram = "logical"))

#' @rdname dataClasses
#' @aliases gramDataMatrix-class
#' @exportClass gramDataMatrix
setClass("gramDataMatrix",
         slots = c(orig = "matrix",
                   hasorig = "logical"),
         contains = "gramData")

#' @rdname dataClasses
#' @aliases gramDataFrame-class
#' @exportClass gramDataFrame
setClass("gramDataFrame",
         slots = c(orig = "data.frame"),
         contains = "gramData")

#----------------------------------------
# initialize methods for adjacencyData


#----------------------------------------
# initialize methods for gramData

setMethod("initialize",
          "gramDataFrame",
          function(.Object, orig, conversion, keep.gram = FALSE, ...){
            # Create gram and eigen from original
            gram <- conversion(orig)
            if(!isSymmetric(gram))
              stop("conversion didn't result in a symmetric gram matrix.")
            eigen <- eigen(gram, symmetric = TRUE)
            # Call constructor
            if(keep.gram){
              .Object <- callNextMethod(.Object, orig = orig,
                                        gram = gram,
                                        eigen = eigen,
                                        conversion = conversion,
                                        hasgram = TRUE,...)
            } else {
              .Object <- callNextMethod(.Object,
                                        orig = orig,
                                        eigen = eigen,
                                        conversion = conversion,
                                        hasgram = FALSE)
            }

            .Object@orig <- orig
            .Object
          })

setMethod("initialize",
          "gramDataMatrix",
          function(.Object, orig = NULL, gram = NULL,
                   conversion = identity, keep.orig = !is.null(orig),
                   keep.gram = !keep.orig, ...){
            if(is.null(orig) && is.null(gram)){
              stop("Either orig or gram needs to be passed")
            }
            if(is.null(gram)){
              gram <- conversion(orig)
              if(!isSymmetric(gram))
                stop("conversion didn't result in a symmetric gram matrix.")
            }
            eigen <- eigen(gram, symmetric = TRUE)

            if(keep.orig){
              if(keep.gram){
                .Object <- callNextMethod(.Object,
                                          orig = orig,
                                          gram = gram,
                                          eigen = eigen,
                                          conversion = conversion,
                                          hasorig = TRUE,
                                          hasgram = TRUE)
              } else {
                .Object <- callNextMethod(.Object,
                                          orig = orig,
                                          eigen = eigen,
                                          conversion = conversion,
                                          hasorig = TRUE,
                                          hasgram = FALSE)

              } # end keeporig & !keepgram
            } else { # ! keeporig
              if(!keep.gram){
                .Object <- callNextMethod(.Object,
                                          eigen = eigen,
                                          conversion = conversion,
                                          hasorig = FALSE,
                                          hasgram = FALSE)
              } else { # ! keeporig & keepgram
                .Object <- callNextMethod(.Object,
                                          gram = gram,
                                          eigen = eigen,
                                          conversion = conversion,
                                          hasorig = FALSE,
                                          hasgram = TRUE)

              }
            }
            return(.Object)
          })
