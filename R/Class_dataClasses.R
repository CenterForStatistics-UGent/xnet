#' gramData classes
#'
#' The classes defined here serve as objects to store and
#' manipulate the original data used for creating a
#' \code{\link{tskrr}} model. They can contain the original
#' data structure and the functions to transform the
#' original data to an a gram matrix.
#'
#' The class \code{gramData} contains the eigendecomposition of the gram matrix. If requested, the gram matrix itself is stored in the object as well, and the slot \code{hasgram} is set to TRUE.
#'
#' The classes inheriting from \code{gramData} can store the original data used to create a gram matrix, as well as the conversion function used for this. This is especially useful when a kernel function is applied to either a matrix or a data frame. The following classes inherit from \code{gramData}:
#'
#' \describe{
#'   \item{gramDataMatrix}{This contains an original matrix and a conversion function to convert it to a gram matrix. Standard it would be some kind of kernel function.}
#'   \item{gramDataFrame}{This contains a data frame and a conversion function to convert it to a gram matrix. The function \code{\link{gramData}} can do this conversion based on a kernel function and a formula that indicates which variables should be used to calculate the gram matrix. }
#' }
#'
#' @slot orig The original data, if provided.
#' @slot conversion A function used for the conversion of the original object to either an adjacency matrix or a gram matrix.
#' @slot conv.args a list with arguments to be used when calling the function in \code{conversion}.
#' @slot hasorig a logical value, indicating whether objects contain an original matrix.
#' @slot symmetric a logical value, indicating whether the adjacency matrix is a symmetric one (homogeneous networks) or not (heterogeneous networks).
#'
#' @slot eigen The eigen-decomposition of the calculated
#' gram matrix.
#' @slot gram The calculated gram matrix if \code{hasgram} is \code{TRUE}.
#' @slot hasgram a logical value indicating whether the gram matrix
#' is contained in the object or not.
#' @slot labels a character vector with the labels for the gram matrix.
#'
#' @seealso \code{\link{gramData}} for constructing \code{gramData} objects.
#'
#' @rdname dataClasses
#' @name dataClasses
setOldClass("eigen")

#' @rdname dataClasses
#' @aliases gramData-class
#' @exportClass gramData
setClass("gramData",
         slots = c(eigen = "eigen",
                   gram = "matrix",
                   hasgram = "logical",
                   hasorig = "logical",
                   labels = "character"))

#' @rdname dataClasses
#' @aliases gramDataMatrix-class
#' @exportClass gramDataMatrix
setClass("gramDataMatrix",
         slots = c(orig = "matrix",
                   conversion = "function",
                   conv.args = "list"),
         contains = "gramData")

#' @rdname dataClasses
#' @aliases gramDataFrame-class
#' @exportClass gramDataFrame
setClass("gramDataFrame",
         slots = c(orig = "data.frame",
                   conversion = "function",
                   conv.args = "list"),
         contains = "gramData")


#----------------------------------------
# initialize methods for gramData

setMethod("initialize",
          "gramData",
          function(.Object, gram, hasgram = FALSE, ...){
            eig <- eigen(gram)
            if(hasgram){
              .Object <- callNextMethod(.Object,
                                        gram = gram,
                                        eigen = eig,
                                        hasgram = TRUE,
                                        ...)
            } else {
              .Object <- callNextMethod(.Object,
                                        eigen = eig,
                                        hasgram = FALSE,
                                        ...)
            }
            .Object@hasorig <- "orig" %in% slotNames(.Object)
            .Object
          })

setMethod("initialize",
          "gramDataFrame",
          function(.Object, orig, conversion,
                   conv.args = list(), ...){
            # Create gram and eigen from original
            gram <- do.call(conversion,
                            c(list(orig), conv.args))
            if(!isSymmetric(gram))
              stop("conversion didn't result in a symmetric gram matrix.")
            callNextMethod(.Object,
                           gram = gram,
                           orig = orig,
                           conversion = conversion,
                           conv.args = conv.args,
                           ...)
          })

setMethod("initialize",
          "gramDataMatrix",
          function(.Object, orig,
                   conversion,
                   conv.args = list(), ...){
            gram <- do.call(conversion,
                            c(list(orig), conv.args))
            if(!isSymmetric(gram))
              stop("gram matrix should be symmetric after conversion.")
            callNextMethod(.Object,
                           gram = gram,
                           orig = orig,
                           conversion = conversion,
                           conv.args = conv.args,
                           ...)
          })
