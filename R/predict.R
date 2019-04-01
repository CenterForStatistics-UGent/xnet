#' predict method for tskrr fits
#'
#' Obtains predictions from a \code{\link{tskrr}} model for new
#' data. To get the predictions on the training data, use the
#' function \code{\link[xnet:fitted]{fitted}} or set both \code{k}
#' to \code{NULL}.
#'
#' Predictions can be calculated between new vertices and the vertices
#' used to train the model, between new sets of vertices, or both. Which
#' predictions are given, depends on the kernel matrices passed to the
#' function.
#'
#' In any case, both the K and G matrix need the kernel values for
#' every combination of the new vertices and the vertices used to
#' train the model. This is illustrated for both homogenous and
#' heterogenous networks in the examples.
#'
#'
#' @param object an object of class \code{\link[xnet:tskrr-class]{tskrr}}.
#' @param k a new K matrix or \code{NULL}. if \code{NULL}, the fitted
#' values on the training data are returned.
#' @param g a new G matrix or \code{NULL}. If \code{NULL}, K is used
#' for both.
#' @param ... arguments passed to or from other methods
#'
#' @return a matrix with predicted values.
#'
#' @seealso \code{\link{tskrr}} and \code{\link{tskrrTune}} for
#' fitting the models.
#'
#' @examples
#'
#' ## Predictions for homogenous networks
#'
#' data(proteinInteraction)
#'
#' idnew <- sample(nrow(Kmat_y2h_sc), 20)
#'
#' trainY <- proteinInteraction[-idnew,-idnew]
#' trainK <- Kmat_y2h_sc[-idnew,-idnew]
#'
#' testK <- Kmat_y2h_sc[idnew, - idnew]
#'
#' mod <- tskrr(trainY, trainK, lambda = 0.1)
#' # Predict interaction between test vertices
#' predict(mod, testK, t(testK))
#'
#' # Predict interaction between test and train vertices
#' predict(mod, testK)
#' predict(mod, g = t(testK))
#'
#' ## Predictions for heterogenous networks
#' data("drugtarget")
#'
#' idnewK <- sample(nrow(targetSim), 10)
#' idnewG <- sample(ncol(drugSim), 10)
#'
#' trainY <- drugTargetInteraction[-idnewK, -idnewG]
#' trainK <- targetSim[-idnewK, -idnewK]
#' trainG <- drugSim[-idnewG, -idnewG]
#'
#' testK <- targetSim[idnewK, -idnewK]
#' testG <- drugSim[-idnewG, idnewG]
#'
#' mod <- tskrr(trainY, trainK, trainG, lambda = 0.01)
#'
#' # Predictions for new targets on drugs in model
#' predict(mod, testK)
#' # Predictions for new drugs on targets in model
#' predict(mod, g = testG)
#' # Predictions for new drugs and targets
#' predict(mod, testK, testG)
#'
#' @include all_generics.R
#' @rdname predict
#' @method predict tskrr
#' @export
predict.tskrr <- function(object,
                          k = NULL,
                          g = NULL,
                          ...){
  gnull <- is.null(g)
  knull <- is.null(k)
  if(knull && gnull)
    return(fitted(object))
  if(gnull && is_homogenous(object)){
    Keig <- get_eigen(object)
    g <- eigen2matrix(Keig$vectors, Keig$values)
  } else if (gnull){
    Geig <- get_eigen(object, which = "column")
    g <- eigen2matrix(Geig$vectors, Geig$values)
  } else if (knull){
    Keig <- get_eigen(object)
    k <- eigen2matrix(Keig$vectors, Keig$values)
  }

  out <- k %*% weights(object) %*% g

  if(knull){
    rownames(out) <- labels(object)$k
  }
  if(gnull){
    colnames(out) <- labels(object)$g
  }
  return(out)
}

#' @rdname predict
#' @export
setMethod("predict",
          "tskrr",
          predict.tskrr)
