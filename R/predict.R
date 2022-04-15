#' predict method for tskrr fits
#'
#' Obtains the predictions from a \code{\link{tskrr}} model for new data.
#' To get the predictions on the training data,
#' use the function \code{\link[xnet:fitted]{fitted}}
#' or set both \code{k} and \code{g} to \code{NULL}.
#'
#' Predictions can be calculated between new vertices and the vertices
#' used to train the model, between new sets of vertices, or both. Which
#' predictions are given, depends on the kernel matrices passed to the
#' function.
#'
#' In any case, both the K and G matrix need the kernel values for
#' every combination of the new vertices and the vertices used to
#' train the model. This is illustrated for both homogeneous and
#' heterogeneous networks in the examples.
#'
#' To predict the links between a new set of vertices and the training
#' vertices, you need to provide the kernel matrix for either the K
#' or the G set of vertices. If you want to predict the mutual links
#' between two new sets of vertices, you have to provide both the
#' K and the G matrix. This is particularly important for homogeneous
#' networks: if you only supply the \code{k} argument, you will get
#' predictions for the links between the new vertices and the vertices
#' on which the model is trained. So in order to get the
#' mutual links between the new vertices, you need to provide the kernel
#' matrix as the value for both the \code{k} and the \code{g} argument.
#'
#' @section Warning:
#' This function is changed in version 0.1.9 so it's more consistent
#' in how it expects the K and G matrices to be ordered. Up to version
#' 0.1.8 the new vertices should be on the rows for the K matrix and on
#' the columns for the G matrix. This lead to confusion.
#'
#' If you're using old code, you'll get an error pointing this out.
#' You need to transpose the G matrix in the old code to make it work
#' with the new version.
#'
#' @param object an object of class \code{\link[xnet:tskrr-class]{tskrr}}.
#' @param k a new K matrix or \code{NULL}. if \code{NULL}, the fitted
#' values on the training data are returned.
#' @param g a new G matrix or \code{NULL}. If \code{NULL}, K is used
#' for both.
#' @param testdim a logical value indicating whether the dimensions should
#' be checked prior to the calculation. You can set this to \code{FALSE} but
#' you might get more obscure errors if dimensions don't match.
#' @param platt return Platt scores(probabilities) instead of raw predictions (see \code{\link{platt_scores}}). If NULL, the function will check whether the object contains information to calculate these and will use them if so.
#' @param ... arguments passed to or from other methods
#'
#' @return a matrix with predicted values.
#'
#' @seealso \code{\link{tskrr}} and \code{\link{tskrrTune}} for
#' fitting the models.
#'
#' @examples
#'
#' ## Predictions for homogeneous networks
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
#' predict(mod, testK, testK)
#'
#' # Predict interaction between test and train vertices
#' predict(mod, testK)
#' predict(mod, g = testK)
#'
#' ## Predictions for heterogeneous networks
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
#' testG <- drugSim[idnewG, -idnewG]
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
                          testdim = TRUE,
                          platt = NULL,
                          ...){

  gnull <- is.null(g)
  knull <- is.null(k)

  if(is.null(platt)){
    coefs <- object@platt
    platt <- length(coefs) > 0
  } else if(platt){
    coefs <- object@platt
    if(!length(coefs))
      coefs <- add_platt(object)@platt
  }

  if(testdim){
    dims <- dim(object)

    if(!knull && ncol(k) != dims[1]){
      stopmsg <- paste("The k matrix needs",dims[1],
                       "columns. The new vertices should be on the rows.")
      if(nrow(k) == dims[1])
        stopmsg <- paste(stopmsg,
                         "/nYou might have transposed the K matrix.",
                         "See also ?predict.tskrr.")

        stop(stopmsg, call. = FALSE)
    }

    if(!gnull && ncol(g) != dims[2]){
      stopmsg <- paste("The g matrix needs",dims[2],
                       "columns. The new vertices should be on the rows.")

      if(nrow(g) == dims[2]){
        stopmsg <- paste(stopmsg,
                         "\nYou might have transposed the G matrix or",
                         "used old code that relied on versions <0.1.9.",
                         "Please see the warning in ?predict.tskrr")
      }

      stop(stopmsg, call. = FALSE)
    }
  }

  if(knull && gnull)
    return(fitted(object))

  if(gnull && is_homogeneous(object)){

    Keig <- get_eigen(object)
    g <- eigen2matrix(Keig$vectors, Keig$values)

  } else if (gnull){

    Geig <- get_eigen(object, which = "column")
    g <- eigen2matrix(Geig$vectors, Geig$values)

  } else if (knull){

    Keig <- get_eigen(object)
    k <- eigen2matrix(Keig$vectors, Keig$values)

  }

  out <- k %*% tcrossprod(weights(object), g)

  if(knull){
    rownames(out) <- labels(object)$k
  }
  if(gnull){
    colnames(out) <- labels(object)$g
  }

  if(platt) out <- .plattscore(out,coefs)

  return(out)
}

#' @rdname predict
#' @export
setMethod("predict",
          "tskrr",
          predict.tskrr)
