#' Extract weights from a tskrr model
#'
#' This function calculates the weight matrix for
#' calculating the predictions of a tskrr model.
#'
#' The weight matrix is calculated from the map matrices through the
#' function \code{\link{eigen2map}}.
#'
#' @note The package \code{xnet} adds a S4 generic function
#' for \code{\link[stats]{weights}}.
#'
#' @param object a \code{\link{tskrr}} object for which the weights
#' have to be calculated.
#'
#' @param ... other arguments passed to methods.
#'
#' @return a matrix with the weights for the tskrr model.
#'
#' @rdname weights
#' @aliases weights
#' @export
setMethod("weights",
          "tskrrHeterogeneous",
          function(object){
            eigK <- get_eigen(object, 'row')
            eigG <- get_eigen(object, 'column')
            l <- lambda(object)

            Mk <- eigen2map(eigK$vectors, eigK$values, l[1])
            Mg <- eigen2map(eigG$vectors, eigG$values, l[2])

            Mk %*% response(object) %*% Mg
          })

#' @rdname weights
#' @export
setMethod("weights",
          "tskrrHomogeneous",
          function(object){
            eigK <- get_eigen(object)
            l <- lambda(object)

            Mk <- eigen2map(eigK$vectors, eigK$values, l)

            Mk %*% response(object) %*% Mk
          })
