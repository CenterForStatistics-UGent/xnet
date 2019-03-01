#' Update a tskrr object with a new lambda
#'
#' This function allows you to refit a \code{\link{tskrr}} with a
#' new lambda. It can be used to do manual tuning/crossvalidation.
#'
#' @param object a \code{\link[xnet:tskrr-class]{tskrr}} object
#' @inheritParams tskrr
#' @param ... arguments passed to methods
#'
#' @return an updated \code{\link[xnet:tskrr-class]{tskrr}} object
#' fitted with the new lambdas.
#'
#' @rdname update
#' @export
setMethod("update",
          "tskrrHomogenous",
          function(object, lambda){

            if(missing(lambda) || !is.numeric(lambda) || length(lambda) != 1){
              stop(paste("lambda should be a single numeric value",
                         "for homogenous networks."))
            }

            decomp <- get_eigen(object)

            Hk <- eigen2hat(decomp$vectors,
                            decomp$values,
                            lambda)

            object@lambda.k <- lambda
            object@pred <- Hk %*% object@y %*% Hk

            return(object)
          })

#' @rdname update
#' @export
setMethod("update",
          "tskrrHeterogenous",
          function(object, lambda){

            if(missing(lambda) ||
               !is.numeric(lambda) ||
               (ll<- length(lambda)) < 1 ||
               ll > 2){
              stop(paste("lambda should be a numeric vector",
                         "with one or two values"))
            }

            if(ll == 1){
              lambda.k <- lambda.g <- lambda
            } else {
              lambda.k <- lambda[1]
              lambda.g <- lambda[2]
            }

            decompk <- get_eigen(object, 'row')
            decompg <- get_eigen(object, 'column')

            Hk <- eigen2hat(decompk$vectors,
                            decompk$values,
                            lambda.k)
            Hg <- eigen2hat(decompg$vectors,
                            decompg$values,
                            lambda.g)

            object@lambda.k <- lambda.k
            object@lambda.g <- lambda.g
            object@pred <- Hk %*% object@y %*% Hg

            return(object)
          })