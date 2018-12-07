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

            decomp <- get_eigen(x)

            Hk <- eigen2hat(decomp$vectors,
                            decomp$values,
                            lambda)

            x@lambda.k <- lambda
            x@pred <- Hk %*% x@y %*% Hk

            return(x)
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

            decompk <- get_eigen(x, 'row')
            decompg <- get_eigen(x, 'column')

            Hk <- eigen2hat(decompk$vectors,
                            decompk$values,
                            lambda.k)
            Hg <- eigen2hat(decompg$vectors,
                            decompg$values,
                            lambda.g)

            x@lambda.k <- lambda.k
            x@lambda.g <- lambda.g
            x@pred <- Hk %*% x@y %*% Hg

            return(x)
          })