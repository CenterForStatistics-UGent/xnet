#' Update a tskrr object with a new lambda
#'
#' This function allows you to refit a \code{\link{tskrr}} with a
#' new lambda. It can be used to do manual tuning/cross-validation.
#' If the object has the hat matrices stored, these are updated
#' as well.
#'
#' @param object a \code{\link[xnet:tskrr-class]{tskrr}} object
#' @inheritParams tskrr
#' @param ... arguments passed to methods
#'
#' @return an updated \code{\link[xnet:tskrr-class]{tskrr}} object
#' fitted with the new lambdas.
#'
#' @examples
#' data(drugtarget)
#'
#' mod <- tskrr(drugTargetInteraction, targetSim, drugSim)
#'
#' # Update with the same lambda
#' mod2 <- update(mod, lambda = 1e-3)
#'
#' # Use different lambda for rows and columns
#' mod3 <- update(mod, lambda = c(0.01,0.001))
#'
#' # A model with the hat matrices stored
#' lambda <- c(0.001,0.01)
#' modkeep <- tskrr(drugTargetInteraction, targetSim, drugSim, keep = TRUE)
#' Hk_1 <- hat(modkeep, which = "row")
#' modkeep2 <- update(modkeep, lambda = lambda)
#' Hk_2 <- hat(modkeep2, which = "row")
#'
#' # Calculate new hat matrix by hand:
#' decomp <- get_eigen(modkeep, which = "row")
#' Hk_byhand <- eigen2hat(decomp$vectors,
#'                        decomp$values,
#'                        lambda = lambda[1])
#' identical(Hk_2, Hk_byhand)
#'
#' @rdname update
#' @export
setMethod("update",
          "tskrrHomogeneous",
          function(object, lambda){

            if(missing(lambda) || !is.numeric(lambda) || length(lambda) != 1){
              stop(paste("lambda should be a single numeric value",
                         "for homogeneous networks."))
            }

            decomp <- get_eigen(object)

            Hk <- eigen2hat(decomp$vectors,
                            decomp$values,
                            lambda)

            object@lambda.k <- lambda
            object@pred <- Hk %*% object@y %*% Hk

            if(has_hat(object))
              object@Hk <- Hk

            return(object)
          })

#' @rdname update
#' @export
setMethod("update",
          "tskrrHeterogeneous",
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

            if(has_hat(object)){
              object@Hk <- Hk
              object@Hg <- Hg
            }

            return(object)
          })
