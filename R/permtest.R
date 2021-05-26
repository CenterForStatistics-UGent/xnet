#' Calculate the relative importance of the edges
#'
#' This function does a permutation-based evaluation of the impact of
#' different edges on the final result. It does so by permuting the kernel
#' matrices, refitting the model and calculating a loss function.
#'
#' The test involved uses a normal approximation. It assumes that under the
#' null hypothesis, the loss values are approximately normally distributed.
#' The cumulative probability of a loss as small or smaller than
#' the one found in the original model, is calculated based on a normal
#' distribution from which the mean and sd are calculated from the permutations.
#'
#' @section Warning: It should be noted that this normal approximation is an ad-hoc approach.
#' There's no guarantee that the actual distribution of the loss under the
#' null hypothesis is normal. Depending on the loss function, a significant
#' deviation from the theoretic distribution can exist. Hence this functions should only
#' be used as a rough guidance in model evaluation.
#'
#' @param x either a \code{\link{tskrr-class}} or a
#' \code{\link{tskrrTune-class}} object
#' @param permutation a character string that defines whether the row,
#' column or both kernel matrices should be permuted. Ignored in case of
#' a homogeneous network
#' @param n the number of permutations for every kernel matrix
#' @param exclusion the exclusion to be used in the \code{\link{loo}} function. See also \code{\link{get_loo_fun}}
#' @param replaceby0 a logical value indicating whether \code{\link{loo}}
#' removes a value in the leave-one-out procedure or replaces it by zero.
#' See also \code{\link{get_loo_fun}}.
#' @param fun a function (or a character string with the name of a
#' function) that calculates the loss. See also \code{\link{tune}} and
#' \code{\link{loss_functions}}
#' @param exact a logical value that indicates whether or not an
#' exact p-value should be calculated, or be approximated based on
#' a normal distribution.
#' @param ... arguments passed to other methods
#'
#' @return An object of the class permtest.
#'
#' @examples
#'
#' # Heterogeneous network
#'
#' data(drugtarget)
#'
#' mod <- tskrr(drugTargetInteraction, targetSim, drugSim)
#' permtest(mod, fun = loss_auc)
#'
#' @importFrom stats pnorm printCoefmat sd
#' @rdname permtest
#' @aliases permtest
#' @export
setMethod("permtest","tskrrHeterogeneous",
          function(x,
                   n = 100,
                   permutation = c("both","row","column"),
                   exclusion = c("interaction","row","column","both"),
                   replaceby0 = FALSE,
                   fun = loss_mse,
                   exact = FALSE){
  # Process arguments
  exclusion <- match.arg(exclusion)
  lossfun <- match.fun(fun)
  permutation <- match.arg(permutation)
  if(n <= 0 || !is_whole_positive(n))
    stop("n should be a positive integer value.")

  # extract information
  k <- get_kernelmatrix(x, "row")
  g <- get_kernelmatrix(x, "column")
  y <- response(x)
  lambdas <- lambda(x)

  loofun <- get_loo_fun("tskrrHeterogeneous",
                        exclusion = exclusion,
                        replaceby0 = replaceby0)

  # Calculate the original loss function
  orig_loss <- lossfun(y,loofun(y,
                                hat(x,"row"),
                                hat(x,"column"),
                                fitted(x)))

  # Do the permutation test
  perms <- .permtest_hetero(y,k,g,lambdas,
            n, permutation,
            lossfun, loofun)
  if(exact){
    pval <- mean(orig_loss > perms)
  } else {
  pval <- pnorm(orig_loss,
                mean = mean(perms),
                sd = sd(perms))
  }

  new("permtest",
      orig_loss = orig_loss,
      perm_losses = perms,
      n = n,
      loss_function = lossfun,
      exclusion = exclusion,
      replaceby0 = replaceby0,
      permutation = permutation,
      pval = pval,
      exact = exact)

}) # END setMethod tskrrHeterogeneous

#' @rdname permtest
#' @export
setMethod("permtest","tskrrHomogeneous",
          function(x,
                   n = 100,
                   permutation = c("both"),
                   exclusion = c("interaction","both"),
                   replaceby0 = FALSE,
                   fun = loss_mse,
                   exact = FALSE){
            # Process arguments
            exclusion <- match.arg(exclusion)
            lossfun <- match.fun(fun)

            if(permutation != "both")
              stop("For a homogeneous model setting the permutation to anything else but 'both' doesn't make sense.")

            if(n <= 0 || !is_whole_positive(n))
              stop("n should be a positive integer value.")

            # extract information
            k <- get_kernelmatrix(x, "row")
            y <- response(x)
            lambdas <- lambda(x)

            loofun <- get_loo_fun("tskrrHomogeneous",
                                  exclusion = exclusion,
                                  replaceby0 = replaceby0)

            # Calculate the original loss function
            orig_loss <- lossfun(y,loofun(y,
                                          hat(x,"row"),
                                          pred = fitted(x)))

            # Do the permutation test
            perms <- .permtest_homo(y,k,lambdas,
                             n, lossfun, loofun)
            if(exact){
              pval <- mean(orig_loss > perms)
            } else {
              pval <- pnorm(orig_loss,
                            mean = mean(perms),
                            sd = sd(perms))
            }
            new("permtest",
                orig_loss = orig_loss,
                perm_losses = perms,
                n = n,
                loss_function = lossfun,
                exclusion = exclusion,
                replaceby0 = replaceby0,
                permutation = permutation,
                pval = pval,
                exact = exact)

}) # END setMethod tskrrHomogeneous


#' @rdname permtest
#' @export
setMethod("permtest",
          "tskrrTune",
          function(x,
                   permutation = c("both","row","column"),
                   n = 100,
                   ...){

            permutation <- match.arg(permutation)
            hetero <- is_heterogeneous(x)

            # Extract info. No getters available!
            exclusion <- slot(x, "exclusion")
            replaceby0 <- slot(x, "replaceby0")
            lossfun <- slot(x, "loss_function")

            x <- as_tskrr(x)

            callGeneric(x = x,
                        permutation = permutation,
                        n = n,
                        exclusion = exclusion,
                        replaceby0 = replaceby0,
                        fun = lossfun,
                        ...)
          })

# Internal permtest function.
# y: the response matrix
# k: the k kernel
# g: the g kernel
# lambda : the lambdas
# n: number of permutations
# permutation: which permutations need to happen
# lossfun: the function for calculating the loss
# loofun: the function for the loo

.permtest_hetero <- function(y,k,g,lambda,
                      n,permutation,
                      lossfun, loofun){

  pk <- permutation == "row" || permutation == "both"
  pg <- permutation == "column" || permutation == "both"

  nk <- dim(k)[1]
  ng <- dim(g)[1]
  lambda.k <- lambda[1]
  lambda.g <- lambda[2]

  if(pk && !pg ){

    # This needs only to be calculated once
    g_eig <- eigen(g, symmetric = TRUE)
    gH <- eigen2hat(g_eig$vectors, g_eig$values, lambda.g)

    out <- replicate(n,{

      id <- sample(nk)
      knew <- k[id,id]
      k_eig <- eigen(knew, symmetric = TRUE)
      kH <- eigen2hat(k_eig$vectors, k_eig$values, lambda.k)

      pred <- kH %*% y %*% gH
      loopred <- loofun(y,kH,gH,pred)
      lossfun(y, loopred)
    })

  } else if (pg && !pk){

    # This needs only to be calculated once
    k_eig <- eigen(k, symmetric = TRUE)
    kH <- eigen2hat(k_eig$vectors, k_eig$values, lambda.k)

    out <- replicate(n,{

      id <- sample(ng)
      gnew <- g[id,id]
      g_eig <- eigen(gnew, symmetric = TRUE)
      gH <- eigen2hat(g_eig$vectors, g_eig$values, lambda.g)

      pred <- kH %*% y %*% gH
      loopred <- loofun(y,kH,gH,pred)
      lossfun(y, loopred)
    })

  } else if (pk && pg){
    # This needs only to be calculated once

    out <- replicate(n,{

      id <- sample(nk)
      knew <- k[id,id]
      k_eig <- eigen(knew, symmetric = TRUE)
      kH <- eigen2hat(k_eig$vectors, k_eig$values, lambda.k)

      id2 <- sample(ng)
      gnew <- g[id2,id2]
      g_eig <- eigen(gnew, symmetric = TRUE)
      gH <- eigen2hat(g_eig$vectors, g_eig$values, lambda.g)

      pred <- kH %*% y %*% gH
      loopred <- loofun(y,kH,gH,pred)
      lossfun(y, loopred)
    })
  } else {
    stop("No permutations done. Please contact the package maintainer.")
  }
  return(out)

}

# Internal permtest function for homogeneous networks.
# y: the response matrix
# k: the k kernel
# lambda : the lambda
# n: number of permutations
# lossfun: the function for calculating the loss
# loofun: the function for the loo

.permtest_homo <- function(y,k,lambda,
                             n,
                           lossfun, loofun){

  nk <- dim(k)[1]
  lambda.k <- lambda

    out <- replicate(n,{

      id <- sample(nk)
      knew <- k[id,id]
      k_eig <- eigen(knew, symmetric = TRUE)
      kH <- eigen2hat(k_eig$vectors, k_eig$values, lambda.k)

      pred <- kH %*% y %*% kH
      loopred <- loofun(y,kH,pred = pred)
      lossfun(y, loopred)
    })

    return(out)

}

