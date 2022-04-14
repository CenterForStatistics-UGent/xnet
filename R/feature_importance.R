#' Importance of features
#'
#' With this function you can calculate the importance of the features used to create the \code{\link{gramData}} object. This doesn't work if you provided the kernel matrices directly.
#'
#' The function will select each feature in the original data frame in turn, and resample it \code{n} times. Each time the loss function is recalculated, and the difference with the original loss value is stored. These values can then later be summarized or plotted.
#'
#' When a \code{\link{tskrr}} model was tuned earlier with the function \code{\link{tune}}, the values for \code{exclusion} and \code{replaceby0} are taken from the tuned object. Otherwise these arguments are passed to the function \code{\link{loss}} to calculate the loss value.
#'
#' @param x a \code{\link{tskrr}} oject.
#' @param n a single integer indicating the number of bootstrap samples used to estimate the feature importance.
#' @param dimension a character value indicating for which kernel matrix the features should be evaluated (rows or columns). Ignored in the case of a homogeneous network.
#' @param features a vector with indices (numeric, character or logical) that selects the feature columns that need to be evaluated. If empty, the function will loop over all features.
#' @param verbose a logical value that turns on a message indicating which feature is processed. By default, it's turned on in interactive sessions.
#' @inheritParams get_loo_fun
#' @inheritParams loss
#'
#' @importFrom utils flush.console
#'
#' @rdname feature_importance
#' @export
setMethod("feature_importance",
         "tskrr",
         function(x, n = 100,
                  dimension = c("row", "column"),
                  exclusion = "interaction",
                  replaceby0 = FALSE,
                  fun = loss_auc,
                  features,
                  verbose = interactive()){
  # check arguments
  dimension <- match.arg(dimension)
  exclusion <- match.arg(exclusion)
  fun <- match.fun(fun)

  gmd <- gramData(x, which = dimension)

  if(!has_orig(gmd))
    stop("Original data not present in the object. Feature importance cannot be calculated without data.")

  # Get the feature data
  orig <- get_orig(gmd)
  if(missing(features)){
    features <- colnames(orig)
  } else if(is.numeric(features) || is.logical(features)){
    features <- colnames(orig)[features]
  } else if(!is.vector(features)){
    stop("features should be a vector")
  }
  nf <- length(features)
  hetero <- is_heterogeneous(x)

  origloss <- loss(x,
                   fun = fun, exclusion = exclusion,
                   replaceby0 = replaceby0)
  y <- response(x)

  simloss <- matrix(NA, nrow = n,
                    ncol = nf)

  looclass <- if(hetero) "tskrrHeterogeneous" else
    "tskrrHomogeneous"
  loofun <- get_loo_fun(looclass, exclusion = exclusion,
                        replaceby0 = replaceby0)
  lambda <- lambda(x)

  if(hetero){
    # Get the other hat matrix
    other <- setdiff(c("row","column"), dimension)
    Ho <- hat(x, other)

    lambda <- if(dimension == "row") lambda[1] else lambda[2]

    predfun <- if(dimension == "row")
      function(x) Hf %*% y %*% Ho
    else
      function(x) Ho %*% y %*% Hf

  } else {
    lambda <- lambda[1]
    predfun <- function(x) Hf %*% y %*% Hf
  }


  for(i in seq_len(nf)){
    if(verbose){
      message(paste("\rProcessing feature",features[i]),appendLF = (i == nf))
      flush.console()
    }
    tmp <- orig

    for(j in seq_len(n)){
      tmp[,i] <- sample(orig[,i], replace = TRUE)
      eig <- eigen(convert(tmp, gmd))
      Hf <- eigen2hat(eig$vectors, eig$values,lambda)

      preds <- predfun(y)
      if(hetero){
        if(dimension == "row")
          loo <- loofun(y,Hf,Ho,preds)
        else if(dimension == "column")
          loo <- loofun(y,Ho,Hf,preds)
      } else {
        loo <- loofun(y,Hf,preds)
      }
      simloss[j,i] <- fun(y,loo)
    }
  }

  out <- new("feature_importance",
             orig_loss = origloss,
             simloss = simloss,
             n = n,
             loss_function = fun,
             exclusion = exclusion,
             replaceby0 = replaceby0,
             features = features)
  return(out)
}
)
