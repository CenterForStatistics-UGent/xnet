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
#' @param feature a character value indicating for which kernel matrix the features should be evaluated. Ignored in the case of a homogeneous network.
#' @inheritParams loss
#'
#' @rdname feature_importance
#' @export
# setMethod("feature_importance",
#           "tskrr",
test <-           function(x, n = 100,
                   feature = c("row", "column"),
                   exclusion = c("interaction", "row", "column", "both"),
                   replaceby0 = FALSE,
                   fun = loss_auc){
  # check arguments
  feature <- match.arg(feature)
  exclusion <- match.arg(exclusion)
  fun <- match.fun(fun)

  gmd <- gramData(x, which = feature)

  if(!has_orig(gmd))
    stop("Original data not present in the object. Feature importance cannot be calculated without data.")

  # Get the feature data
  orig <- get_orig(gmd)
  features <- colnames(gmd)
  nf <- length(features)

  origloss <- loss(x,
                   fun = fun, exclusion = exclusion,
                   replaceby0 = replaceby0)

  # Get the necessary input
  l <- lambda(mod)
  args <- list(
    y = response(mod),
    lambda.k = l[1],
    lambda.g = l[2],
  )

  eig2 <- get_eigen(gramData(mod,
                             which = setdiff(c("row","column"),
                                             feature)))


  simloss <- matrix(NA, nrow = n,
                    ncol = nf)

  for(i in seq_len(nf)){
    tmp <- orig
    simloss[,i] <- replicate(n,{
      tmp[,i] <- sample(tmp[,i], replace = TRUE)
      eig1 <- eigen(convert(tmp, gmd))



    })
  }

  return(simloss)
          }
#          )
do.call(tskrr.fit,
        )
tskrr.fit(response(mod), eigen(xx), eigen(get_gram(xt)), lambda.k = 0.1, lambda.g = 0.1)