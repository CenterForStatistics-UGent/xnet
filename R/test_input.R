# Function to test inputs for tskrr etc.

# This function tests the input for all fitting functions
# It returns a list with the following elements:
# - lambda.k
# - lambda.g
# - homogenous
.test_input <- function(y,k,g,lambda,testdim,testlabels){

  # SET FLAGS
  homogenous <- is.null(g)

  # TESTS INPUT
  if( !(is.matrix(y) && is.numeric(y)) )
    stop("y should be a matrix.")

  if( !(is.matrix(k) && is.numeric(k)) )
    stop("k should be a matrix.")

  if(!is.numeric(lambda))
    stop("lambda should be numeric.")

  if(!homogenous){
    if( !(is.matrix(g) && is.numeric(g)) )
      stop("g should be a matrix.")

    nl <- length(lambda)
    if(nl < 1 || nl > 2)
      stop("lambda should contain one or two values. See ?tskrr")

  } else {
    if(length(lambda) != 1)
      stop("lambda should be a single value. See ?tskrr")
  }

  if(any(is.na(y)))
    stop(paste("Missing values in the y matrix are not allowed. You can",
               "use the function impute_tskrr for imputations."))

  # TEST KERNELS
  if(testdim){
    if(!is_symmetric(k))
      stop("k should be a symmetric matrix.")

    if(!homogenous && !is_symmetric(g))
      stop("g should be a symmetric matrix.")

    if(!valid_dimensions(y,k,g))
      stop(paste("The dimensions of the matrices don't match.",
                 "Did you maybe switch the k and g matrices?",
                 sep = "\n"))
  }
  if(testlabels){

    valid_labels(y,k,g) # Generates errors if something's wrong

    rk <- rownames(k) # not when there's no row/-colnames
    if(!is.null(rk)){
      y <- match_labels(y,rk,colnames(g))
    }

  }

  # SET LAMBDAS
  lambda.k <- lambda[1]
  lambda.g <- if(!homogenous){
    if(nl == 1) lambda else lambda[2]
  } else NULL

  return(list(
    lambda.k = lambda.k,
    lambda.g = lambda.g,
    homogenous = homogenous
  ))
}