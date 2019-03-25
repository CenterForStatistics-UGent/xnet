# Function to construct lambdas for the tuning

# This functions prepare the lambdas for the function tune and
# does some basic checks. Out comes a list with the lambdas
# to be used in the tuning

# Returns a list with lambdas that fits the tskrrTune slot

.prepare_lambdas <- function(lim, ngrid, lambda = NULL, homogenous){

  if(homogenous){
    # Processing for homogenous networks
    if(is.null(lambda)){

      lim <- .check_for_one(lim, "lim")
      ngrid <- .check_for_one(ngrid, "ngrid")
      lambda <- create_grid(lim, ngrid)

    } else {
      lambda <- .check_for_one(lambda, "lambda")
    }
    return(list(k = lambda))
  } else {
    # Processing for heterogenous networks
    if(is.null(lambda)){
      lim <- .check_for_two(lim, "lim")
      ngrid <- .check_for_two(ngrid, "ngrid")
      lambdas <- mapply(create_grid, lim, ngrid, SIMPLIFY = FALSE)
      return(lambdas)
    } else {
      lambdas <- .check_for_two(lambda, "lambda")
    }
  }
}

.check_for_one <- function(x, arg = "argument"){
  if(is.atomic(x) && is.numeric(x)){
    return(x)
  } else {
    if(length(x) == 1 && is.numeric(x[[1]]))
      return(x[[1]])
    else
      stop(paste(arg, "can have only a single series of numeric values for this model."))
  }
}

.check_for_two <- function(x, arg = "argument"){
  if(is.atomic(x) && is.numeric(x)){
    return(list(k = x,g = x))
  } else {
    if(length(x) == 2 && is.numeric(x[[1]]) && is.numeric(x[[2]]) ){
      names(x) <- c("k","g")
      return(x)
    } else{
      stop(paste(arg,"should either be a numeric vector or a list with two numeric elements for this model."))
    }
  }
}
