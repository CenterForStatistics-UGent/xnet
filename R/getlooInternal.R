# Internal functions for get_loo_fun

.getloo_heterogenous <- function(exclusion, replaceby0){
  if(exclusion == "interaction"){
    if(replaceby0) loo.i0 else loo.i
  } else if(exclusion == "row"){
    loo.r
  } else if(exclusion == "column"){
    loo.c
  } else if(exclusion == "both"){
    loo.b
  } else {
    stop("Exclusion should be one of interaction, row, column or both.")
  }
}

.getloo_homogenous <- function(exclusion, replaceby0, symmetry){
  if(exclusion == "interaction"){
    if(symmetry == "symmetric"){
      if(replaceby0) loo.e0.sym else loo.e.sym
    } else if(symmetry == "skewed"){
      if(replaceby0) loo.e0.skew else loo.e.skew
    } else {
      stop("No loo optimization for homogenous networks that aren't symmetric or skewed.")
    }
  } else if(exclusion == c("both")) { # exclusion is not interaction
    loo.v
  } else {
    stop("Exclusion should be one of interaction or both")
  }
}

# dots catch other arguments to avoid errors when exclusion is passed
.getloo_linearfilter <- function(replaceby0, ...){
  if(replaceby0) loo.i0.lf else loo.i.lf
}