# Internal functions for get_loo_fun

.getloo_heterogeneous <- function(exclusion, replaceby0){
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

.getloo_homogeneous <- function(exclusion, replaceby0, symmetry){
  # Translate edges and vertices
  if(exclusion %in% c("interaction","both"))
    exclusion <- switch(exclusion,
                        interaction = "edges",
                        both = "vertices")

  if(exclusion == "edges"){
    if(symmetry == "symmetric"){
      if(replaceby0) loo.e0.sym else loo.e.sym
    } else if(symmetry == "skewed"){
      if(replaceby0) loo.e0.skew else loo.e.skew
    } else {
      stop("No loo optimization for homogeneous networks that aren't symmetric or skewed.")
    }
  } else if(exclusion == c("vertices")) { # exclusion is not interaction
    loo.v
  } else {
    stop("Exclusion should be one of edges or vertices")
  }
}

# dots catch other arguments to avoid errors when exclusion is passed
.getloo_linearfilter <- function(replaceby0, ...){
  if(replaceby0) loo.i0.lf else loo.i.lf
}
