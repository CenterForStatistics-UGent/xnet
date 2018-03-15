#' Leave one out crossvalidation for tskrr
#'
#' Perform a leave-one-out cross validation for two-step kernel
#' ridge regression based on the shortcuts described in
#' PAPER TO BE ADDED.
#'
#' @details The parameter \code{exclusion} defines what exactly is left out.
#' The value "interaction" means that a single interaction is removed.
#' In the case of a homogenous model this can be interpreted as the
#' removal of the interaction between two edges. The values "row" and
#' "column" mean that all interactions for a row edge resp. a column
#' edge are removed. The value "both" removes all interactions for
#' a row and a column edge.
#'
#' In the case of a homogenous model, "row" and "column" don't make sense
#' and will be replaced by "both" with a warning. This can be interpreted
#' as removing vertices, i.e. all interactions between one edge and
#' all other edges. For more information, see Stock et al PAPER TO BE ADDED.
#'
#' @param x an object of class \code{\link[xnet:tskrr-class]{tskrr}}
#' @param exclusion a character value with possible values "interaction",
#' "row", "column" or "both". Defaults to "interaction". See details.
#' @param replaceby0 a logical value indicating whether the interaction
#' should be simply removed (\code{FALSE}) or replaced by 0 (\code{TRUE}).
#' This only makes sense when \code{exclusion = "interaction"} and the
#' response matrix contains only 0 and 1 values. In all other cases, setting
#' this value to \code{TRUE} will result in an error.
#'
#' @return a numeric matrix with the leave-one-out predictions for
#' the model.
#'
#' @rdname loo
#' @export
loo <- function(x, exclusion = c("interaction","row","column","both"),
                replaceby0 = FALSE){

  if(!inherits(x, "tskrr"))
    stop("x should be a tskrr model.")

  exclusion <- match.arg(exclusion)

  if(exclusion !="interaction" && replaceby0)
    stop("replaceby0 can only be set to TRUE when exclusion = 'interaction'.")

  homogenous <- is_homogenous(x)

  if(homogenous && exclusion %in% c("row","column")){

    warning(sprintf(paste("For homogenous networks exclusion can only be",
                  "'interaction' or 'both'. Exclusion is set from '%s'",
                  "to 'both',"), exclusion))
    exclusion <- "both"
  }

  # For homogenous networks
  if(homogenous){

    Hk <- hat(x)
    symmetry <- symmetry(x)

    if(exclusion == 'interaction'){

      if(symmetry == 'symmetric'){

        out <- if(replaceby0)
          loo.e0.sym(x@y, Hk, x@pred)
        else
          loo.e.sym(x@y, Hk, x@pred)

      } else if(symmetry == 'skewed'){

        out <- if(replaceby0)
          loo.e0.skew(x@y, Hk, x@pred)
        else
          loo.e.skew(x@y, Hk, x@pred)

      } else {
        stop("Symmetry of homogenous network not recognized.")
      }

    } else { # eclusion == anything else
      out <- loo.v(x@y, Hk)
    }

  } else {
  # For heterogenous networks

    Hk <- hat(x, "row")
    Hg <- hat(x, "column")

    if(exclusion == "interaction"){

      out <- if(replaceby0)
        loo.i0(x@y, Hk, Hg, x@pred)
      else
        loo.i(x@y, Hk, Hg, x@pred)

    } else if(exclusion == "row"){

      out <- loo.r(x@y, Hk, Hg)

    } else if(exclusion == "column"){

      out <- loo.c(x@y, Hk, Hg)

    } else {

      out <- loo.b(x@y, Hk, Hg)
    }
  }

  return(out)
}