#' Plot the grid of a tuned tskrr model
#'
#' With this function you can visualize the grid search for optimal
#' lambdas from a \code{\link[xnet:tskrrTune-class]{tskrrTune}} object.
#' In the case of two dimensional grid search, this function will plot a
#' contour plot on a grid, based on the functions \code{\link{image}}
#' and \code{\link{contour}}. For one dimensional grid search the function
#' creates a single line plot.
#'
#' @param x an object that inherits from
#' \code{\link[xnet:tskrrTune-class]{tskrrTune}}
#' @param addlambda a logical value indicating whether the
#' lambda with the minimum loss should be added to the plot.
#' In case of a one dimensional plot, this adds a colored
#' vertical line. In the case of a two dimensional plot, this
#' adds a colored point at the minimum.
#' @param lambdapars a list with named \code{\link{par}} values
#' passed to the function \code{\link{abline}} or
#' \code{\link{points}} for plotting the best lambda value when
#' \code{addmin = TRUE}.
#' @param log a logical value indicating whether the lambdas should be
#' plotted at a log scale (the default) or not.
#' @param opts.contour options passed to the function
#' \code{\link{contour}} for 2D grid plots. Ignored for 1D
#' grid plots.
#' @param ... arguments passed to other functions. For a one
#' dimensional plot, this will be the function \code{\link{plot}}
#'
#' @return \code{NULL} invisibly
#'
#' @examples
#'
#' data(drugtarget)
#'
#' ## One dimensional tuning
#' tuned1d <- tune(drugTargetInteraction, targetSim, drugSim,
#'                 lim = c(1e-4,2), ngrid = 40,
#'                 fun = loss_auc, onedim = TRUE)
#'
#' plot_grid(tuned1d)
#' plot_grid(tuned1d, lambdapars = list(col = "green",
#'                                      lty = 1, lwd = 2),
#'           log = FALSE, las = 2, main = "1D tuning")
#'
#' ## Two dimensional tuning
#' tuned2d <- tune(drugTargetInteraction, targetSim, drugSim,
#'                 lim = c(1e-4,10), ngrid = 20,
#'                 fun = loss_auc)
#'
#' plot_grid(tuned2d)
#'
#' @importFrom graphics plot abline axis contour par box
#' @importFrom graphics image points
#' @importFrom grDevices heat.colors
#' @include Class_tskrrTune.R
#' @rdname plot_grid
#' @export
plot_grid <- function(x,
                      addlambda = TRUE,
                      lambdapars = list(
                        col = "red"
                      ),
                      log = TRUE,
                      opts.contour = list(
                        nlevels = 10
                      ),
                      ...){

  if(!inherits(x, "tskrrTune"))
    stop("x has to be a tskrrTune object")
  if(!is.logical(addlambda) || length(addlambda) != 1)
    stop("addlambda should be a single logical value.")
  if(addlambda && !is.list(lambdapars))
    stop("lambdapars should be a named list.")

  if(has_onedim(x))
    .plot_1d_grid(x, addlambda = addlambda,
                  lambdapars = lambdapars,
                  log = log, ...)
  else
    .plot_2d_grid(x, addlambda = addlambda,
                  lambdapars = lambdapars,
                  log = log, opts.contour = opts.contour,
                  ...)

  return(invisible(NULL))
}

# 1D plotting --------------------------------------------

.plot_1d_grid <- function(x,
                          type = "l",
                          xlab = "Lambda values",
                          ylab = "loss",
                          addlambda,
                          lambdapars,
                          log,
                          opts.contour,
                          ...){
  xval <- get_grid(x)$k
  yval <- get_loss_values(x)
  dim(yval) <- NULL

  log <- if(log) "x" else ""


  plot(xval, yval, type = type, xlab = xlab, ylab = ylab,
       log = log, ...)

  if(addlambda){
    l <- unname(lambda(x)[1])

    if(! "lty" %in% names(lambdapars))
      lambdapars <- c(lambdapars, lty = 2)

    do.call(abline, c(list(v=l), lambdapars))
  }


}

# 2D plotting --------------------------------------------

.plot_2d_grid <- function(x,
                          addlambda,
                          lambdapars,
                          log,
                          opts.contour,
                          col = rev(heat.colors(20)),
                          xlab = "lambda k",
                          ylab = "lambda g",
                          las = par("las"),
                          ...
                          ){
  gridvals <- get_grid(x)
  z <- get_loss_values(x)

  # extract x and y values
  if(log){
    xval <- log10(gridvals$k)
    yval <- log10(gridvals$g)
  } else {
    xval <- gridvals$k
    yval <- gridvals$g
  }

  image(xval, yval, z, xlab = xlab, ylab = ylab,
        col = col,
        xaxt = "n", yaxt = "n", ...)

  # create axes
  xaxis <- pretty(xval)
  yaxis <- pretty(yval)
  xlabs <- if(log) 10^xaxis else xaxis
  ylabs <- if(log) 10^yaxis else yaxis

  axis(1, at = xaxis, labels = prettyNum(xlabs), las = las)
  axis(2, at = yaxis, labels = prettyNum(ylabs), las = las)

  # Add contours
  c.opts <- c(list(x = xval,
                   y = yval,
                   z = z),
              opts.contour)
  c.opts$add <- TRUE
  do.call(contour, c.opts)
  box()

  if(addlambda){
    lambda <- lambda(x)
    if(log) lambda <- log10(lambda)
    p.opts <- c(list(x = lambda[1L],
                     y = lambda[2L]),
                lambdapars)
    if(is.null(p.opts$pch)) p.opts$pch <- "+"
    do.call(points, p.opts)
  }
}
