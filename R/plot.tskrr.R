#' plot a heatmap of the predictions from a tskrr model
#'
#' This function plots a heatmap of the fitted values in a
#' \code{\link{tskrr}} model. The function is loosely based on
#' \code{\link{heatmap}}, but uses a different mechanism and adds
#' a legend by default.
#'
#' The function can select part of the model for plotting. Either you
#' specify \code{rows} and \code{cols}, or you specify \code{nbest}.
#' If \code{nbest} is specified, \code{rows} and \code{cols} are ignored.
#' The n highest values are looked up in the plotted values, and only
#' the rows and columns related to these values are shown then. This
#' allows for a quick selection of the highest predictions.
#'
#' Dendrograms are created by converting the kernel matrices to
#' a distance, using
#'
#' d(x,y) = K(x,x)^2 + K(y,y)^2 - 2*K(x,y)
#'
#' with K being the kernel function. The resulting distances are
#' clustered using \code{\link{hclust}} and converted to a
#' dendrogram using \code{\link{as.dendrogram}}.
#'
#' @param x a tskrr model
#' @param dendro a character value indicating whether a dendrogram
#' should be constructed.
#' @param which a character value indicating whether the fitted values,
#' the leave-one-out values or the original response values should be
#' fitted.
#' @param exclusion if \code{which = "loo"}, this argument is passed to
#' \code{\link{loo}} for the exclusion settings
#' @param replaceby0 if \code{which = "loo"}, this argument is passed to
#' \code{\link{loo}}.
#' @param nbest a single integer value indicating the amount of best values
#' that should be selected. If \code{0}, all data is shown.
#' @param rows a numeric or character vector indicating which rows should be
#' selected from the model.
#' @param cols a numeric or character vector indicating which columns should be
#' selected from the model.
#' @param col a vector with colors to be used for plotting
#' @param legend a logical value indicating whether or not the legend should
#' be added to the plot.
#' @param main a character value with a title for the plot
#' @param xlab a character label for the X axis
#' @param ylab a character label for the Y axis
#' @param labRow a character vector with labels to be used on the rows.
#' Note that these labels are used as is (possibly reordered to match
#' the dendrogram). They can replace the labels from the model.
#' @param labCol the same as \code{labRow} but then for the columns.
#' @param margins a numeric vector with 2 values indicating the margins to
#' be used for the row and column labels (cfr \code{par("mar")})
#' @param ... currently ignored
#'
#' @return \code{NULL} invisibly.
#'
#' @importFrom stats as.dendrogram as.dist hclust order.dendrogram
#' @importFrom graphics par layout plot image axis mtext
#' @importFrom grDevices dev.hold dev.flush
#' @importFrom graphics frame title plot.new plot.window rect
#' @rdname plot.tskrr
#' @method plot tskrr
#' @export
plot.tskrr <- function(x, dendro = c("both","row","col","none"),
                       which = c("fitted", "loo", "response"),
                       exclusion = c("interaction", "row", "column", "both"), replaceby0 = FALSE,
                       nbest = 0, rows, cols,
                       col = rev(heat.colors(20)),
                       legend = TRUE,
                       main = NULL,
                       xlab = NULL,
                       ylab = NULL,
                       labRow = NULL,
                       labCol = NULL,
                       margins = c(5,5),
                       ...){

  ## PROCESS INPUT AND PREPARE
  if(!is.logical(legend))
    stop("legend should be a logical value")

  # process input dendro
  dendro <- match.arg(dendro)

  dendroK <- dendro %in% c("both", "row")
  dendroG <- dendro %in% c("both", "col")
  dendro <- dendroK || dendroG

  # process input which
  which <- match.arg(which)
  if(which != "loo"){
    val <- if(which == "fitted") fitted(x) else response(x)
  } else {
    exclusion <- match.arg(exclusion)
    val <- loo(x, exclusion, replaceby0)
  }

  labs <- labels(x)
  rownames(val) <- labs$k
  colnames(val) <- labs$g

  # get kernel info
  if(dendroK){
    K <- get_eigen(x, "row")
    K <- eigen2matrix(K$vectors, K$values)
  }
  if(dendroG){
    G <- get_eigen(x, "column")
    G <- eigen2matrix(G$vectors, G$values)
  }

  ## PROCESS INPUT SELECTION
  if(nbest > 0){
    # select the n best values to show
    bestpos <- find_best_pos(val, nbest)
    rows <- unique(bestpos[,1])
    cols <- unique(bestpos[,2])

    # Select values
    val <- val[rows, cols]
    if(dendroK) K <- K[rows, rows]
    if(dendroG) G <- G[cols, cols]

  } else if(!missing(rows) && !missing(cols)){
    # process the rows and cols
    if(is.numeric(rows) && any((rows %% 1) != 0))
      stop("rows contains non-integer values.")
    if(is.numeric(cols) && any((cols %% 1) != 0))
      stop("cols contains non-integer values.")

    if(is.character(rows)){
      rows <- match(rows, labs$k, nomatch = 0L)
      if(any(rows == 0))
        stop("Not all row labels were found in the model.")
    }
    if(is.character(cols)){
      cols <- match(cols, labs$g, nomatch = 0L)
      if(any(cols == 0))
        stop("not all column labels were found in the model.")
    }

    # Select values
    val <- val[rows, cols]
    if(dendroK) K <- K[rows, rows]
    if(dendroG) G <- G[cols, cols]
  }

  nr <- nrow(val)
  nc <- ncol(val)

  ## CONSTRUCT THE DENDROGRAMS IF NEEDED
  if(dendroK){
    ddK <- .kernel2dendro(K)
    rowid <- order.dendrogram(ddK)
    val <- val[rowid,]
  }
  if(dendroG){
    ddG <- .kernel2dendro(G)
    colid <- order.dendrogram(ddG)
    val <- val[, colid]
  }

  ## CREATE THE LABELS
  if(is.null(labRow)){
    labRow <- rownames(val)
  }
  if(is.null(labCol)){
    labCol <- colnames(val)
  }


  ## PROCESS THE COLORS
  ncolor <- length(col)
  minmax <- range(pretty(val, ncolor))
  breaks <- seq(minmax[1], minmax[2], length.out = ncolor + 1)

  ## CREATE THE PLOT LAYOUT
  lmat <- matrix(c(0,2,3,1), ncol = 2)
  lwid <- if(dendroK) c(1,4) else c(0.05,4)
  lhei <- if(dendroG) c(1,4) else c(0.05,4)

  if(!is.null(main)) lhei[1] <- lhei[1] + 0.2

  if(legend){
    lmat <- rbind(lmat, c(4,4))
    lhei <- c(lhei,0.8)
  }
  margmain <- if(is.null(main)) 0 else 1.5

  ## PLOT THE DIFFERENT ELEMENTS
  dev.hold()
  on.exit(dev.flush())

  op <- par(no.readonly = TRUE)
  on.exit(par(op), add = TRUE)

  layout(lmat, widths = lwid, heights = lhei)

  # Plot the heatmap
  par(mar = c(margins[1L],0,0,margins[2L]))
  image(1L:nc, 1L:nr,
        t(val),
        xlim = 0.5 + c(0,nc),
        ylim = 0.5 + c(0,nr),
        axes = FALSE, xlab = "", ylab = "",
        col = col,
        breaks = breaks)
  axis(1,1L:nc, labels = labCol,
       las = 2,
       line = -0.5,
       tick = 0)
  if (!is.null(xlab))
    mtext(xlab, side = 1, line = margins[1L] - 1.25)
  axis(4, 1L:nr, labels = labRow,
       las = 2,
       line = -0.5,
       tick = 0)
  if (!is.null(ylab))
    mtext(ylab, side = 4, line = margins[2L] - 1.25)
  box()

  # If needed, plot dendros
  par(mar = c(margins[1L],0,0,0))
  if(dendroK){
    plot(ddK, horiz = TRUE, axes = FALSE,yaxs = "i",
         leaflab = "none")
  } else {
    frame()
  }
  par(mar = c(0,0,margmain, margins[2L]))
  if(dendroG){
    plot(ddG, axes = FALSE, xaxs = "i", leaflab = "none")
  } else {
    frame()
  }
  if (!is.null(main)) {
    par(xpd = NA)
    title(main)
  }

  if(legend){
    nbreaks <- length(breaks)
    par(mar = c(3,2,0,2))
    dev.hold()
    on.exit(dev.flush(), add = TRUE)
    plot.new()
    plot.window(ylim = c(0,1),
                xlim = range(breaks),
                xaxs = "i", yaxs = "i")
    ybottom <- rep(0,nbreaks-1)
    ytop <- rep(1,nbreaks-1)
    xleft <- breaks[-nbreaks]
    xright <- breaks[-1]
    rect(xleft, ybottom, xright,ytop,col = col,
         border = NA)
    axis(1, las = 1)
    box()
  }
  return(invisible(NULL))

}

# Helper function to create dendrograms
.kernel2dendro <- function(x){
  dists <- outer(diag(x)^2, diag(x)^2, `+`) - 2*x
  dists[dists < 0] <- 0
  dists <- sqrt(dists)
  as.dendrogram(hclust(as.dist(dists)))
}

# Helper function to find the best positions
find_best_pos <- function(x, n){

  id <- order(x, decreasing = TRUE)[seq_len(n)]
  nr <- nrow(x)
  rowid <- id %% nr
  rowid[rowid == 0] <- nr
  colid <- id %/% nr + 1*(rowid != nr)
  return(cbind(rowid,colid))
}