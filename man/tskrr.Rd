% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tskrr.R
\name{tskrr}
\alias{tskrr}
\title{Fitting a two step kernel ridge regression}
\usage{
tskrr(
  y,
  k,
  g = NULL,
  lambda = 1e-04,
  testdim = TRUE,
  testlabels = TRUE,
  symmetry = c("auto", "symmetric", "skewed"),
  keep = FALSE
)
}
\arguments{
\item{y}{a label matrix}

\item{k}{a kernel matrix for the rows}

\item{g}{an optional kernel matrix for the columns}

\item{lambda}{a numeric vector with one or two values for the
hyperparameter lambda. If two values are given, the first one is
used for the k matrix and the second for the g matrix.}

\item{testdim}{a logical value indicating whether symmetry
and the dimensions of the kernel(s) should be tested.
Defaults to \code{TRUE}, but for large matrices
putting this to \code{FALSE} will speed up the function.}

\item{testlabels}{a logical value indicating wether the row- and column
names of the matrices have to be checked for consistency. Defaults to
\code{TRUE}, but for large matrices putting this to \code{FALSE} will
speed up the function.}

\item{symmetry}{a character value with the possibilities
"auto", "symmetric" or "skewed". In case of a homogeneous fit, you
can either specify whether the label matrix is symmetric or
skewed, or you can let the function decide (option "auto").}

\item{keep}{a logical value indicating whether the kernel hat
matrices should be stored in the model object. Doing so makes the
model object quite larger, but can speed up predictions in
some cases. Defaults to \code{FALSE}.}
}
\value{
a \code{\link[xnet:tskrr-class]{tskrr}} object
}
\description{
\code{tskrr} is the primary function for fitting a two-step kernel
ridge regression model. It can be used for both homogeneous and heterogeneous
networks.
}
\examples{

# Heterogeneous network

data(drugtarget)

mod <- tskrr(drugTargetInteraction, targetSim, drugSim)

Y <- response(mod)
pred <- fitted(mod)

# Homogeneous network

data(proteinInteraction)

modh <- tskrr(proteinInteraction, Kmat_y2h_sc)

Yh <- response(modh)
pred <- fitted(modh)

}
\seealso{
\code{\link{response}}, \code{\link{fitted}},
\code{\link{get_eigen}}, \code{\link{eigen2hat}}
}
