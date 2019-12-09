#' drug target interactions for neural receptors
#'
#' A dataset for examining the interaction between 54 drugs and 26
#' neural receptors. It consists of three different matrices.
#'
#' The dataset consists of the following objects :
#'
#' \itemize{
#'   \item drugTargetInteraction: a matrix indicating whether or not a
#'   certain drug compound interacts with a certain neural receptor.
#'   \item targetSim: a similarity matrix for the neural receptors.
#'   \item drugSim: a similarity matrix for the drugs
#' }
#'
#' The data originates from Yamanishi et al (2008) but was partly reworked
#' to be suitable for two-step kernel ridge regression. This is explained
#' in detail in  the \href{../doc/Preparation_example_data.html}{Preparation of
#' the example data} vignette.
#'
#' @format
#' \itemize{
#'   \item for drugTargetInteraction: a numeric matrix of 26 rows by
#' 54 columns.
#'   \item For drugSim: a numeric square matrix with 54 rows/columns.
#'   \item For targetSim: a numeric square matrix with 26 rows/columns.
#' }
#'
#' @references \href{https://doi.org/10.1093/bioinformatics/btn162}{Yamanishi et al, 2008} : Prediction of drug-target interaction networks from the
#' integration of chemical and genomic spaces.
#'
#' @source \url{https://doi.org/10.1093/bioinformatics/btn162}
#' @aliases drugtarget drugSim targetSim
"drugTargetInteraction"
