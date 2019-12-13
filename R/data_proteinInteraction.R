#' Protein interaction for yeast
#'
#' A dataset for examining the interaction between proteins of
#' yeast. The dataset consists of the following objects:
#'
#' \itemize{
#'   \item proteinInteraction: the label matrix based on the protein
#'   network taken from the KEGG/PATHWAY database
#'   \item Kmat_y2h_sc: a kernel matrix indicating similarity of proteins.
#' }
#'
#' The proteins in the dataset are a subset of the 769 proteins
#' used in Yamanishi et al (2004). The kernel matrix used is the
#' combination of 4 kernels: one based on expression data, one
#' on protein interaction data, one on localization data and one
#' on phylogenetic profile. These kernels and their combination are
#' also explained in Yamanishi et al (2004).
#'
#' @format
#' \itemize{
#'   \item proteinInteraction: a numeric square matrix with 150 rows/columns
#'   \item Kmat_y2h_sc: a numeric square matrix with 150 rows/columns
#' }
#'
#' @references \href{https://doi.org/10.1093/bioinformatics/bth910}{Yamanishi et al, 2004}: Protein network inference from multiple genomic data: a supervised approach.
#'
#' @source \url{https://doi.org/10.1093/bioinformatics/bth910}
#' @aliases Kmat_y2h_sc
"proteinInteraction"
