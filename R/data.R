#' isoploteR
#' Takes an expression matrix and subsets to sample IDs of interest for the selected genes/isoforms.
#' Additionally, the function can also return a basic box plot with the returned data.
#'
"_PACKAGE"

#' Gene Annotations.
#'
#' Gene annotations with isoform, Entrez Gene ID and gene symbol in HUGO format.
#'
#' A data frame with gene information in different formats.
#'
#' \itemize{
#'  \item isoform. Isoform annotation in Entrez format.
#'  \item entrez_id. Gene annotation in Entrez format.
#'  \item gene_symbol. Gene annotation in HUGO format.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name gene_annotations
#' @usage data(gene_annotations)
#' @format A data frame with 252894 rows (genes) and 3 columns (different gene formats).
NULL

#' Expression Matrix Subset.
#'
#' Example expression matrix subset to only include 10 samples.
#'
#' A data frame with samples in columns and expression values in rows (TPM).
#'
#' \itemize{
#'  \item entrz_id. Isoform annotation in Entrez format.
#'  \item X18KFU0001. Sample 1
#'  \item X18KFU0002. Sample 2
#'  \item X18KFU0003. Sample 3
#'  \item X18KFU0004. Sample 4
#'  \item X18KFU0006. Sample 5
#'  \item X18KFU0007. Sample 6
#'  \item X18KFU0008. Sample 7
#'  \item X18KFU0009. Sample 8
#'  \item X18KFU00010. Sample 9
#'  \item X18KFU00012. Sample 10
#' }
#'
#' @docType data
#' @keywords datasets
#' @name expression_sub
#' @usage data(expression_sub)
#' @format A data frame with 252894 rows (genes) and 11 columns.
NULL
