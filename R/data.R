#' UroBladR
#' A collection of analysis tools in R, directed towards research in the urothelial cancer field.
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

#' SWOG Expressions.
#'
#' Example gene expression dataset. Obtained from the Southwest Oncology Group (SWOG).
#' doi: 10.1158/1078-0432.CCR-23-0602.
#'
#' A data frame with samples in columns and expression values in rows with gene
#' names (HGNC symbols) as row names.
#'
#' \itemize{
#' }
#'
#' @docType data
#' @keywords datasets
#' @name swog_exp
#' @usage data(swog_exp)
#' @format A data frame with 15986 rows (genes) and 163 columns (samples).
NULL

#' SWOG Metadata.
#'
#' Metadata associated with the bundled expression dataset.Obtained from the
#' Southwest Oncology Group (SWOG). doi: 10.1158/1078-0432.CCR-23-0602.
#'
#' A data frame with samples in rows and metadata variables in the columns.
#'
#' \itemize{
#'  \item ALTPATID. Unique sample ID.
#'  \item ARMNAME.
#'  \item STRAT1.
#'  \item STRAT2.
#'  \item AGE.
#'  \item SEX.
#'  \item RACE.
#'  \item prim_analysis.
#'  \item itt_analysis.
#'  \item adequate_tiss.
#'  \item ANY_TRT.
#'  \item GFS.
#'  \item adewuate_chemo.
#'  \item CYSTECTOMY.
#'  \item PATHSTHT.
#'  \item PATHSTGN.
#'  \item PATHSTGM.
#'  \item NLYMPHN.
#'  \item NPOSNOD.
#'  \item PATH_RESP.
#'  \item pT0.
#'  \item OS_IND.
#'  \item OS_TIM.
#'  \item PFS_IND.
#'  \item PFS_TIM.
#'  \item TT_offtx.
#'  \item TT_cyst.
#'  \item GC_FAV.
#'  \item ddMVAC_FAV.
#'  \item GC_FAV_dup.
#'  \item ddMVAC_FAV_dup.
#'  \item GEO.
#'  \item tchagrp.
#'  \item tcga.
#'  \item LundTax2023_simple.
#'  \item consensus.
#'  \item mda.
#'  \item mdagrp.
#'  \item poor_quality.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name swog_meta
#' @usage data(swog_meta)
#' @format A data frame with 163 rows (samples) and 39 columns (metadata).
NULL
