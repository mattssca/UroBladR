#' @title Get DEG Score.
#'
#' @description Return a DEG object with score for each gene.
#'
#' @details See parameter descriptions for more info, still wip.
#'
#' @param subtypes Optional parameter.Allows the user to subset samples to a specific subtype of interest. To be used together with `this_variable`.
#' @param expression_data Required. A data frame with expression values of interest.
#' @param this_metadata Required. A data frame with metadata for the samples in the `expression_data` object.
#' @param exclude Optional, Lets the user exclude samples of interest. Should be a vector with sample IDs available in the metadata.
#' @param plot Boolean parameter, set to TRUE to output plot. Still WIP, not yet available...
#' @param out_path Required if `write_degs = TRUE`, target path to exported DEGs, otherwise disregarded.
#' @param write_degs Boolean parameter, default is set to FALSE. Set to TRUE for exporting DEGs, if set to TRUE, `out_path` also needs to be specified.
#' @param this_variable Required if `subtypes` is defined. Should be the name of the column in the metadata holding subtype classification. Otherwise not needed.
#' @param sample_id_col Required parameter, should be the name of the column in the metadata data frame with sample IDs.
#' @param return_this Decides what output is returned, could be one of the following; sample_ids, metadata, expressions, eset, design_matrix, degs, all, and nothing
#'
#' @return Data frame with DEGs as well as ranked score.
#'
#' @import dplyr limma
#' @rawNamespace import(Biobase, except = combine)
#'
#' @export
#'
#' @examples
#' #load packages
#' library(limma)
#' library(Biobase)
#'
#' ## 1. Read and format data
#' #read expression data
#' load("C:/Users/matts/Desktop/projects/spp1/SWOG_GEX_ProteinCoding_without_duplica.Rdata", )
#'
#' #convert to data frame
#' swog_expressions = as.data.frame(SWOG_matrix)
#'
#' #read in metadata
#' swog_meta = read.table("C:/Users/matts/Desktop/projects/spp1/S1314 metadata.txt", sep = "\t", header = TRUE)
#'
#' #Return DEGs for Uro
#' test_df = degscore(expression_data = swog_expressions,
#'                    this_metadata = swog_meta,
#'                    subtypes = "Uro",
#'                    return_this = "degs")
#'
degscore = function(subtypes = NULL,
                    expression_data = swog_expressions,
                    this_metadata = swog_meta,
                    exclude = NULL,
                    plot = FALSE,
                    out_path = NULL,
                    write_degs = FALSE,
                    this_variable = "LundTax2023_simple",
                    sample_id_col = "ALTPATID",
                    return_this = "degs"){

  if(write_degs && is.null(out_path)){
    stop("No output path provided...")
  }

  if(!is.null(subtypes)){
    #subset metadata to sample IDs based on the subtype
    message("Fiiltering samples based on selected subtype...")
    my_metadata = dplyr::filter(this_metadata, !!as.symbol(this_variable) %in% subtypes)
  }else{
    message("No subtype is defined, the function will use all sample IDs available in the metadata...")
    my_metadata = this_metadata
  }

  #return the sample IDs for the selected subtype
  my_samples = my_metadata %>%
    pull(sample_id_col)

  if(!is.null(exclude)){
    my_samples[!my_samples %in% exclude]
    message(paste0("The following samples are excluded: ", exclude))
  }

  #return the expression matric for the selected subtypes
  my_expr = dplyr::select(expression_data, my_samples)

  #generate an ExpressionSet (Biobase) for the selected subtypes
  my_eset = ExpressionSet(assayData = as.matrix(my_expr))

  #get the design matrix
  my_pCR = factor(my_metadata$pT0)
  my_design = model.matrix(~my_pCR)

  message("Calculate DEGs (moderated t-test) for pCR overall")

  fit = lmFit(my_eset,
              my_design)

  fit = eBayes(fit,
               trend = TRUE,
               robust = TRUE)

  results = decideTests(fit)

  summary(results)

  my_degs = topTable(fit,
                     coef = "my_pCR1",
                     n = nrow(my_expr))

  #add rank
  order.t = order(decreasing = TRUE, my_degs$t)
  my_degs = my_degs[order.t,]
  my_degs$rank = seq.int(nrow(my_degs))

  if(write_degs){
    write.table(my_degs,
                file = paste0(out_path, "DEGs.txt"),
                sep = "\t",
                col.names = TRUE)
  }

  if(plot){
    plotMD(fit,
             coef = "my_pCR",
             status = results[,5],
             values = c(1,-1),
             hl.col = c("red","blue"))
  }

  #deal with returns
  if(return_this == "sample_ids"){
    message(paste0(length(my_samples), " sample IDs returned for ", subtypes))
    return(my_samples)
  }else if (return_this == "metadata"){
    message(paste0("Metadata returned for the following subtypes;  ", subtypes))
    return(my_metadata)
  }else if(return_this == "expressions"){
    message(paste0("Expression values returned for ", subtypes))
    return(my_expr)
  }else if(return_this == "eset"){
    message(paste0("ExpressionSet returned for ", subtypes))
    return(my_eset)
  }else if(return_this == "design_matrix"){
    message(paste0("Design matrix returned for ", subtypes))
    return(my_design)
  }else if(return_this == "degs"){
    message(paste0("DEGs returned for ", subtypes))
    return(my_degs)
  }else if(return_this == "all"){
    message(paste0("All data returned for ", subtypes))
    my_list = list(sample_ids = my_samples,
                   metadata = my_metadata,
                   expression_data = my_expr,
                   eset = my_eset,
                   design = my_design,
                   degs = my_degs)
  }else if(return_this == "nothing"){
    message("Nothing is returned")
    return()
  }else{
    stop("Possible values for return_this are; sample_ids, metadata, expressions, eset, design_matrix, degs, all, and nothing...")
  }
}
