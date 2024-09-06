#' @title Get Isos
#'
#' @description Subset a data frame with expression values for different genes and isoforms to only
#' include a set number of genes/isoforms for selected sample IDs.
#'
#' @details Takes an expression matrix and subsets to sample IDs of interest for the selected genes/isoforms.
#' Additionally, the function can also return a basic box plot with the returned data.
#' To ensure compatibility (i.e column names and data types) of annotations and expression matrix format,
#' please see the bunlded data object.
#'
#' @param these_sample_ids Optional, a vector of multiple sample IDs (or a single sample ID as a string)
#' that you want results for.
#' @param these_samples_metadata  Optional, a metadata table (with sample IDs in a column) to subset
#' the return to. If not provided (and if `these_sample_ids` is not provided), the function will
#' return all samples.
#' @param this_data Optional parameter for specifying the expression matrix of interest.
#' If not provided, the function will use a bundled data object for this purpose.
#' @param annotations Gene annotations. A data frame with txID, GeneID, and Symbol as columns.
#' @param my_genes Optional. A list with genes of interest. If provided the function will subset to
#' all isoforms for the selected gene(s)
#' @param these_isoforms Required if not `my_genes` is provided. A vector of characters with isoforms
#' of interest in Gene ID format.
#' @param verbose Boolean parameter. Set to FALSE to minimize output to console. Default is TRUE.
#' @param to_frac Boolean parameter. If TRUE (default), each fraction per isoform for each sample
#' will be reported. If `return_plot` is set to TRUE, this parameter will auto-default to TRUE.
#' To get back TPM values in the data frame, first set `return_plot = FALSE`.
#' @param return_all Boolean parameter. Set to TRUE to return use all sample IDs in the provided
#' dataset with `this_data`. Default is FALSE.
#' @param return_plot Boolean parameter. Set to TRUE (default) to return box plot for selected
#' isoforms and samples.
#' @param plot_title Optional parameter for naming the returned plot (if return_plot = TRUE).
#' @param plot_subtitle Optional parameter for adding a subtitle to the returned plot
#' (if return_plot = TRUE).
#' @param my_color
#'
#' @return A data frame with gene IDs and the corresponding GEX as rows and sample IDs in the columns.
#'
#' @import dplyr tidyr ggplot2 tibble
#' @rawNamespace import(reshape, except = c(rename, expand))
#' @rawNamespace import(stats, except = c(filter, lag))
#'
#' @export
#'
#' @examples
#' #get samples
#' my_samples = colnames(expression_sub)[-1]
#'
#' #run function
#' these_isos = get_isos(this_data = expression_sub,
#'                       annotations = gene_annotations,
#'                       these_sample_ids = my_samples,
#'                       these_isoforms = c("ENST00000395080",
#'                                          "ENST00000237623",
#'                                          "ENST00000360804",
#'                                          "ENST00000508233",
#'                                          "ENST00000681973"),
#'                       plot_title = "SPP1",
#'                       plot_subtitle = "Isoforms Frequency")
#'
get_isos = function(these_sample_ids = NULL,
                    these_samples_metadata= NULL,
                    this_data,
                    annotations,
                    my_genes,
                    these_isoforms,
                    verbose = TRUE,
                    to_frac = TRUE,
                    return_all = FALSE,
                    return_plot = TRUE,
                    plot_title = "My Plot",
                    plot_subtitle = "My subtitle",
                    my_color = "#499851"){

  #deal with nonsensical parameter combinations
  if(return_plot){
    to_frac = TRUE
  }

  #check the sample IDs and metadata
  if(is.null(these_sample_ids) && is.null(these_samples_metadata)){
    message("WARNING! You have not provided any sample IDs or metadata to subset return to...")
    message("This function will retreive all sample IDs available in the this_data object...")
    these_samples = colnames(this_data)[-1]
    if(verbose){
      message(paste0(length(these_samples), " Samples found in the provided dataset..."))
    }
  }else if(is.null(these_sample_ids) && !is.null(these_samples_metadata)){
    if(!"sample_id" %in% colnames(these_samples_metadata)){
      stop("The provided metadata has no column named 'sample_id'...")
    }else{
      these_samples = these_samples_metadata$sample_id
    }
  }else if(is.null(these_samples_metadata) && !is.null(these_sample_ids)){
    these_samples = these_sample_ids
  }else{
    message("Both these_sample_ids and these_samples_metadata are provided, the function will use
            the sample IDs available in these_samples_metadata...")
    these_samples = these_samples_metadata$sample_id
  }

  #ensure incoming data is in data frame format
  this_data = as.data.frame(this_data)

  #get sample IDs from the provided data set
  data_samples = colnames(this_data)[-1]

  #subset to unavailable samples
  not_in_data = setdiff(these_samples, data_samples)

  #check if the sample IDs are available in the provided data set
  if(length(not_in_data) > 0){
    message("WARNING! The following samples were not found in the provided dataset:")
    print(not_in_data)
    message("The above sample(s) will not be included in the return...")

    #remove samples not available in the incoming data
    these_samples = these_samples[!these_samples %in% not_in_data]
  }

  #get gene IDs for provided gene(s)
  if(missing(these_isoforms) && !missing(my_genes)){
    if(verbose){
      message("Retreiving Gene IDs for isoforms of the selected gene(s)...")
    }

    these_genes = dplyr::filter(annotations, gene_symbol %in% my_genes)
    isoforms = these_genes$isoform

    if(verbose){
      message(paste0(length(isoforms), " Isoforms found for ", my_genes))
    }
  }else if(!missing(these_isoforms) && missing(my_genes)){

    #get isoforms from the supplied annotation set
    all_isos = annotations$isoform

    #find out what isoforms are not in the annotations data set
    no_iso = setdiff(these_isoforms, all_isos)

    isoforms = these_isoforms

    if(length(no_iso) > 0){
      message("WARNING! The following isoforms were not found in the annotations data:")
      print(no_iso)
      message("The above isoforms will not be included in the return...")

      #remove isoforms not available in the incoming data
      isoforms = isoforms[!isoforms %in% no_iso]
    }
  }

  #filter out the isoforms of interest
  filtered_data = subset(this_data, entrez_id %in% isoforms)

  #subset to samples of interest
  filtered_data = dplyr::select(filtered_data, entrez_id, all_of(these_samples))

  #convert to percentages, if wanted
  if(to_frac){
    filtered_data = filtered_data %>%
      dplyr::mutate(across(where(is.numeric), prop.table))
  }

  #plot
  if(return_plot){

    #transpose column name to row name column
    plot_df = filtered_data  %>%
      melt(id.var = "entrez_id") %>%
      dplyr::rename(isoform = entrez_id, sample_id = variable, frac = value)

    #convert to factors
    plot_df$isoform = as.factor(plot_df$isoform)

    #build plot
    my_plot = ggplot(data = plot_df, aes(x = reorder(isoform, -frac), y = frac)) +
      geom_boxplot(varwidth = TRUE, fill = my_color) +
      labs(title = plot_title,
           subtitle = plot_subtitle,
           caption = paste0("Samples (n): ", length(these_samples)),
           x = "Gene Isoform",
           y = "") +
      theme_minimal() +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

    #print plot
    print(my_plot)

    if(verbose){
      message("Box plot successfully printed!")
    }
  }

  #return data frame
  return(filtered_data)
}
