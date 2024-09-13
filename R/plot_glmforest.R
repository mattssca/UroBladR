#' @title GLM Forest Plot.
#'
#' @description Construct a forest plot using GLM data for a set of signature scores.
#'
#' @details This function internally calls `get_glmscore`, which internally 
#' calls `int_predwrangler`. This happens if the user does not call the plotting 
#' function using the `this_glm` parameter. Many of the parameters in this 
#' function are recycled in the internal funciton calls to improve user flexibility.
#' The only parameters that are directly called by this function is; `plot_title`,
#' `all_subs` and `this_glm`. It's possible to return a forest plot for all 
#' subtypes within the specified `subtype_class`. To do so, set `all_subs = TRUE`. 
#' This will trigger a loop for which `get_glmscore` is internally called for each subtype.
#' To return a plot for just one subtype, set `all_subs = FALSE` and specify the 
#' subtype and class with `this_subtype` and `subtype_class`. To return a plot 
#' for more than one subtype, but not all, call get_glmscore for each itteration 
#' of subtypes, see last example in the docs.
#'
#' @param these_predictions Required parameter if `this_glm` is not provided. 
#' Should be output from [LundTax2023Classifier::predict_LundTax2023()].
#' @param these_samples_metadata Required parameter if `this_glm`is not provided. 
#' Metadata associated with he prediction output. Also possible for the user to 
#' provide a metadata subset with samples of interest, the return will be 
#' restricted to the samples within the specified group
#' @param this_glm Required paraemter if `these_predictions` and `these_samples_metadata` 
#' is not provided. The output from `get_glmscores`.
#' @param plot_title Title for plot.
#' @param all_subs Boolean, default is FALSE. Set to TRUE to return a forest plot 
#' for all subtypes within the specified class (`subtype_class`).
#' @param subtype_class Can be one of the following; 5class or 7class. Default is 5class.
#' @param cat_variable Required parameter if `this_glm`is not provided. 
#' This should be the categorical variable that is intended for testing. 
#' In addition, this should also be a variable of type factor, with exactly 2 levels.
#' @param this_subtype Required parameter if `this_glm` is not provided, see `get_glmscores`.
#' @param sample_id_col Required parameter if `this_glm` is not provided, see `get_glmscores`.
#' @param row_to_col Required parameter if `this_glm` is not provided, see `get_glmscores`.
#'
#' @return A forest plot as grub object.
#'
#' @import ggplot2 dplyr
#'
#' @export
#'
#' @examples
#' #load pacakges
#' library(dplyr, ggplot2)
#' 
#' #plot using prediction and metadata, Uro
#' plot_glmforest(these_predictions = swog_pred,
#'                these_samples_metadata = swog_meta,
#'                plot_title = "Uro, pT0",
#'                all_subs = FALSE,
#'                subtype_class = "5class", 
#'                this_subtype = "Uro",
#'                cat_variable = "pT0",
#'                sample_id_col = "ALTPATID")#'
#'
#' #plot all subtypes within the 7 class
#' plot_glmforest(these_predictions = swog_pred,
#'                these_samples_metadata = swog_meta,
#'                plot_title = "All 7class",
#'                all_subs = TRUE,
#'                subtype_class = "7class", 
#'                cat_variable = "pT0",
#'                sample_id_col = "ALTPATID")#'
#' 
#' #plot Uro and GU
#' #get Uro glms
#' uro_glm = get_glmscores(these_predictions = swog_pred,
#'                         these_samples_metadata = swog_meta,
#'                         subtype_class = "5class",
#'                         this_subtype = "Uro",
#'                         cat_variable = "pT0",
#'                         sample_id_col = "ALTPATID")#'
#'
#' #get GU glms
#' gu_glm = get_glmscores(these_predictions = swog_pred,
#'                        these_samples_metadata = swog_meta,
#'                        subtype_class = "5class",
#'                        this_subtype = "GU",
#'                        cat_variable = "pT0",
#'                        sample_id_col = "ALTPATID")#'
#'
#' #draw plot
#' plot_glmforest(this_glm = rbind(uro_glm, gu_glm))
#'
plot_glmforest = function(these_predictions = NULL,
                          these_samples_metadata = NULL,
                          this_glm = NULL,
                          plot_title = "My Plot",
                          all_subs = FALSE,
                          subtype_class = "5class",
                          cat_variable = NULL,
                          this_subtype = NULL,
                          sample_id_col = NULL, 
                          row_to_col = FALSE){
  #checks
  if(length(this_subtype) > 1){
    stop("If you want more than one subtype (but not all), it's recommended to provide these as this_glm")
  }
  
  if(!is.null(this_subtype)){
    if(!this_subtype %in% names(lund_colors$lund_colors)){
      stop("Please check spelling of subtype...")
    }
  }

  #run get_glm if user has provided prediction data and not glm object
  #if not all subtypes are requested
  if(!is.null(these_predictions) && is.null(this_glm)){
    if(!all_subs){
      this_glm = get_glmscores(these_predictions = these_predictions,
                               these_samples_metadata = these_samples_metadata,
                               subtype_class = subtype_class,
                               cat_variable = cat_variable,
                               this_subtype = this_subtype,
                               sample_id_col = sample_id_col, 
                               row_to_col = row_to_col)
    }else{
      if(subtype_class == "5class"){
        uro_glms = get_glmscores(these_predictions = these_predictions,
                                 these_samples_metadata = these_samples_metadata,
                                 subtype_class = "5class",
                                 cat_variable = cat_variable,
                                 this_subtype = "Uro",
                                 sample_id_col = sample_id_col, 
                                 row_to_col = row_to_col)

      }else if(subtype_class == "7class"){
        uroa_glms = get_glmscores(these_predictions = these_predictions,
                                  these_samples_metadata = these_samples_metadata,
                                  subtype_class = subtype_class,
                                  cat_variable = cat_variable,
                                  this_subtype = "UroA",
                                  sample_id_col = sample_id_col, 
                                  row_to_col = row_to_col)

        urob_glms = get_glmscores(these_predictions = these_predictions,
                                  these_samples_metadata = these_samples_metadata,
                                  subtype_class = subtype_class,
                                  cat_variable = cat_variable,
                                  this_subtype = "UroB",
                                  sample_id_col = sample_id_col, 
                                  row_to_col = row_to_col)

        uroc_glms = get_glmscores(these_predictions = these_predictions,
                                  these_samples_metadata = these_samples_metadata,
                                  subtype_class = subtype_class,
                                  cat_variable = cat_variable,
                                  this_subtype = "UroC",
                                  sample_id_col = sample_id_col, 
                                  row_to_col = row_to_col)
      }

      gu_glms = get_glmscores(these_predictions = these_predictions,
                              these_samples_metadata = these_samples_metadata,
                              subtype_class = subtype_class,
                              cat_variable = cat_variable,
                              this_subtype = "GU",
                              sample_id_col = sample_id_col, 
                              row_to_col = row_to_col)

      basq_glms = get_glmscores(these_predictions = these_predictions,
                                these_samples_metadata = these_samples_metadata,
                                subtype_class = subtype_class,
                                cat_variable = cat_variable,
                                this_subtype = "BaSq",
                                sample_id_col = sample_id_col, 
                                row_to_col = row_to_col)

      scne_glms = get_glmscores(these_predictions = these_predictions,
                                these_samples_metadata = these_samples_metadata,
                                subtype_class = subtype_class,
                                cat_variable = cat_variable,
                                this_subtype = "ScNE",
                                sample_id_col = sample_id_col, 
                                row_to_col = row_to_col)

      mes_glms = get_glmscores(these_predictions = these_predictions,
                               these_samples_metadata = these_samples_metadata,
                               subtype_class = subtype_class,
                               cat_variable = cat_variable,
                               this_subtype = "Mes",
                               sample_id_col = sample_id_col, 
                               row_to_col = row_to_col)

      if(subtype_class == "5class"){
        this_glm = rbind(uro_glms, gu_glms, basq_glms, scne_glms, mes_glms)
      }else if(subtype_class == "7class"){
        this_glm = rbind(uroa_glms, urob_glms, uroc_glms, gu_glms, basq_glms, scne_glms, mes_glms)
      }
    }
  }else if(is.null(these_predictions) && !is.null(this_glm)){
    message("Both predictions and GLM object are provided, function will plot the provided GLM...")
  }

  #build plot
  forestplot = ggplot(data = this_glm, aes(x = score, y = odds_ratio, ymin = conf_2.5, ymax = conf_97.5, color = subtype)) +
    geom_hline(yintercept = 1, lty = 2) +
    geom_pointrange(position = position_dodge(width = 0.5)) +
    coord_flip() +
    xlab("Signature") +
    ylab("Odds Ratio") +
    theme_bw() +
    scale_color_manual(values = lund_colors$lund_colors) +
    labs(title = plot_title) +
    theme(legend.title = element_blank())

  print(forestplot)
  return(forestplot)
}
