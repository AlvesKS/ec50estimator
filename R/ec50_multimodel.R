#' Calculate EC50 for one or more isolates with one or several models.
#' @importFrom magrittr %>%
#' @export ec50_multimodel


ec50_multimodel =  function(formula,
                            data,
                            EC_lvl = 50,
                            isolate_col,
                            strata_col= NULL,
                            fct,
                            interval = c("none", "delta", "fls", "tfls"),
                            type = c("relative", "absolute")
                            ){

  { if(missing(formula)){stop(gettextf("Please, specify the 'formula'"))}}
  { if(missing(data)){stop(gettextf("Missing 'data'"))}}
  { if(missing(isolate_col)){stop(gettextf("Missing 'isolate_col'"))}}
  { if(missing(fct)){stop(gettextf("Please, specify the 'fct'"))}}
  { if(is.list(fct)==FALSE){stop(gettextf("'fct' should be a list"))}}




  single_fun = function(j){
    models_descript = NULL
    model_text = fct[[j]]$text
    aa = drc::getMeanFunctions(display = F)
    models_df1 = as.data.frame(aa)
    colnames(models_df1) = models_df1[1,]
    models_df2 = models_df1[-1,] %>%
      tidyr::pivot_longer(1:ncol(models_df1),
                          names_to = "models",
                          values_to = "models_descript") %>%
      dplyr::filter(models_descript == model_text)
    model_name = models_df2$models

    box = data.frame()

    if(is.null(strata_col)){
      data_uni=data %>%
        dplyr::mutate(strata = "")
      strata_col= "strata"
    }else{
      data_uni = data %>%
        tidyr::unite(strata, tidyselect::all_of(strata_col), sep = "---")
    }

    STRATA = data_uni[["strata"]]
    strata = as.character(unique(STRATA))


    for(i in 1:length(strata)){

      rowi = data_uni[["strata"]]==strata[i]
      datai = data_uni[rowi,]


      ID = datai[[isolate_col]]
      id = as.character(unique(ID))

      for(k in 1:length(id)){
        rowk = datai[[isolate_col]]==id[k]
        datak = datai[rowk,]


        try({
          model = drc::drm(formula,  fct = fct[[j]] , data = datak)

          ed = drc::ED(model, EC_lvl, interval = interval, display = F, type = type)

          model_stats1  = drc::mselect(model, fctList = list(fct[[j]]))
          model_stats2 = as.data.frame(model_stats1)[1,]
          rownames(model_stats2) <- NULL
          model_stats2$model = model_name



          lil_box = data.frame(ID = as.character(id[k]), strata = as.character(strata[i]) ,ed) %>%
            tibble::remove_rownames() %>%
            dplyr::mutate(strata = as.character(strata),
                          ID = as.character(ID)) %>%
            tidyr::separate(strata, into = strata_col,sep = "---") %>%
            dplyr::bind_cols(model_stats2)

          box = box %>%
            dplyr::bind_rows(lil_box)

        },silent = T)


      }}

    computed_isolates =  unique(box$ID)
    all_isolates = as.character(unique(data[[isolate_col]]))

    true_false = !all_isolates %in% computed_isolates
    did_not = all_isolates[true_false]

    if(length(did_not)>0){
      print(paste0("Isolates = c(", toString(paste0("'",did_not,"'")), ") did not produced ec50 estimates due to error during fitting procedure", collapse=", "))
    }

    return(box)
  }

  list_dfs = lapply(1:length(fct), single_fun)
  df = dplyr::bind_rows(list_dfs)
  return(df)
}
