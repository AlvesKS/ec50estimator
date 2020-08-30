estimate_EC50 = function(formula, data, EC_lvl = 50, isolate_col, strata_col = NULL, fct,
                         interval = c("none", "delta", "fls", "tfls")){

  { if(missing(formula)){stop(gettextf("Please, specify the 'formula'"))}}
  { if(missing(data)){stop(gettextf("Missing 'data'"))}}
  { if(missing(isolate_col)){stop(gettextf("Missing 'isolate_col'"))}}
  { if(missing(fct)){stop(gettextf("Please, specify the 'fct'"))}}

  box = data.frame()

  if(is.null(strata_col)){
    data_uni=data %>%
      dplyr::mutate(strata = "")
    strata_col= "strata"
  }else{
    data_uni = data %>%
      tidyr::unite(strata, strata_col, sep = "---")
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
        model = drc::drm(formula,  fct =fct , data = datak)

        ed = drc::ED(model, EC_lvl, interval = interval, display = F)

        lil_box = data.frame(ID = as.character(id[k]), strata = as.character(strata[i]) ,ed) %>%
          tibble::remove_rownames() %>%
          dplyr::mutate(strata = as.character(strata),
                 ID = as.character(ID)) %>%
          tidyr::separate(strata, into = strata_col,sep = "---")
        box = box %>%
          dplyr::bind_rows(lil_box)

      },silent = T)


    }}

 computed_isolates =  unique(box$ID)

  true_false = !isolate_col %in% computed_isolates
  did_not = isolate_col[true_false]

  if(length(did_not)>0){
    print(paste0("Isolates = c(", toString(paste0("'",did_not,"'")), ") did not produced ec50 estimates due to error during fitting procedure", collapse=", "))
  }

  box
}

