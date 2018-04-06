if (input$tipodadoabiotico == "CLIMA") {
  arquivo <- list()
  arquivo.future <- list()
  arquivo.past <- list()
  
  check.files <- list()
  check.files_future <- list()
  check.files_past <- list()
  
  path <- paste0(getwd(), "/ex/clima/current/", input$resolution)
  path_future <- paste0(getwd(), "/ex/clima/", input$future_wc_dates, "/",input$resolution, "/", input$gcm_future_wc, "/", input$rcp)
  path_past <- paste0(getwd(), "/ex/clima/", input$past_wc_dates, "/",input$resolution, "/", input$gcm_past_wc)
  
  pred_lay_wc <- paste(input$pred_vars_wc)
  
  if(length(input$pred_vars_wc) > 0) {
    for(i in c(1:length(input$pred_vars_wc))){
      add.layer <- paste0(path, "/", pred_lay_wc[i], ".bil")
      arquivo <- c(arquivo, add.layer)
      check.files <- c(check.files, file.exists(add.layer))
    }
    
    if (!is.null(input$forecasting_wc) ) {
      
      if ('future_wc' %in% input$forecasting_wc) {
        group_predvars$future <- TRUE
        year <<- sub(".*(\\d+{2}).*$", "\\1", input$periodo)
        for(i in c(1:length(input$pred_vars_wc))){
          nbi<- sub("bio", "" ,  pred_lay_wc[i])
          add.layer <-paste0(path_future, "/", input$gcm_future_wc, input$rcp, "bi",year, nbi, ".tif")
          arquivo_future <- c(arquivo_future, add.layer.future) 
          check.files_future <- c(check.files_future, file.exists(add.layer.future))
        }
      } #future
      
      if ('past_wc' %in% input$forecasting_wc) {
        group_predvars$past <- TRUE
        for(i in c(1:length(input$pred_vars_wc))){
          nbi <- sub("bio", "" ,  pred_lay_wc[i])
          add.layer <-paste0(path_past, "/", input$gcm_past_wc, input$past_wc_dates, "bi", nbi, ".tif")
          arquivo_past <- c(arquivo_past, add.layer.past) 
          check.files_past <- c(check.files_past, file.exists(add.layer.past))
        }
      } #past
    } #forcasting
  }
  
# Check whether the raster files exist and if not download from WC database
if (any(check.files == F)) {
  if (input$resolution != "30s") {
    zip_current <- paste0("bio_", input$resolution, "_bil.zip")
    url <- paste0(
      "http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/",
      zip_current
    )
    download.file(url, zip_current, mode = "wb")
    unzip(zip_current, exdir = path)
    unlink(zip_current)
  }
  
  if (input$resolution == "30s") {
    group1 <- c('bio1', 'bio2', 'bio3','bio4','bio5','bio6','bio7', 'bio8','bio9')
    group2 <- c('bio10', 'bio11', 'bio12','bio13','bio14','bio15','bio16','bio17', 'bio18','bio19')
    
    if (any(group1 %in% input$pred_vars_wc)) {
      download.file("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/bio1-9_30s_bil.zip", 
        "bio_30s1.zip", mode = "wb")
      unzip("bio_30s1.zip", exdir = path)
      unlink("bio_30s1.zip")
      files <- paste0(path, "/", list.files(path))
      file.rename(from = files, to = sub(
        pattern = "bio_", replacement = "bio",
        files
      ))
    }
    
    if (any(group2 %in% input$pred_vars_wc)) {
      download.file("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/bio10-19_30s_bil.zip", 
        "bio_30s2.zip", mode = "wb")
      unzip("bio_30s2.zip", exdir = path)
      unlink("bio_30s2.zip")
      files <- paste0(path, "/", list.files(path))
      file.rename(from = files, to = sub(
        pattern = "bio_", replacement = "bio",
        files
      ))
    }
  }
}
if (any(check.files_future == F)) {
  zip_future_wc <- paste0(input$gcm_future_wc, input$rcp, "bi", year, ".zip")
  
  if (input$resolucao == "2-5m") {
    url <- paste0( "http://biogeo.ucdavis.edu/data/climate/cmip5/2_5m", "/", zip_future)
  }
  if (input$resolucao != "2-5m") {
    url <- paste0("http://biogeo.ucdavis.edu/data/climate/cmip5/", input$resolution, "/", zip_future
    )
  }
  download.file(url, zip_future, mode = "wb")
  unzip(zip_future, exdir = path_future)
  unlink(zip_future)
}
if (any(check.files_past == F)) {
  
  url <- paste0( "http://biogeo.ucdavis.edu/data/climate/cmip5/",input$gcm_past_wc,"/",zip_past_wc)
  
  if (input$resolucao == "2-5m") {
    zip_past_wc <- paste0(input$gcm_past_wc, input$past_wc_dates, "bi_2-5m", ".zip")
  }
  if (input$resolucao != "2-5m") {
    zip_past_wc <- paste0(input$gcm_past_wc, input$past_wc_dates, "bi_", input$resolution, ".zip")
  }
  download.file(url, zip_past_wc, mode = "wb")
  unzip(zip_past_wc, exdir = path_past)
  unlink(zip_past_wc)
  }

  group_predvars$selecionado <- TRUE
  group_predvars$data <- arquivo
  group_predvars$data_future <- arquivo_future
  group_predvars$data_past <- arquivo_past
}