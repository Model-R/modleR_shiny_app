if (input$tipodadoabiotico == "CLIMA") {
  arquivo <- list()
  arquivo.future <- list()
  
  check.files <- list()
  check.files_future <- list()
  
  path <- paste0(getwd(), "/ex/clima/current/", input$resolution)
  path_future <- paste0(getwd(), "/ex/clima/", input$periodo, "/",input$resolucao, "/", input$gcm, "/", input$rcp)
  
  pred_lay_wc <- paste(input$pred_vars_wc)
  
  if(length(input$pred_vars_wc)>0){
    for(i in c(1:length(input$pred_vars_wc))){
      add.layer <- paste0(path, "/", pred_lay_wc[i], ".bil")
      arquivo <- c(arquivo, add.layer)
      check.files <- c(check.files, file.exists(add.layer))
      
      
      if (!is.null(input$forecasting_wc) ) {
       
         if ('future_wc' %in% input$forecasting_wc) {
          group_predvars$future <- TRUE
          year <- sub(".*(\\d+{2}).*$", "\\1", input$periodo)
          
          for(i in c(1:length(input$pred_vars_wc))){
            nbi<- sub("bio", "" ,  pred_lay_wc[i])
            add.layer.future <-paste0(pathfuturo, "/", input$gcm, input$rcp, "bi",
              year, nbi, ".tif"
              arquivo_future <- c(arquivo_future, add.layer.future) 
              check.files_future <- c(check.files_future, file.exists(add.layer.future))
          }
        }#future
      
        
        }
    }
    
  }
    group_predvars$selecionado <- TRUE
    
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
  group_predvars$data_future <- arquivo_future
  group_predvars$data <- arquivo
}