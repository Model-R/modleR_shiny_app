if (any(checkfiles_future == F)) {
  zip_future_wc <- paste0(input$gcm_future_wc, input$rcp, "bi", year, ".zip")
  
  if (input$resolution == "2-5m") {
    url <- paste0( "http://biogeo.ucdavis.edu/data/climate/cmip5/2_5m", "/", zip_future_wc)
  }
  
  if (input$resolution != "2-5m") {
    url <- paste0("http://biogeo.ucdavis.edu/data/climate/cmip5/", input$resolution, "/", zip_future_wc)
  }
  download.file(url,zip_future_wc, mode = "wb")
  unzip(zip_future_wc, exdir = path_future)
  unlink(zip_future_wc)
}

if (any(checkfiles_past == F)) {
  url <- paste0( "http://biogeo.ucdavis.edu/data/climate/cmip5/",input$gcm_past_wc,"/",zip_past_wc)
  zip_past_wc <- paste0(input$gcm_past_wc, input$past_dates_wc, "bi_", input$resolution, ".zip")
  download.file(url, zip_past_wc, mode = "wb")
  unzip(zip_past_wc, exdir = path_past)
  unlink(zip_past_wc)
}


if (any(checkfiles_current == F)) {
  if (input$resolution != "30s") {
    zip_current <- paste0("bio_", input$resolution, "_bil.zip")
    url <- paste0("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/", zip_current)
    download.file(url, zip_current, mode = "wb")
    unzip(zip_current, exdir = path_current)
    unlink(zip_current)
  }
  if (input$resolution == "30s") {
    group1 <- c('bio1', 'bio2', 'bio3','bio4','bio5','bio6','bio7', 'bio8','bio9')
    group2 <- c('bio10', 'bio11', 'bio12','bio13','bio14','bio15','bio16','bio17', 'bio18','bio19')
    
    if (any(group1 %in% input$pred_vars_wc)) {
      download.file("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/bio1-9_30s_bil.zip", 
        "bio_30s1.zip", mode = "wb")
      unzip("bio_30s1.zip", exdir = path_current)
      unlink("bio_30s1.zip")
      files <- paste0(path_current, "/", list.files(path_current))
      file.rename(from = files, to = sub(
        pattern = "bio_", replacement = "bio",
        files
      ))
    }
    if (any(group2 %in% input$pred_vars_wc)) {
      download.file("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/bio10-19_30s_bil.zip", 
        "bio_30s2.zip", mode = "wb")
      unzip("bio_30s2.zip", exdir = path_current)
      unlink("bio_30s2.zip")
      files <- paste0(path_current, "/", list.files(path_current))
      file.rename(from = files, to = sub(
        pattern = "bio_", replacement = "bio",
        files
      ))
    }
  }
}
