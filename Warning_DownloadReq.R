
Download.request <- function(failed = FALSE) {
  modalDialog(title = "The selected predictors are not available localy ",
    span('Would you like to download the raster files? '),
    footer = tagList(
      actionButton("cancel", "Cancel"),
      actionButton("ok", "OK"))
  )
}


arquivo_test <- c(1:length(arquivo))
for(i in c(1:length(arquivo))){
  arquivo_test[i] <- arquivo[[i]]
}

if (input$resolucao != "30s") {
  if (any(file.exists(arquivo_test) != T)) {
    
    showModal(Download.request())
    
  }
  
  observeEvent(input$ok, {
    
    removeModal()
    zip_current <- paste0("bio_", input$resolucao, "_bil.zip")
    url <- paste0(
      "http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/",
      zip_current)
    download.file(url, zip_current, mode = "wb")
    unzip(zip_current, exdir = path)
    unlink(zip_current)
  })
}

if (input$resolucao == "30s") {
  if (any(file.exists(arquivo_test) != T)) {
    
    showModal(Download.request())
    
  }
  observeEvent(input$ok, {
    
    removeModal()
    
    url1 <- "http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/bio1-9_30s_bil.zip"
    url2 <- "http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/bio10-19_30s_bil.zip"
    
    if (any(
      input$Bio1, input$Bio2, input$Bio3, input$Bio4, input$Bio5,
      input$Bio6, input$Bio7, input$Bio8, input$Bio9
    )) {
      download.file(url1, "bio_30s1.zip", mode = "wb")
      unzip("bio_30s1.zip", exdir = path)
      unlink("bio_30s1.zip")
      files <- paste0(path, "/", list.files(path))
      file.rename(from = files, to = sub(
        pattern = "bio_", replacement = "bio",
        files))
    }
    
    if (any(
      input$Bio10, input$Bio11, input$Bio12, input$Bio13, input$Bio14,
      input$Bio15, input$Bio16, input$Bio17, input$Bio18, input$Bio19
    )) {
      download.file(url2, "bio_30s2.zip", mode = "wb")
      unzip("bio_30s2.zip", exdir = path)
      unlink("bio_30s2.zip")
      files <- paste0(path, "/", list.files(path))
      file.rename(from = files, to = sub(
        pattern = "bio_", replacement = "bio",
        files))
    }
    
  })
}
