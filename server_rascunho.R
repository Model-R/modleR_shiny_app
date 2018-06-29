#   CHECKING/ INSTALLING/ LOADING REQUIRED PACKAGES
ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}

ipak(
  c("devtools",
  "shinydashboard",
  "leaflet",
  "R.utils",
  "raster",
  "rjson",
  "maps",
  "rgdal",
  "raster",
  "dismo",
  "rgbif",
  "XML",
  "randomForest",
  "kernlab",
  "data.table",
  "DT",
  "shinyjs",
  "sdmpredictors")
  )

install_github("Model-R/modelr_pkg", build_vignettes = FALSE, ref="brt")

# Download and decompress maxent.jar file
jar <- paste0(system.file(package = "dismo"), "/java/maxent.jar")
if (file.exists(jar) != T) {
  url <- "http://biodiversityinformatics.amnh.org/open_source/maxent/maxent.php?op=download"
  download.file(url, destfile = "maxent.zip", mode = "wb")
  unzip("maxent.zip", files = "maxent.jar", exdir = system.file("java", package = "dismo"))
  unlink("maxent.zip")
}

jdk_version <- list.files("/Library/Java/JavaVirtualMachines/")
if (length(jdk_version) != 0) {
  dyn.load(paste0("/Library/Java/JavaVirtualMachines/", jdk_version, "/Contents/Home/lib/server/libjvm.dylib"))
} else{
  library("rJava")
}

library("ModelR", lib.loc="/Library/Frameworks/R.framework/Versions/3.4/Resources/library")


#   DEFINING DEFAULT VALUES
# home <- "/"
t <- 7
ext11 <<- ext12 <<- -90
ext21 <<- ext22 <<- -33
ext31 <<- ext32 <<- -32
ext41 <<- ext42 <<- 23

#ETAPA <<- 0
#spname <<- ""

#   DEFINING FUNTIONS
panel.reg <- function(x, y, bg = NA, cex = 1, col.regres = "red", ...) {
  points(x, y, cex = cex)
  abline(stats::lm(y ~ x), col = col.regres, ...)
}

panel.cor <- function(x, y, digits = 2, prefix = "", ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  text(0.5, 0.5, txt, cex = 1.5)
}

panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y / max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "gray", ...)
}

mkdirs <- function(fp) {
  if (!file.exists(fp)) {
    mkdirs(dirname(fp))
    dir.create(fp, showWarnings = FALSE, recursive = FALSE, mode = "777")
  }
}

getOccurrences_gbif <- function(spname) {
  key <- name_backbone(name = spname)$speciesKey
  if (!is.null(key)) {
    gbif_data <- occ_search(hasCoordinate = TRUE,
                            hasGeospatialIssue = F,
                            taxonKey = key,
                            return = "data" )
    gbif_data <- subset(gbif_data, !is.na(decimalLongitude) & !is.na(decimalLatitude))
    gbif_data <- subset(gbif_data, (decimalLongitude != 0) & (decimalLatitude != 0))
    occur.data <- cbind(gbif_data$name, gbif_data$decimalLongitude, gbif_data$decimalLatitude)
    colnames(occur.data) <- c("name", "longitude", "latitude")
    occur.data <- as.data.frame(occur.data)
    return(occur.data)
  } else {
    showModal(modalDialog(
      title = "No results!",
      paste0("Please insert a valid species scientific name."),
      easyClose = TRUE
    ))
  }
}

getOccurrences_jabot <- function(spname) {
  library("rjson")
  pTaxon <- gsub(" ", "_", spname)
  json_file <- paste0 ("https://model-r.jbrj.gov.br/execjabot.php?especie=", pTaxon)
  json_data <- fromJSON(file = json_file, method = "C")
  final_data <- do.call(rbind, json_data)
  jabot_data <- final_data[, c("taxoncompleto", "longitude", "latitude")]
  occur.data <- cbind(as.character(jabot_data[, 1]), as.numeric(jabot_data[, 2]), as.numeric(jabot_data[, 3]))
  colnames(occur.data) <- c("name", "longitude", "latitude")
  return(occur.data)
}

# limparResultadosAnteriores <- function() ({
#   lista <- list.files(paste0("www/", projeto, "/models/", full.names = T, pattern = paste0(".")))
#   if (length(lista > 0)) {
#     file.remove(paste0("www/", projeto, "/models/", lista))
#   }
#   lista <- list.files(paste0("www/", projeto, "/final/", full.names = T, pattern = paste0(".")))
#   if (length(lista > 0)) {
#     file.remove(paste0("www/", projeto, "/final/", lista))
#   }
#   lista <- list.files(paste0("www/", projeto, "/proj/", full.names = T, pattern = paste0(".")))
#   if (length(lista > 0)) {
#     file.remove(paste0("www/", projeto, "/proj/", lista))
#   }
#   lista <- list.files(paste0("www/", projeto, "/proj_time/", full.names = T, pattern = paste0(".")))
#   if (length(lista > 0)) {
#     file.remove(paste0("www/", projeto, "/proj_time/", lista))
#   }
#   lista <- list.files(paste0("www/", projeto, "/jpg/", full.names = T, pattern = paste0(".jpg")))
#   if (length(lista > 0)) {
#     file.remove(paste0("www/", projeto, "/jpg/", lista))
#   }
# })

maparesultado_model <- function(model_ensemble = model_ensemble,
  model_title = model_title) {
  if (file.exists(paste0("www/", projeto, "/final/", model_ensemble, ".tif"))) {
    r <- raster::raster(paste0("www/", projeto, "/final/", model_ensemble, ".tif"))
    pal <- colorNumeric(c("#FFFFFF", "#FDBB84", "#31A354"), values(r), na.color = "transparent")
    occ_points <<- coordinates(occurrences)
    lng <<- occ_points[,1]
    lat <<-  occ_points[,2]
    map <- leaflet() %>%
      addTiles() %>%
      addRasterImage(r, colors = pal, opacity = 0.7) %>%
      addLegend(pal = pal, values = values(r), title = model_title) %>%
      addCircles(color = "red",lat = lat, lng =lng, weight = 2, fill = TRUE) %>%
      addRectangles(ext11, ext31, ext21, ext41, color = "red", fill = FALSE, dashArray = "5,5", weight = 2)
  }
}

maparesultado_model_proj <- function() {
  if (file.exists(paste0("www/", projeto, "/final/proj_ensemble.tif")) && input$project_ext == TRUE) {
    rproj <- raster::raster(paste0("www/", projeto, "/final/proj_ensemble.tif"))
    palproj <- colorNumeric (c("#FFFFFF", "#FDBB84", "#31A354"), values(rproj), na.color = "transparent")
    occ_points <<- coordinates(occurrences)
    lng <<- occ_points[,1]
    lat <<- occ_points[,2]
    map_proj <- leaflet() %>%
      addTiles() %>%
      addRasterImage(rproj, colors = palproj, opacity = 0.7) %>%
      addLegend(pal = palproj, values = values(rproj), title = "") %>%
      addCircles(color = "red", lat = lat, lng = lng, weight = 2, fill = TRUE) %>%
      addRectangles(ext12, ext32, ext22, ext42, color = "green", fill = FALSE, dashArray = "5,5", weight = 2)
  }
}

#   SETTING SHINY PARAMETERS
options(shiny.maxRequestSize = 100 * 1024^2)
dirColors <- c(`1` = "#595490", `2` = "#527525", `3` = "#A93F35", `4` = "#BA48AA")


#   START SERVER FUNCTION
function(input, output, session) {
  
  ##### CREATE NEW/LOAD PROJECT #####
  observeEvent(input$btnrefreshprojeto, {
    
    # Create new project
    if (input$select_project == "new_proj") {
      
      models_dir <- paste0("./www/results/", input$models_dir.new)
      
      if (models_dir == "./www/results/") {
        showModal(modalDialog(
          title = "Unable to create new project",
          paste0("Project name cannot be blank!", "Please enter a valid name."),
          easyClose = TRUE
        ))
      }
      
      if (file.exists(models_dir)) {
        showModal(modalDialog(
          title = "Unable to create new project",
          paste0("Project is already in use."),
          easyClose = TRUE
        ))
      }
      
      if (!file.exists(models_dir)) {
        mkdirs(models_dir)
        showModal(modalDialog(
          title = "Project succesfully created!",
          paste0("Project directory: ", models_dir),
          easyClose = TRUE
        ))
        models_dir <<- paste0("./www/results/", input$models_dir.new)
      }
    }
    
    # Load previous project
    if (input$select_project == "load_proj") {
      
      models_dir <- paste0("./www/results/", input$models_dir.load)
      
      if (models_dir != "./www/results/") {
        
        if (file.exists(models_dir)) {
          
          # Display Stats results at outputs tab
          output$dbgridresultado <- renderDataTable({
            stats.file <- list.files(path =  models_dir, recursive = T, pattern = "final_statistics.csv")
            read.csv(stats.file)
          }, options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
          
          # Display final models  - png files
          output$uifinal <- renderUI({
            display_finalpng <- list.files(path = paste0(models_dir,"/present/final_models"), recursive = T, full.names = T, pattern = ".png")
            lapply(1:length(order(display_finalpng)), function(i) {
              tags$a(
                href = display_finalpng[i],
                tags$img(src = display_finalpng[i], height = "200px"),
                target = "_blank"
              )
            })
          })
          
          # Display ensemble models - png files
          output$uiensemble <- renderUI({
            display_ensemblepng <- list.files(path = paste0(models_dir,"/present/ensemble_models"), recursive = T, full.names = T, pattern = ".png")
            lapply(1:length(order(display_ensemblepng)), function(i) {
              tags$a(
                href = display_ensemblepng[i],
                tags$img(src = display_ensemblepng[i], height = "200px"),
                target = "_blank"
              )
            })
          })
          
          # List statistics files
          output$uiestatistica <- renderUI({
            stats_file <- list.files(path = models_dir, recursive = T, full.names = T, pattern = "final_statistics.csv")
            lapply(1:length(stats_file), function(i) {
              tags$div(tags$a(
                href = stats_file[i],
                paste0(stats_file[i]),
                target = "_blank"
              ))
            })
          })
          
          # List species occurrence dataset file (.csv)
          output$uiarquivosdados <- renderUI({
            list_csv <- list.files(path = models_dir, recursive = T, full.names = T, pattern = "occurrences.csv")
            lapply(1:length(list_csv), function(i) {
              tags$div(tags$a(
                href = list_csv[i],
                paste0(list_csv[i]),
                target = "_blank"
              ))
            })
          })
          
          # List partitions
          output$uiarquivosmodelos <- renderUI({
            list_partitions <- list.files(path = paste0(models_dir,"/present/partitions"), recursive = T, full.names = T, pattern = ".tif")
            lapply(1:length(sort(list_partitions)), function(i) {
              tags$div(tags$a(
                href = list_partitions[i],
                paste0(list_partitions[i]),
                target = "_blank"
              ))
            })
          })
          
          # List final files
          output$uiarquivosfinal <- renderUI({
            list_final <-  list.files(path = paste0(models_dir,"/present/final_models"), recursive = T, full.names = T, pattern = ".tif")
            lapply (1:length(sort(list_final)), function(i) {
              tags$div(tags$a(
                href = list_final[i],
                paste0(list_final[i]),
                target = "_blank"
              ))
            })
          })
          
          # List ensemble files
          output$uiarquivosensemble <- renderUI({
            list_ensemble <-  list.files(path = paste0(models_dir,"/present/ensemble_models"), recursive = T, full.names = T, pattern = ".tif")
            lapply(1:length(sort(list_ensemble)), function(i) {
              tags$div(tags$a(
                href = list_ensemble[i],
                paste0(list_ensemble[i]),
                target = "_blank"
              ))
            })
          })
          
          showModal(modalDialog(
            title = paste0("Project ", input$models_dir.load," succefully loaded!"),
            paste0("Output files are dispalyed at the 'Outputs' tab."),
            easyClose = TRUE
          ))
          models_dir <<-  paste0("./www/results/", input$models_dir.load)
        }
      }
      # if (models_dir == "./www/results/") {
      #   showModal(modalDialog(
      #     title = "Error! Project name cannot be blank!",
      #     paste0("Please enter a valid name."),
      #     easyClose = TRUE
      #   ))
      # }
    }
  })





  
  
  #####  IMPORT SPECIES OCCURRENCE DATASET #####
  # Load species occurrence dataset from gbif/jabot databases
  loadspdata <- eventReactive(input$btnsearch_spdata, {
    #ETAPA <<- 1
    species_name <<- input$edtespecie
    if (input$bio_datasource == "gbif") {
      occur.data <- getOccurrences_gbif(input$edtespecie)
      occur.data_gbif <- occur.data [, c(2, 3)]
      occurrences <<- occur.data_gbif
    }
    if (input$bio_datasource == "jabot") {
      occur.data <- getOccurrences_jabot(input$edtespecie)
      occur.data <- as.data.frame(occur.data, stringsAsFactors = F)
      occur.data_jabot <- occur.data[, c(2, 3)]
      occur.data_jabot[, 1] <- as.numeric(occur.data_jabot[, 1])
      occur.data_jabot[, 2] <- as.numeric(occur.data_jabot[, 2])
      occurrences <<- occur.data_jabot
    }
    occurrences
  })
  
  # Browse occurrence dataset from local csv file
  loadspdata_csv <- eventReactive(input$btnsearch_spdatacsv, {
    #ETAPA <<- 1
    inFile <<- input$file1
    if (is.null(inFile)) {
      return(NULL)
    } else {
      sp_data <- read.csv(
        inFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      )
      species_name <<- as.character(sp_data[1,1])
      arquivo_path <- inFile$datapath
      arquivo_header <- input$header
      arquivo_sep <- input$sep
      arquivo_quote <- input$quote
      sp_data_csv <- sp_data [, 2:3]
      occurrences <<- sp_data_csv
    }
    occurrences
  })
  
  # Exhibit table with occurrence records
  output$spdata_table <- DT::renderDataTable({
    progress <- shiny::Progress$new()
    progress$set(message = "Importing species occurrence dataset...", value = 0)
    on.exit(progress$close())
    input$btnsearch_spdatacsv
    input$btnsearch_spdata
    if (input$bio_datasource == "csv") {
      loadspdata_csv()
    } else {
      loadspdata()
    }
    occurrences
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  
  # Display map with loaded occurrence records
  output$mapadistribuicao <- renderLeaflet({
    input$btnsearch_spdatacsv
    input$btnsearch_spdata
    progress <- shiny::Progress$new()
    progress$set(message = "Updating occurrence map...", value = 0)
    on.exit(progress$close())
    
    if (!is.null(occurrences)) {
      latitude<-as.character(occurrences$Latitude)
      latitude<-as.numeric(latitude)
      longitude<-as.character(occurrences$Longitude)
      longitude<-as.numeric(longitude)
      if (input$bio_datasource == "csv") {
        map <- leaflet(occurrences) %>%
          addTiles() %>%
          addCircles(color = "red", lat = ~ latitude, lng = ~ longitude) %>%
          setView(lng = -31.5, lat = -13.4, zoom = 3)
      }
      
      if (input$bio_datasource == "gbif") {
        map <- leaflet(occurrences) %>%
          addTiles() %>%
          addCircles(color = "red", lat = ~ latitude, lng = longitude) %>%
          setView(lng = -31.5, lat = -13.4, zoom = 3)
      }
      
      if (input$bio_datasource == "jabot") {
        map <- leaflet(occurrences) %>%
          addTiles() %>%
          addCircles(color = "red", lat = ~ latitude, lng = ~ longitude) %>%
          setView(lng = -31.5, lat = -13.4, zoom = 3)
      }
      map
    } else {
      showModal(modalDialog(
        title = "Error!",
        "Please inform species occurrence dataset",
        easyClose = TRUE
      ))
    }
  })





#####  DATA CLEANING #####
output$dgbriddadosdatacleaning <- renderDataTable({
  input$btnapagar
  input$btneliminarduplicatas
  input$btnsearch_spdatacsv
  input$btnsearch_spdata
  ETAPA<<-2
  if (is.null(occurrences)) {
    n <- 0
  }
  n <- nrow(occurrences)
  if (n > 0) {
    if (exists("occurrences")) {
      if (input$btneliminarduplicatas > 0) {
        progress <- shiny::Progress$new()
        progress$set(message = "Excluding duplicates...", value = 0)
        on.exit(progress$close())
        occurrences <<- unique(occurrences)
      }
      isolate({
        input$edtelemento
        if (input$edtelemento != "0") {
          if (input$btnapagar == 0) {
            return()
          }
          occurrences <<- occurrences[-input$edtelemento, ]
        }
        rownames(occurrences) <- NULL
        occurrences$id <- 1:nrow(occurrences)
      })
      occurrences
    }
  }
}, options = list(searching = FALSE, lengthMenu = c(5, 30, 50), pageLength = 5))

observeEvent(input$saveDataset, {
  write.csv(occurrences, file=paste0(getwd(),"/www/",projeto,"/csv/occurrences.csv"), row.names = FALSE)
})

output$mapadistribuicaodatacleaning <- renderLeaflet({
  input$btnapagar
  input$btneliminarduplicatas
  input$btnsearch_spdatacsv
  input$btnsearch_spdata
  
  if (!is.null(occurrences)) {
    if (exists("occurrences")) {
      rownames(occurrences) <- NULL
      occurrences$id <- 1:nrow(occurrences)
      map <- leaflet(occurrences) %>%
        addTiles() %>%
        addCircles(color = "red", lat = ~ Latitude, lng = ~ Longitude) %>%
        addMarkers(clusterOptions = markerClusterOptions()) %>%
        addMarkers(~ Longitude, ~ Latitude, popup = ~ as.character(id))
      map
    }
  } else {
    showModal(modalDialog(
      title = "Error!",
      "Please inform species occurrence data.",
      easyClose = TRUE
    ))
  }
})


# datasetInput <- reactive({
#   if (exists("occurrences")) {
#     switch("occurrences", occurrences = occurrences)
#   }
# })

######### ENVIRONMENTAL VARIABLES #########
observeEvent(input$btnAtualizaSelecaoVariaveis, {
  environmental_data <- reactiveValues(
    write_timeproj = FALSE,
    data_current = list(),
    data_timeproj = list()
  )
  
  ##  WorldClim variables 
  if (input$tipodadoabiotico == "CLIMA") {
    vars_selection <<- paste(input$pred_vars_wc)
    path_current <- paste0(getwd(), "/ex/clima/current/", input$resolution)
    checkfiles <- list()
    
    if (length(vars_selection) >= 1) {
      for (i in c(1:length(input$pred_vars_wc))) {
        layer <- paste0(path_current, "/", vars_selection[i], ".bil")
        environmental_data$data_current <- c(environmental_data$data_current, layer)
        checkfiles <- c(checkfiles, file.exists(layer))
      }
      
      # Check/Download layers - Current conditions
      if (length(vars_selection) >= 1 && any(checkfiles == F)) {
        if (input$resolution != "30s") {
          zip_current <- paste0("bio_", input$resolution, "_bil.zip")
          url <- paste0(
            "http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/",
            zip_current
          )
          download.file(url, zip_current, mode = "wb")
          unzip(zip_current, exdir = path_current)
          unlink(zip_current)
        }
        
        if (input$resolution == "30s") {
          group1 <- c(
            "bio1",
            "bio2",
            "bio3",
            "bio4",
            "bio5",
            "bio6",
            "bio7",
            "bio8",
            "bio9"
          )
          group2 <- c(
            "bio10",
            "bio11",
            "bio12",
            "bio13",
            "bio14",
            "bio15",
            "bio16",
            "bio17",
            "bio18",
            "bio19"
          )
          
          if (any(group1 %in% input$pred_vars_wc)) {
            download.file("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/bio1-9_30s_bil.zip",
                          "bio_30s1.zip",
                          mode = "wb"
            )
            unzip("bio_30s1.zip", exdir = path_current)
            unlink("bio_30s1.zip")
          }
          if (any(group2 %in% input$pred_vars_wc)) {
            download.file("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/bio10-19_30s_bil.zip",
                          "bio_30s2.zip",
                          mode = "wb"
            )
            unzip("bio_30s2.zip", exdir = path_current)
            unlink("bio_30s2.zip")
          }
          
          files <- paste0(path_current, "/", list.files(path_current))
          file.rename(from = files, to = sub(pattern = "bio_", replacement = "bio", files))
        }
      }
      
      ### Include time projections ###
      if (input$forecasting_wc != "current_wc") {
        environmental_data$write_timeproj <- TRUE
        checkfiles_timeproj <- list()
        
        ## Include future projection
        if (input$forecasting_wc == "future_wc") {
          timescale <<- "Future"
          path_future <- paste0(
            getwd(),
            "/ex/clima/",
            input$future_dates_wc,
            "/",
            input$resolution,
            "/",
            input$gcm_future_wc,
            "/",
            input$rcp_wc
          )
          
          year <- sub(".*(\\d+{2}).*$", "\\1", input$future_dates_wc)
          
          for (i in c(1:length(input$pred_vars_wc))) {
            nbi <- sub("bio", "", vars_selection[i])
            layer <- paste0(
              path_future,
              "/",
              input$gcm_future_wc,
              input$rcp_wc,
              "bi",
              year,
              nbi,
              ".tif"
            )
            environmental_data$data_timeproj <- c(environmental_data$data_timeproj, layer)
            checkfiles_timeproj <- c(checkfiles_timeproj, file.exists(layer))
          }
        }
        
        ## Include paleo projection
        if (input$forecasting_wc == "past_wc") {
          timescale <<- "Past"
          
          if (input$past_dates_wc == "mid") {
            gcm_past <- input$gcm_past_wc_mid
          }
          
          if (input$past_dates_wc == "lgm") {
            gcm_past <- input$gcm_past_wc_lgm
          }
          
          path_past <- paste0(
            getwd(), "/ex/clima/",
            input$past_dates_wc,
            "/",
            input$resolution,
            "/",
            gcm_past
          )
          
          for (i in c(1:length(input$pred_vars_wc))) {
            nbi <- sub("bio", "", vars_selection[i])
            layer <- paste0(
              path_past,
              "/",
              gcm_past,
              input$past_dates_wc,
              "bi",
              nbi,
              ".tif"
            )
            environmental_data$data_timeproj <- c(environmental_data$data_timeproj, layer)
            checkfiles_timeproj <- c(checkfiles_timeproj, file.exists(layer))
          }
        }
        
        ### Check/Download layers - Future/Paleo Conditions ###
        if (any(checkfiles_timeproj == F)) {
          
          # Download future layers
          if (input$forecasting_wc == "future_wc") {
            zip <- paste0(input$gcm_future_wc, input$rcp_wc, "bi", year, ".zip")
            if (input$resolution == "2-5m") {
              url <- paste0(
                "http://biogeo.ucdavis.edu/data/climate/cmip5/2_5m",
                "/",
                zip
              )
            }
            
            if (input$resolution != "2-5m") {
              url <- paste0(
                "http://biogeo.ucdavis.edu/data/climate/cmip5/",
                input$resolution,
                "/",
                zip
              )
            }
            
            download.file(url, zip, mode = "wb")
            unzip(zip, exdir = path_future)
            unlink(zip)
          }
          
          # Download paleo layers
          if (input$forecasting_wc == "past_wc") {
            zip <- paste0(
              gcm_past,
              input$past_dates_wc,
              "bi_",
              input$resolution,
              ".zip"
            )
            url <- paste0(
              "http://biogeo.ucdavis.edu/data/climate/cmip5/",
              input$past_dates_wc,
              "/",
              zip
            )
            download.file(url, zip, mode = "wb")
            unzip(zip, exdir = path_past)
            unlink(zip)
          }
        }
      }
    }
  }
  
  ## Bio-ORACLE variables 
  if (input$tipodadoabiotico == "BIOORACLE") {
    path_current <- paste0(getwd(), "/ex/biooracle/current")
    
    ## Include future projections ##
    if (input$forecasting_bo == "future_bo") {
      timescale <<- "Future"
      environmental_data$write_timeproj <- TRUE
      
      if (input$future_dates_bo == "2100") {
        scenario_bo <- input$scenario_bo_2100
      }
      if (input$future_dates_bo == "2200") {
        scenario_bo <- input$scenario_bo_2200
      }
      path_future <- paste0(getwd(), "/ex/biooracle/", input$future_dates_bo, "/", scenario_bo)
      vars_selection <<- paste(input$pred_vars_bo_fut)
      
      if (length(vars_selection) != 0) {
        for (i in c(1:length(vars_selection))) {
          # Future layers
          layer_code_future <- paste0("BO_", scenario_bo, "_", input$future_dates_bo, "_", vars_selection[i])
          environmental_data$data_timeproj <- c(environmental_data$data_timeproj, load_layers(layer_code_future, rasterstack = FALSE, datadir = path_future))
          # Current layers
          layer_code <- paste0("BO_", vars_selection[i])
          environmental_data$data_current <- c(environmental_data$data_current, load_layers(layer_code, rasterstack = FALSE, datadir = path_current))
        }
      }
      ## Do not include future projections ##
    } else {
      vars_selection <<- paste(input$pred_vars_bo)
      for (i in c(1:length(vars_selection))) {
        if (length(vars_selection) != 0) {
          layer_code <- paste0("BO_", vars_selection[i])
          environmental_data$data_current <- c(environmental_data$data_current, load_layers(layer_code, rasterstack = FALSE, datadir = path_current))
        }
      }
    }
  }
  
  ## Browse variables from local files 
  if (input$tipodadoabiotico == "Others") {
    path_current <- paste(getwd(), "/ex/outros/", sep = "")
    vars_selection <<- paste(input$pred_vars_other)
    for (i in c(1:length(input$pred_vars_other))) {
      layer <- paste0(path_current, vars_selection[i])
      environmental_data$data_current <- c(environmental_data$data_current, layer)
    }
  }
  
  if (length(vars_selection) == 0) {
    showModal(modalDialog(
      title = "Unable to load layers!",
      paste0("Please select one or more environmental variables"),
      easyClose = TRUE
    ))
  } else {
    withProgress(message = "", value = 0, {
      n <- 3
      incProgress(1 / n, detail = paste0("Loading variables..."))
      ETAPA <<- 3
      env_data <<- environmental_data$data_current
      write_timeproj <<- environmental_data$write_timeproj
      envdata_timeproj <<- environmental_data$data_timeproj
      
      if (!is.null(env_data) && exists("occurrences")) {
        
        # Stack and crop environmental layers
        predictors <- stack(env_data)
        ext <- extent(ext11, ext21, ext31, ext41)
        pred_nf <<- crop(predictors, ext)
        if (length(envdata_timeproj) >= 1) {
          predictorsfuturo <- stack(envdata_timeproj)
          pred_nffuturo <<- crop(predictorsfuturo, ext)
        }
        ext2 <- extent(ext12, ext22, ext32, ext42)
        pred_nf2 <<- crop(predictors, ext2)
        
        # Calculate correlation between selected variables (current conditions only)
        if (length(env_data) > 1) {
          incProgress(2 / n, detail = paste0("Calculating corelation..."))
          presvals <- raster::extract(pred_nf, occurrences)
          backgr <- randomPoints(pred_nf, 300)
          colnames(backgr) <- c("Longitude", "Latitude")
          absvals <- raster::extract(pred_nf, backgr)
          sdmdata <- data.frame(cbind(absvals))
          
          # Exhibit correlation panel plots
          output$grafico_correlacao <- renderPlot({
            if (is.null(env_data) | length(env_data) <= 1) {
              return(NULL)
            } else {
              pairs(sdmdata,
                    cex = 0.1,
                    fig = TRUE,
                    lower.panel = panel.reg,
                    diag.panel = panel.hist,
                    upper.panel = panel.cor
              )
              
              # Exhibit correlation matrix
              output$dgbriddadoscorrelacao <- renderDataTable({
                round(cor(sdmdata), 2)
              }, options = list(
                searching = FALSE,
                rownames = TRUE,
                colnames = c("Variable" = 1)
              ))
            }
          })
        }
      }
      incProgress(3 / n, detail = paste0("Ploting correlation..."))
    })
  }
})

output$mapaabiotico <- renderPlot({
  input$btnAtualizaSelecaoVariaveis
  if (is.null(env_data)) {
    return(NULL)
  } else {
    plot(pred_nf)
  }
})

#### Model Extent ####

output$mapapontosextend <- renderLeaflet({
  input$btnapagar
  input$btneliminarduplicatas
  input$btnsearch_spdatacsv
  input$btnsearch_spdata
  
  if (!is.null(occurrences)) {
    ext11 <<- input$edtextend1
    ext31 <<- input$edtextend3
    ext21 <<- input$edtextend2
    ext41 <<- input$edtextend4
    
    ext12 <<- input$edtextend1
    ext32 <<- input$edtextend3
    ext22 <<- input$edtextend2
    ext42 <<- input$edtextend4
    
    occurrences <<- occurrences
    map <- leaflet(occurrences) %>%
      addTiles() %>%
      addMarkers(clusterOptions = markerClusterOptions()) %>%
      addMarkers(~ Longitude, ~ Latitude) %>%
      addRectangles(ext11,
                    ext31, ext21, ext41,
                    color = "red",
                    fill = TRUE, dashArray = "5,5", weight = 3)
    map
  }
})

#### Geographic projection area ####

output$mapapontosextend2 <- renderLeaflet({
  input$btnapagar
  input$btneliminarduplicatas
  input$btnsearch_spdatacsv
  input$btnsearch_spdata
  if (!is.null(occurrences)) {
    ext12 <<- input$edtextend12
    ext32 <<- input$edtextend32
    ext22 <<- input$edtextend22
    ext42 <<- input$edtextend42
    
    occurrences <<- occurrences
    map <- leaflet(occurrences) %>%
      addTiles() %>%
      addMarkers(clusterOptions = markerClusterOptions()) %>%
      addMarkers(~ Longitude, ~ Latitude) %>%
      addRectangles(ext12,
                    ext32, ext22, ext42,
                    color = "green", fill = TRUE, dashArray = "5,5",
                    weight = 3
      )
    map
  }
})

#### GROUPING ALL MODELING PROCESSES BY CLICKING THE EXECUTE BUTTON ####
observeEvent(input$btnModelar, {
  if ((input$DOMAIN == "TRUE") || (input$MAXENT == "TRUE") ||
      (input$BIOCLIM == "TRUE") || (input$GLM == "TRUE") ||
      (input$RF == "TRUE") || (input$SVM == "TRUE") ||
      (input$GLM == "TRUE")) {
    
    if (ETAPA > 1) {
      if (exists("occurrences")) {
        progress <<- shiny::Progress$new()
        progress$set(message = "Processing...", value = 0)
        on.exit(progress$close()) 
        modelagem()
      }else{
        showModal(modalDialog(
          title = "Error!",
          "Please inform species occurrence data.",
          easyClose = TRUE
        ))
      }
    }
  }
})

output$maparesultadomax <- renderLeaflet({
  input$btnModelar
  maparesultado_model(model_ensemble = "mx_ensemble", model_title = "Maxent")
})

output$maparesultadosvm <- renderLeaflet({
  input$btnModelar
  maparesultado_model(model_ensemble = "svm_ensemble", model_title = "SVM")
})

output$maparesultadomh <- renderLeaflet({
  input$btnModelar
  maparesultado_model(model_ensemble = "ma_ensemble", model_title = "Mahal")
})

output$maparesultadorf <- renderLeaflet({
  input$btnModelar
  maparesultado_model(model_ensemble = "rf_ensemble", model_title = "RF")
})

output$maparesultadoglm <- renderLeaflet({
  input$btnModelar
  maparesultado_model(model_ensemble = "glm_ensemble", model_title = "GLM")
})

output$maparesultadobc <- renderLeaflet({
  input$btnModelar
  maparesultado_model(model_ensemble = "bc_ensemble", model_title = "BioClim")
})

output$maparesultadodo <- renderLeaflet({
  input$btnModelar
  maparesultado_model(model_ensemble = "do_ensemble", model_title = "Domain")
})

#### Exhibit geo. projected model ensemble ####
output$maparesultado_proj <- renderLeaflet({
  input$btnModelar
  maparesultado_model_proj()
})

#### Display results at the Outputs tab ####
output$uiarquivosmodelos <- renderUI({
  lista_models <- list.files(paste0("www/", projeto, "/models"),
                             full.names = F,
                             pattern = paste0("pre_")
  )
  lapply(1:length(sort(lista_models)), function(i) {
    tags$div(tags$a(
      href = paste0(home, projeto, "/models/", lista_models[i]),
      paste0(lista_models[i])
    ))
  })
})

output$ui <- renderUI({
  lista_jpg <- list.files(paste0("www/", projeto, "/jpg"),
                          full.names = F,
                          pattern = paste0(".jpg")
  )
  lapply(1:length(order(lista_jpg)), function(i) {
    tags$a(href = paste0(home, projeto, "/jpg/", lista_jpg[i]), tags$img(src = paste0(
      projeto,
      "/jpg/", lista_jpg[i]
    ), height = "200px"), target = "_blank")
  })
})

output$uiscript <- renderUI({
  lista_txt <- list.files(paste0("www/", projeto, "/"), full.names = F, pattern = paste0("script.R"))
  lapply(1:length(lista_txt), function(i) {
    tags$div(tags$a(
      href = paste0(home, projeto, "/", lista_txt[i]), paste0(lista_txt[i]),
      target = "_blank"
    ))
  })
})

output$uiestatistica <- renderUI({
  lista_txt <- list.files(paste0("www/", projeto, "/models"),
                          full.names = F,
                          pattern = paste0("statsALL.txt")
  )
  lapply(1:length(lista_txt), function(i) {
    tags$div(tags$a(
      href = paste0(home, projeto, "/models/", lista_txt[i]),
      paste0(lista_txt[i]), target = "_blank"
    ))
  })
})

output$uiarquivosdados <- renderUI({
  lista_csv <- list.files(paste0("www/", projeto, "/csv"),
                          full.names = F,
                          pattern = paste0(".csv")
  )
  lapply(1:length(lista_csv), function(i) {
    tags$div(tags$a(
      href = paste0(home, projeto, "/csv/", lista_csv[i]),
      paste0(lista_csv[i]), target = "_blank"
    ))
  })
})

output$uiarquivosensemble <- renderUI({
  lista_final <- list.files(paste0("www/", projeto, "/final"),
                            full.names = F,
                            pattern = paste0(".tif")
  )
  lapply(1:length(sort(lista_final)), function(i) {
    tags$div(tags$a(
      href = paste0(home, projeto, "/final/", lista_final[i]),
      paste0(lista_final[i]), target = "_blank"
    ))
  })
})

output$uiarquivosprojecao <- renderUI({
  lista_proj <- list.files(paste0("www/", projeto, "/proj"),
                           full.names = F,
                           pattern = paste0(".tif")
  )
  lapply(1:length(sort(lista_proj)), function(i) {
    tags$div(tags$a(
      href = paste0(home, projeto, "/proj/", lista_proj[i]),
      paste0(lista_proj[i]), target = "_blank"
    ))
  })
})

output$uiarquivosprojecaofuturo <- renderUI({
  lista_futuro <- list.files(paste0("www/", projeto, "/proj_time"),
                             full.names = F,
                             pattern = paste0(".tif")
  )
  lapply(1:length(sort(lista_futuro)), function(i) {
    tags$div(tags$a(
      href = paste0(home, projeto, "/proj_time/", lista_futuro[i]),
      paste0(lista_futuro[i]), target = "_blank"
    ))
  })
})
})
