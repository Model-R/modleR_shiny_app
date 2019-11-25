#############################
## ----   modleR     ----  ##
##                         ##
## ANDREA SÁNCHEZ TAPIA    ##
## FELIPE SODRÉ BARROS     ##
## GUILHERME GALL          ##
## DIOGO SOUZA B. ROCHA    ##
## RAFAEL OLIVEIRA LIMA    ##
## RENATA DE T. CAPELLÃO   ##
## SARA RIBEIRO MORTARA    ##
## MARIA LUIZA MONDELLI    ##
##                         ##
## 21 DE SETEMBRO DE 2019  ##
#############################

# Thanks to Steven Worthington for function ipak https://gist.github.com/stevenworthington/3178163 (HT Karlo Guidoni Martins)

#isto também tem que sair
#jar <- paste0(system.file(package = "dismo"), "/java/maxent.jar")
#if (file.exists(jar) != T) {
 # url <- "http://biodiversityinformatics.amnh.org/open_source/maxent/maxent.php?op=download"
  #download.file(url, destfile = "maxent.zip", mode = "wb")
  #unzip("maxent.zip", files = "maxent.jar", exdir = system.file("java", package = "dismo"))
  #unlink("maxent.zip")
#}

#startapp----
function(input, output, session) {

  ###### SET PROJECT DIRECTORY #####
  
  # Create new project----
  observeEvent(input$btnrefreshprojeto, {
    if (input$select_project == "new_proj") {
      modelsDir <- paste0("./www/results/", input$modelsDir.new)

      if (modelsDir == "./www/results/") {
        showModal(modalDialog(
          title = "Unable to create project",
          paste0("Project name cannot be blank!", "Please enter a valid name."),
          easyClose = TRUE
        ))
      }
      if (file.exists(modelsDir)) {
        showModal(modalDialog(
          title = "Unable to create project",
          paste0("Inserted name is already in use."),
          easyClose = TRUE
        ))
      }

      if (!file.exists(modelsDir)) {
        showModal(modalDialog(
          title = "Project succesfully created! Click anywhere to continue",
          paste0("Project directory: ", modelsDir),
          easyClose = TRUE
        ))
        modelsDir <<-  paste0("./www/results/", input$modelsDir.new)
        mk.dirs(modelsDir)
      }
    }

    # Load previous project----
      #ast: aqui só está setando um path e achando que já tudo foi modelado. mas nãõ carrega nada de sessões anteriores - tip oocorrencias, ambiente, a pessoa teria de fazer de novo tudo -tendo as coisas no hd. isto pode melhorar.
    if (input$select_project == "load_proj") {
      modelsDir <- paste0("./www/results/", input$modelsDir.load)
      #actually load things
      if (dir.exists(modelsDir) & modelsDir != "./www/results/") {
        showModal(modalDialog(
          title = paste0("Project ", input$modelsDir.load, " succefully loaded! Click anywhere to continue"),
          paste0("Output files are displayed at the 'Outputs' tab."),
          easyClose = TRUE
        ))
        modelsDir <<- modelsDir
      } else {
        showModal(modalDialog(
          title = paste0("Unable to load project"),
          paste0("Please check the models directory"),
          easyClose = TRUE
        ))
      }
    }
  })

  #####  IMPORT SPECIES OCCURRENCE DATASET #####


  # Load species occurrence dataset from gbif/jabot databases----
  loadspdata <- eventReactive(input$btnsearch_spdata, {
    species_name <<- input$species_name
    print(paste0('CHECK BIO_DATASOURCE - ', input$bio_datasource))
    if (input$bio_datasource == "package_dataset") {
        occur.data <- modleR::example_occs[[1]]#a primeira especie do dataset interno
        occurrences <<- occur.data[, c(2,3)]
    }
    if (input$bio_datasource == "gbif") {
      occur.data <- getOccurrences_gbif(input$species_name)
      occur.data_gbif <- occur.data[, c(2, 3)]
      #occur.data_gbif <- occur.data
      occurrences <<- occur.data_gbif
    }
    if (input$bio_datasource == "jabot") {
      occur.data <- getOccurrences_jabot(input$species_name)
      occur.data <- as.data.frame(occur.data, stringsAsFactors = F)
      occur.data_jabot <- occur.data[, c(2, 3)]
      #occur.data_jabot <- occur.data
      occurrences <<- occur.data_jabot
    }
    occurrences
  })

  # Browse occurrence dataset from local csv file----
  loadspdata_csv <- eventReactive(input$btnsearch_spdatacsv, {
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
      species_name <<- as.character(sp_data[1, 1])
      arquivo_path <- inFile$datapath
      arquivo_header <- input$header
      arquivo_sep <- input$sep
      arquivo_quote <- input$quote
      sp_data_csv <- sp_data[, 2:3]
      occurrences <<- sp_data_csv
    }
    occurrences
  })

  # Exhibit table with occurrence records----
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
    format(occurrences, digits = 4, nsmall = 4)
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))

  observeEvent(input$btn_saveDatasetRaw, {
    save_raw <- data.frame(species_name, occurrences)
    names(save_raw) <- c("species_name", "lon", "lat")
    occ_folder <- paste(modelsDir, species_name, "occurrences", sep = "/")
    dir.create(occ_folder, recursive = T)
    write.csv(save_raw, file = paste0(occ_folder, "/occurrences_", input$bio_datasource, "_", species_name, ".csv"), row.names = FALSE)
  })

  # Display map with occurrence records----
  output$mapadistribuicao <- renderLeaflet({
    input$btnsearch_spdatacsv
    input$btnsearch_spdata
    progress <- shiny::Progress$new()
    progress$set(message = "Updating occurrence map...", value = 0)
    on.exit(progress$close())

    #if (!is.null(occurrences)) {
    req(occurrences)
     map <- leaflet(occurrences) %>%
        addTiles() %>%
        addCircles(color = "red", lat = ~lat, lng = ~lon) %>%
        setView(lng = -31.5, lat = -13.4, zoom = 3)
      map
    #} #else {
      #showModal(modalDialog(
      #  title = "Error!",
      #  "Please inform species occurrence dataset",
      #  easyClose = TRUE
      #)
  #)
    #}
  })


  #####  DATA CLEANING #####
  #data table
  output$dgbriddadosdatacleaning <- renderDataTable({
    input$btnapagar
    input$btneliminarduplicatas
    input$btnsearch_spdatacsv
    input$btnsearch_spdata

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
        format(occurrences, digits = 4, nsmall = 4)
      }
    }
  }, options = list(searching = FALSE, lengthMenu = c(5, 30, 50), pageLength = 5))

  #save dataset
  observeEvent(input$btn_saveDatasetClean, {
    save_clean <- data.frame(species_name, occurrences)
    names(save_clean) <- c("name", "lon", "lat")
    occ_folder <- paste(modelsDir, species_name, "occurrences", sep = "/")
    dir.create(occ_folder, recursive = T, showWarnings = F)
    write.csv(save_clean, file = paste0(occ_folder, "/occurrences_clean_", species_name, ".csv"), row.names = FALSE)
  })
#mapa datacleaning----
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
          addCircles(color = "red", lat = ~lat, lng = ~lon) %>%
          addMarkers(clusterOptions = markerClusterOptions()) %>%
          addMarkers(~lon, ~lat, popup = ~as.character(id))
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

  #### Model Extent ####
  output$mapapontosextend <- renderLeaflet({
    input$btnapagar
    input$btneliminarduplicatas
    input$btnsearch_spdatacsv
    input$btnsearch_spdata

    if (!is.null(occurrences)) {
      ext12 <<- ext11 <<- input$edtextend11
      ext32 <<- ext31 <<- input$edtextend31
      ext22 <<- ext21 <<- input$edtextend21
      ext42 <<- ext41 <<- input$edtextend41

      map <- leaflet(occurrences) %>%
        addTiles() %>%
        addMarkers(clusterOptions = markerClusterOptions()) %>%
        addMarkers(~lon, ~lat) %>%
        addRectangles(ext11, ext31, ext21, ext41,
          color = "red",
          fill = TRUE, dashArray = "5,5", weight = 3
        )
      map
    }
  })

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

      map <- leaflet(occurrences) %>%
        addTiles() %>%
        addMarkers(clusterOptions = markerClusterOptions()) %>%
        addMarkers(~lon, ~lat) %>%
        addRectangles(ext12, ext32, ext22, ext42,
          color = "green", fill = TRUE, dashArray = "5,5",
          weight = 3
        )
      map
    }
  })

  ######### ENVIRONMENTAL VARIABLES #########
  observeEvent(input$btnAtualizaSelecaoVariaveis, {
    environmental_data <- reactiveValues(
      write_timeproj = FALSE,
      data_current = list(),
      data_timeproj = list()
    )
    ## Package variables #ops no projection
    if (input$tipodadoabiotico == "package_dataset") {
      vars_selection <<- paste(input$pred_vars_pac)
      environmental_data$data_current <<-
        raster::subset(modleR::example_vars, vars_selection)
    }

    ##  WorldClim variables
    if (input$tipodadoabiotico == "WorldClim") {
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

          ## Include future projections
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

          ## Include paleo projections
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
            # Present layers
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
        env_data <<- environmental_data$data_current
        write_timeproj <<- environmental_data$write_timeproj
        envdata_timeproj <<- environmental_data$data_timeproj

        if (!is.null(env_data) && exists("occurrences")) {

          # Stack and crop environmental layers
          predictors.raw <- stack(env_data)
          ext <- extent(ext11, ext21, ext31, ext41)
          predictors <<- crop(predictors.raw, ext)

          if (length(envdata_timeproj) >= 1) {
            predictorsfuturo.raw <- stack(envdata_timeproj)
            pred_nffuturo <<- crop(predictorsfuturo.raw, ext)
          }
          ext2 <- extent(ext12, ext22, ext32, ext42)
          pred_nf2 <<- crop(predictors, ext2)

          # Calculate correlation between selected variables
          if (length(env_data) > 1) {
            incProgress(2 / n, detail = paste0("Calculating corelation..."))
            presvals <- raster::extract(predictors, occurrences)
            backgr <- randomPoints(predictors, 300)
            colnames(backgr) <- c("lon", "lat")
            absvals <- raster::extract(predictors, backgr)
            envdata <<- data.frame(cbind(absvals))

            # Exhibit correlation panel
            output$grafico_correlacao <- renderPlot({
              if (is.null(env_data) | length(env_data) <= 1) {
                return(NULL)
              } else {
                pairs(envdata,
                  cex = 0.1,
                  fig = TRUE,
                  lower.panel = panel.reg,
                  diag.panel = panel.hist,
                  upper.panel = panel.cor
                )
                output$dgbriddadoscorrelacao <- renderDataTable({
                  round(cor(envdata), 2)
                }, options = list(
                  searching = FALSE,
                  rownames = TRUE,
                  colnames = c("Variable" = 1)
                ))
              }
            })
          }
        }
        incProgress(3 / n, detail = paste0("Plotting correlation panel..."))
      })
    }
  })

  output$mapaabiotico <- renderPlot({
    input$btnAtualizaSelecaoVariaveis
    if (is.null(env_data)) {
      return(NULL)
    } else {
      plot(predictors)
    }
  })

  ###setup é importante o suficiente para ter aba e botão independente e vai ser mais fácil tudo
#btnSetup----
  observeEvent(input$btnSetup, {
    #chequear si algo es necesario aqui, que existan las cosas probablemente
      #se há occurrences, preditores e species_name --aqui é que deveria ter no caso de carregar um projeto preexistente
      if (exists("occurrences") && exists("predictors") && exists("species_name")) {
        #rodar tudo
        progress <<- shiny::Progress$new()
        progress$set(message = "Processing...", value = 0)
        modelsDir.sp <<- paste(modelsDir, species_name, sep = "/")
        on.exit(progress$close())
        print(paste0('CHECK partition_type: ', input$partition_type))
        if (input$partition_type == "crossvalidation") {
          cv_n <- input$cv_n
          cv_partitions <- input$cv_partitions
          boot_n <- NULL
          boot_proportion <- NULL
        }
        if (input$partition_type == "bootstrap") {
          cv_n <- NULL
          cv_partitions <- NULL
          boot_n <- input$boot_n
          boot_proportion <- input$boot_proportion
        }
        if (input$geo_filt == TRUE) {
          geo_filt_dist <- input$geo_filt_dist
        } else {
          geo_filt_dist <- NULL
        }
        #setup_sdmdata
        print(paste0('CHECK env_filt: ',input$env_filt))
        print(paste0('CHECK env_dist: ',input$env_dist)) 
        print(paste0('CHECK max_env_dist: ',input$max_env_dist) )
        setup_sdmdata(
          species_name = species_name,
          occurrences = occurrences,
          predictors = predictors,
          models_dir = modelsDir,
          real_absences = NULL,
          lon = "lon",
          lat = "lat",
          buffer_type = input$buffer_type,
          dist_buf = input$buf_dist,
          env_buffer = input$env_filt,
          env_distance = input$env_dist,
          dist_min = input$min_dist,
          buffer_shape = input$user_shape,
          max_env_dist = input$max_env_dist,
          write_buffer = T,
          #seed = NULL,
          #clean_dupl = T,
          #clean_nas = T,
          #clean_uni = T,
          geo_filt = input$geo_filt,
          geo_filt_dist = input$geo_filt_dist,
          #select_variables = T
          #cutoff = 0.8,
          #percent = 0.8,
          n_back = input$n_back,
          partition_type = input$partition_type,
          boot_n = input$boot_n,
          boot_proportion = input$boot_proportion,
          cv_n = input$cv_n,
          cv_partitions = input$cv_partitions,
          plot_sdmdata = TRUE)

#toca hacer algo similar a esto para mostrar el sdmdata y el buffer
        #output$maparesultadomax <- renderLeaflet({
        #  input$btnModelar
        #  MapPreview.final(algorithm = "maxent")
        #})

      } else {
        showModal(modalDialog(
          title = "Error!",
          "Please inform species occurrence data, predictor variables and species name.",
          easyClose = TRUE
        ))
      }#error
    } #termina el loop del setup
  )#termina observeevent y btmsetup

# btnModelar-----
  observeEvent(input$btnModelar, {
    #se algum algoritmo é marcado
    if (any(
      input$bioclim,
      input$domain,
      input$brt,
      input$glm,
      input$mahal,
      input$maxent,
      input$rf,
      input$svme,
      input$svmk)) {
      
      #Important: must be ordered as the algorithms (global variable)
      which_algo <- c(input$bioclim,
                     input$domain,
                     input$brt,
                     input$glm,
                     input$mahal,
                     input$maxent,
                     input$rf,
                     input$svme,
                     input$svmk
                     )
     
      print(paste0('CHECK any modeling - ', which(which_algo)))
      algorithms2final <- algorithms[which(which_algo)]
      #Updating the checkbox of the final tab according to the algorithms used in modeling
      updateCheckboxGroupInput(session, "algorithms",
                               label = "Make final models for the following algorithms: ",
                               choices = algorithms2final
      )
      
      #se há occurrences, preditores e species_name --aqui é que deveria ter no caso de carregar um projeto preexistente
      if (exists("occurrences") && exists("predictors") && exists("species_name")) {
        #rodar tudo
        progress <<- shiny::Progress$new()
        progress$set(message = "Processing...", value = 0)

        modelsDir.sp <<- paste(modelsDir, species_name, sep = "/")

        on.exit(progress$close())

#do_any/do_many #ainda falta ver o que é melhor
        do_many(
          species_name = species_name,
          predictors = predictors,
          models_dir = modelsDir,
          bioclim = input$bioclim,
          domain = input$domain,
          glm = input$glm,
          mahal = input$mahal,
          maxent = input$maxent,
          rf = input$rf,
          svmk = input$svmk,
          svme = input$svme,
          brt = input$brt,
          #project_model,
          #proj_data_folder =
          mask = NULL,
          write_png = T,
          write_bin_cut = T,
          dismo_threshold = "spec_sens",
          conf_mat = F,
          equalize = T,
          proc_threshold = 0.5)
      } else {
        showModal(modalDialog(
          title = "Error!",
          "Please inform species occurrence data, predictor variables and species name.",
          easyClose = TRUE
        ))
      }#error
    } #termina el loop del modelo geral los tres pasos
    x <- 'bioclim'
    # Update algorithm options - Final tab
    
    
  })#termina observeevent y btmmodelar

  ###SOLO final model ----

  observeEvent(input$whichmodelsensemble,{
    if ('bin_consensus' %in% input$whichmodelsensemble){
      shinyjs::show("cut_bin_ensemble")
    }else{
      shinyjs::hide("cut_bin_ensemble")
    }
  })
  
  observeEvent(input$which_models_final,{
    if ('bin_consensus' %in% input$which_models_final){
      shinyjs::show("cut_bin_final")
    }else{
      shinyjs::hide("cut_bin_final")
    }
  })
  
  # btnFinal-----
  observeEvent(input$btnFinal, {
    #se algum algoritmo é marcado
    if (length(input$algorithms) > 0) {
      #se há occurrences, preditores e species_name --aqui é que deveria ter no caso de carregar um projeto preexistente
      if (exists("occurrences") && exists("predictors") && exists("species_name")) {
        #rodar tudo
        progress <<- shiny::Progress$new()
        progress$set(message = "Processing...", value = 0)

        modelsDir.sp <<- paste(modelsDir, species_name, sep = "/")

        on.exit(progress$close())

      #final_model
        #algof <<- paste(input$algorithms)
        #sp <<- input$select_partitions
        #spp <<- input$select_par
        #spv <<- input$select_par_val
        #cl <<- input$consensus_level
        #wmf <<- paste(input$which_models_final, collapse = ",")
        #wmf <<- paste(input$which_models_final)
        #uf <<- input$incertidumbre
        
        # Verifica select partitions
        # [Malu]: ifelse não funciona para atribuir NULL, por isso if e else separados
        if (input$select_partitions == F){ 
          select_par <- NULL 
          select_par_val <- NULL
        }else{ 
          select_par <- input$select_par 
          select_par_val <- input$select_par_val
        } 
        
        # Verifica weight partitions
        if (input$weigh_yesno == F){
          weight <- NULL 
        }else{ 
          weight <- input$weight_par
        } 
        
        #CHECKS
        print(paste0('CHECK algo - ', input$algorithms))
        print(paste0('CHECK incertidumbre - ', input$incertidumbre))
        print(paste0('CHECK consensuslevel - ', input$consensus_level))
        print(paste0('CHECK which_models_final - ', str(input$which_models_final)))
        print(paste0('CHECK select_par - ', input$select_par))
        print(paste0('CHECK select_par_val - ', input$select_par_val))
        print(paste0('CHECK select_partitions - ', input$select_partitions))
        print(paste0('CHECK weight - ', input$weigh_yesno, ' - ', weight))
        
        final_model(
          species_name = species_name,
          algorithms = input$algorithms,
          weight_par = weight, # [Malu]: verificar se os resultados estão consistentes
          select_partitions = input$select_partitions,
          select_par = select_par, 
          select_par_val = select_par_val,
          cut_level = c("spec_sens"), #note to self não sei se gosto de cut_level como nome do parametro nunca lembro o que é
          scale_models = TRUE,
          consensus_level = input$consensus_level,
          models_dir = modelsDir,
          final_dir = "final_models",
          proj_dir = "present", #parametrizar
          which_models = input$which_models_final,
          uncertainty = input$incertidumbre,
          write_png = T,
          write_final = T,
          overwrite = T
        )
        
        #Output maps
        if ('bioclim' %in% input$algorithms){
          insertTab(inputId = "final_tabs",
                    tabPanel("Bioclim", 
                             renderLeaflet({
                               input$btnFinal
                               MapPreview.final(algorithm = "bioclim")
                              })),
                    target = 'Results'
          )
        }
        
        if ('brt' %in% input$algorithms){
          insertTab(inputId = "final_tabs",
                    tabPanel("BRT", 
                             renderLeaflet({
                               input$btnFinal
                               MapPreview.final(algorithm = "brt")
                             })),
                    target = 'Results'
          )
        }

        if ('domain' %in% input$algorithms){
          insertTab(inputId = "final_tabs",
                    tabPanel("Domain", 
                             renderLeaflet({
                               input$btnFinal
                               MapPreview.final(algorithm = "domain")
                             })),
                    target = 'Results'
          )
        }
        
        if ('glm' %in% input$algorithms){
          insertTab(inputId = "final_tabs",
                    tabPanel("GLM", 
                             renderLeaflet({
                               input$btnFinal
                               MapPreview.final(algorithm = "glm")
                             })),
                    target = 'Results'
          )
        }
        
        if ('mahal' %in% input$algorithms){
          insertTab(inputId = "final_tabs",
                    tabPanel("Mahalanobis", 
                             renderLeaflet({
                               input$btnFinal
                               MapPreview.final(algorithm = "mahal")
                             })),
                    target = 'Results'
          )
        }
        
        if ('maxent' %in% input$algorithms){
          insertTab(inputId = "final_tabs",
                    tabPanel("Maxent", 
                             renderLeaflet({
                               input$btnFinal
                               MapPreview.final(algorithm = "maxent")
                             })),
                    target = 'Results'
          )
        }
        
        if ('rf' %in% input$algorithms){
          insertTab(inputId = "final_tabs",
                    tabPanel("RandomForest", 
                             renderLeaflet({
                               input$btnFinal
                               MapPreview.final(algorithm = "rf")
                             })),
                    target = 'Results'
          )
        }
        
        if ('svme' %in% input$algorithms){
          insertTab(inputId = "final_tabs",
                    tabPanel("SVM (e1071)", 
                             renderLeaflet({
                               input$btnFinal
                               MapPreview.final(algorithm = "svme")
                             })),
                    target = 'Results'
          )
        }
        
        if ('svmk' %in% input$algorithms){
          insertTab(inputId = "final_tabs",
                    tabPanel("SVM (kernlabs)", 
                             renderLeaflet({
                               input$btnFinal
                               MapPreview.final(algorithm = "svmk")
                             })),
                    target = 'Results'
          )
        }
        
        #Remove 'Results' tab - only needed as a reference to add previous tabs 
        removeTab(inputId = "final_tabs", target = "Results")
        
      } else {
        showModal(modalDialog(
          title = "Error!",
          "Please inform species occurrence data, predictor variables and species name.",
          easyClose = TRUE
        ))
      }#error
    } #
  })#termina observeevent y btmfinal

###ensemble
  #ensemble#está haciendo para todo lo que hay endisco entonces si una cosa cambia da error de extents. acabo de pedir solamente bioclim pero rodó todo. no tiene sentido.
  # btnEnsemble-----
  observeEvent(input$btnEnsemble, {
      #se há occurrences, preditores e species_name --aqui é que deveria ter no caso de carregar um projeto preexistente
      if (exists("occurrences") && exists("predictors") && exists("species_name")) {
        #rodar tudo
        progress <<- shiny::Progress$new()
        progress$set(message = "Processing...", value = 0)

        modelsDir.sp <<- paste(modelsDir, species_name, sep = "/")

        on.exit(progress$close())

        print(paste0('CHECK which_models_ensemble - ', input$whichmodelsensemble))
        print(paste0('CHECK consensus - ', input$consensus_ensemble))
        print(paste0('CHECK consensus_level - ', input$consensus_level_ensemble))
        
        print(paste0('teste ---- ', 'bin_consensus' %in% input$whichmodelsensemble))
        
        modleR::ensemble_model(
          species_name = species_name,
          occurrences = occurrences,
          models_dir = modelsDir,
          final_dir = "final_models",
          ensemble_dir = "ensemble",
          proj_dir = "present",
          which_final = input$whichmodelsensemble,
          consensus = input$consensus_ensemble,
          consensus_level = input$consensus_level_ensemble,
          write_ensemble = T,
          overwrite = T
        )
        
        #preview ensemble
        output$mapaensemble <- renderLeaflet({
          input$btnEnsemble
          MapPreview.ensemble()
        })
        #ensemble
            } else {
              showModal(modalDialog(
                title = "Error!",
                "Please inform species occurrence data, predictor variables and species name.",
                easyClose = TRUE
              ))
            }#error
        })#termina observeevent y btmensemble


  #### PLOTTING RESULTS ####
  observeEvent({
    input$btnrefreshprojeto
    input$btnModelar
  }, {
    sp_dirs <<- list.dirs(modelsDir, recursive = F, full.names = F)
    updateSelectInput(session, "Select_spdir",
      label = paste("Select species"),
      choices = c(sp_dirs)
    )
  }, ignoreNULL = F, ignoreInit = T)

  observeEvent({
    input$btnrefreshprojeto
    input$btnModelar
    input$Select_spdir
  }, {
    if (length(sp_dirs) == 0) {
      modelsDir.sp <<- NULL
    } else {
      modelsDir.sp <<- paste0(modelsDir, "/", input$Select_spdir)
    }

    # metadata
    output$metadata_table <- renderDataTable({
      metadata <- list.files(path = paste0(modelsDir.sp, "/present"), recursive = T, full.names = T, pattern = "metadata.csv")[1]
      metadata <- data.frame(read.csv(metadata))
      metadata$res.x <- format(metadata$res.x, digits = 3, nsmall = 3)
      metadata$res.y <- format(metadata$res.y, digits = 3, nsmall = 3)
      metadata
    }, options = list(scrollX = TRUE, scrollY = TRUE, searching = FALSE))

    # sdmdata txt table
    output$sdmdata_table <- renderDataTable({
      sdmdata <- list.files(path = paste0(modelsDir.sp, "/present"), recursive = T, full.names = T, pattern = "sdmdata.csv")[1]
      sdmdata <- data.frame(read.csv(sdmdata))
      sdmdata$lon <- format(sdmdata$lon, digits = 3, nsmall = 3)
      sdmdata$lat <- format(sdmdata$lat, digits = 3, nsmall = 3)
      sdmdata
    }, options = list(scrollX = TRUE, searching = FALSE))

    # sdmdata png map
    output$sdmdata_png <- renderImage({
      list(
        src = list.files(path = paste0(modelsDir.sp, "/present"), recursive = T,
                         full.names = T, pattern = "sdmdata_.*png$")[1],
        contentType = "image/png"
        #alt = "This is alternate text"
      )
    })

    # stats
    output$stats <- renderDataTable({
      stats.file <- list.files(path = paste0(modelsDir.sp, "/present/final_models"), recursive = T, full.names = T, pattern = "final_statistics.csv")[1]
      stats.file <- read.csv(stats.file, row.names = 1)
      for (i in c(1:ncol(stats.file))) {
        if (is.numeric(stats.file[, i])) {
          stats.file[, i] <- format(stats.file[, i], digits = 3, nsmall = 4)
        }
      }
      stats.file
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 10, scrollX = TRUE, searching = FALSE))

    #    ####  Binary and Continuous models ####

    #### Output list ####

    # partitions dir
    output$partitions <- renderUI({
      listPartitions.full <- list.files(path = paste0(modelsDir.sp, "/present/partitions"), recursive = F, full.names = T, pattern = ".tif")
      listPartitions <- list.files(path = paste0(modelsDir.sp, "/present/partitions"), recursive = F, full.names = F, pattern = ".tif")
      lapply(1:length(sort(listPartitions)), function(i) {
        tags$div(tags$a(
          href = listPartitions.full[i],
          paste0(listPartitions[i])
        ))
      })
    })

    #  final_models dir
    output$final <- renderUI({
      listFinal.full <- list.files(path = paste0(modelsDir.sp, "/present/final_models"), recursive = F, full.names = T, pattern = ".tif")
      listFinal <- list.files(path = paste0(modelsDir.sp, "/present/final_models"), recursive = F, full.names = F, pattern = ".tif")
      lapply(1:length(sort(listFinal)), function(i) {
        tags$div(tags$a(
          href = listFinal.full[i],
          paste0(listFinal[i])
        ))
      })
    })

    # ensemble dir
    output$ensemble <- renderUI({
      listEnsemble.full <- list.files(path = paste0(modelsDir.sp, "/present/ensemble"), recursive = F, full.names = T, pattern = ".tif")
      listEnsemble <- list.files(path = paste0(modelsDir.sp, "/present/ensemble"), recursive = F, full.names = F, pattern = ".tif")
      lapply(1:length(sort(listEnsemble)), function(i) {
        tags$div(tags$a(
          href = listEnsemble.full[i],
          paste0(listEnsemble[i])
        ))
      })
    })
  }, ignoreNULL = T, ignoreInit = T)
}
