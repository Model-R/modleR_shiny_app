observeEvent(input$btnrefreshprojeto, {
  
## Create new project
  if (input$project_name == "new_proj") {
    
    models_dir <- paste0("/www/results/", input$models_dir.new)
    
    if (models_dir == "/www/results/") {
      showModal(modalDialog(
        title = "Unable to create new project",
        paste0("Project name cannot be blank!", "Please enter a valid name."),
        easyClose = TRUE
      ))
    }
    
    if (file.exists(paste0("/www/results/", models_dir))) {
      showModal(modalDialog(
        title = "Unable to create new project",
        paste0("Project is already in use."),
        easyClose = TRUE
      ))
    }
    
    if (!file.exists(paste0("/www/results/", models_dir))) {
     
       models_dir <<- paste0("/www/results/", input$models_dir.new)
     
        showModal(modalDialog(
        title = "Project succesfully created!",
        paste0("Project directory: ", models_dir),
        easyClose = TRUE
      ))
    }
  }
  
  # Load previous project
  if (input$project_name == "load_proj") {
   
     models_dir <- paste0("/www/results/", input$models_dir.load)
    
     if (models_dir != "/www/results/") {
      
      if (file.exists(paste0("/www/results/", models_dir))) {
        
        path <- paste0(models_dir, "/", input$load_spname_models)
        
        # Display Stats results at outputs tab
        output$dbgridresultado <- renderDataTable({
          stats.file <- list.files(path = path, recursive = T, pattern = "final_statistics.csv")
          read.csv(stats.file)
        }, options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
        
        # Display final models  - png files
        output$uifinal <- renderUI({
          display_finalpng <- list.files(path = paste0(path,"/present/final_models"), recursive = T, full.names = T, pattern = ".png")
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
          display_ensemblepng <- list.files(path = paste0(path,"/present/ensemble_models"), recursive = T, full.names = T, pattern = ".png")
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
          stats_file <- list.files(path = path, recursive = T, full.names = T, pattern = "final_statistics.csv")
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
          list_csv <- list.files(path = path, recursive = T, full.names = T, pattern = "occurrences.csv")
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
          list_partitions <- list.files(path = paste0(path,"/present/partitions"), recursive = T, full.names = T, pattern = ".tif")
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
          list_final <-  list.files(path = paste0(path,"/present/final_models"), recursive = T, full.names = T, pattern = ".tif")
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
          list_ensemble <-  list.files(path = paste0(path,"/present/ensemble_models"), recursive = T, full.names = T, pattern = ".tif")
          lapply(1:length(sort(list_ensemble)), function(i) {
            tags$div(tags$a(
              href = list_ensemble[i],
              paste0(list_ensemble[i]),
              target = "_blank"
            ))
          })
        })

        showModal(modalDialog(
          title = paste0("Project ", input$models_dir.load, " - ",input$load_spname_models ," succefully loaded!"),
          paste0("Output files are dispalyed at the 'Outputs' tab."),
          easyClose = TRUE
        ))
        
        models_dir <<- paste0("/www/results/", input$models_dir.load, "/", input$load_spname_models)
      }
    }
    
    if (models_dir == "/www/results/") {
      showModal(modalDialog(
        title = "Error! Project name cannot be blank!",
        paste0("Please enter a valid name."),
        easyClose = TRUE
      ))
    }
  }
})
