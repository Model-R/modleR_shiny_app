# tabPanel: Select predictors ####
tabPanel(
  "Download",
  shinydashboard::box(collapsible = T,
    width = NULL,
    title = "WordClim v.1.4",
    checkboxGroupInput("download_resolution_wc", 
                       "Resolution:", 
                       choices = resolution,
                       selected = "10min", 
                       inline = TRUE),
    h5(tags$b("Project model across timescales:")),
    checkboxInput("download_future_wc",
                  label = 'Future'),
    conditionalPanel(
      "input.download_future_wc == 1", # se future for marcado
      checkboxGroupInput("download_future_dates_wc",
                  "Choose period: ", 
                  choices = future_dates_wc,
                  inline =  TRUE),
      checkboxGroupInput("download_rcp_wc",
                         "Emission Scenarios (RCP):",
                         choices = rcp_wc,
                         inline = TRUE),
      tags$div(align = 'top',
               class = 'multicol', 
               checkboxGroupInput("download_gcm_future_wc",
                         "General Circulation Models (GCM):",
                         choices = gcm_future_wc,
                         selected = "bc")
      ),
      tags$hr()
      ), #end future conditionalPanel
    checkboxInput("download_past_wc",
                  label = 'Past'),
    
    conditionalPanel(
           "input.download_past_wc == 1",
           # checkboxGroupInput("download_past_dates_wc", 
           #             "Choose period: ",
           #             choices = past_dates_wc, 
           #             selected = "mid"),
           h5(tags$b("Choose period:")),
           checkboxInput("download_mid_wc",
                         label = 'Mid Holocene'),
           conditionalPanel("input.download_mid_wc == 1",
                            tags$div(align = 'top',
                                     class = 'multicol', 
                                     checkboxGroupInput("download_gcm_past_wc_mid",
                                                        "General Circulation Models (GCM):",
                                                        choices = gcm_past_wc_mid))
                                     ), #end mid conditionalPanel
           checkboxInput("download_lgm_wc",
                         label = 'Last Glacial Maximum'),
           conditionalPanel("input.download_lgm_wc == 1 ",
                            checkboxGroupInput(
                              "download_gcm_past_wc_lgm",
                              "General Circulation Models (GCM):",
                              choices = gcm_past_wc_lgm)
                            ) #end conditionalPanel
           ) #end conditionalPanel

    # tags$div(align = 'left', 
    #          class = 'multicol', 
    #         checkboxGroupInput("download_pred_vars_wc",
    #                            "Select variables: ",
    #                            pred_vars_wc,
    #                            selected = c("bio1", "bio2", "bio3"),
    #                            inline = FALSE)) #end checkbox
  ),
  
  shinydashboard::box(collapsible = T,
    width = NULL,
    title = "BIORACLE"
  )
  
  #   shinydashboard::box(
  #     width = NULL,
  #     status = "danger",
  #     selectInput(
  #       "dtipodadoabiotico",
  #       "Variables dataset:",
  #       choices = env_datasource,
  #       selected = "WorldClim"
  #     ), #end selectInput
  #     #others
  #     conditionalPanel("input.dtipodadoabiotico == 'Others' ",
  #                      helpText("All layers should have the same spatial extent, resolution, origin, and projection"),
  #                      helpText("Before loading multi-file variables, make sure that all corresponding files are placed in the same directory."),
  #                      #fileInput(outros, "Select the folder with the environmental variables"),
  #                      if (length(list.files("ex/outros/", full.names = T, pattern = c(".*")) > 0)) {
  #                        lista_outros <- list.files("ex/outros/", full.names = F, pattern = ".tif|.bil|.grd")
  #                        checkboxGroupInput(
  #                          "dpred_vars_other",
  #                          "Select rasters: ",
  #                          choiceNames = c(lista_outros),
  #                          choiceValues = c(lista_outros)
  #                        ) # end checkboxGroupInput
  #                      } # end if
  #     ), #ends conditionalPanel others
  #     
  #     # BIOORACLE ####
  #     conditionalPanel("input.dtipodadoabiotico == 'BIOORACLE' ",
  #                      selectInput(
  #                        "dforecasting_bo",
  #                        "Project model across timescales",
  #                        choices = forecasting_bo,
  #                        selected = "current_bo"
  #                      ),#end selectInput
  #                      conditionalPanel(
  #                        "input.dforecasting_bo == 'future_bo'",
  #                        shinydashboard::box(
  #                          width = NULL,
  #                          collapsible = T,
  #                          collapsed = T,
  #                          title = "Forecasting parameters",
  #                          selectInput("dfuture_bo_dates",
  #                                      "Choose dates",
  #                                      choices = future_bo_dates,
  #                                      selected = "2100"
  #                          ), #end selectInput
  #                          conditionalPanel(
  #                            "input.dfuture_bo_dates == '2100'",
  #                            selectInput("dscenario_bo_2100",
  #                                        "Scenario",
  #                                        choices = c("A1B", "A2", "B1"),
  #                                        selected = "A1B"
  #                            ) #end selectInput
  #                          ), #end conditionalPanel
  #                          conditionalPanel(
  #                            "input.dfuture_bo_dates == '2200'",
  #                            selectInput("dscenario_bo_2200",
  #                                        "Scenario",
  #                                        choices = c("A1B", "B1"),
  #                                        selected = "A1B"
  #                            )#end selectInput
  #                          ) #end conditionalPanel
  #                        ), #end box
  #                        checkboxGroupInput(
  #                          "dpred_vars_bo_fut",
  #                          "Select variables: ",
  #                          choices = pred_vars_bo_fut
  #                        ) # end checkboxGroupInput
  #                      ),
  #                      
  #                      conditionalPanel(
  #                        "input.dforecasting_bo == 'current_bo'",
  #                        checkboxGroupInput("dpred_vars_bo",
  #                                           "Select variables: ",
  #                                           choices = pred_vars_bo 
  #                        )# end checkboxGroupInput pred_vasr_bo
  #                      ) #end conditionalPanel current_bo
  #     ), #end conditionalPanel bioracle
  #     
  #     # WorldClim ####
  #     conditionalPanel("input.dtipodadoabiotico == 'WorldClim' ",
  #                      selectInput("dresolution", 
  #                                  "Resolution:", 
  #                                  choices = resolution,
  #                                  selected = "10min"),
  #                      selectInput("dforecasting_wc",
  #                                  "Project model across timescales",
  #                                  choices = forecasting_wc,
  #                                  selected = "current_wc"),
  #                      conditionalPanel(
  #                        "input.dforecasting_wc != 'current_wc'",
  #                        shinydashboard::box(
  #                          width = NULL,
  #                          collapsible = T,
  #                          collapsed = T,
  #                          title = "Set time projetion parameters",
  #                          conditionalPanel(
  #                            "input.dforecasting_wc == 'future_wc' ",
  #                            selectInput("dfuture_dates_wc",
  #                                        "Choose period: ", 
  #                                        choices = future_dates_wc),
  #                            selectInput(
  #                              "drcp_wc",
  #                              "Emission Scenarios (RCP)",
  #                              choices = rcp_wc),
  #                            selectInput(
  #                              "dgcm_future_wc",
  #                              "General Circulation Models (GCM)",
  #                              choices = gcm_future_wc,
  #                              selected = "bc")
  #                          ), #end conditionalPanel
  #                          conditionalPanel(
  #                            "input.dforecasting_wc == 'past_wc'",
  #                            selectInput("dpast_dates_wc", 
  #                                        "Choose period: ",
  #                                        choices = past_dates_wc, 
  #                                        selected = "mid"),
  #                            conditionalPanel(
  #                              "input.dpast_dates_wc == 'mid'",
  #                              selectInput(
  #                                "dgcm_past_wc_mid",
  #                                "General Circulation Models (GCM)",
  #                                choices = gcm_past_wc_mid)
  #                            ), #end conditionalPanel
  #                            
  #                            conditionalPanel(
  #                              "input.dpast_dates_wc == 'lgm' ",
  #                              selectInput(
  #                                "dgcm_past_wc_lgm",
  #                                "General Circulation Models (GCM)",
  #                                choices = gcm_past_wc_lgm)
  #                            ) #end conditionalPanel
  #                          ) #end conditionalPanel
  #                        ) #end box
  #                      ),
  #                      
  #                      checkboxGroupInput(
  #                        "dpred_vars_wc",
  #                        "Select variables: ",
  #                        pred_vars_wc,
  #                        selected = c("bio1", "bio2", "bio3")
  #                      ) #end checkbox
  #     ) #end worldclim
  #   ) #end box
  # )
) #end tabPanel select prd
