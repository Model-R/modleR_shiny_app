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


#### DASHBOARD #####
header <- shinydashboard::dashboardHeader(title = "modleR 3.0")
body <- dashboardBody(useShinyjs(),
  fluidRow(
    tabBox(
      side = "left",
      title = "Steps",
      width = NULL,
      height = "1000px",#si esto no se pone queda gris detrás
      
      #### WELCOME ####
      tabPanel(
        "Project",
        column(
          width = 12,
          tabPanel(
            "",
            #aqui a parte dos projetos, que deveria ser central----
            column(
              width = 6,
              shinydashboard::box(
                width = NULL,
                if (length(list.files("./www/results/") > 0)) {
                  list_projects <- list.files("./www/results/", full.names = F, recursive = F)
                  shinydashboard::box(
                    title = "Create/Open project",
                    status = "danger",
                    solidHeader = TRUE,
                    width = NULL,
                    selectInput("select_project",
                                label = "Select project: ",
                                choices = select_project
                    ), #end select_project
                    conditionalPanel("input.select_project == 'load_proj' ",
                                     radioButtons("modelsDir.load",
                                                  "Open project:",
                                                  choiceNames = c(list_projects),
                                                  choiceValues = c(list_projects),
                                                  selected = NULL
                                     ) #end modelsDis.load
                    ), #end conditionalPanel
                    conditionalPanel("input.select_project == 'new_proj'",
                                     textInput("modelsDir.new", 
                                               label = "Insert project name:",
                                               value = ""
                                     ) #end modelsDir.new
                    ), #end conditionalPanel
                    actionButton("btnrefreshprojeto", "Submit", icon = icon(""))
                  ) #end box if
                } else {
                  shinydashboard::box(
                    title = "Create/Open project",
                    status = "danger",
                    solidHeader = TRUE,
                    width = NULL,
                    selectInput("select_project", "",
                                choices = c("Create new project" = "new_proj")
                    ), #end select_project
                    conditionalPanel("input.select_project == 'new_proj' ",
                                     textInput("modelsDir.new", label = "Insert project name: ", 
                                               value = "")
                    ), #end conditionalPanel
                    actionButton("btnrefreshprojeto", "Submit", icon = icon("ok", lib = "glyphicon"))
                  ) #end box else
                } #end else
              ) #end box
            ), #end col
            # aqui a citação, que deveria ser menos importante no layout----
            column(
              width = 6,
              shinydashboard::box(
                width = NULL,
                column(
                  width = 6,
                  br(),
                  img(src = "modleR_Miriam_Libre.png", width = 200)
                ), #end column 6
                column(
                  width = 9,
                  h4("A workflow to perform ecological niche modeling based on dismo")
                ), #end column 9
                column(
                  width = 12,
                  br(),
                  p("Please cite:"),
                  br(),
                  p("Sánchez-Tapia, Andrea ; de Siqueira, Marinez Ferreira ; Lima, Rafael Oliveira ; Barros, Felipe Sodré M. ; Gall, Guilherme M. ; Gadelha, Luiz M. R. ; da Silva, Luís Alexandre E. ; Osthoff, Carla . Model-R: A Framework for Scalable and Reproducible Ecological Niche Modeling. Communications in Computer and Information Science. 1ed.: Springer International Publishing, 2018, v. 796, p. 218-232.")
                ) #end column 12
              ) #end box
            ) #end column
          ) #end tabPanel
        ) #end column
      ),#end tabPanel Project
      
      #### OCCURRENCE DATA ####
      tabPanel(
        "Species occurrence data",
        column(
          width = 12,
          #el de import vs cleaning
          tabBox(
            side = "left",
            title = "",
            width = NULL,
            height = "600px",
            selected = "Import occurrence dataset",
            #el de import vs. map pero queremos solo uno
            tabPanel(
              "Import occurrence dataset",
              #primera columna: la busqueda
              column(
                width = 4,
                shinydashboard::box(
                  width = NULL,
                  status = "danger",
                  helpText("Select species occurrence database or browse csv dataset"),
                  selectInput("bio_datasource",
                              "Occurrence data",
                              choices = bio_datasource,
                              selected = "package_dataset"),
                  #dependiendo de lo que selecciona cambia
                  conditionalPanel("input.bio_datasource == 'csv' ",
                                   helpText("Format: [Species, Longitude, Latitude]"),
                                   fileInput("file1",
                                             "",
                                             accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                                   ), #end file1
                                   checkboxInput("header", "Header", TRUE),
                                   radioButtons("sep",
                                                "Separator",
                                                c("Comma" = ",", 
                                                  "Semicolon" = ";", 
                                                  "Tab" = "\t"),
                                                ",",
                                                inline = TRUE
                                   ), #end radioButtons sep
                                   radioButtons("quote",
                                                "Quote",
                                                c("Without" = "",
                                                  "Double" = '"',
                                                  "Simple" = "'"),
                                                '"',
                                                inline = TRUE
                                   ), #end radioButtons quote
                                   actionButton("btnsearch_spdatacsv", 
                                                "Viewer", 
                                                icon = icon("search")
                                   ) #end actionButton
                  ), #end conditionalPanel
                  conditionalPanel("input.bio_datasource != 'csv' ",
                                   helpText("Insert species scientific name"),
                                   textInput("species_name", 
                                             label = "Species name:", 
                                             value = "Abarema_langsdorffii"),
                                   actionButton("btnsearch_spdata", 
                                                "Search", 
                                                icon = icon("search")
                                   ) #end actionButton
                  ) #end conditionalPanel
                ) #end box
              ), #end column 1
              #segunda columna: los datos----
              # "Load occurrence dataset",
              conditionalPanel("btnsearch_spdata",
                               column(width = 8,
                                      shinydashboard::box(
                                        width = NULL,
                                        DT::dataTableOutput("spdata_table")
                                      ), #end box
                                      actionButton("btn_saveDatasetRaw", 
                                                   "Save species dataset",
                                                   icon = icon("next")
                                      ) #end actionButton
                               ) #end column
              ), #end conditionalPanel
              #tercera columna es un panel ahora: el mapa----
              conditionalPanel("btnsearch_spdata",
                               column(width = 12,
                                      solidHeader = TRUE,
                                      leafletOutput("mapadistribuicao"), #isto só deveria aparecer quando o botão search for clicado - tipo um actionButton da vida, se não, dá erro de que não acha occurrences
                                      height = 500
                               ) #end column
              )#ends conditionalPanel
            ) #end tabPanel
          ) #end tabBox
        ) #end column
      ), #end tabPanels occurrence data
      
      
      #### ENVIRONMENTAL DATA ####
      tabPanel(
        "Environmental data",
        column(
          width = 12,
          tabPanel(
            side = "left",
            title = "",
            width = 12,
            height = "600px",
            selected = "Select predictors",
            
            ####A SELECAO DE VARIAVEIS TEM QUE IR ANTES DO EXTENT POR ISSO ESTÁ DANDO EXTENTS DO NOT OVERLAP!!!!!
            # tabPanel: Modeling extent ####
            tabPanel(
              "Modeling extent",
              tabBox(
                side = "left",
                title = "",
                width = NULL,
                height = "600px",
                selected = "Study area extent",
                tabPanel(
                  "Study area extent",
                  shinydashboard::box(
                    width = 8,
                    solidHeader = TRUE,
                    leafletOutput("mapapontosextend", height = 500)
                  ), #end box
                  column(
                    width = 4,
                    solidHeader = TRUE,
                    shinydashboard::box(
                      width = NULL,
                      status = "danger",
                      numericInput(
                        "edtextend11",
                        "Min Lon:",
                        min = -180,
                        max = 180,
                        value = -60,
                        step = 1
                      ), #end numericInput
                      numericInput(
                        "edtextend21",
                        "Max Lon:",
                        min = -180,
                        max = 180,
                        value = -32,
                        step = 1
                      ), #end numericInput
                      numericInput(
                        "edtextend41",
                        "Max Lat:",
                        min = -90,
                        max = 90,
                        value = 5,
                        step = 1
                      ), #end numericInput
                      numericInput(
                        "edtextend31",
                        "Min Lat:",
                        min = -90,
                        max = 90,
                        value = -33,
                        step = 1
                      ) #end numericInput
                    ) #end box
                  ) #end column
                ), #ends tabPanel study area extent
                
                # tabPanel: Select predictors ####
                tabPanel(
                  "Select predictors",
                  column(
                    width = 4,
                    shinydashboard::box(
                      width = NULL,
                      status = "danger",
                      actionButton(
                        "btnAtualizaSelecaoVariaveis",
                        "Update selected"
                      ), #end actionButton
                      selectInput(
                        "tipodadoabiotico",
                        "Variables dataset:",
                        choices = env_datasource,
                        selected = "WorldClim"
                      ), #end selectInput
                      #others
                      conditionalPanel("input.tipodadoabiotico == 'Others' ",
                                       helpText("All layers should have the same spatial extent, resolution, origin, and projection"),
                                       helpText("Before loading multi-file variables, make sure that all corresponding files are placed in the same directory."),
                                       #fileInput(outros, "Select the folder with the environmental variables"),
                                       if (length(list.files("ex/outros/", full.names = T, pattern = c(".*")) > 0)) {
                                         lista_outros <- list.files("ex/outros/", full.names = F, pattern = ".tif|.bil|.grd")
                                         checkboxGroupInput(
                                           "pred_vars_other",
                                           "Select rasters: ",
                                           choiceNames = c(lista_outros),
                                           choiceValues = c(lista_outros)
                                         ) # end checkboxGroupInput
                                       } # end if
                      ), #ends conditionalPanel others
                      
                      # BIOORACLE ####
                      conditionalPanel("input.tipodadoabiotico == 'BIOORACLE' ",
                                       selectInput(
                                         "forecasting_bo",
                                         "Project model across timescales",
                                         choices = forecasting_bo,
                                         selected = "current_bo"
                                       ),#end selectInput
                                       conditionalPanel(
                                         "input.forecasting_bo == 'future_bo'",
                                         shinydashboard::box(
                                           width = NULL,
                                           collapsible = T,
                                           collapsed = T,
                                           title = "Forecasting parameters",
                                           selectInput("future_bo_dates",
                                                       "Choose dates",
                                                       choices = future_bo_dates,
                                                       selected = "2100"
                                           ), #end selectInput
                                           conditionalPanel(
                                             "input.future_bo_dates == '2100'",
                                             selectInput("scenario_bo_2100",
                                                         "Scenario",
                                                         choices = c("A1B", "A2", "B1"),
                                                         selected = "A1B"
                                             ) #end selectInput
                                           ), #end conditionalPanel
                                           conditionalPanel(
                                             "input.future_bo_dates == '2200'",
                                             selectInput("scenario_bo_2200",
                                                         "Scenario",
                                                         choices = c("A1B", "B1"),
                                                         selected = "A1B"
                                             )#end selectInput
                                           ) #end conditionalPanel
                                         ), #end box
                                         checkboxGroupInput(
                                           "pred_vars_bo_fut",
                                           "Select variables: ",
                                           choices = pred_vars_bo_fut
                                         ) # end checkboxGroupInput
                                       ),
                                       
                                       conditionalPanel(
                                         "input.forecasting_bo == 'current_bo'",
                                         checkboxGroupInput("pred_vars_bo",
                                                            "Select variables: ",
                                                            choices = pred_vars_bo 
                                         )# end checkboxGroupInput pred_vasr_bo
                                       ) #end conditionalPanel current_bo
                      ), #end conditionalPanel bioracle
                      
                      # WorldClim ####
                      conditionalPanel("input.tipodadoabiotico == 'WorldClim' ",
                                       selectInput("resolution", 
                                                   "Resolution:", 
                                                   choices = resolution,
                                                   selected = "10min"),
                                       selectInput("forecasting_wc",
                                                   "Project model across timescales",
                                                   choices = forecasting_wc,
                                                   selected = "current_wc"),
                                       conditionalPanel(
                                         "input.forecasting_wc != 'current_wc'",
                                         shinydashboard::box(
                                           width = NULL,
                                           collapsible = T,
                                           collapsed = T,
                                           title = "Set time projetion parameters",
                                           conditionalPanel(
                                             "input.forecasting_wc == 'future_wc' ",
                                             selectInput("future_dates_wc",
                                                         "Choose period: ", 
                                                         choices = future_dates_wc),
                                             selectInput(
                                               "rcp_wc",
                                               "Emission Scenarios (RCP)",
                                               choices = rcp_wc),
                                             selectInput(
                                               "gcm_future_wc",
                                               "General Circulation Models (GCM)",
                                               choices = gcm_future_wc,
                                               selected = "bc")
                                           ), #end conditionalPanel
                                           conditionalPanel(
                                             "input.forecasting_wc == 'past_wc'",
                                             selectInput("past_dates_wc", 
                                                         "Choose period: ",
                                                         choices = past_dates_wc, 
                                                         selected = "mid"),
                                             conditionalPanel(
                                               "input.past_dates_wc == 'mid'",
                                               selectInput(
                                                 "gcm_past_wc_mid",
                                                 "General Circulation Models (GCM)",
                                                 choices = gcm_past_wc_mid)
                                             ), #end conditionalPanel
                                             
                                             conditionalPanel(
                                               "input.past_dates_wc == 'lgm' ",
                                               selectInput(
                                                 "gcm_past_wc_lgm",
                                                 "General Circulation Models (GCM)",
                                                 choices = gcm_past_wc_lgm)
                                             ) #end conditionalPanel
                                           ) #end conditionalPanel
                                         ) #end box
                                       ),
                                       
                                       checkboxGroupInput(
                                         "pred_vars_wc",
                                         "Select variables: ",
                                         pred_vars_wc,
                                         selected = c("bio1", "bio2", "bio3")
                                       ) #end checkbox
                      ) #end worldclim
                    ) #end box
                  ), #end column
                  
                  # Correlation ####
                  column(
                    width = 8,
                    shinydashboard::box(
                      "Check correlation",
                      plotOutput(
                        outputId = "grafico_correlacao",
                        width = "100%",
                        height = "400px"),
                      DT::dataTableOutput("dgbriddadoscorrelacao")
                    ), #end box
                    shinydashboard::box(
                      "View raster layers",
                      plotOutput(outputId = "mapaabiotico", height = "400px")
                    ) #end box
                  ) #end column
                ) #end tabPanel select prd
              )
            )
          )
        )
      ),#ends env data
      
      ####DATA CLEANING AND SETUP####
      tabPanel("Data cleaning",
               column(
                 width = 6,
                 shinydashboard::box(
                   width = NULL,
                   solidHeader = TRUE,
                   leafletOutput("mapadistribuicaodatacleaning", height = 500)
                 )
               ),
               column(
                 width = 6,
                 shinydashboard::box(
                   width = NULL,
                   status = "danger",
                   numericInput(
                     "edtelemento",
                     "Occurrence record ID:",
                     min = 0,
                     max = 100,
                     value = 0
                   ),
                   actionButton("btnapagar", "Delete selected ID", icon = icon("trash")),
                   actionButton("btneliminarduplicatas", "Delete duplicates", icon = icon("cubes")),
                   actionButton("btn_saveDatasetClean", "Save dataset", icon = icon("next"))
                 ),#box
                 shinydashboard::box(
                   width = NULL,
                   DT::dataTableOutput("dgbriddadosdatacleaning")
                 )#box
               )#column
      )#tabpanel
      ,
      #ö faltan muchos parámetros
      ####DATA SETUP####
      tabPanel(
        "Data setup",
        column(
          width = 4,
          shinydashboard::box(
            width = NULL,
            #height = "800px",
            shinydashboard::box(
              #width = 4,
              title = "Data partitioning",
              #height = 400,
              status = "danger",
              ######
              #Partition type
              selectInput(
                "partition_type",
                "Partitioning type",
                choices = partition_type
              ),
              conditionalPanel(
                condition = "input.partition_type == 'crossvalidation'",
                sliderInput(
                  "cv_n",
                  "Number of crossvalidation runs",
                  min = 1,
                  max = 50,
                  value = 1,
                  step = 1
                ),
                sliderInput(
                  "cv_partitions",
                  "Number of partitions:",
                  min = 1,
                  max = 50,
                  value = 3,
                  step = 1
                )
              ),
              conditionalPanel(
                condition = "input.partition_type == 'bootstrap'",
                sliderInput(
                  "boot_proportion",
                  "Proportion of points to be sampled for bootstrap:",
                  min = 0,
                  max = 1,
                  value = 0.8,
                  step = 0.1
                ),
                sliderInput(
                  "boot_n",
                  "Number of bootstrap runs:",
                  min = 1,
                  max = 50,
                  value = 1,
                  step = 1
                )
              )
            ), #fecha el primer box
            shinydashboard::box(
              #width = 4,
              title = "Calibration area settings",
              #height = 800,
              status = "danger",
              
              ######
              #####pseudoabsence sampling
              sliderInput(
                "n_back",
                "Number of pseudo-absence points:",
                min = 100,
                max = 2000,
                value = 300,
                step = 100
              ),
              #buffer_type
              radioButtons(
                "buffer_type",
                "Buffer type:",
                buffer_type
              ),
              conditionalPanel(
                condition = "input.buffer_type == 'distance'",
                sliderInput(
                  inputId = "buf_dist",
                  label = "Distance buffer",
                  min = 1,
                  max = 20,
                  value = 4,
                  step = 1
                )
              ),
              conditionalPanel(
                condition = "input.buffer_type == 'user'",
                fileInput(
                  inputId = "user_shape",
                  label = "User-defined shape",
                  multiple = FALSE,
                  accept = NULL,
                  width = NULL,
                  buttonLabel = "Browse...",
                  placeholder = "No file selected"
                )
              ),
              #mindist
              checkboxInput("exclusion",
                            "Set a minimum distance from occurrences?",
                            value = FALSE),
              
              conditionalPanel(
                condition = "input.exclusion",
                sliderInput(
                  "min_dist",
                  "Minimum distance from occurrences:",
                  min = 0,
                  max = 30,
                  value = 1,
                  step = 1
                )
              ),
              #envfilt
              checkboxInput("env_filt",
                            "Set an environmental distance filter?",
                            value = FALSE ),
              conditionalPanel(
                condition = "input.env_filt",
                selectInput("env_filt",
                  "distance",
                  choices = distance,
                  selected = "centroid"
                ),
                sliderInput(
                  "max_env_dist",
                  "Maximum environmental distance (quantiles)",
                  min = 0,
                  max = 1,
                  value = 0.5,
                  step = 0.1
                )
              )
            ),
            shinydashboard::box(
              #width = 4,
              title = "Occurrence thining",
              #height = 100,
              status = "danger",
              #geo_filt: occurrence thining
              checkboxInput("geo_filt",
                            "Thin occurrences that are too close?",
                            value = FALSE),
              
              conditionalPanel(
                condition = "input.geo_filt",
                textInput(#make it a slider?
                  "geo_filt_dist",
                  "Minimum distance between records (in km):",
                  value = 10
                )
              )
            ), #box
            shinydashboard::box(
              #width = 6,
              #height = 100,
              status = "danger",
              actionButton("btnSetup", "Run", icon = icon("cogs"))
            )
          ) #cierra el box de parametros de setup
        ),#cierra columna de 4
        column(
          width = 6,
          tabsetPanel(
            tabPanel("sdmdata",
                     imageOutput("sdmdata_png", height = 300)
            ),
            tabPanel("smdatatable",
                     DT::dataTableOutput("sdmdata_table")
                     
            )
          )
        )
      ), #cierra pan setup
      ####PROJECTION####
      tabPanel("Projection setup",
               tabBox(
                 width = 12,
                 tabPanel("Project to another extent",
                          height = 1000,
                          shinydashboard::box(
                            #width = 12,
                            status = "danger",
                            checkboxInput("project_ext",
                                          "Project to another geographical extent",
                                          value = FALSE),
                            conditionalPanel(
                              "input.project_ext",
                              shinydashboard::box(
                                width = 8,
                                solidHeader = TRUE,
                                leafletOutput("mapapontosextend2", height = 500)
                              ),
                              
                              shinydashboard::box(
                                width = 4,
                                solidHeader = TRUE,
                                numericInput(
                                  "edtextend12",
                                  "Min Lon:",
                                  min = -180,
                                  max = 180,
                                  value = -60,#isto tem que ser reativo e igual ao valor selecionado pelo usuário
                                  step = 1
                                ),
                                numericInput(
                                  "edtextend22",
                                  "Max Lon:",
                                  min = -180,
                                  max = 180,
                                  value = -32,#isto tem que ser reativo e igual ao valor selecionado pelo usuário
                                  step = 1
                                ),
                                numericInput(
                                  "edtextend42",
                                  "Max Lat:",
                                  min = -90,
                                  max = 90,
                                  value = 5,#isto tem que ser reativo e igual ao valor selecionado pelo usuário
                                  step = 1
                                ),
                                numericInput(
                                  "edtextend32",
                                  "Min Lat:",
                                  min = -90,
                                  max = 90,
                                  value = -33,#isto tem que ser reativo e igual ao valor selecionado pelo usuário
                                  step = 1
                                )
                              )#fecha box
                            )#fecha cond pan
                          )#fecha box
                 ),#ends projection extent
                 tabPanel("Project to another timescale",
                          height = 1000,
                          shinydashboard::box(
                            #width = 12,
                            status = "danger",
                            checkboxInput("project_ext",
                                          "Project to another timescale",
                                          value = FALSE)))
               )#fecha coluna
      ),#fecha projection setup
      #### MODELING ####
      tabPanel(
        "Modeling",
        column(
          width = 12,
          shinydashboard::box(
            width = NULL,
            height = "800px",
            shinydashboard::box(
              width = 6,
              height = 400,
              status = "danger",
              actionButton("btnModelar", "Run", icon = icon("cogs")),
              h4("Which algorithms do you want to fit?"),
              checkboxInput("bioclim", "Bioclim", value = FALSE),
              checkboxInput("brt", "BRT", value = FALSE),
              checkboxInput("domain", "Domain", value = FALSE),
              checkboxInput("glm", "GLM", value = FALSE),
              checkboxInput("mahal", "Mahalanobis", value = FALSE),
              checkboxInput("maxent", "Maxent", value = FALSE),
              checkboxInput("rf", "RandomForest", value = FALSE),
              checkboxInput("svme", "SVM (e1071)", value = FALSE),
              checkboxInput("svmk", "SVM (kernlab)", value = FALSE)
            ),
            shinydashboard::box(
              checkboxGroupInput(
                "threshold",
                "Which threshold do you want to use to generate binary models?",
                choices = c("maxTSS")
              )
            )
            
          )#cierra el box
        )#cierra la columna
      ),#fecha el panel modeling
      tabPanel("Projection results?",
               column(
                 width = 12,
                 tabBox(
                   side = "left",
                   title = "a",
                   width = NULL,
                   conditionalPanel(
                     "input.project_ext",
                     shinydashboard::box(
                       title = "Projection ensemble",
                       width = NULL,
                       height = "320px",
                       leafletOutput(
                         "maparesultado_proj",
                         width = "100%",
                         height = "250px")
                     ) #end box
                   ) #end conditionalPanel
                 ) #end tabBox left
               ) #end column
      ), #end projection
      
      # tabPanel: Final models #####
      tabPanel(
        "Final models",
        column(
          width = 4,
          shinydashboard::box(
            actionButton("btnFinal", "Run", icon = icon("cogs")),
            checkboxGroupInput(
              "algorithms",
              "Make final models for the following algorithms:"
            ) #end checkbox
          ), #end box run final
          shinydashboard::box(
            checkboxInput(
              "select_partitions",
              "Select the best partitions?",
              value = F
            ),
            conditionalPanel(
              "input.select_partitions",
              radioButtons(
                "select_par",
                "Select partitions by:",
                choices = c("TSS", "AUC", "pROC")
                #selected = "TSS"
                ),
              sliderInput(
                "select_par_val",
                paste("select partitions with", "input.select_par", "over"),
                min = 0,
                max = 1,
                value = 0.5,
                step = 0.1)
            ), #end conditionalPanel
            
            checkboxInput(
              "weigh_yesno",
              "Weigh partitions?",
              value = F
            ),
            conditionalPanel(
              "input.weigh_yesno",
              radioButtons(
                "weight_par",
                "Weigh partitions by:",
                choices = weight_par
              )
            ), #end conditionalPanel
            checkboxGroupInput(
              "which_models_final",
              label = "Which output should be created?",
              choices = which_models_final,
              selected = "raw_mean"
            ), #end checkbox
            shinyjs::hidden(
              div(
                id = "cut_bin_final",
                sliderInput(
                  "consensus_level",
                  "Cut binary models at:",
                  min = 0,
                  max = 1,
                  value = 0.5,
                  step = 0.1
            ))),
            checkboxInput(
              "incertidumbre",
              "Calculate uncertainty?",
              value = F)
            
          )#fecha box parametros final
        ),#fecha col
        column(
          width = 8,
          tabsetPanel(id = 'final_tabs',
                      tabPanel("Results")
          )
        )
      ), #end final panel
      ####ensemble----
      tabPanel(
        "Ensemble models",
        column(
          width = 2,
          shinydashboard::box(
            width = NULL,
            actionButton("btnEnsemble", "Run", icon = icon("cogs")),
            checkboxGroupInput(
              inputId = "whichmodelsensemble",
              label = "Which output should be created?",
              choices = which_models_ensemble
            ), #end checkbox
            # [Malu]: Conditionalpanel não funcionava, solução foi usar div - tratando no server tb
            shinyjs::hidden(
              div(
                id = "cut_bin_ensemble",
                sliderInput(
                  "consensus_level_ensemble",
                  "Cut binary models at:",
                  min = 0,
                  max = 1,
                  value = 0.5,
                  step = 0.1
              ))),
            checkboxInput(
              "consensus_ensemble",
              "Apply a consensus between algorithms?",
              value = F)
          ) #end box
        ), #end column 2
        column(
          width = 10,
          shinydashboard::box(
            width = NULL,
            leafletOutput("mapaensemble")
            ) #end box
        )#fecha col 10
      ),#fecha panel
      #### RESULTS ####
      tabPanel(
        "Results",
        column(
          width = 12,
          tabBox(
            side = "left",
            title = selectInput("Select_spdir", "Select species", c(" ")),
            width = NULL,
            tabPanel(
              "Input data",
              shinydashboard::box(
                width = 12,
                h4("SDM data"),
                column(
                  width = 8
                ),
                column(width = 4
                       #imageOutput("sdmdata_png", height = 300)
                )
              ), #fecha box
              column(width = 12,
                     shinydashboard::box(
                       width = NULL,
                       h4("Metadata"),
                       DT::dataTableOutput("metadata_table")
                     )
              )#fecha col
            ), #fecha input
            tabPanel(
              "Stats",
              shinydashboard::box(
                width = 12,
                DT::dataTableOutput("stats")
              )
            ),#fecha tab stats
            tabPanel(
              "Output list",
              column(
                width = 12,
                column(
                  width = 4,
                  shinydashboard::box(
                    width = NULL,
                    status = "danger",
                    h4("Partitions"),
                    htmlOutput("partitions")
                  )#box
                ),#fecha col
                column(
                  width = 4,
                  shinydashboard::box(
                    width = NULL,
                    status = "danger",
                    h4("Final models"),
                    htmlOutput("final")
                  )
                ),
                column(
                  width = 4,
                  shinydashboard::box(
                    width = NULL,
                    status = "danger",
                    h4("Ensemble"),
                    htmlOutput("ensemble")
                  )#box
                )#col
              )#col output 12
            ), #output list
            
            tabPanel(
              "Projections",
              column(
                width = 12,
                shinydashboard::box(
                  width = NULL,
                  status = "danger",
                  h4("Projections"),
                  htmlOutput("projections")
                )#box
              )#col projections 12
            )#fecha tabPanel projections
          )#tabbox
        )#fecha column results 12
      )#termina results
    )#cierra steps
  )#cierra fluid row
)#cierra dashboard body

dashboardPage(
  skin = "red",
  header,
  dashboardSidebar(disable = TRUE),
  body
)

