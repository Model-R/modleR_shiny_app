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
## MARIA LUISA MONDELLI    ##
##                         ##
## 21 DE SETEMBRO DE 2019  ##
#############################


# Thanks to Steven Worthington for function ipak https://gist.github.com/stevenworthington/3178163 (HT Karlo Guidoni Martins)

# ast: isto tem que sair
# ipak <- function(pkg) {
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#   if (length(new.pkg)) {
#     install.packages(new.pkg, dependencies = TRUE)
#   }
#   sapply(pkg, require, character.only = TRUE)
# }
#
library(shinydashboard)
library(leaflet)
library(DT)
devtools::load_all("../../modleR")
#### VARIABLES ####
bio_datasource <- c(
  "The example dataset" = "package_dataset",
  "GBif - The Global Biodiversity Information Facility" = "gbif",
  "Jabot - JBRJ Database" = "jabot",
  "CSV - Comma Separated Values" = "csv"
)

env_datasource <- c(
  "The example dataset" = "package_dataset",
  "WorldClim v.1.4" = "WorldClim",
  "Bio-ORACLE v.1" = "BIOORACLE",
  "Upload Dataset" = "Others"
)

resolution <- c(
  "10 arc-minutes" = "10m",
  "5 arc-minutes" = "5m",
  "2.5 arc-minutes" = "2-5m",
  "30 arc-seconds" = "30s"
)

wc_forecasting_timescale <- c(
  "Future conditions" = "future",
  "Past conditions" = "past"
)

future_dates_wc <- c(
  "2050" = "2050",
  "2070" = "2070"
)

gcm_future_wc <- c(
  "BCC-CSM1-1" = "bc",
  "CCSM4" = "cc",
  "GISS-E2-R" = "gs",
  "HadGEM2-AO" = "hd",
  "HadGEM2-ES" = "he",
  "IPSL-CM5A-LR" = "ip",
  "MIROC-ESM-CHEM" = "mi",
  "MIROC-ESM" = "mr",
  "MIROC5" = "mc",
  "MRI-CGCM3" = "mg",
  "NorESM1-M" = "no"
)

gcm_past_wc_mid <- c(
  "CCSM4" = "cc",
  "MIROC-ESM" = "mr",
  "MPI-ESM-P" = "me",
  "BCC-CSM1-1" = "bc",
  "CNRM-CM5 " = "cn",
  "HadGEM2-CC" = "hg",
  "HadGEM2-ES" = "he",
  "IPSL-CM5A-LR" = "ip",
  "MRI-CGCM3" = "mg"
)

gcm_past_wc_lgm <- c(
  "CCSM4" = "cc",
                     "MIROC-ESM" = "mr",
  "MPI-ESM-P" = "me"
)

rcp <- c(
  "rcp26" = "26",
  "rcp45" = "45",
  "rcp60" = "60",
  "rcp85" = "85"
)
past_dates_wc <- c(
  "Mid Holocene" = "mid",
  "Last Glacial Maximum" = "lgm"
)

future_bo_dates <- c(
  "2100" = "2100",
  "2200" = "2200"
)

#### DASHBOARD #####why 2.0 but neyse
header <- shinydashboard::dashboardHeader(title = "modleR 2.0")
body <- dashboardBody(
  fluidRow(
    column(
      width = 12,
      tabBox(
        side = "left",
        title = "Steps",
        width = NULL,
        height = "1000px",

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
                      status = "primary",
                      solidHeader = TRUE,
                      width = NULL,
                      selectInput("select_project",
                                  label = "Select project: ",
                                  choices = c(
                                    "Create new project" = "new_proj",
                                    "Open project" = "load_proj"
                                    )
                                  ),

                      conditionalPanel(
                        "input.select_project == 'load_proj' ",
                        radioButtons(
                          "modelsDir.load",
                          "Open project:",
                          choiceNames = c(list_projects),
                          choiceValues = c(list_projects),
                          selected = NULL
                          )
                        ),

                      conditionalPanel(
                        "input.select_project == 'new_proj'",
                        textInput("modelsDir.new", label = "Insert project name:",
                                  value = "")
                        ),
                      actionButton("btnrefreshprojeto", "Submit", icon = icon(""))
                      )
                    } else {
                      shinydashboard::box(
                        title = "Create/Open project",
                        status = "primary",
                        solidHeader = TRUE,
                        width = NULL,
                        selectInput("select_project", "",
                                    choices = c("Create new project" = "new_proj")
                                    ),

                        conditionalPanel(
                          "input.select_project == 'new_proj' ",
                          textInput("modelsDir.new", label = "Insert project name: ", value = "")
                          ),
                        actionButton("btnrefreshprojeto", "Submit", icon = icon("ok", lib = "glyphicon"))
                      )
                      }
                  )#fecha box
                ),#fecha col
              #aqui a citação, que deveria ser menos importante no layout----
              column(
                width = 6,
                shinydashboard::box(
                  width = NULL,
                  column(
                    width = 6,
                    br(),
                    img(src = "logo.png", width = 200)
                    ),
                  column(
                    width = 9,
                    h4("A workflow to perform Environmental Niche Modeling based on dismo")
                    ),
                  column(
                    width = 12,
                    br(),
                    p("Please cite:"),
                    br(),
                    p(
                      "Sánchez-Tapia, Andrea ; de Siqueira, Marinez Ferreira ; Lima, Rafael Oliveira ; Barros, Felipe Sodré M. ; Gall, Guilherme M. ; Gadelha, Luiz M. R. ; da Silva, Luís Alexandre E. ; Osthoff, Carla . Model-R: A Framework for Scalable and Reproducible Ecological Niche Modeling. Communications in Computer and Information Science. 1ed.: Springer International Publishing, 2018, v. 796, p. 218-232."
                          ),
                    br(),
                    p("...ABSTRACT...")
                    )
                  )
                )
              )
            )
          ),#fecha tab project
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
                       status = "warning",
                       helpText("Select species occurrence database or browse csv dataset"),
                       selectInput("bio_datasource",
                                   "Occurrence data",
                                   bio_datasource,
                                   selected = "package_dataset"),
                       #dependiendo de lo que selecciona cambia
                       conditionalPanel(
                         "input.bio_datasource == 'csv' ",
                         helpText("Format: [Species, Longitude, Latitude]"),
                         fileInput(
                           "file1",
                           "",
                           accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")
                         ),
                         checkboxInput("header", "Header", TRUE),
                         radioButtons(
                           "sep",
                           "Separator",
                           c(
                             "Comma" = ",",
                             "Semicolon" = ";",
                             "Tab" = "\t"
                           ),
                           ",",
                           inline = TRUE
                         ),
                         radioButtons(
                           "quote",
                           "Quote",
                           c(
                             "Without" = "",
                             "Double" = '"',
                             "Simple" = "'"
                           ),
                           '"',
                           inline = TRUE
                         ),
                         actionButton("btnsearch_spdatacsv", "Viewer", icon = icon("search"))
                       )
                       ,
                       conditionalPanel(
                         "input.bio_datasource != 'csv' ",
                         helpText("Insert species scientific name"),
                         textInput("species_name", label = "Species name:", value = "Abarema_langsdorffii"),
                         actionButton("btnsearch_spdata", "Search", icon = icon("search"))
                       )
                     )
                   ), #ends primera columna
                   #segunda columna: los datos----
                   # "Load occurrence dataset",
                   conditionalPanel(
                     "btnsearch_spdata",
                     column(
                       width = 8,
                       shinydashboard::box(
                         width = NULL,
                         DT::dataTableOutput("spdata_table")
                         ),
                       actionButton("btn_saveDatasetRaw", "Save species dataset",
                                    icon = icon("next"))
                     )#ends columna
                   ),#ends condpanel
                   #tercera columna es un panel ahora: el mapa----
                   conditionalPanel(
                     "btnsearch_spdata",
                     column(
                       width = 12,
                       solidHeader = TRUE,
                       leafletOutput("mapadistribuicao"),#isto só deveria aparecer quando o botão search for clicado - tipo um actionButton da vida, se não, dá erro de que não acha occurrences
                       height = 500
                     )#ends columna
                   )#ends condpan
                 ),#ends tabpanel
                 #aqui datacleaning velho
                 tabPanel(
                   "Data Cleaning",
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
                       status = "warning",
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
               )#tabbox
             )#column
          ),#tabpanelsp occ data


        #### ENVIRONMENTAL DATA ####
        tabPanel(
          "Environmental data",
          column(
            width = 12,
            tabBox(
              side = "left",
              title = "",
              width = NULL,
              height = "600px",
              selected = "Modeling extent",

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
                    ),
                    shinydashboard::box(
                      width = 4,
                      solidHeader = TRUE,
                      shinydashboard::box(
                        width = NULL,
                        status = "warning",
                        numericInput(
                          "edtextend11",
                          "Min Lon:",
                          min = -180,
                          max = 180,
                          value = -60,
                          step = 1
                        ),
                        numericInput(
                          "edtextend21",
                          "Max Lon:",
                          min = -180,
                          max = 180,
                          value = -32,
                          step = 1
                        ),
                        numericInput(
                          "edtextend41",
                          "Max Lat:",
                          min = -90,
                          max = 90,
                          value = 5,
                          step = 1
                        ),
                        numericInput(
                          "edtextend31",
                          "Min Lat:",
                          min = -90,
                          max = 90,
                          value = -33,
                          step = 1
                        )
                      )
                                )
                              )#ends study area extent
                              ,
                              tabPanel(
                                "Projection extent",
                                shinydashboard::box(
                                  width = 12,
                                  status = "warning",
                                  checkboxInput("project_ext", "Project to another extension", value = FALSE),

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
                                    )
                                  )
                                )
                              )#ends projection extent
                            )#ends study area extent
                          ),#ends modeling extent
                          # Select predictors----
                          tabPanel(
                            "Select Predictors",
                            column(
                              width = 5,
                              shinydashboard::box(
                                width = NULL,
                                status = "warning",
                                actionButton(
                                  "btnAtualizaSelecaoVariaveis",
                                             "Update selected"
                                  ),
                                selectInput(
                                  "tipodadoabiotico",
                                  "Variables dataset:",
                                  env_datasource,
                                  selected = "WorldClim"
                                ),
                                #datos del paquete
                                #        conditionalPanel(
                                #"input.tipodadoabiotico == package_dataset",
                                #checkboxGroupInput(inputId = "pred_vars_pac",
                                #                   label = "Select variables: ",
                                #                   choices = c("PC1" = "layer.1",
                                #                               "PC2" = "layer.2",
                                #                               "PC3" = "layer.3",
                                #                               "PC4" = "layer.4",
                                #                               "PC5" = "layer.5",
                                #                               "PC6" = "layer.6"),
                                #                   selected = c("layer.1", "layer.2", "layer.3", "layer.4", "layer.5", "layer.6"))#closes checkbox
                                #),#ends datos del paquete
                                conditionalPanel(
                                  #others
                                  "input.tipodadoabiotico == 'Others' ",
                                  helpText("All layers should have the same spatial extent, resolution, origin, and projection"),
                                  helpText(""),
                                  helpText(
                                    "Before loading multi-file variables, make sure that all corresponding files are placed in the same directory."
                                  ),
                                  if (length(list.files("ex/outros/", full.names = T, pattern = c(".*")) > 0)) {
                                    lista_outros <- list.files("ex/outros/", full.names = F, pattern = ".tif|.bil|.grd")
                                    checkboxGroupInput(
                                      "pred_vars_other",
                                      "Select rasters: ",
                                      choiceNames = c(lista_outros),
                                      choiceValues = c(lista_outros)
                                    )
                                  }
                                ),#ends others

                                #todo biooracle
                                conditionalPanel(
                                  "input.tipodadoabiotico == 'BIOORACLE' ",
                                  selectInput(
                                    "forecasting_bo",
                                    "Project model across timescales",
                                    c("Future" = "future_bo",
                                      "None" = "current_bo"),
                                    selected = "current_bo"
                                  ),
                                  conditionalPanel(
                                    "input.forecasting_bo == 'future_bo'",
                                    shinydashboard::box(
                                      width = NULL,
                                      collapsible = T,
                                      collapsed = T,
                                      title = "Forecasting parameters",
                                      selectInput("future_bo_dates",
                                                  "Choose dates",
                                                  future_bo_dates,
                                                  selected = "2100"),
                                      conditionalPanel(
                                        "input.future_bo_dates == '2100'",
                                        selectInput(
                                          "scenario_bo_2100",
                                          "Scenario",
                                          choices = c("A1B", "A2", "B1"),
                                          selected = "A1B"
                                        )
                                      ),
                                      conditionalPanel(
                                        "input.future_bo_dates == '2200'",
                                        selectInput(
                                          "scenario_bo_2200",
                                          "Scenario",
                                          choices = c("A1B", "B1"),
                                          selected = "A1B"
                                        )
                                      )
                                    ),
                                    checkboxGroupInput(
                                      "pred_vars_bo_fut",
                                      "Select variables: ",
                                      choices = c(
                                        "Temperature (Max) " = "sstmax",
                                        "Temperature (Min) " = "sstmin",
                                        "Temperature (Range)" = "sstrange",
                                        "Temperature (Mean)" = "sstmean",
                                        "Salinity" = "salinity"
                                      )
                                    )
                                  ),

                                  conditionalPanel(
                                    "input.forecasting_bo == 'current_bo'",
                                    checkboxGroupInput(
                                      "pred_vars_bo",
                                      "Select variables: ",
                                      choices = c(
                                        "Temperature (Max) " = "sstmax",
                                        "Temperature (Min) " = "sstmin",
                                        "Temperature (Range)" = "sstrange",
                                        "Temperature (Mean)" = "sstmean",
                                        "Salinity" = "salinity",
                                        "Calcite" = "calcite",
                                        "Nitrate" = "nitrate",
                                        "pH" = "ph",
                                        "Silicate" = "silicate",
                                        "Phosphate" = "phosphate",
                                        "Dissolved mol. oxygen" = "dissox",
                                        "Chlorophyll (Min)" = "chlomin",
                                        "Chlorophyll (Max)" = "chlomax",
                                        "Chlorophyll (Range)" = "chlorange",
                                        "Chlorophyll(Mean)" = "chlomean",
                                        "Cloud cover (Mean)" = "cloudmean",
                                        "Cloud cover (Max)" = "cloudmax",
                                        "Cloud cover (Min)" = "cloudmin",
                                        "Diffuse attenuation (Mean)" = "damean",
                                        "Diffuse attenuation (Min)" = "damin",
                                        "Diffuse attenuation (Max)" = "damax",
                                        "Photosynt. Avail. Radiation (Max)" = "parmax",
                                        "Photosynt. Avail. Radiation (Mean)" = "parmean"
                                      )#choices
                                    )#pred_vasr_bo
                                  )#current_bo
                                ),#ends bioracle
                                conditionalPanel(#worldclim
                                  "input.tipodadoabiotico == 'WorldClim' ",
                                  selectInput("resolution", "Resolution:", resolution,
                                              selected = "10min"),
                                  selectInput(
                                    "forecasting_wc",
                                    "Project model across timescales",
                                    c(
                                      "Future" = "future_wc",
                                      "Past" = "past_wc",
                                      "None" = "current_wc"
                                    ),
                                    selected = "current_wc"
                                  ),
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
                                                    "Choose period: ", future_dates_wc),
                                        selectInput(
                                          "rcp_wc",
                                          "Emission Scenarios (RCP)",
                                          c(
                                            "rcp26" = "26",
                                            "rcp45" = "45",
                                            "rcp60" = "60",
                                            "rcp85" = "85"
                                          )
                                        ),
                                        selectInput(
                                          "gcm_future_wc",
                                          "General Circulation Models (GCM)",
                                          gcm_future_wc,
                                          selected = "bc"
                                        )
                                      ),

                                      conditionalPanel(
                                        "input.forecasting_wc == 'past_wc'",
                                        selectInput("past_dates_wc", "Choose period: ",
                                                    past_dates_wc, selected = "mid"),
                                        conditionalPanel(
                                          "input.past_dates_wc == 'mid'",
                                          selectInput(
                                            "gcm_past_wc_mid",
                                            "General Circulation Models (GCM)",
                                            gcm_past_wc_mid
                                          )
                                        ),

                                        conditionalPanel(
                                          "input.past_dates_wc == 'lgm' ",
                                          selectInput(
                                            "gcm_past_wc_lgm",
                                            "General Circulation Models (GCM)",
                                            gcm_past_wc_lgm
                                          )
                                        )
                                      )
                                    )
                                  ),

                                  checkboxGroupInput(
                                    "pred_vars_wc",
                                    "Select variables: ",
                                    choices = c(
                                      "(Bio1) Annual Mean Temperature" = "bio1",
                                      "(Bio2) Mean Diurnal Range" = "bio2",
                                      "(Bio3) Isothermality" = "bio3",
                                      "(Bio4) Temperature Seasonality" = "bio4",
                                      "(Bio5) Max Temperature of Warmest Month" = "bio5",
                                      "(Bio6) Min Temperature of Coldest Month" = "bio6",
                                      "(Bio7) Temperature Annual Range" = "bio7",
                                      "(Bio8) Mean Temperature of Wettest Quarter" = "bio8",
                                      "(Bio9) Mean Temperature of Driest Quarter" = "bio9",
                                      "(Bio10) Mean Temperature of Warmest Quarter" = "bio10",
                                      "(Bio11) Mean Temperature of Coldest Quarter" = "bio11",
                                      "(Bio12) Annual Precipitation" = "bio12",
                                      "(Bio13) Precipitation of Wettest Month" = "bio13",
                                      "(Bio14) Precipitation of Driest Month" = "bio14",
                                      "(Bio15) Precipitation Seasonality" = "bio15",
                                      "(Bio16) Precipitation of Wettest Quarter" = "bio16",
                                      "(Bio17) Precipitation of Driest Quarter" = "bio17",
                                      "(Bio18) Precipitation of Warmest Quarter" = "bio18",
                                      "(Bio19) Precipitation of Coldest Quarter" = "bio19"
                                    ),#ends choices
                                    selected = c("bio1", "bio2", "bio3")
                                  )#ends checkbox
                                )#ends worldclim
                              )#ends box!
                            ),#ends column
                            column(
                              width = 7,
                              tabBox(
                                width = NULL,
                                side = "right",
                                selected = "Check correlation",
                                tabPanel(
                                  "View raster layers",
                                  plotOutput(outputId = "mapaabiotico", height = "400px")
                                ),

                                tabPanel(
                                  "Check correlation",
                                  plotOutput(outputId = "grafico_correlacao", width = "100%", height = "400px"),
                                  DT::dataTableOutput("dgbriddadoscorrelacao")
                                )#tabpan
                              )#tabbox
                            )#col
                            )#tab panel select pred
                        )#tabbox
                      )#end column
                      ),#ends env data
    ####DATA CLEANING AND SETUP####
    tabPanel("Data cleaning"),
    #ö faltan muchos parámetros
    ####DATA SETUP####
    tabPanel(
      "Data setup",
      shinydashboard::box(
        width = NULL,
        height = "800px",
        shinydashboard::box(
          width = 6,
          title = "Data partitioning",
          height = 400,
          status = "warning",
          ######
          #Partition type
          selectInput(
            "partition_type",
            "Partitioning type",
            choices = c("crossvalidation",
                        "bootstrap")
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
          width = 6,
          title = "Calibration area settings",
          height = 800,
          status = "warning",

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
            c(
              "Mean" = "mean",
              "Median" = "median",
              "Maximal" = "maximum",
              "Distance" = "distance",
              "User-defined shapefile" = "user"
            )
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
                        value = FALSE),
          conditionalPanel(
            condition = "input.env_filt",
            selectInput(
              "distance",
              "env_dist",
              choices = c("centroid", "mindist"),
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
          width = 6,
          title = "Occurrence thining",
          height = 100,
          status = "warning",
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
          width = 6,
          height = 100,
          status = "warning",
          actionButton("btnSetup", "Run", icon = icon("cogs"))
        )
      ) #cierra el box de parametros de setup
    ), #cierra pan setup
    ####PROJECTION####
    tabPanel("Projection",
             column(
               width = 12,
               tabBox(
                 side = "left",
                 title = "a",
                 width = NULL,
                 tabPanel("Projection extent"),
                 tabPanel("Projection timescales")
               )
             )
             ),
    #### MODELING ####
    tabPanel(
      "Modeling",
      column(
        width = 12,
        tabBox(
          side = "left",
          title = "",
          width = 6,
          tabPanel(
            "Algorithms",
            column(width = 6,
                   shinydashboard::box(
                     width = NULL,
                     height = "800px",
                     shinydashboard::box(
                       width = 6,
                       height = 400,
                       status = "warning",
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
                     conditionalPanel(
                       "input.project_ext",
                       shinydashboard::box(
                         title = "Projection ensemble",
                         width = NULL,
                         height = "320px",
                         leafletOutput(
                           "maparesultado_proj",
                           width = "100%",
                           height = "250px"
                         )
                         )
                       )#cierra el conditional panel
                     )#cierra el box
                   )#cierra la columna
                        )#cierra el tab algorithm
        )
      )
          ),#fecha el panel modeling
    ###tab final ----
    tabPanel(
      "Final models",
      column(
        width = 12,
        tabPanel(
          "",
          side = "left",
          width = 3,
          shinydashboard::box(
            width = NULL,
            actionButton("btnFinal", "Run", icon = icon("cogs")),
            checkboxGroupInput(
              "which_final",
              "Make final models for the following algorithms:",
              choices = c(
                "Bioclim" = "bioclim",
                "BRT" = "brt" ,
                "Domain" = "domain",
                "GLM" = "glm",
                "Mahalanobis" = "mahal",
                "Maxent" = "maxent",
                "RandomForest" = "rf",
                "SVM (e1071)" = "svme",
                "SVM (kernlab)" = "svmk"
              )
              )#fecha checkbox
            ),#fecha box run final
          shinydashboard::box(
            checkboxInput(
              "selectpartitions",
              "Select the best partitions?",
              value = F
              ),
            conditionalPanel(
              "input.selectpartitions",
              radioButtons(
                "select_par",
                "Select partitions by:",
                choices = c("TSS", "AUC", "pROC")
                ),
              sliderInput(
                "select_par_val",
                "over",
                min = 0,
                max = 1,
                value = 0.7,
                step = 0.1
                )
              ),#fecha cond

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
                choices = c("TSS", "AUC", "pROC")
              )
            ),#fecha cond
            checkboxGroupInput(
              "which_models_final",
              label = "Which output should be created?",
              choices = c("raw_mean",
                          "raw_mean_th",
                          "raw_mean_cut",
                          "bin_mean",
                          "bin_consensus",
                          "cut_mean"),
              selected = "raw_mean"
            ),#fecha check
            conditionalPanel(
              "'bin_consensus' %in% input.which_models_final",
              sliderInput(
                "consensus_level",
                "Cut binary models at:",
                min = 0,
                max = 1,
                value = 0.5,
                step = 0.1
              )
              )#fecha conditional consensus
            )#fecha box parametros final
          ),#fecha tabbox
        tabPanel("bioclim",
                 column(width = 12,
                        leafletOutput("mapafinalbc"))),
        tabPanel("brt",
                 column(width = 12,
                        leafletOutput("mapafinalbrt"))),
        tabPanel("domain",
                 column(width = 12,
                        leafletOutput("mapafinaldo"))),
        tabPanel("GLM",
                 column(width = 12,
                        leafletOutput("mapafinalglm"))),
        tabPanel("mahal",
                 column(width = 12,
                        leafletOutput("mapafinalmh"))),
        tabPanel("maxent",
                 column(width = 12,
                        leafletOutput("mapafinalmax"))),
        tabPanel("RF",
                 column(width = 12,
                        leafletOutput("mapafinalrf"))),
        tabPanel("SVME",
                 column(width = 12,
                        leafletOutput("mapafinalsvme"))),
        tabPanel("SVMK",
                 column(width = 12,
                        leafletOutput("mapafinalsvmk")))#cierra svmk
        )#cierra col
      ),#fecha final panel
    ####ensemble----
    tabPanel(
      "Ensemble models",
      column(
        width = 6,
        tabBox(
          side = "left",
          title = "",
          width = NULL,
          tabPanel("",
                   side = "left",
                   shinydashboard::box(
                     width = NULL,
                     actionButton("btnEnsemble", "Run", icon = icon("cogs")),
                     leafletOutput("mapaensemble")
                     )
                   )#fecha ensemble tab
          )#fecha el box con los modelos
        )#fecha col
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
                width = 8,
                DT::dataTableOutput("sdmdata_table")
                ),
              column(width = 4,
                     imageOutput("sdmdata_png", height = 300)
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
                  status = "warning",
                  h4("Partitions"),
                  htmlOutput("partitions")
                )#box
              ),#fecha col
              column(
                width = 4,
                shinydashboard::box(
                  width = NULL,
                  status = "warning",
                  h4("Final models"),
                  htmlOutput("final")
                )
              ),
              column(
                width = 4,
                shinydashboard::box(
                  width = NULL,
                  status = "warning",
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
                  status = "warning",
                  h4("Projections"),
                  htmlOutput("projections")
                )#box
            )#col projections 12
          )#fecha tabPanel projections
        )#tabbox
      )#fecha column results 12
    )#termina results
  )#cierra steps
)#cierra columna 12
)#cierra fluid row
)#cierra dashboard body
dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
  )
