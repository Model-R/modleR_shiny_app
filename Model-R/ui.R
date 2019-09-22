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
                width = 3,
                box(
                  width = NULL,
                  if (length(list.files("./www/results/") > 0)) {
                    list_projects <- list.files("./www/results/", full.names = F, recursive = F)
                    box(
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
                        radioButtons("modelsDir.load",
                          "Open project:",
                          choices = c(list_projects),
                          choiceValues = c(list_projects),
                          selected = NULL
                        )
                      ),

                      conditionalPanel(
                        "input.select_project == 'new_proj' ",
                        textInput("modelsDir.new", label = "Insert project name: ", value = "")
                      ),
                      actionButton("btnrefreshprojeto", "Submit", icon = icon(""))
                    )
                  } else {
                    box(
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
                )
              ),#
              #aqui a citação, que deveria ser menos importante no layout----
              column(
                  width = 9,
                  box(
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
        ),

        #### OCCURRENCE DATA ####
        tabPanel(
          "Species occurrence data",
          column(
            width = 12,
            tabBox(
              side = "left",
              selected = "Import occurrence dataset",
              title = "",
              width = NULL,
              height = "600px",

              tabPanel(
                "Import occurrence dataset",
                column(
                  width = 12,
                  tabBox(
                    side = "right",
                    selected = "Load occurrence dataset",
                    title = "",
                    width = NULL,
                    height = "600px",
                   # id = "tabset1",

                    tabPanel(
                      "View occurrence map",
                      column(
                        width = 12,
                        box(
                          width = NULL,
                          solidHeader = TRUE,
                          leafletOutput("mapadistribuicao"),
                          height = 500
                        )
                      )
                    ),

                    tabPanel(
                      "Load occurrence dataset",
                      column(
                        width = 8,
                        box(
                          width = NULL,
                          DT::dataTableOutput("spdata_table")
                        ),
                        actionButton("btn_saveDatasetRaw", "Save species dataset", icon = icon("next"))
                      ),

                      column(
                        width = 4,
                        box(
                          width = NULL, status = "warning",
                          helpText("Select species occurrence database or browse csv dataset"),
                          selectInput("bio_datasource", "Occurrence data", bio_datasource, selected = "package_dataset"),

                          conditionalPanel(
                            "input.bio_datasource == 'csv' ",
                            helpText("Format: [Species, Longitude, Latitude]"),
                            fileInput("file1", "", accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                            checkboxInput("header", "Header", TRUE),
                            radioButtons("sep", "Separator",
                              c(
                                "Comma" = ",",
                                "Semicolon" = ";",
                                "Tab" = "\t"
                              ),
                              ",",
                              inline = TRUE
                            ),

                            radioButtons("quote", "Quote",
                              c(
                                "Without" = "",
                                "Double" = '"',
                                "Simple" = "'"
                              ),
                              '"',
                              inline = TRUE
                            ),
                            actionButton("btnsearch_spdatacsv", "Viewer", icon = icon("search"))
                          ),

                          conditionalPanel(
                            "input.bio_datasource != 'csv' ",
                            helpText("Insert species scientific name"),
                            textInput("species_name", label = "Species name:", value = "Abarema_langsdorffii"),
                            actionButton("btnsearch_spdata", "Search", icon = icon("search"))
                          )
                        )
                      )
                    )
                  )
                )
              ),

              tabPanel(
                "Data Cleaning",
                column(
                  width = 6,
                  box(
                    width = NULL,
                    solidHeader = TRUE,
                    leafletOutput("mapadistribuicaodatacleaning", height = 500)
                  )
                ),
                column(
                  width = 6,
                  box(
                    width = NULL,
                    status = "warning",
                    numericInput("edtelemento",
                      "Occurrence record ID:",
                      min = 0,
                      max = 100,
                      value = 0
                    ),
                    actionButton("btnapagar", "Delete selected ID", icon = icon("trash")),
                    actionButton("btneliminarduplicatas", "Delete duplicates", icon = icon("cubes")),
                    actionButton("btn_saveDatasetClean", "Save dataset", icon = icon("next"))
                  ),
                  box(
                    width = NULL,
                    DT::dataTableOutput("dgbriddadosdatacleaning")
                  )
                )
              )
            )
          )
        ),

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
                  side = "right",
                  title = "",
                  width = NULL,
                  height = "600px",
                  selected = "Study area extent",

                  tabPanel(
                    "Projection extent",
                    box(
                      width = 12,
                      status = "warning",
                      checkboxInput("project_ext", "Project to another extension", value = FALSE),

                      conditionalPanel(
                        "input.project_ext",
                        box(
                          width = 8,
                          solidHeader = TRUE,
                          leafletOutput("mapapontosextend2", height = 500)
                        ),

                        box(
                          width = 4,
                          solidHeader = TRUE,
                          numericInput(
                            "edtextend12",
                            "Min Lon:",
                            min = -180,
                            max = 180,
                            value = -90,
                            step = 1
                          ),
                          numericInput(
                            "edtextend22",
                            "Max Lon:",
                            min = -180,
                            max = 180,
                            value = -32,
                            step = 1
                          ),
                          numericInput(
                            "edtextend42",
                            "Max Lat:",
                            min = -90,
                            max = 90,
                            value = 23,
                            step = 1
                          ),
                          numericInput(
                            "edtextend32",
                            "Min Lat:",
                            min = -90,
                            max = 90,
                            value = -33,
                            step = 1
                          )
                        )
                      )
                    )
                  ),

                  tabPanel(
                    "Study area extent",
                    box(
                      width = 8,
                      solidHeader = TRUE,
                      leafletOutput("mapapontosextend", height = 500)
                    ),
                    box(
                      width = 4,
                      solidHeader = TRUE,
                      box(
                        width = NULL,
                        status = "warning",
                        numericInput(
                          "edtextend11",
                          "Min Lon:",
                          min = -180,
                          max = 180,
                          value = -90,
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
                          value = 23,
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
                  )
                )
              ),

              tabPanel(
                "Select Predictors",
                column(
                  width = 5,
                  box(
                    width = NULL,
                    status = "warning",
                    actionButton(
                      "btnAtualizaSelecaoVariaveis",
                      "Update selected"
                    ),
                    selectInput("tipodadoabiotico", "Variables dataset:", env_datasource, selected = "package_dataset"),

                    conditionalPanel(
                      "input.tipodadoabiotico == 'Others' ",
                      helpText("All layers should have the same spatial extent, resolution, origin, and projection"),
                      helpText(""),
                      helpText("Before loading multi-files extentions, make sure that all corresponding files are placed in the same directory."),
                      if (length(list.files("ex/outros/", full.names = T, pattern = c(".*")) > 0)) {
                        lista_outros <- list.files("ex/outros/", full.names = F, pattern = ".tif|.bil|.grd")
                        checkboxGroupInput("pred_vars_other", "Select rasters: ", choiceNames = c(lista_outros), choiceValues = c(lista_outros))
                      }
                    ),

                    conditionalPanel(
                      "input.tipodadoabiotico == 'BIOORACLE' ",
                      selectInput("forecasting_bo",
                        "Project model across timescales",
                        c(
                          "Future" = "future_bo",
                          "None" = "current_bo"
                        ),
                        selected = "current_bo"
                      ),

                      conditionalPanel(
                        "input.forecasting_bo == 'future_bo'",
                        box(
                          width = NULL,
                          collapsible = T,
                          collapsed = T,
                          title = "Forcasting parameters",
                          selectInput("future_bo_dates",
                            "Choose dates",
                            future_bo_dates,
                            selected = "2100"
                          ),

                          conditionalPanel(
                            "input.future_bo_dates == '2100'",
                            selectInput("scenario_bo_2100",
                              "Scenario",
                              choices = c("A1B", "A2", "B1"),
                              selected = "A1B"
                            )
                          ),

                          conditionalPanel(
                            "input.future_bo_dates == '2200'",
                            selectInput("scenario_bo_2200",
                              "Scenario",
                              choices = c("A1B", "B1"),
                              selected = "A1B"
                            )
                          )
                        ),
                        checkboxGroupInput("pred_vars_bo_fut",
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
                        checkboxGroupInput("pred_vars_bo", "Select variables: ",
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
                          )
                        )
                      )
                    ),

                    conditionalPanel(
                      "input.tipodadoabiotico == 'WorldClim' ",
                      selectInput("resolution", "Resolution:", resolution, selected = "10min"),
                      selectInput("forecasting_wc",
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
                        box(
                          width = NULL,
                          collapsible = T,
                          collapsed = T,
                          title = "Set time projetion parameters",

                          conditionalPanel(
                            "input.forecasting_wc == 'future_wc' ",
                            selectInput("future_dates_wc", "Choose period: ", future_dates_wc),
                            selectInput(
                              "rcp_wc", "Emission Scenarios (RCP)",
                              c(
                                "rcp26" = "26",
                                "rcp45" = "45",
                                "rcp60" = "60",
                                "rcp85" = "85"
                              )
                            ),
                            selectInput("gcm_future_wc", "General Circulation Models (GCM)", gcm_future_wc, selected = "bc")
                          ),

                          conditionalPanel(
                            "input.forecasting_wc == 'past_wc'",
                            selectInput("past_dates_wc", "Choose period: ", past_dates_wc, selected = "mid"),
                            conditionalPanel(
                              "input.past_dates_wc == 'mid'",
                              selectInput("gcm_past_wc_mid", "General Circulation Models (GCM)", gcm_past_wc_mid)
                            ),

                            conditionalPanel(
                              "input.past_dates_wc == 'lgm' ",
                              selectInput("gcm_past_wc_lgm", "General Circulation Models (GCM)", gcm_past_wc_lgm)
                            )
                          )
                        )
                      ),
                      checkboxGroupInput("pred_vars_wc", "Select variables: ",
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
                        )
                      )
                    )
                  )
                ),
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
                    )
                  )
                )
              )
            )
          )
        ),
        ####DATA CLEANING AND SETUP####
        tabPanel("Data cleaning and setup",
                 column(
                     width = 6,
                     tabBox(side = "left",
                            title = "a",
                            width = NULL,
                            tabPanel("Occurrence data cleaning"),
                            tabPanel("Modeling round setup"))
                 )),
        ####PROJECTION####
        tabPanel("Projection",
                 column(
                     width = 6,
                     tabBox(side = "left",
                            title = "a",
                            width = NULL,
                            tabPanel("Projection extent"),
                            tabPanel("Projection timescales"))
                 )),
        #### MODELING ####
        tabPanel(
          "Modeling",
          column(
            width = 6,
            tabBox(
              side = "left",
              title = "",
              width = NULL,

              tabPanel(
                "BC",
                column(
                  width = 12,
                  leafletOutput("maparesultadobc")
                )
              ),

              tabPanel(
                "MH",
                column(
                  width = 12,
                  leafletOutput("maparesultadomh")
                )
              ),

              tabPanel(
                "MX",
                column(
                  width = 12,
                  leafletOutput("maparesultadomax")
                )
              ),

              tabPanel(
                "GLM",
                column(
                  width = 12,
                  leafletOutput("maparesultadoglm")
                )
              ),

              tabPanel(
                "RF",
                column(
                  width = 12,
                  leafletOutput("maparesultadorf")
                )
              ),

              tabPanel(
                "SVM.K",
                column(
                  width = 12,
                  leafletOutput("maparesultadosvm.k")
                )
              ),

              tabPanel(
                "SVM.E",
                column(
                  width = 12,
                  leafletOutput("maparesultadosvm.e")
                )
              ),

              tabPanel(
                "DO",
                column(
                  width = 12,
                  leafletOutput("maparesultadodo")
                )
              ),

              tabPanel(
                "ENSEMBLE",
                column(
                  width = 12,
                  leafletOutput("maparesultadoensemble")
                )
              )
            )
          ),
          column(
            width = 6,
            box(
              width = NULL,
              height = "800px",
              box(
                width = 6,
                height = 400,
                status = "warning",
                checkboxInput("geo_filt",
                  "Delete occurrence that are too close?",
                  value = FALSE
                ),

                conditionalPanel(
                  condition = "input.geo_filt",
                  textInput("geo_filt_dist",
                    "Minimum distance between records (in km):",
                    value = NULL
                  )
                ),
                sliderInput(
                  "n_back",
                  "Pseudo-absences points:",
                  min = 100,
                  max = 2000,
                  value = 300,
                  step = 100
                ),
                sliderInput(
                  "TSS",
                  "TSS score cutoff:",
                  min = 0,
                  max = 1,
                  value = 0.7,
                  step = 0.1
                ),
                radioButtons(
                  "buffer_type", "Buffer type :",
                  c(
                    "Mean" = "mean",
                    "Median" = "median",
                    "Maximal" = "maximal"
                  )
                ),
                checkboxInput("brt",
                  "Execute boosted regression trees?",
                  value = FALSE
                ),
                selectInput("partition_type",
                  "Partitioning type",
                  choices = c(
                    "crossvalidation",
                    "bootstrap"
                  )
                ),

                conditionalPanel(
                  condition = "input.partition_type == 'crossvalidation'",
                  sliderInput("cv_n",
                    "Number of crossvalidation runs",
                    min = 1,
                    max = 50,
                    value = 1,
                    step = 1
                  ),
                  sliderInput("cv_partitions",
                    "Number of partitions:",
                    min = 1,
                    max = 50,
                    value = 3,
                    step = 1
                  )
                ),

                conditionalPanel(
                  condition = "input.partition_type == 'bootstrap'",
                  sliderInput("boot_proportion",
                    "Proportion of points to be sampled for bootstrap:",
                    min = 0,
                    max = 1,
                    value = 0.8,
                    step = 0.1
                  ),
                  sliderInput("boot_n",
                    "Number bootstrap runs:",
                    min = 1,
                    max = 50,
                    value = 1,
                    step = 1
                  )
                )
              ),
              box(
                width = 6,
                height = 400,
                status = "warning",
                actionButton("btnModelar", "Run", icon = icon("cogs")),
                h4("Algorithms"),
                checkboxInput("bioclim", "Bioclim", value = FALSE),
                checkboxInput("mahal", "Mahalanobis", value = FALSE),
                checkboxInput("maxent", "Maxent", value = FALSE),
                checkboxInput("glm", "GLM", value = FALSE),
                checkboxInput("rf", "RandomForest", value = FALSE),
                checkboxInput("svm.k", "SVM.K", value = FALSE),
                checkboxInput("svm.e", "SVM.E", value = FALSE),
                checkboxInput("domain", "Domain", value = FALSE)
              ),

              conditionalPanel(
                "input.project_ext",
                box(
                  title = "Projection ensemble",
                  width = NULL,
                  height = "320px",
                  leafletOutput("maparesultado_proj",
                    width = "100%",
                    height = "250px"
                  )
                )
              )
            )
          )
        ),

        #### RESULTS ####
        tabPanel(
          "Results",
          column(
            width = 12,
            tabBox(
              side = "left",
              title = selectInput(
                "Select_spdir", "Select species",
                c(" ")
              ),
              width = NULL,

              tabPanel(
                "Input data",
                box(
                  width = 12,
                  h4("SDM data"),
                  column(
                    width = 8,
                    DT::dataTableOutput("sdmdata_table")
                  ),
                  column(
                    width = 4,
                    imageOutput("sdmdata_png", height = 300)
                  )
                ),
                column(
                  width = 12,
                  box(
                    width = NULL,
                    h4("Metadata"),
                    DT::dataTableOutput("metadata_table")
                  )
                )
              ),

              tabPanel(
                "Stats",
                box(
                  width = 12,
                  DT::dataTableOutput("stats")
                )
              ),

              tabPanel(
                "Output list",
                column(
                  width = 12,
                  column(
                    width = 4,
                    box(
                      width = NULL,
                      status = "warning",
                      h4("Partitions"),
                      htmlOutput("partitions")
                    )
                  ),
                  column(
                    width = 4,
                    box(
                      width = NULL,
                      status = "warning",
                      h4("Final models"),
                      htmlOutput("final")
                    )
                  ),
                  column(
                    width = 4,
                    box(
                      width = NULL,
                      status = "warning",
                      h4("Ensemble"),
                      htmlOutput("ensemble")
                    )
                  )
                )
              ),

              tabPanel(
                "Projections",
                column(
                  width = 12,
                  column(
                    width = 12,
                    box(
                      width = NULL,
                      status = "warning",
                      h4("Projections"),
                      htmlOutput("projections")
                    )
                  )
                )
              )
            )
          )
        ),

        #### HELP ####
        tabPanel("Help")
      )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
