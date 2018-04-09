#############################
## ----   MODEL-R    ----  ##
##                         ##
## ANDREA SÁNCHEZ TAPIA    ##
## FELIPE SODRÉ BARROS     ##
## GUILHERME GALL          ##
## DIOGO SOUZA B. ROCHA    ##
## RAFAEL OLIVEIRA LIMA    ##
## RENATA DE T. CAPELLÃO   ##
##                         ##
## 08 DE FEVEREIRO DE 2018 ##
#############################

# Thanks to Steven Worthington for function ipak https://gist.github.com/stevenworthington/3178163 (HT Karlo Guidoni Martins)

###############################################################################

ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(c("shinydashboard", "leaflet"))

bio_datasource <- c(
  "GBif - The Global Biodiversity Information Facility" = "gbif",
  "Jabot - JBRJ Database" = "jabot",
  "CSV - Comma Separated Values" = "csv"
)

env_datasource <- c(
  "WorldClim v.1.4" = "CLIMA",
  "Bio-ORACLE v.1" = "BIOORACLE",
  "Upload Dataset" = "Others"
)


resolution <- c(
  "10 arc-minutes" = "10m",
  "5 arc-minutes" = "5m",
  "2.5 arc-minutes" = "2-5m",
  "30 arc-seconds" = "30s"
)
wc_forecasting_timescale <- c( 'Future conditions' = 'future',
  'Past conditions' = 'past')
  
future_dates_wc <- c("2050" = "2050",
  "2070" = "2070")

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

gcm_past_wc_mid<-c(
  "CCSM4" = "cc",
  "MIROC-ESM" = "mr",
  "MPI-ESM-P" = "me",
  "BCC-CSM1-1" = "bc",
  "CNRM-CM5 " = "cn",
  "HadGEM2-CC" = "hg",
  "HadGEM2-ES" = "he",
  "IPSL-CM5A-LR" = "ip",
  "MRI-CGCM3" = "mg")

gcm_past_wc_lgm<-c(
  "CCSM4" = "cc",
  "MIROC-ESM" = "mr",
  "MPI-ESM-P" = "me")
  
rcp <- c(
  "rcp26" = "26",
  "rcp45" = "45",
  "rcp60" = "60",
  "rcp85" = "85"
)
past_dates_wc <- c("Mid Holocene" = "mid",
  "Last Glacial Maximum" = "lgm")

future_bo_dates<-c("2100"='2100',
  "2200"='2200'
)

scenario_bo_2100<- c("A1B" = "A1B",
  "A2" = "A2",
  "B1" = "B1")

scenario_bo_2200<- c("A1B" = "A1B",
      "B1" = "B1")

################################################################################
header <- dashboardHeader(title = "Model-R v1.25")
body <- dashboardBody(
  fluidRow(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    column (width = 12,
      tabBox(
        side = "left",
        title = "Steps",
        width = NULL,
        height = "1000px",
        id = "tabset1",
        
        ########################################################################
        tabPanel("Welcome",
          column(width = 12,
            id = "tabset1",
            tabPanel("", column(width = 9,
              box(width = NULL,
                column (width = 2,
                  br(),
                  img (src = "logo.png", width = 90)),
                column (width = 9,
                  h2("Model-R"),
                  h4(
                    "A Framework for Scalable and
                  Reproducible Ecological Niche Modeling"
                  )
                ),
                column (width = 12,
                  br(),
                  p("Please cite..."),
                  br(),
                  p("...ABSTRACT..."))
              )
            ),
              column(width = 3,
                box(width = NULL,
                  if (length(list.files (
                    "./www/projeto/",
                    full.names = F,
                    pattern = paste0(".")) > 0)) {
                    list_projects <- list.files("./www/projeto/",full.names = F,pattern = paste0("."))
                    box( title = "Create/Open project",
                      status = "primary",
                      solidHeader = TRUE,
                      width = NULL,
                      selectInput("select_project",
                        label= "Select project: ",
                        choices = c(
                          "Create new project" = "new_proj", 
                          "Open project" = "load_proj")
                      ),
                      conditionalPanel("input.select_project == 'load_proj' ",
                        helpText('Select project: '),
                        radioButtons(
                          "edtprojeto.load",
                          "Open project:",
                          choices = c(list_projects),
                          choiceValues =  c(list_projects),
                          selected = NULL
                        )
                      ),
                      conditionalPanel("input.select_project == 'new_proj' ",
                        textInput("edtprojeto.create", label = "Insert Project Id: ", value = "")
                      ),
                      actionButton("btnrefreshprojeto", "Submit", icon = icon("ok", lib = "glyphicon"))
                    )
                  } else {
                    box(
                      title = "Create/Open project",
                      status = "primary",
                      solidHeader = TRUE,
                      width = NULL,
                      selectInput(
                        "select_project",
                        "",
                        choices = c("Create new project" = "new_proj")
                      ),
                      conditionalPanel("input.select_project == 'new_proj' ",
                        textInput("edtprojeto.create", label = "Insert Project Id: ", value = "")
                      ),
                      actionButton("btnrefreshprojeto", "Submit", icon = icon("gear"))
                    )
                  }
                )
              )
            )
          )
        ),
        ########################################################################
        tabPanel("Species occurrence data",
          column(width = 12,
            tabBox(
              side = "left",
              selected = "Import occurrence dataset",
              title = "",
              width = NULL,
              height = "600px",
              id = "tabset_biodata",
              tabPanel("Import occurrence dataset",
                column(width = 12,
                  tabBox(
                    side = "right",
                    selected = "Load occurrence data",
                    title = "",
                    width = NULL,
                    height = "600px",
                    id = "tabset1",
                    tabPanel(
                      "Load occurrence data",
                      column(width = 8,
                        box(width = NULL,
                          dataTableOutput('dgbriddados'))),
                      column(width = 4,
                        box(width = NULL,
                          status = "warning",
                          helpText('Select species occurence database or browse csv dataset'),
                          selectInput("tipodado", "Occurence data", bio_datasource, selected = "jabot"),
                          conditionalPanel("input.tipodado == 'csv' ",
                            helpText('Format: [Species, Longitude, Latitude]'),
                            fileInput(
                              'file1',
                              '',
                              accept = c('text/csv',
                                'text/comma-separated-values,text/plain',
                                '.csv')
                            ),
                            checkboxInput('header', 'Header', TRUE),
                            radioButtons(
                              'sep',
                              'Separator',
                              c(
                                "Comma" = ',',
                                "Semicolon" = ';',
                                "Tab" = '\t'
                              ),
                              ',',
                              inline = TRUE
                            ),
                            radioButtons(
                              'quote',
                              'Quote',
                              c(
                                'Without' = '',
                                'Double' = '"',
                                'Simple' = "'"
                              ),
                              '"',
                              inline = TRUE
                            ),
                            actionButton("btnbuscarespecieCSV", "Viewer", icon = icon("search"))
                          ),
                          conditionalPanel("input.tipodado == 'jabot' ",
                            helpText('Insert Species Scientific Name'),
                            textInput("edtespeciejabot", label = "Species name:", value = "Caesalpinia echinata"),
                            actionButton("btnbuscarespeciejabot", "Search", icon = icon("search"))
                          ),
                          conditionalPanel("input.tipodado == 'gbif' ",
                            helpText('Insert Species Scientific Name'),
                            textInput("edtespecie", label = "Species name:", value = "Caesalpinia echinata"),
                            actionButton("btnbuscarespecie", "Search", icon = icon("search"))
                          )
                        )
                      )
                    ),
                    tabPanel("View occurrence map",
                      column(width = 12,
                        box(width = NULL,
                          solidHeader = TRUE,
                          leafletOutput('mapadistribuicao'),
                          height = 500
                        )
                      )
                    )
                  )
                )
              ),
              tabPanel(
                "Data Cleaning",
                column(width = 6,
                  box(width = NULL,
                    solidHeader = TRUE,
                    leafletOutput('mapadistribuicaodatacleaning', height = 500)
                  )
                ),
                column(width = 6,
                  box(width = NULL,
                    status = "warning",
                    numericInput(
                      "edtelemento",
                      "Occurence record ID:",
                      min = 0,
                      max = 100,
                      value = 0
                    ),
                    actionButton("btnapagar", "Delete selected ID", icon = icon("trash")),
                    actionButton('btneliminarduplicatas', 'Delete duplicates', icon = icon("cubes")),
                    downloadButton('downloadData', 'Download data')
                  ),
                  box(width = NULL,
                    dataTableOutput('dgbriddadosdatacleaning'))
                )
              )
            )
          )
        ),
        
        ########################################################################
        tabPanel("Environmental data",
          column(width = 12,
            tabBox(side = "left",
              title = "",
              width = NULL,
              height = "600px",
              selected = "Modeling extent", 
              
              tabPanel("Modeling extent",
                tabBox(side = "right",
                  title = "" ,
                  width = NULL,
                  height = "600px",
                  selected = "Study area extent", 
                  
                  tabPanel("Study area extent",
                    box(width = 8,
                      solidHeader = TRUE,
                      leafletOutput('mapapontosextend', height = 500)
                    ),
                    box(width = 4,
                      solidHeader = TRUE,
                      box(width = NULL,
                        status = "warning",
                        numericInput(
                          "edtextend1",
                          "Longitude left:",
                          min = -180,
                          max = 180,
                          value = -90,
                          step = 1
                        ),
                        numericInput(
                          "edtextend2",
                          "Longitude right:",
                          min = -180,
                          max = 180,
                          value = -32,
                          step = 1
                        ),
                        numericInput(
                          "edtextend4",
                          "Latitude higher:",
                          min = -90,
                          max = 90,
                          value = 23,
                          step = 1
                        ),
                        numericInput(
                          "edtextend3",
                          "Latitude lower:",
                          min = -90,
                          max = 90,
                          value = -33,
                          step = 1
                        )
                      )
                    )
                  ),
                  tabPanel("Projection extent",
                    box(width=12,
                      status = "warning",
                      checkboxInput('project_ext', 'Design on another extension', value = FALSE)
                    ),
                    conditionalPanel("input.project_ext", 
                      box(width=8,
                        solidHeader = FALSE,
                        leafletOutput('mapapontosextend2', height = 400)
                      ),
                      box(width = 4,
                        numericInput(
                          "edtextend12",
                          "Longitude left:",
                          min = -180,
                          max = 180,
                          value = -90,
                          step = 1),
                        numericInput(
                          "edtextend22",
                          "Longitude right:",
                          min = -180,
                          max = 180,
                          value = -32,
                          step = 1),
                        numericInput(
                          "edtextend42",
                          "Latitude higher:",
                          min = -90,
                          max = 90,
                          value = 23,
                          step = 1),
                        numericInput(
                          "edtextend32",
                          "Latitude lower:",
                          min = -90,
                          max = 90,
                          value = -33,
                          step = 1)
                      )
                    )
                  )
                )
              ),
              tabPanel("Select Predictors",
                column(width = 5,
                  box(width = NULL,
                    status = "warning",
                    actionButton("btnAtualizaSelecaoVariaveis", "Update selected"),
                    selectInput("tipodadoabiotico", "Variables dataset:", env_datasource),
                    
                    conditionalPanel("input.tipodadoabiotico == 'BIOORACLE' ",
                      checkboxInput('forecasting_bo' , "Project model to another timescale", value = FALSE),
                      
                      conditionalPanel("input.forecasting_bo",
                        box(width=NULL,
                          collapsible = T,
                          collapsed = T,
                          title = "Forcasting parameters",
                          
                          radioButtons("future_bo_dates", future_bo_dates , label = NULL, inline = TRUE),
                          conditionalPanel("input.future_bo_dates == '2100'",
                            radioButtons("scenario_bo_2100", "Scenario", scenario_bo_2100, inline=TRUE)
                          ),
                          conditionalPanel("input.future_bo_dates == '2200'",
                            radioButtons("scenario_bo_2200", "Scenario", scenario_bo_2200, inline=TRUE)
                          )
                        ),
                        checkboxGroupInput("pred_vars_bo_future","Select variables: ",
                          choices = c(
                            'Temperature (Max) ' = 'sstmax',
                            'Temperature (Min) ' = 'sstmin',
                            'Temperature (Range)' = 'sstrange',
                            'Temperature (Mean)' = 'sstmean',
                            'Salinity' = 'salinity')
                        )
                      ),
                      conditionalPanel("input.forecasting_bo == false ",
                        checkboxGroupInput("pred_vars_bo", "Select variables: ",
                          choices = c(
                            'Temperature (Max) ' = 'sstmax',
                            'Temperature (Min) ' = 'sstmin',
                            'Temperature (Range)' = 'sstrange',
                            'Temperature (Mean)' = 'sstmean',
                            'Salinity' = 'salinity',
                            'Calcite' =   'calcite',
                            'Nitrate' =  'nitrate',
                            'pH' = 'ph',
                            'Silicate' = 'silicate' ,
                            'Phosphate' = 'phosphate',
                            'Dissolved mol. oxygen' = 'dissox',
                            'Chlorophyll (Min)' =  'chlomin',
                            'Chlorophyll (Max)' =  'chlomax',
                            'Chlorophyll (Range)' =  'chlorange',
                            'Chlorophyll(Mean)' =  'chlomean',
                            'Cloud cover (Mean)' = 'cloudmean',
                            'Cloud cover (Max)' = 'cloudmax',
                            'Cloud cover (Min)' =  'cloudmin',
                            'Diffuse attenuation (Mean)' =  'damean',
                            'Diffuse attenuation (Min)' =   'damin',
                            'Diffuse attenuation (Max)' = 'damax',
                            'Photosynt. Avail. Radiation (Max)' =  'parmax',
                            'Photosynt. Avail. Radiation (Mean)' =  'parmean')
                        )
                      )
                    ),
                    
                    conditionalPanel("input.tipodadoabiotico == 'CLIMA' ",
                      selectInput("resolution", "Resolution:", resolution, selected = "10min"),
                      
                      checkboxInput("forecasting_wc", "Project model across timescales", value = FALSE),
                      conditionalPanel("input.forecasting_wc",
                       
                        box(width=NULL,
                          collapsible = T,
                          collapsed = T,
                          title = "Forcasting parameters",
                          radioButtons("wc_forecasting_timescale", "Timescale", wc_forecasting_timescale),
                          conditionalPanel("input.wc_forecasting_timescale == 'future' ",
                            radioButtons("future_dates_wc",  "Choose period: ", future_dates_wc, inline = TRUE),
                            radioButtons("rcp_wc", "Emission Scenarios (RCP)", rcp),
                            radioButtons("gcm_future_wc","General Circulation Models (GCM)", gcm_future_wc, selected = "bc")
                          ),
                          conditionalPanel("input.wc_forecasting_timescale == 'past'",
                            radioButtons("past_dates_wc","Choose period: " , past_dates_wc, inline= TRUE),
                            conditionalPanel("input.past_dates_wc == 'mid')",
                              radioButtons("gcm_past_wc_mid", "General Circulation Models (GCM)", gcm_past_wc_mid)
                            ),
                            conditionalPanel("input.past_dates_wc == 'lgm' ",
                              radioButtons("gcm_past_wc_lgm","General Circulation Models (GCM)", gcm_past_wc_lgm)
                            )
                          )
                        )
                      ),
                      
                      checkboxGroupInput("pred_vars_wc", "Select variables: ",
                        choices = c(
                          '(Bio1) Annual Mean Temperature' = 'bio1',
                          '(Bio2) Mean Diurnal Range' = 'bio2',
                          '(Bio3) Isothermality' = 'bio3',
                          '(Bio4) Temperature Seasonality' = 'bio4',
                          '(Bio5) Max Temperature of Warmest Month' = 'bio5',
                          '(Bio6) Min Temperature of Coldest Month' = 'bio6',
                          '(Bio7) Temperature Annual Range' = 'bio7',
                          '(Bio8) Mean Temperature of Wettest Quarter' = 'bio8',
                          '(Bio9) Mean Temperature of Driest Quarter' = 'bio9',
                          '(Bio10) Mean Temperature of Warmest Quarter' = 'bio10',
                          '(Bio11) Mean Temperature of Coldest Quarter' = 'bio11',
                          '(Bio12) Annual Precipitation' = 'bio12',
                          '(Bio13) Precipitation of Wettest Month' = 'bio13',
                          '(Bio14) Precipitation of Driest Month' = 'bio14',
                          '(Bio15) Precipitation Seasonality' = 'bio15',
                          '(Bio16) Precipitation of Wettest Quarter' = 'bio16',
                          '(Bio17) Precipitation of Driest Quarter' = 'bio17',
                          '(Bio18) Precipitation of Warmest Quarter' = 'bio18',
                          '(Bio19) Precipitation of Coldest Quarter' = 'bio19'
                        )
                      )
                    ),
                    
                    conditionalPanel("input.tipodadoabiotico == 'Others' ",
                      helpText('All layers should have the same spatial extent, resolution, origin, and projection'),
                      helpText(''),
                      helpText('Before loading multi-files extentions, make sure that all corresponding files are placed in the same directory.'),
                      lista_outros <-list.files("ex/outros/",full.names = F, pattern = ".tif|.bil|.grd"),
                      if (length(list.files( "ex/outros/", full.names = T) > 0)) {
                        checkboxGroupInput("pred_vars_other",  "Select rasters: ", choiceNames = c(lista_outros), choiceValues=c(lista_outros))
                      }
                    )
                  )
                ),
                
                column(width = 7,
                  tabBox(width = NULL,
                    tabPanel("Check correlation",
                      plotOutput(outputId = "grafico_correlacao", width = "100%", height="400px"),
                      dataTableOutput('dgbriddadoscorrelacao')
                    ),
                    tabPanel("View raster layers",
                      plotOutput(outputId = "mapaabiotico", height = "400px")
                    )
                  )
                )
              )
              
              
            )
          )
        ),
        
        ########################################################################
        tabPanel("Modeling",
          column(width = 6,
            tabBox(
              side = "left",
              title = "",
              width = NULL,
              id = "tabset2",
              tabPanel("BC",
                column(width = 12,
                  leafletOutput('maparesultadobc')
                )
              ),
              tabPanel("MH",
                column(width = 12,
                  leafletOutput('maparesultadomh')
                ))
              ,
              tabPanel("MX",
                column(width = 12,
                  leafletOutput('maparesultadomax')
                ))
              ,
              tabPanel("GLM",
                column(width = 12,
                  leafletOutput('maparesultadoglm')
                ))
              ,
              tabPanel("RF",
                column(width = 12,
                  leafletOutput('maparesultadorf')
                ))
              ,
              tabPanel("SVM",
                column(width = 12,
                  leafletOutput('maparesultadosvm')
                ))
              ,
              tabPanel("Domain",
                column(width = 12,
                  leafletOutput('maparesultadodo')
                ))
            )
            #plotOutput(outputId = "plotmodelagem")
          ),
          column(width = 6,
            box(width = NULL,
              height=800,
              box(width = 6,
                height=400,
                status = "warning",
                selectInput("dataset", "Partitioning type",
                  choices = c("KFold")),
                # choices = c("KFold", "Bootstrap")),
                sliderInput(
                  "edtnumgrupo",
                  "No. of partitions:",
                  min = 1,
                  max = 50,
                  value = 3,
                  step = 1
                ),
                sliderInput(
                  "edtnumpontos",
                  "Pseudo-absences:",
                  min = 100,
                  max = 2000,
                  value = 1000,
                  step = 100
                ),
                sliderInput(
                  "edtTSS",
                  "TSS score cutoff:",
                  min = 0,
                  max = 1,
                  value = 0.7,
                  step = 0.05
                )
                # radioButtons("edtBuffer", "Buffer:",
                #   c("Median" = "MEDIAN",
                #     "Maximum" = "MAX",
                #     "Minimum" = "MIN",
                #     "False" = "FALSE"))
                
              ),
              box(width = 6,
                height=400,
                status = "warning",
                actionButton("btnModelar", "Run", icon = icon("cogs")),
                h4("Algorithms"),
                checkboxInput('BIOCLIM', 'Bioclim', value = FALSE),
                checkboxInput('MAHALANOBIS', 'Mahalanobis', value = FALSE),
                checkboxInput('MAXENT', 'Maxent', value = FALSE),
                checkboxInput('GLM', 'GLM', value = FALSE),
                checkboxInput('RF', 'RandomForest', value = FALSE),
                checkboxInput('SVM', 'SVM', value = FALSE),
                checkboxInput('DOMAIN', 'Domain', value = FALSE)
              ),
              conditionalPanel("input.project_ext",
                box(title="Projection ensemble",
                  width = 12,
                  height = 320,
                  leafletOutput('maparesultado_proj',  width = "100%", height = 250)
                )
              )
            )
          )
        ),
        ########################################################################
        tabPanel("Outputs",
          column(width = 12,
            tabBox(
              side = "left",
              title = "",
              width = '100%',
              height = 500,
              id = "tabset2",
              tabPanel("Binary and continuous models",
                column(width = 12,
                  htmlOutput("ui"))),
              tabPanel("Stats",
                column(width = 12,
                  dataTableOutput('dbgridresultado')
                )),
              tabPanel(
                "Output files",
                column(width = 12,
                  column(width = 2,
                    box(width = NULL,
                      status = "warning",
                      h4("Script"),
                      htmlOutput("uiscript")
                    )),
                  column(width = 2,
                    box(width = NULL,
                      status = "warning",
                      h4("Statistics"),
                      htmlOutput("uiestatistica")
                    )),
                  column(width = 2,
                    box(width = NULL,
                      status = "warning",
                      h4("Occurence dataset"),
                      htmlOutput("uiarquivosdados")
                    )),
                  column(width = 2,
                    box(width = NULL,
                      status = "warning",
                      h4("Models"),
                      htmlOutput("uiarquivosmodelos")
                    )),
                  column(width = 2,
                    box(width = NULL,
                      status = "warning",
                      h4("Ensemble"),
                      htmlOutput("uiarquivosensemble")
                    )),
                  column(width = 2,
                    box(width = NULL,
                      status = "warning",
                      h4("Projection"),
                      htmlOutput("uiarquivosprojecao")
                    )),
                  column(width = 2,
                    box(width = NULL,
                      status = "warning",
                      h4("Projection future"),
                      htmlOutput("uiarquivosprojecaofuturo")
                    ))
                )
              )
            )
          )),
        ########################################################################
        tabPanel("About",
          column(width = 2,
            img(src = "Logoenbtpequeno.gif")
          ),
          column(width = 10,
            h4('Instituto de Pesquisas Jardim Bot??nico do Rio de Janeiro'),
            h4('Escola Nacional de Bot??nica Tropical'),
            h4('Model-R: A Framework for Scalable and Reproducible Ecological Niche Modeling'),
            h4('https://github.com/Model-R/Model-R'),
            h5('The shiny app was written as Rafael Oliveira Lima dissertation: '),
            h5('Programa de Mestrado Profissional: Biodiversidade em Unidades de Conserva????o'),
            h5(
              'Projeto de Trabalho de Conclus??o de Curso de Mestrado Profissional - 2015'
            ),
            h5('T??tulo: Desenvolvimento de programas para automatiza????o de processos em an??lises espaciais e ecol??gicas no ambiente R.'
            ),
            h5('Aluno: Rafael Oliveira Lima'),
            h5('Orientador: Marinez Ferreira de Siqueira'),
            h5('Coorientador: Luis Alexandre da Silva Estev??o'),
            h5('The backend code was written by: Andrea S??nchez Tapia, Felipe Sodr?? Barros, Guilherme Gall'),
            h5('Please cite `dismo()` package: Robert J. Hijmans, Steven Phillips, John Leathwick and Jane Elith (2017). dismo: Species Distribution Modeling. R package version 1.1-4. http://CRAN.R-project.org/package=dismo'),
            h5('Cite also randomForest() if you fit random forests, and `kernlab() if you fit SVM'
            )
          )
        )
        ########################################################################
        
      ) # tabBox
    ) # column
  ) # fluidrow
)#Body

dashboardPage(header,
  dashboardSidebar(disable = TRUE),
  body)
