############################
## MODEL-R                ##
## RAFAEL OLIVEIRA LIMA   ##
## ANDREA SANCHEZ TAPIA   ##
## FELIPE SODRÉ BARROS    ##
## 5 DE JULHO DE 2017     ##
############################

# Thanks to Steven Worthington for function ipak https://gist.github.com/stevenworthington/3178163 (HT Karlo Guidoni Martins)

ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg))
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}
ipak(c("shinydashboard", "leaflet"))

vars <- c(	"GBif - The Global Biodiversity Information Facility" = "gbif",
  "Jabot - JBRJ Database" = "jabot",
  "CSV - Comma Separated Values" = "csv")

varabiotico <- c(	"WorldClim v.1.4" = "CLIMA",
  "Bio-ORACLE v.1" = "BIOORACLE",
  "Upload Dataset" = "Others")

varabioticopassado <- c("WorldClim" = "CLIMA",
  "Bio-ORACLE" = "BIOORACLE")

varabioticofuturo <- c(	"Bio-ORACLE A1B 2100" = "BIOORACLEA1B2100",
  "Bio-ORACLE A1B 2200" = "BIOORACLEA1B2200",
  "Bio-ORACLE A2 2100" = "BIOORACLEA22100",
  "Bio-ORACLE B1 2100" = "BIOORACLEB12100",
  "Bio-ORACLE B1 2200" = "BIOORACLEB12200",
  "WorldClim" = "CLIMA")

gcm <- c(	"BCC-CSM1-1" = "bc",
  "CCSM4" = "cc",
  "GISS-E2-R" = "gs",
  "HadGEM2-AO" = "hd",
  "HadGEM2-ES" = "he",
  "IPSL-CM5A-LR" = "ip",
  "MIROC-ESM-CHEM" = "mi",
  "MIROC-ESM" = "mr",
  "MIROC5" = "mc",
  "MRI-CGCM3" = "mg",
  "NorESM1-M" = "no")

rcp <- c("rcp26" = "26",
  "rcp45" = "45",
  "rcp60" = "60",
  "rcp85" = "85")

rcp_biooracle <- c(	"rcp26" = "26",
  "rcp45" = "45",
  "rcp60" = "60",
  "rcp85" = "85")

periodobiooracle <- c("Current" = "current",
  "2100" = "2100",
  "2200" = "2200")


periodo <- c(
  "Future 2050" = "2050",
  "Future 2070" = "2070")

tipomapa <- c("World" = "world",
  "South America" = "South America")

resolucao <- c("10 arc-minutes" = "10m",
  "5 arc-minutes" = "5m",
  "2.5 arc-minutes" = "2-5m",
  "30 arc-seconds" = "30s")

cenariobiooracle <- c("A1B" = "A1B",
  "A2" = "A2",
  "B1" = "B1")

cenariobiooracle_2200 <- c("A1B" = "A1B",
  "B1" = "B1")


projections<-c("No projection" = "no_projection", 
  "Geographic" = "geo_projection", 
  "Time" = "time_projection", 
  "Geographic + time" = "geotime_projection")


################################################################################
header <- dashboardHeader(title = "Model-R v1.25")
body <- dashboardBody(
  fluidRow(
    column (width = 12,
      tabBox(side = "left",
        title = "Steps", width = NULL, height= "1000px",
        id = "tabset1",
        
        ########################################################################
        tabPanel("Welcome",
          column(width = 12,
            id = "tabset1",
            
            tabPanel("", column(
              width = 9,
              box(
                width = NULL,
                column (width = 2,
                  br(),
                  img (src="logo.png", width = 90)
                ),
                column (width = 9,
                  h2("Model-R"),
                  h4("A Framework for Scalable and
                  Reproducible Ecological Niche Modeling")
                  
                ), 
                column (width = 12,
                  br(),
                  p("Please cite..."),
                  br(),
                  p("...ABSTRACT...")
                )
              )
            ),
              column(width = 3,
                box(width = NULL,
                  if (length(
                    list.files ("./www/projeto/",full.names = F, pattern = paste0("."))> 0)){
                    list_projects <-	list.files("./www/projeto/",full.names = F, pattern = paste0("."))
                    box(title = "Create/Open project", status = "primary", solidHeader = TRUE,
                      width = NULL,
                      selectInput("select_project", "Select project: ", 
                        choices= c(
                          "Create new project" = "new_proj", "Open project" = "load_proj")),
                      
                      
                      conditionalPanel("input.select_project == 'load_proj' ",
                        helpText('Select project: '),
                        radioButtons("edtprojeto.load", "Open project:",
                          choices = c(list_projects), choiceValues =  c(list_projects), selected = NULL)
                      ),
                      conditionalPanel("input.select_project == 'new_proj' ",
                        textInput("edtprojeto.create", label = "Insert Project Id: ", value = "")
                      ),
                      actionButton("btnrefreshprojeto", "Submit", icon = icon("ok", lib = "glyphicon"))
                    )
                  } else {
                    box(title = "Create/Open project", status = "primary", solidHeader = TRUE,
                      width = NULL,
                      selectInput("select_project", "", choices= c("Create new project" = "new_proj")),
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
            tabBox(side = "left", selected = "Import occurrence dataset",
              title = "",width = NULL, height= "600px",
              id = "tabset1",
              
              tabPanel("Import occurrence dataset",  
                column(width = 12,
                  tabBox(side = "right",selected = "Load occurrence data",
                    title = "",width = NULL,height= "600px",
                    id = "tabset1",
                    
                    tabPanel("Load occurrence data", 
                      column(width = 8,
                        box(width = NULL,
                          dataTableOutput('dgbriddados')
                        )
                      ),
                      column(width = 4,
                        box(width = NULL, status = "warning",
                          helpText('Select species occurence database or browse csv dataset'),
                          selectInput("tipodado", "Occurence data", vars, selected = "jabot"),
                          conditionalPanel("input.tipodado == 'csv' ",
                            helpText('Format: [Species, Longitude, Latitude]'),
                            fileInput('file1', '',
                              accept=c('text/csv',
                                'text/comma-separated-values,text/plain',
                                '.csv')),
                            checkboxInput('header', 'Header', TRUE),
                            radioButtons('sep', 'Separator',
                              c("Comma"=',',
                                "Semicolon"=';',
                                "Tab"='\t'),
                              ',', inline = TRUE),
                            radioButtons('quote', 'Quote',
                              c('Without'='',
                                'Double'='"',
                                'Simple'="'"),
                              '"', inline = TRUE),
                            actionButton("btnbuscarespecieCSV", "Viewer",icon = icon("search"))
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
                        box(width = NULL, solidHeader = TRUE,
                          leafletOutput('mapadistribuicao'), height = 500)
                      )
                    )
                  )
                )
              ),
              
              tabPanel("Data Cleaning",
                column(width = 6,
                  box(width = NULL, solidHeader = TRUE,
                    leafletOutput('mapadistribuicaodatacleaning', height = 500)
                  )
                ),
                column(width = 6,
                  box(width = NULL, status = "warning",
                    numericInput("edtelemento", "Occurence record ID:", min = 0, max = 100, value = 0),
                    actionButton("btnapagar", "Delete selected ID",icon = icon("trash")),
                    actionButton('btneliminarduplicatas', 'Delete duplicates',icon = icon("cubes")),
                    downloadButton('downloadData', 'Download data')
                  ),
                  box(width = NULL,
                    dataTableOutput('dgbriddadosdatacleaning')
                  )
                )
              )
            )
          )
        ), 
        
        ########################################################################      
        tabPanel("Environmental data",
          column(width = 12,
            tabBox(side = "left",
              title = "",width = NULL,height= "600px",selected = "Model extent",
            
                tabPanel("Model extent", 
                column(width = 8,
                  box(width = NULL, solidHeader = TRUE,
                    leafletOutput('mapapontosextend', height = 500)
                  )
                ),
                column(width = 4,
                  box(width = NULL, solidHeader = TRUE,
                    box(width = NULL, status = "warning",
                      numericInput("edtextend1", "Longitude left:",
                        min = -180, max = 180, value = -90, step = 1),
                      numericInput("edtextend2", "Longitude right:",
                        min = -180, max = 180, value = -32, step = 1),
                      numericInput("edtextend4", "Latitude higher:",
                        min = -90, max = 90, value = 23, step = 1),
                      numericInput("edtextend3", "Latitude lower:",
                        min = -90, max = 90, value = -33, step = 1)
                    )
                  )
                )
              ),
              
              tabPanel("Raster data", 
                column(width = 12,
                  
                  tabBox(side = "right", selected = "Predictor Variables",
                    title = "",width = NULL,height= "600px", 
                    
                    tabPanel("Variables correlation", 
                      column(width = 8,
                        box(width = NULL,solidHeader = TRUE,
                          plotOutput(outputId = "grafico_correlacao", width = "500px"),
                          dataTableOutput('dgbriddadoscorrelacao')
                        )
                      )
                    ),
                    
                    
                    
                    tabPanel("Predictor Variables",
                      column(width = 12,
                        
                        column(width = 8,
                          box(width = NULL, solidHeader = TRUE,
                            plotOutput(outputId = "mapaabiotico", height = "400px")
                          )
                        ),
                        
                        column(width = 4,
                          box(width = 12, status = "warning", height=NULL,
                            actionButton("btnAtualizaSelecaoVariaveis", "Update selected"),
                            selectInput("tipodadoabiotico", "Variables dataset:", varabiotico),
                            
                            conditionalPanel("input.tipodadoabiotico == 'BIOORACLE' ",
                              selectInput("projections", "Map projections:", projections, selected = "no_projection"),
                              
                              #conditionalPanel("input.projections == 'no_projection' ",
                              checkboxInput('sstmax','Temperature (Max) ', value = FALSE),
                              checkboxInput('sstmin','Temperature (Min) ', value = FALSE),
                              checkboxInput('sstrange','Temperature (Range)', value = FALSE),
                              checkboxInput('sstmean', 'Temperature (Mean)', value = FALSE),
                              checkboxInput('salinity', 'Salinity', value = FALSE),
                              #),
                              conditionalPanel("input.projections == 'no_projection' ",
                                checkboxInput('calcite', 'Calcite', value = FALSE),
                                checkboxInput('nitrate', 'Nitrate', value = FALSE),
                                checkboxInput('ph', 'pH', value = FALSE),
                                checkboxInput('silicate', 'Silicate', value = FALSE),
                                checkboxInput('phosphate', 'Phosphate', value = FALSE),
                                checkboxInput('dissox', 'Dissolved mol. oxygen', value = FALSE),
                                checkboxInput('chlomin', 'Chlorophyll (Min)', value = FALSE),
                                checkboxInput('chlomax', 'Chlorophyll (Max)', value = FALSE),
                                checkboxInput('chlorange', 'Chlorophyll (Range)', value = FALSE),
                                checkboxInput('chlomean', 'Chlorophyll(Mean)', value = FALSE),
                                checkboxInput('cloudmean', 'Cloud cover (Mean)', value = FALSE),
                                checkboxInput('cloudmax', 'Cloud cover (Max)', value = FALSE),
                                checkboxInput('cloudmin', 'Cloud cover (Min)', value = FALSE),
                                checkboxInput('damean', 'Diffuse attenuation (Mean)', value = FALSE),
                                checkboxInput('damin', 'Diffuse attenuation (Min)', value = FALSE),
                                checkboxInput('damax', 'Diffuse attenuation (Max)', value = FALSE),
                                checkboxInput('parmax', 'Photosynt. Avail. Radiation (Max)', value = FALSE),
                                checkboxInput('parmean', 'Photosynt. Avail. Radiation (Mean)', value = FALSE)
                              )
                            ), 
                            
                            
                            conditionalPanel("input.tipodadoabiotico == 'CLIMA' ",
                              selectInput("resolucao", "Resolution:", resolucao, selected = "10min"),
                              selectInput("projections", "Map projections:", projections, selected = "no_projection"),
                              checkboxInput('Bio1', '(Bio1) Annual Mean Temperature', value = FALSE),
                              checkboxInput('Bio2', '(Bio2) Mean Diurnal Range', value = FALSE),
                              checkboxInput('Bio3', '(Bio3) Isothermality', value = FALSE),
                              checkboxInput('Bio4', '(Bio4) Temperature Seasonality', value = FALSE),
                              checkboxInput('Bio5', '(Bio5) Max Temperature of Warmest Month', value = FALSE),
                              checkboxInput('Bio6', '(Bio6) Min Temperature of Coldest Month', value = FALSE),
                              checkboxInput('Bio7', '(Bio7) Temperature Annual Range', value = FALSE),
                              checkboxInput('Bio8', '(Bio8) Mean Temperature of Wettest Quarter', value = FALSE),
                              checkboxInput('Bio9', '(Bio9) Mean Temperature of Driest Quarter', value = FALSE),
                              checkboxInput('Bio10', '(Bio10) Mean Temperature of Warmest Quarter', value = FALSE),
                              checkboxInput('Bio11', '(Bio11) Mean Temperature of Coldest Quarter', value = FALSE),
                              checkboxInput('Bio12', '(Bio12) Annual Precipitation', value = FALSE),
                              checkboxInput('Bio13', '(Bio13) Precipitation of Wettest Month', value = FALSE),
                              checkboxInput('Bio14', '(Bio14) Precipitation of Driest Month', value = FALSE),
                              checkboxInput('Bio15', '(Bio15) Precipitation Seasonality', value = FALSE),
                              checkboxInput('Bio16', '(Bio16) Precipitation of Wettest Quarter', value = FALSE),
                              checkboxInput('Bio17', '(Bio17) Precipitation of Driest Quarter', value = FALSE),
                              checkboxInput('Bio18', '(Bio18) Precipitation of Warmest Quarter', value = FALSE),
                              checkboxInput('Bio19', '(Bio19) Precipitation of Coldest Quarter', value = FALSE)
                            ),
                            
                            conditionalPanel("input.tipodadoabiotico == 'Others' ",
                              helpText('All layers should have the same spatial extent, resolution, origin, and projection'),
                              helpText(''),
                              helpText('Accepted formats: ".tif";  '),
                              if (length(list.files("ex/outros/",full.names=T,pattern=c('.*'))>0))
                              {
                                lista_outros <- list.files("ex/outros/",full.names=F,pattern=c('.*'))
                                lapply(1:length(lista_outros), function(i) {
                                  checkboxInput( paste0('chboxoutro',i), lista_outros[i] , value = TRUE)
                                })
                              }
                            )#others
                            
                          )
                        )
                      )
                    )
                  )
                )
              ), 
              tabPanel("Set projections",
                
                column(width = 6,
                
                    box(width = NULL,
                      height= 600, status = "warning", solidHeader = TRUE, collapsible = TRUE,title="Geographic projection" , padding=0,
                      column(width = 8,
                        box(width = NULL, solidHeader = FALSE,
                          leafletOutput('mapapontosextend2', height = 500)
                        )
                      ),
                      
                      column(width = 4,
                        box(width = NULL, solidHeader = TRUE,
                          box(width = NULL, 
                            numericInput("edtextend12", "Longitude left:",
                              min = -180, max = 180, value = -90, step = 1),
                            numericInput("edtextend22", "Longitude right:",
                              min = -180, max = 180, value = -32, step = 1),
                            numericInput("edtextend42", "Latitude higher:",
                              min = -90, max = 90, value = 23, step = 1),
                            numericInput("edtextend32", "Latitude lower:",
                              min = -90, max = 90, value = -33, step = 1)
                          )
                        )
                      )
                  )
                    ),
                    column(width = 6,
                      box(width = NULL, title="Time projections" , padding=0,
                      box(width = 6, solidHeader = TRUE, collapsible = TRUE, title="Future conditions" ,
                      checkboxGroupInput("periodo", "Time period:", periodo),
                      checkboxGroupInput("gcm", "General Circulation Models (GCM):", gcm, selected = "bc"),
                      checkboxGroupInput("rcp", "Emission Scenarios (RCP):", rcp, selected = "26")
                      ),
                      box(width =6, solidHeader = TRUE,collapsible = TRUE, title="Past conditions" ,
                        checkboxGroupInput("periodo", "Time period:", periodo),
                        checkboxGroupInput("gcm", "General Circulation Models (GCM):", gcm, selected = "bc"),
                        checkboxGroupInput("rcp", "Emission Scenarios (RCP):", rcp, selected = "26")
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
            tabBox(side = "left",
              title = "",width = NULL, height = 500,
              id = "tabset2",
              
              tabPanel("BC", 
                column(width = 12,
                  leafletOutput('maparesultadobc', height = 500)
                )
              ),
              
              tabPanel("MH", 
                column(width = 12,
                  leafletOutput('maparesultadomh', height = 500)
                )
              )
              ,
              
              tabPanel("MX", 
                column(width = 12,
                  leafletOutput('maparesultadomax', height = 500)
                )
              )
              ,
              
              tabPanel("GLM", 
                column(width = 12,
                  leafletOutput('maparesultadoglm', height = 500)
                )
              )
              ,
              
              tabPanel("RF", 
                column(width = 12,
                  leafletOutput('maparesultadorf', height = 500)
                )
              )
              ,
              
              tabPanel("SVM", 
                column(width = 12,
                  leafletOutput('maparesultadosvm', height = 500)
                )
              )
              ,
              
              tabPanel("Domain", 
                column(width = 12,
                  leafletOutput('maparesultadodo', height = 500)
                )
              )
            ),
            plotOutput(outputId = "plotmodelagem",height = "500px",width = "500px")
          ),
          column(width = 3,
            box(width = NULL, status = "warning",
              selectInput("dataset", "Partitioning type",
                choices = c("KFold")),
              # choices = c("KFold", "Bootstrap")),
              sliderInput("edtnumgrupo", "No. of partitions:",
                min = 1, max = 50, value = 3, step = 1),
              sliderInput("edtnumpontos", "Pseudo-absences:",
                min = 100, max = 2000, value = 300, step = 100),
              sliderInput("edtTSS", "TSS score cutoff:",
                min = 0, max = 1, value = 0.7, step = 0.05)
              # radioButtons("edtBuffer", "Buffer:",
              #   c("Median" = "MEDIAN",
              #     "Maximum" = "MAX",
              #     "Minimum" = "MIN",
              #     "False" = "FALSE"))
            )
          ),
          column(width = 3,
            box(width = NULL, status = "warning",
              h4("Algorithms"),
              checkboxInput('BIOCLIM', 'Bioclim', value = FALSE),
              checkboxInput('MAHALANOBIS', 'Mahalanobis', value = FALSE),
              checkboxInput('MAXENT', 'Maxent', value = FALSE),
              checkboxInput('GLM', 'GLM', value = FALSE),
              checkboxInput('RF', 'RandomForest', value = FALSE),
              checkboxInput('SVM', 'SVM', value = FALSE),
              checkboxInput('DOMAIN', 'Domain', value = FALSE),
              tags$hr(),
              checkboxInput('PROJETAR', 'Design on another extension', value = FALSE)
            ),
            box(width = NULL, status = "warning",
              actionButton("btnModelar", "Run",icon = icon("cogs"))
            )
          )
        ), 
        
        ########################################################################      
        tabPanel("Outputs",
          column(width = 12,
            tabBox(side = "left",
              title = "",width = '100%',height = 500,
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "tabset2",
              
              tabPanel("Binary and continuous models", 
                column(width = 12,
                  htmlOutput("ui")
                )
              ),
              
              tabPanel("Stats",
                column(width = 12,
                  dataTableOutput('dbgridresultado')
                )
              ),
              
              tabPanel("Output files", 
                column(width = 12,
                  column(width = 2,
                    box(width = NULL,status = "warning",
                      h4("Script"),
                      htmlOutput("uiscript")
                    )
                  ),
                  column(width = 2,
                    box(width = NULL,status = "warning",
                      h4("Statistics"),
                      htmlOutput("uiestatistica")
                    )
                  ),
                  column(width = 2,
                    box(width = NULL,status = "warning",
                      h4("Occurence dataset"),
                      htmlOutput("uiarquivosdados")
                    )
                  ),
                  column(width = 2,
                    box(width = NULL,status = "warning",
                      h4("Models"),
                      htmlOutput("uiarquivosmodelos")
                    )
                  ),
                  column(width = 2,
                    box(width = NULL,status = "warning",
                      h4("Ensemble"),
                      htmlOutput("uiarquivosensemble")
                    )
                  ),
                  column(width = 2,
                    box(width = NULL,status = "warning",
                      h4("Projection"),
                      htmlOutput("uiarquivosprojecao")
                    )
                  ),
                  column(width = 2,
                    box(width = NULL,status = "warning",
                      h4("Projection future"),
                      htmlOutput("uiarquivosprojecaofuturo")
                    )
                  )
                )
              )
            )
          )
        ),
        
        ########################################################################      
        tabPanel("About",
          column(width = 2,
            img(src="Logoenbtpequeno.gif")
          ),
          column(width = 10,
            h4('Instituto de Pesquisas Jardim Botânico do Rio de Janeiro'),
            h4('Escola Nacional de Botânica Tropical'),
            h4('Model-R: A Framework for Scalable and Reproducible Ecological Niche Modeling'),
            h4('https://github.com/Model-R/Model-R'),
            h5('The shiny app was written as Rafael Oliveira Lima dissertation: '),
            h5('Programa de Mestrado Profissional: Biodiversidade em Unidades de Conservação'),
            h5('Projeto de Trabalho de Conclusão de Curso de Mestrado Profissional - 2015'),
            h5('Título: Desenvolvimento de programas para automatização de processos em análises espaciais e ecológicas no ambiente R.'),
            h5('Aluno: Rafael Oliveira Lima'),
            h5('Orientador: Marinez Ferreira de Siqueira'),
            h5('Coorientador: Luis Alexandre da Silva Estevão'),
            h5('The backend code was written by: Andrea Sánchez Tapia, Felipe Sodré Barros, Guilherme Gall'),
            h5('Please cite `dismo()` package: Robert J. Hijmans, Steven Phillips, John Leathwick and Jane Elith (2017). dismo: Species Distribution Modeling. R package version 1.1-4. http://CRAN.R-project.org/package=dismo'),
            h5('Cite also randomForest() if you fit random forests, and `kernlab() if you fit SVM')
          )
        ),
        ########################################################################      
        tabPanel("Help",
          column(width = 12,
            h4('User manual')
            
          )
        )
        
        ########################################################################
      ) # tabBox
    ) # column
  ) # fluidrow
) # Body

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)