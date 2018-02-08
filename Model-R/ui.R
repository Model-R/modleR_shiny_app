############################
## MODEL-R                ##
## RAFAEL OLIVEIRA LIMA   ##
## ANDREA SANCHEZ TAPIA   ##
## FELIPE SODRÉ BARROS    ##
## 5 DE JULHO DE 2017     ##
############################

# Thanks to Steven Worthington for function ipak https://gist.github.com/stevenworthington/3178163 (HT Karlo Guidoni Martins)

# ipak <- function(pkg) {
#     new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#     if (length(new.pkg))
#         install.packages(new.pkg, dependencies = TRUE)
#     sapply(pkg, require, character.only = TRUE)
# }
# ipak(c("shinydashboard", "leaflet"))

library("shinydashboard")
library("leaflet")



vars <- c(
  "GBif - The Global Biodiversity Information Facility" = "gbif",
  "Jabot - Banco de dados JBRJ" = "jabot",
  "CSV - Comma Separated Values" = "csv"
)

varabiotico <- c(
  "Clima" = "CLIMA",
  "Bio Oracle" = "BIOORACLE",
  "Outros" = "Others"
  #,
  #"Pessoal" = "MEU"
)

varabioticopassado <- c(
  "Clima" = "CLIMA",
  "Bio Oracle" = "BIOORACLE"
)


varabioticofuturo <- c(
  "Bio Oracle A1B 2100" = "BIOORACLEA1B2100",
  "Bio Oracle A1B 2200" = "BIOORACLEA1B2200",
  "Bio Oracle A2 2100" = "BIOORACLEA22100",
  "Bio Oracle B1 2100" = "BIOORACLEB12100",
  "Bio Oracle B1 2200" = "BIOORACLEB12200",
  "Clima" = "CLIMA"
)

cenariobiooracle <- c(
  "A1B" = "A1B",
  "A2" = "A2",
  "B1" = "B1"
)

periodobiooracle <- c(
  "Atual" = "current",
  "2100" = "2100",
  "2200" = "2200"
)

periodo <- c(
  "Atual" = "current",
  "Futuro 2050" = "future2050",
  "Futuro 2070" = "future2070",
  "Passado" = "past"
)


tipomapa <- c(
  "Mundo" = "world",
  "America do Sul" = "South America"
)

resolucao <- c(
  "30 arc-minutes" = "30min",
  "10 arc-minutes" = "10min",
  "5 arc-minutes" = "5min",
  "2.5 arc-minutes" = "25min",
  "30 seg. arc-minutes" = "30s"
)

header <- dashboardHeader(
  title = "Model-R v1.35"
)
body <- dashboardBody(
  fluidRow(
    column(width = 12,
           tabBox(
             title = "Steps",width = NULL,height= "1000px",
             # The id lets us use input$tabset1 on the server to find the current tab
             id = "tabset1",
             tabPanel("Biotic data",  column(width = 12,

                                     tabBox(side = "right",selected = "Project",
                                       title = "",width = NULL,height= "600px",

                                       # The id lets us use input$tabset1 on the server to find the current tab
                                       id = "tabset1",

                                       tabPanel("Occurrence", column(width = 12,
                                                                             box(width = NULL, solidHeader = TRUE,
                                                                                 leafletOutput('mapadistribuicao'), height = 500)

                                         )
                                      ),
                                      tabPanel("Source", column(width = 9,
                                                                     box(width = NULL,
                                                                         dataTableOutput('dgbriddados')
                                                                     )
                                      ),


                                      column(width = 3,
                                             box(width = NULL, status = "warning",
                                                 selectInput("tipodado", "Source", vars, selected = "jabot"),
                                                 conditionalPanel("input.tipodado == 'csv' ",
                                                                  helpText('Formato: [Espécie,Longitude,Latitude]'),

                                                                  fileInput('file1', '',
                                                                            accept=c('text/csv',
                                                                                     'text/comma-separated-values,text/plain',
                                                                                     '.csv')),
                                                                  checkboxInput('header', 'Header', TRUE),
                                                                  radioButtons('sep', 'Separator',
                                                                               c(Virgula=',',
                                                                                 "Semicolon"=';',
                                                                                 Tab='\t'),
                                                                               ',', inline = TRUE),
                                                                  radioButtons('quote', 'Quotation marks',
                                                                               c('Without'='',
                                                                                 'Double'='"',
                                                                                 'Simple'="'"),
                                                                               '"', inline = TRUE),
                                                                  actionButton("btnbuscarespecieCSV", "Viewer",icon = icon("search"))

                                                 ),
                                                 conditionalPanel("input.tipodado == 'jabot' ",
                                                                  helpText('Name of species.'),
                                                                  textInput("edtespeciejabot", label = "Species", value = "Caesalpinia echinata"),
                                                                  actionButton("btnbuscarespeciejabot", "Search", icon = icon("search"))

                                                 ),
                                                 conditionalPanel("input.tipodado == 'gbif' ",
                                                                  helpText('Name of species.'),
                                                                  textInput("edtespecie", label = "Species", value = ""),
                                                                  actionButton("btnbuscarespecie", "Search", icon = icon("search"))

                                                 )
                                             )

                                      )





                                      ),
									  tabPanel("Project", column(width = 9,id="idprojeto",
                                                                     box(width = NULL,
                                                                         textInput("edtprojeto", label = "Project", value = ""),
																		 actionButton("btncriarprojeto", "New project",icon = icon("gear")),
																		 actionButton("btnconsultarprojeto", "Search project",icon = icon("search"))
                                                                     )
                                      ),



																		 column(width = 3,
																		        box(width = NULL, helpText('Projects'),

																		            if (length(list.files("./www/projeto/",full.names=F,pattern=paste0("."))>0))
																		            {
																		              lista_outros <- list.files("./www/projeto/",full.names=F,pattern=paste0("."))
																		              #       checkboxInput('Bio1', 'BIO1 Annual Mean Temperature', value = FALSE)
																		              #   tags$div(
																		              #     tags$a(href=paste0('csv/',lista_csv[i]), paste0(lista_csv[i]))
																		              #   )
																		              lapply(1:length(lista_outros), function(i) {
																		                tagList(tags$h4(lista_outros[i]))
																		                #checkboxInput( paste0('chboxoutro',i), lista_outros[i] , value = FALSE)
																		              })
																		            }



																		        )
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
                                 numericInput("edtelemento", "ID:", min = 0, max = 100, value = 0),

                                 actionButton("btnapagar", "Delete",icon = icon("trash")),
                                 actionButton('btneliminarduplicatas', 'Delete duplicates',icon = icon("cubes")),
                                 downloadButton('downloadData', 'Download')
                                 ),

                             box(width = NULL,
                                 dataTableOutput('dgbriddadosdatacleaning')
                             )
                      )
             ) ,




             tabPanel("Abiotic data",
                      column(width = 12,

                             tabBox(side = "right",
                                    title = "",width = NULL,height= "600px",selected = "Creating extension",
                                    # The id lets us use input$tabset1 on the server to find the current tab
                                    id = "tabset1",


                                    tabPanel("Abiotic data", column(width = 8,

                                                                       tabBox(side = "left",
                                                                              title = "",width = NULL,height= "600px",
                                                                              # The id lets us use input$tabset1 on the server to find the current tab
                                                                              id = "tabset1",

                                                                              tabPanel("Selected variable", column(width = 12,
                                                                                                             box(width = NULL, solidHeader = TRUE,
                                                                                                                 plotOutput(outputId = "mapaabiotico", height = "400px")
                                                                                                                 )

                                                                              )
                                                                              ),
                                                                              tabPanel("Correlation variables", column(width = 9,
                                                                                                       box(width = NULL,solidHeader = TRUE,
                                                                                                           plotOutput(outputId = "grafico_correlacao", width = "500px"),
                                                                                                           dataTableOutput('dgbriddadoscorrelacao')
                                                                                                       )
                                                                              )
                                                                              )
                                                                       )







                                    ),
                                    column(width = 4,
                                           box(width = NULL, status = "warning",
                                               actionButton("btnAtualizaSelecaoVariaveis", "Update selected"),

                                               selectInput("tipodadoabiotico", "Abiotics data", varabiotico),
                                               conditionalPanel("input.tipodadoabiotico == 'BIOORACLE' ",
                                                                selectInput("periodobiooracle", "Period", periodobiooracle),
                                                                selectInput("cenariobiooracle", "Scenario", cenariobiooracle),
                                                                checkboxInput('calcite', 'calcite', value = FALSE),
                                                                checkboxInput('chlomin', 'chlomin', value = FALSE),
                                                                checkboxInput('cloudmean', 'cloudmean', value = FALSE),
                                                                checkboxInput('damean', 'damean', value = FALSE),
                                                                checkboxInput('nitrate', 'nitrate', value = FALSE),
                                                                checkboxInput('ph', 'ph', value = FALSE),
                                                                checkboxInput('silicate', 'silicate', value = FALSE),
                                                                checkboxInput('sstmin', 'sstmin', value = FALSE),
                                                                checkboxInput('chlomax', 'chlomax', value = FALSE),
                                                                checkboxInput('chlorange', 'chlorange', value = FALSE),
                                                                checkboxInput('cloudmin', 'cloudmin', value = FALSE),
                                                                checkboxInput('damin', 'damin', value = FALSE),
                                                                checkboxInput('parmax', 'parmax', value = FALSE),
                                                                checkboxInput('phosphate', 'phosphate', value = FALSE),
                                                                checkboxInput('sstmax', 'sstmax', value = FALSE),
                                                                checkboxInput('sstrange', 'sstrange', value = FALSE),
                                                                checkboxInput('chlomean', 'chlomean', value = FALSE),
                                                                checkboxInput('cloudmax', 'cloudmax', value = FALSE),
                                                                checkboxInput('damax', 'damax', value = FALSE),
                                                                checkboxInput('dissox', 'dissox', value = FALSE),
                                                                checkboxInput('parmean', 'parmean', value = FALSE),
                                                                checkboxInput('salinity', 'salinity', value = FALSE),
                                                                checkboxInput('sstmean', 'sstmean', value = FALSE)
                                               ),
                                               conditionalPanel("input.tipodadoabiotico == 'CLIMA' ",
                                                                selectInput("periodo", "Period", periodo),
                                                                selectInput("resolucao", "Resolution", resolucao, selected = "10min"),

                                                                checkboxInput('Bio1', 'BIO1 Annual Mean Temperature', value = FALSE),
                                                                checkboxInput('Bio2', 'BIO2 Mean Diurnal Range (Mean of monthly)', value = FALSE),
                                                                checkboxInput('Bio3', 'BIO3 Isothermality', value = FALSE),
                                                                checkboxInput('Bio4', 'BIO4 Temperature Seasonality', value = FALSE),
                                                                checkboxInput('Bio5', 'BIO5 Max Temperature of Warmest Month', value = FALSE),
                                                                checkboxInput('Bio6', 'BIO6 Min Temperature of Coldest Month', value = FALSE),
                                                                checkboxInput('Bio7', 'BIO7 Temperature Annual Range', value = FALSE),
                                                                checkboxInput('Bio8', 'BIO8 Mean Temperature of Wettest Quarter', value = FALSE),
                                                                checkboxInput('Bio9', 'BIO9 Mean Temperature of Driest Quarter', value = FALSE),
                                                                checkboxInput('Bio10', 'BIO10 Mean Temperature of Warmest Quarter', value = FALSE),
                                                                checkboxInput('Bio11', 'BIO11 Mean Temperature of Coldest Quarter', value = FALSE),
                                                                checkboxInput('Bio12', 'BIO12 Annual Precipitation', value = FALSE),
                                                                checkboxInput('Bio13', 'BIO13 Precipitation of Wettest Month', value = FALSE),
                                                                checkboxInput('Bio14', 'BIO14 Precipitation of Driest Month', value = FALSE),
                                                                checkboxInput('Bio15', 'BIO15 Precipitation Seasonality', value = FALSE),
                                                                checkboxInput('Bio16', 'BIO16 Precipitation of Wettest Quarter', value = FALSE),
                                                                checkboxInput('Bio17', 'BIO17 Precipitation of Driest Quarter', value = FALSE),
                                                                checkboxInput('Bio18', 'BIO18 Precipitation of Warmest Quarter', value = FALSE),
                                                                checkboxInput('Bio19', 'BIO19 Precipitation of Coldest Quarter', value = FALSE)
                                               ),
                                               conditionalPanel("input.tipodadoabiotico == 'Others' ",
                                                              # checkboxInput('chboxoutro1', 'outro1' , value = FALSE)
                                                                if (length(list.files("ex/outros/",full.names=F,pattern=c('.*'))>0))
                                                                {
                                                                  lista_outros <- list.files("ex/outros/",full.names=F,pattern=c('.*'))
                                                               #       checkboxInput('Bio1', 'BIO1 Annual Mean Temperature', value = FALSE)
                                                               #   tags$div(
                                                               #     tags$a(href=paste0('csv/',lista_csv[i]), paste0(lista_csv[i]))
                                                               #   )
                                                                  lapply(1:length(lista_outros), function(i) {
                                                                    checkboxInput( paste0('chboxoutro',i), lista_outros[i] , value = TRUE)
                                                                  })
                                                                }

                                               )
                                               #,
                                               #conditionalPanel("input.tipodadoabiotico == 'MEU' ",
                                              #                  htmlOutput("uiwordclim"),
                                              #                  fileInput('fileraster1', 'Raster 1',
                                              #                            accept = c(
                                              #                              'bil',
                                              #                              'grd',
                                              #                              'asc'
                                              #                            )
                                              #                  ),
                                              #                  fileInput('fileraster2', 'Raster 2',
                                              #                            accept = c(
                                              #                              'bil',
                                              #                              'grd',
                                              #                              'asc'
                                              #                            )
                                              #                  )

                                              # )

                                           )
                                    )),




                                    tabPanel("Projection extension", column(width = 8,
                                                                         box(width = NULL, solidHeader = TRUE,
                                                                             leafletOutput('mapapontosextend2', height = 500)
                                                                         )
                                    ),
                                    column(width = 4,
                                           box(width = NULL, solidHeader = TRUE,
                                               box(width = NULL, status = "warning",
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

                                    ),



                                    tabPanel("Creating extension", column(width = 8,
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

                                    )






                             )
                      )

             ) ,














             tabPanel("Modeling",

                      column(width = 6,
                             tabBox(side = "left",
                                    title = "",width = NULL, height = 500,
                                    # The id lets us use input$tabset1 on the server to find the current tab
                                    id = "tabset2",
                                    tabPanel("BC", column(width = 12,
                                                                  leafletOutput('maparesultadobc', height = 500)

                                      )
                                    ),
                                    tabPanel("MH", column(width = 12,
                                                               leafletOutput('maparesultadomh', height = 500)
                                            )
                                    )
                                    ,
                                    tabPanel("MX", column(width = 12,
                                                                   leafletOutput('maparesultadomax', height = 500)
                                            )
                                    )
                                    ,
                                    tabPanel("GLM", column(width = 12,
                                                                   leafletOutput('maparesultadoglm', height = 500)
                                            )
                                    )
                                    ,
                                    tabPanel("RF", column(width = 12,
                                                                   leafletOutput('maparesultadorf', height = 500)
                                            )
                                    )
                                    ,
                                    tabPanel("SVM", column(width = 12,
                                                                   leafletOutput('maparesultadosvm', height = 500)
                                            )
                                    )
                                    ,
                                    tabPanel("Domain", column(width = 12,
                                                           leafletOutput('maparesultadodo', height = 500)
                                            )
                                    )
                                    #,
                                    #tabPanel("Ensemble", column(width = 12,
                                    #                       leafletOutput('maparesultadoessemble', height = 500),
                                    #                       #plotOutput(outputId = "pontosmodelagem", height = "1px",width = "500px"),
                                    #                       plotOutput(outputId = "plotesemble")
                                    #
                                    #)
                                    #)
                             ),
                             plotOutput(outputId = "plotmodelagem",height = "500px",width = "500px")
                      ),
                      column(width = 3,
                             box(width = NULL, status = "warning",

                                 selectInput("dataset", "Partitioning type",
                                             choices = c("KFold", "Bootstrap")),
                                 sliderInput("edtnumgrupo", "No. of partitions:",
                                             min = 1, max = 50, value = 3, step = 1),
                                 sliderInput("edtnumpontos", "No. of points (Pseudo absence):",
                                             min = 100, max = 2000, value = 1000, step = 100),

                                 sliderInput("edtextf", "Buffer:",
                                             min = 1, max = 2, value = 1.25, step = 0.05)
                                 #h4("Modelos"),


                             )

                      )
                      ,
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
                                 #h4("Gerar modelos"),
                                 #checkboxInput('BINARIO', 'Binario', value = TRUE),
                                 #checkboxInput('ENSEMBLE', 'Ensemble', value = TRUE),
                                 #sliderInput("TSS", "TSS:",
                                 #             min = 0, max = 1, value = 0.2, step= 0.1)

                             ),
                             box(width = NULL, status = "warning",
                                 actionButton("btnModelar", "Run",icon = icon("cogs"))

                             )

                      )


                      ),
									  #,
             #tabPanel("Resultado",
            #          box(width = NULL,

                          #dataTableOutput('dbgridresultado')
            #          )
#             ),
             tabPanel("Outputs",
                      column(width = 12,
                             #box(width = NULL,
                                 tabBox(side = "left",
                                        title = "",width = '100%',height = 500,
                                        # The id lets us use input$tabset1 on the server to find the current tab
                                        id = "tabset2",
                                        tabPanel("Binary and continuous models", column(width = 12,

                                                                                            htmlOutput("ui")


                                        )
                                        ),
                                        tabPanel("Data", column(width = 12,
                                                                         dataTableOutput('dbgridresultado')

                                        )
                                        )
                                        ,
                                        tabPanel("Files", column(width = 12,
column(width = 2,
       box(width = NULL,status = "warning",
           h4("Script"),
           htmlOutput("uiscript")
       )
),
column(width = 2,
       box(width = NULL,status = "warning",
           h4("Statistc"),
           htmlOutput("uiestatistica")
       )
),
column(width = 2,
       box(width = NULL,status = "warning",
           h4("Data"),
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
)
,
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

									  tabPanel("Help",
									           column(width = 12,
									                  h4('User manual'),
									                  tags$a(href="Manual_Model-R.pdf", "Version 1.0")
									                  #,
									                  #img(src="fluxo.jpg")
									           )

									  )



           )
    )
  )
)

dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)
