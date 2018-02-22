#########################################
## INICIO UTILIZACAO FUNCAO MODELAGEM
########################################

modelagem <- function()
	({
		limparResultadosAnteriores()
		library(raster)
		numpontos = input$edtnumpontos
		numparticoes <- input$edtnumgrupo
		
		## INICIO UTILIZACAO FUNCAO DISMO.MOD ##
		futuro = FALSE
		if (input$periodo != 'current')
		{
			futuro = T
		}
		
		if (input$periodobiooracle != 'current')
		{
			futuro = T
		}
		
		write.projecao = F
		
		if (input$PROJETAR == T)
		{
			write.projecao = T
		}
		
		## RODANDO A FUNCAO DISMO.MOD
		dismo.mod("",
			especie,
			pred_nf,
			pred_nf2,
			input$MAXENT,
			input$BIOCLIM,
			input$GLM,
			input$RF,
			input$SVM,
			input$MAHALANOBIS,
			input$DOMAIN,
			input$SVM2,
			numparticoes,
			numpontos,
			123,
			T,
			T,
			T,
			F,
			F,
			input$TSS,
			futuro,
			pred_nffuturo,
			futuro,
			write.projecao
		)
		
		progress$set(message = "Saving dataset...", value = 0)
		write.csv(especie, file = paste0("www/", projeto, "/csv/dados.csv"))
		
		
		#### PARA CADA ALGORITMO SELECIONADO MONTO UM MAPA COM O RASTER GERADO ###
		
		# verificando se foi gerado o arquivo de projeto ensemble final
		if (file.exists(paste0('www/', projeto, '/final/proj_ensemble.tif')))
		{
			# gero o arquivo raster para ser colocado no mapa
			rproj <-raster::raster(paste0("www/", projeto, "/final/proj_ensemble.tif"))
		}
		
		
		# EXIBIR MAPAS DOS MODELOS GERADOS NO APP : OUTPUTS DOS RESULTADOS
		output$maparesultadomax <- renderLeaflet({
			input$btnModelar
			if (file.exists(paste0('www/', projeto, '/final/mx_ensemble.tif')))
			{
				r <- raster::raster(paste0("www/", projeto, "/final/mx_ensemble.tif"))
				pal <-
					colorNumeric(c("#FFFFFF", "#FDBB84", "#31A354"), values(r), na.color = "transparent")
				if (file.exists(paste0('www/', projeto, '/final/proj_ensemble.tif')))
				{
					map = leaflet() %>% addTiles %>%
						addRasterImage(r, colors = pal, opacity = 0.8) %>%
						addRasterImage(rproj, colors = pal, opacity = 0.8) %>%
						addLegend(pal = pal,
											values = values(r),
											title = "Maxent") %>%
						addCircles(color = "red",
											 lat = especie[, 2],
											 lng = especie[, 1])  %>%
						#addMarkers(especie[,1], especie[,2]) %>%
						addRectangles(
							ext1,
							ext3,
							ext2,
							ext4,
							color = 'green',
							fill = FALSE,
							dashArray = '5,5',
							weight = 3
						)
				} else
				{
					map = leaflet() %>% addTiles %>%
						addRasterImage(r, colors = pal, opacity = 0.8) %>%
						addLegend(pal = pal,
											values = values(r),
											title = "Maxent") %>%
						addCircles(color = "red",
											 lat = especie[, 2],
											 lng = especie[, 1])  %>%
						#addMarkers(especie[,1], especie[,2]) %>%
						addRectangles(
							ext1,
							ext3,
							ext2,
							ext4,
							color = 'green',
							fill = FALSE,
							dashArray = '5,5',
							weight = 3
						)
				}
				map
			}
		})
		
		output$maparesultadosvm <- renderLeaflet({
			input$btnModelar
			if (file.exists(paste0('www/', projeto, '/final/svm_ensemble.tif')))
			{
				r <-
					raster::raster(paste0("www/", projeto, "/final/svm_ensemble.tif"))
				pal <-
					colorNumeric(c("#FFFFFF", "#FDBB84", "#31A354"), values(r), na.color = "transparent")
				if (file.exists(paste0('www/', projeto, '/final/proj_ensemble.tif')))
				{
					map = leaflet() %>% addTiles %>%
						addRasterImage(r, colors = pal, opacity = 0.8) %>%
						addRasterImage(rproj, colors = pal, opacity = 0.8) %>%
						addLegend(pal = pal,
											values = values(r),
											title = "SVM") %>%
						addCircles(color = "red",
											 lat = especie[, 2],
											 lng = especie[, 1])  %>%
						#addMarkers(especie[,1], especie[,2]) %>%
						addRectangles(
							ext1,
							ext3,
							ext2,
							ext4,
							color = 'green',
							fill = FALSE,
							dashArray = '5,5',
							weight = 3
						)
				}
				else
				{
					map = leaflet() %>% addTiles %>%
						addRasterImage(r, colors = pal, opacity = 0.8) %>%
						addLegend(pal = pal,
											values = values(r),
											title = "SVM") %>%
						addCircles(color = "red",
											 lat = especie[, 2],
											 lng = especie[, 1])  %>%
						#addMarkers(especie[,1], especie[,2]) %>%
						addRectangles(
							ext1,
							ext3,
							ext2,
							ext4,
							color = 'green',
							fill = FALSE,
							dashArray = '5,5',
							weight = 3
						)
				}
				map
			}
		})
		
		output$maparesultadomh <- renderLeaflet({
			input$btnModelar
			if (file.exists(paste0('www/', projeto, '/final/ma_ensemble.tif')))
			{
				r <- raster::raster(paste0("www/", projeto, "/final/ma_ensemble.tif"))
				pal <-
					colorNumeric(c("#FFFFFF", "#FDBB84", "#31A354"), values(r), na.color = "transparent")
				if (file.exists(paste0('www/', projeto, '/final/proj_ensemble.tif')))
				{
					map = leaflet() %>% addTiles %>%
						addRasterImage(r, colors = pal, opacity = 0.8) %>%
						addRasterImage(rproj, colors = pal, opacity = 0.8) %>%
						addLegend(pal = pal,
											values = values(r),
											title = "Maha") %>%
						addCircles(color = "red",
											 lat = especie[, 2],
											 lng = especie[, 1])  %>%
						#addMarkers(especie[,1], especie[,2]) %>%
						addRectangles(
							ext1,
							ext3,
							ext2,
							ext4,
							color = 'green',
							fill = FALSE,
							dashArray = '5,5',
							weight = 3
						)
				}
				else
				{
					map = leaflet() %>% addTiles %>%
						addRasterImage(r, colors = pal, opacity = 0.8) %>%
						addLegend(pal = pal,
											values = values(r),
											title = "Maha") %>%
						addCircles(color = "red",
											 lat = especie[, 2],
											 lng = especie[, 1])  %>%
						#addMarkers(especie[,1], especie[,2]) %>%
						addRectangles(
							ext1,
							ext3,
							ext2,
							ext4,
							color = 'green',
							fill = FALSE,
							dashArray = '5,5',
							weight = 3
						)
					
				}
				map
			}
		})
		
		output$maparesultadorf <- renderLeaflet({
			input$btnModelar
			if (file.exists(paste0('www/', projeto, '/final/rf_ensemble.tif')))
			{
				r <- raster::raster(paste0("www/", projeto, "/final/rf_ensemble.tif"))
				pal <-
					colorNumeric(c("#FFFFFF", "#FDBB84", "#31A354"), values(r), na.color = "transparent")
				if (file.exists(paste0('www/', projeto, '/final/proj_ensemble.tif')))
				{
					map = leaflet() %>% addTiles %>%
						addRasterImage(r, colors = pal, opacity = 0.8) %>%
						addRasterImage(rproj, colors = pal, opacity = 0.8) %>%
						addLegend(pal = pal,
											values = values(r),
											title = "RF") %>%
						addCircles(color = "red",
											 lat = especie[, 2],
											 lng = especie[, 1])  %>%
						#addMarkers(especie[,1], especie[,2]) %>%
						addRectangles(
							ext1,
							ext3,
							ext2,
							ext4,
							color = 'green',
							fill = FALSE,
							dashArray = '5,5',
							weight = 3
						)
				} else
				{
					map = leaflet() %>% addTiles %>%
						addRasterImage(r, colors = pal, opacity = 0.8) %>%
						addLegend(pal = pal,
											values = values(r),
											title = "RF") %>%
						addCircles(color = "red",
											 lat = especie[, 2],
											 lng = especie[, 1])  %>%
						#addMarkers(especie[,1], especie[,2]) %>%
						addRectangles(
							ext1,
							ext3,
							ext2,
							ext4,
							color = 'green',
							fill = FALSE,
							dashArray = '5,5',
							weight = 3
						)
				}
				map
			}
		})
		
		output$maparesultadoglm <- renderLeaflet({
			input$btnModelar
			if (file.exists(paste0('www/', projeto, '/final/glm_ensemble.tif')))
			{
				r <-
					raster::raster(paste0("www/", projeto, "/final/glm_ensemble.tif"))
				pal <-
					colorNumeric(c("#FFFFFF", "#FDBB84", "#31A354"), values(r), na.color = "transparent")
				if (file.exists(paste0('www/', projeto, '/final/proj_ensemble.tif')))
				{
					map = leaflet() %>% addTiles %>%
						addRasterImage(r, colors = pal, opacity = 0.8) %>%
						addRasterImage(rproj, colors = pal, opacity = 0.8) %>%
						addLegend(pal = pal,
											values = values(r),
											title = "GLM") %>%
						addCircles(color = "red",
											 lat = especie[, 2],
											 lng = especie[, 1])  %>%
						#addMarkers(especie[,1], especie[,2]) %>%
						addRectangles(
							ext1,
							ext3,
							ext2,
							ext4,
							color = 'green',
							fill = FALSE,
							dashArray = '5,5',
							weight = 3
						)
				}
				else
				{
					map = leaflet() %>% addTiles %>%
						addRasterImage(r, colors = pal, opacity = 0.8) %>%
						addLegend(pal = pal,
											values = values(r),
											title = "GLM") %>%
						addCircles(color = "red",
											 lat = especie[, 2],
											 lng = especie[, 1])  %>%
						#addMarkers(especie[,1], especie[,2]) %>%
						addRectangles(
							ext1,
							ext3,
							ext2,
							ext4,
							color = 'green',
							fill = FALSE,
							dashArray = '5,5',
							weight = 3
						)
				}
				map
			}
		})
		
		output$maparesultadobc <- renderLeaflet({
			input$btnModelar
			if (file.exists(paste0('www/', projeto, '/final/bc_ensemble.tif')))
			{
				r <- raster::raster(paste0("www/", projeto, "/final/bc_ensemble.tif"))
				crs(r) <-
					sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
				pal <-
					colorNumeric(c("#FFFFFF", "#FDBB84", "#31A354"), values(r), na.color = "transparent")
				if (file.exists(paste0('www/', projeto, '/final/proj_ensemble.tif')))
				{
					map = leaflet() %>% addTiles %>%
						addRasterImage(r, colors = pal, opacity = 0.9) %>%
						addRasterImage(rproj, colors = pal, opacity = 0.9) %>%
						addLegend(pal = pal,
											values = values(r),
											title = "BioClim") %>%
						addCircles(color = "red",
											 lat = especie[, 2],
											 lng = especie[, 1])  %>%
						#addCircles(color = "red", lat = ~ especie[,1], lng = ~ especie[,2]) %>%
						#addMarkers(especie[,1], especie[,2]) %>%
						addRectangles(
							ext1,
							ext3,
							ext2,
							ext4,
							color = 'green',
							fill = FALSE,
							dashArray = '5,5',
							weight = 3
						)
				} else
				{
					map = leaflet() %>% addTiles %>%
						addRasterImage(r, colors = pal, opacity = 0.8) %>%
						addLegend(pal = pal,
											values = values(r),
											title = "BioClim") %>%
						addCircles(color = "red",
											 lat = especie[, 2],
											 lng = especie[, 1])  %>%
						#addCircles(color = "red", lat = ~ especie[,1], lng = ~ especie[,2]) %>%
						#addMarkers(especie[,1], especie[,2]) %>%
						addRectangles(
							ext1,
							ext3,
							ext2,
							ext4,
							color = 'green',
							fill = FALSE,
							dashArray = '5,5',
							weight = 3
						)
				}
				map
			}
		})
		
		output$maparesultadodo <- renderLeaflet({
			input$btnModelar
			if (file.exists(paste0('www/', projeto, '/final/do_ensemble.tif')))
			{
				r <- raster::raster(paste0("www/", projeto, "/final/do_ensemble.tif"))
				pal <-
					colorNumeric(c("#FFFFFF", "#FDBB84", "#31A354"), values(r), na.color = "transparent")
				if (file.exists(paste0('www/', projeto, '/final/proj_ensemble.tif')))
				{
					map = leaflet() %>% addTiles %>%
						addRasterImage(r, colors = pal, opacity = 0.8) %>%
						addRasterImage(rproj, colors = pal, opacity = 0.8) %>%
						addLegend(pal = pal,
											values = values(r),
											title = "BioClim") %>%
						addCircles(color = "red",
											 lat = especie[, 2],
											 lng = especie[, 1])  %>%
						#addMarkers(especie[,1], especie[,2]) %>%
						addRectangles(
							ext1,
							ext3,
							ext2,
							ext4,
							color = 'green',
							fill = FALSE,
							dashArray = '5,5',
							weight = 3
						)
				} else
				{
					map = leaflet() %>% addTiles %>%
						addRasterImage(r, colors = pal, opacity = 0.8) %>%
						addLegend(pal = pal,
											values = values(r),
											title = "BioClim") %>%
						addCircles(color = "red",
											 lat = especie[, 2],
											 lng = especie[, 1])  %>%
						#addMarkers(especie[,1], especie[,2]) %>%
						addRectangles(
							ext1,
							ext3,
							ext2,
							ext4,
							color = 'green',
							fill = FALSE,
							dashArray = '5,5',
							weight = 3
						)
				}
				map
			}
		})
		
		
		## APRESENTO LISTAGEM DOS ARQUIVOS GERADOS NA PÁGINA DE DOWNLOAD (ABA OUTPUT)
		output$uiarquivosmodelos <- renderUI({
			lista_models <-
				list.files(
					paste0("www/", projeto, "/models"),
					full.names = F,
					pattern = paste0("pre_")
				)
			lapply(1:length(sort(lista_models)), function(i) {
				tags$div(tags$a(
					href = paste0(home, projeto, '/models/', lista_models[i]),
					paste0(lista_models[i])
				))
			})
		})
		
		output$ui <- renderUI({
			lista_jpg <-
				list.files(
					paste0("www/", projeto, "/jpg"),
					full.names = F,
					pattern = paste0(".jpg")
				)
			lapply(1:length(order(lista_jpg)), function(i) {
				tags$a(
					href = paste0(home, projeto, '/jpg/', lista_jpg[i]),
					tags$img(
						src = paste0(projeto, '/jpg/', lista_jpg[i]),
						height = "200px"
					),
					target = "_blank"
				)
			})
		})
		
		output$uiscript <- renderUI({
			lista_txt <-
				list.files(
					paste0("www/", projeto, "/"),
					full.names = F,
					pattern = paste0("script.R")
				)
			lapply(1:length(lista_txt), function(i) {
				tags$div(tags$a(
					href = paste0(home, projeto, '/', lista_txt[i]),
					paste0(lista_txt[i]),
					target = "_blank"
				))
			})
		})
		
		output$uiestatistica <- renderUI({
			lista_txt <-
				list.files(
					paste0("www/", projeto, "/models"),
					full.names = F,
					pattern = paste0("statsALL.txt")
				)
			lapply(1:length(lista_txt), function(i) {
				tags$div(tags$a(
					href = paste0(home, projeto, '/models/', lista_txt[i]),
					paste0(lista_txt[i]),
					target = "_blank"
				))
			})
		})
		
		output$uiarquivosdados <- renderUI({
			lista_csv <-
				list.files(
					paste0("www/", projeto, "/csv"),
					full.names = F,
					pattern = paste0(".csv")
				)
			lapply(1:length(lista_csv), function(i) {
				tags$div(tags$a(
					href = paste0(home, projeto, '/csv/', lista_csv[i]),
					paste0(lista_csv[i]),
					target = "_blank"
				))
			})
		})
		
		output$uiarquivosensemble <- renderUI({
			lista_final <-
				list.files(
					paste0("www/", projeto, "/final"),
					full.names = F,
					pattern = paste0(".tif")
				)
			lapply(1:length(sort(lista_final)), function(i) {
				tags$div(tags$a(
					href = paste0(home, projeto, '/final/', lista_final[i]),
					paste0(lista_final[i]),
					target = "_blank"
				))
			})
		})
		
		output$uiarquivosprojecao <- renderUI({
			lista_proj <-
				list.files(
					paste0("www/", projeto, "/proj"),
					full.names = F,
					pattern = paste0(".tif")
				)
			lapply(1:length(sort(lista_proj)), function(i) {
				tags$div(tags$a(
					href = paste0(home, projeto, '/proj/', lista_proj[i]),
					paste0(lista_proj[i]),
					target = "_blank"
				))
			})
		})
		
		output$uiarquivosprojecaofuturo <- renderUI({
			lista_futuro <-
				list.files(
					paste0("www/", projeto, "futuro"),
					full.names = F,
					pattern = paste0(".tif")
				)
			
			lapply(1:length(sort(lista_futuro)), function(i) {
				tags$div(tags$a(
					href = paste0(home, projeto, '/futuro/', lista_futuro[i]),
					paste0(lista_futuro[i]),
					target = "_blank"
				))
			})
		})
		
	})# Fim função modelagem