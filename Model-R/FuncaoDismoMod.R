# INÍCIO FUNÇÃO dismo.mod -------------------------------------------------
dismo.mod <- function(sp,
											occs = spp.filt,
											var = expl,
											var2 = expl2,
											maxent = F,
											Bioclim = F,
											GLM = F,
											RF = F,
											SVM = F,
											Mahal = F,
											Domain = F,
											SVM2 = F,
											part = 3,
											numpontos  = 500,
											seed = 123,
											write.cont = T, # escreve modelos cont?nuos
											bin = T, # faz modelos bin?rios
											write.bin = T, # escreve modelos bin?rios
											mult = T, # faz modelos multiplicados(bin*con)
											write.mult = T, # escreve modelos multiplicados
											TSS.value  = 0.2,
											future.model = F, # faz modelos futuros
											future.raster = newdata, # vari?veis futuras
											write.future = F, # escreve modelos futuros
											write.projecao = F)

{
	## Carregando bibliotecas
	library(dismo)
	library(randomForest)
	library(kernlab)
	library(XML)
	library(raster)
	library(rgdal)
	library(maps)
	
	
	## DELETE OS ARQUIVOS ANTIGOS ANTES DE GERAR UM NOVO
	unlink(paste0("www/",projeto,'/models/evaluate_.txt'), recursive=TRUE)
	unlink(paste0("www/",projeto,'/models/statsALL.txt'), recursive=TRUE)
	unlink(paste0("www/",projeto,'/models/evaluate_ALL_models.txt'), recursive=TRUE)
	
	isolate({
		print(date())
		cat(paste("Modeling",sp,"...",'\n'))
		coord <- especie
		n <- nrow(coord)
		
		## Extraindo os valores das vari?veis onde h? pontos de registros
		presvals<- raster::extract(var,coord)
		
		## Seeting seed para sempre criar os mesmos pontos aleat?rios
		set.seed(seed)
		
		## Gerando as pseudoaus?ncias aleatoriamente
		backgr <- randomPoints(var, numpontos)
		
		## Determina os nomes da colunas de coordenadas para os pontos de background
		colnames(backgr) = c('Longitude', 'Latitude')
		
		## Extraindo os valores das vari?veis onde h? pseudoaus?ncias
		absvals <- raster::extract(var, backgr)
		
		## Cria um vetor contendo algarismo "1" e "0" correspondendo ao n?mero de registros presen?as e aus?ncias respectivamente.
		pre_abs <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
		
		## N?mero de parti??es
		if (n<10) part<-n else part <- part
		
		## Setting seed para distribuir as presen?as sempre para as mesmas parti??es
		set.seed(seed)
		
		## Separando as presen?as e pseudoaus?ncias nos grupos das parti??es
		group_pre <- kfold(coord,part)
		set.seed(seed)
		group_abs <- kfold(backgr,part)
		
		# Cosntruindo o data.frame com todos os dados
		append_1 <- append(group_pre,group_abs)
		cbind_1 <- cbind(coord,presvals)
		cbind_2 <- cbind(backgr,absvals)
		rbind_1 <- rbind(cbind_1,cbind_2)
		sdmdata <- data.frame(cbind(append_1,pre_abs,rbind_1))
		sdmdata2 <- sdmdata[-c(1,2,3,4)]
		# sdmdata2 <- sdmdata2[-1]
		# sdmdata2 <- sdmdata2[-1]
		# sdmdata2 <- sdmdata2[-1]
		
		colnames(sdmdata)[1] <- "group"
		
		####################################################################################################
	
		#PARA CADA PARTIÇÃO:
		for (i in unique(group_pre)) {
			progress$set(message = paste("Processando a modelagem", i),
									 value = 0)
			
			# Separar o sdmdata entre teste e treino
			sdmdata_train <- subset(sdmdata, group != i)
			sdmdata_teste <- subset(sdmdata, group == i)
			
			# Preparando sdmdata train para fazer o modelo
			envtrain <-
				subset(sdmdata_train, select = c(-group, -Longitude, -Latitude))
			
			# Preparando sdmdata test para avaliar modelos que n?o usam s? presen?a
			envtest_pre <-
				subset(sdmdata_teste,
							 pre_abs == 1,
							 select = c(-group, -Latitude, -Latitude, -pre_abs))
			envtest_abs <-
				subset(sdmdata_teste,
							 pre_abs == 0,
							 select = c(-group, -Latitude, -Latitude, -pre_abs))
			
			### Separando os dados (apenas coordenadas) para fazer BioClim, Maxent e Mahalanobis que usam s? presen?a
			coord_pres_train <-
				subset(sdmdata_train, pre_abs == 1, select = c(Longitude, Latitude))
			coord_pres_teste <-
				subset(sdmdata_teste, pre_abs == 1, select = c(Longitude, Latitude))
			coord_abs_train <-
				subset(sdmdata_train, pre_abs == 0, select = c(Longitude, Latitude))
			coord_abs_teste <-
				subset(sdmdata_teste, pre_abs == 0, select = c(Longitude, Latitude))
			
			
			
			### FAZ OS MODELOS
			cat(paste("Modeling...", sp, "Partition", i, '\n'))
			
			if (Bioclim == T) {
				cat(paste("#Bioclim", '\n'))
				# Constr?i o modelo no espa?o ambiental
				bc <- bioclim (var, coord_pres_train)
				
				# Validacao da performance
				ebc <<- dismo::evaluate(coord_pres_teste, coord_abs_teste, bc, var)
				# C?lculo do TSS
				bcTSS <- max(ebc@TPR + ebc@TNR) - 1
				# Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
				tbc <- threshold (ebc, 'spec_sens')
				# Projeta no espa?o geografico o modelo contnuo
				bc_cont <- predict (var, bc, progress = 'text')
				bc_cont_proj <- predict (var2, bc, progress = 'text')
				# Transforma em binario o modelo continuo cortando-o pelo limiar tbc
				bc_bin <- bc_cont > tbc
				# Resgata os valores continuos ao multiplicar o modelo binario pelo continuo
				bc_mult <- bc_bin * bc_cont
				# Normaliza o modelo mult
				bc_mult <- bc_mult / maxValue(bc_mult)
				# Faz os modelos futuros
				if (future.model == T) {
					# Projeta o modelo nas variiveis futuras
					bc_future <- predict(future.raster, bc, progress = 'text')
					# Transforma em bin?rio o modelo futuro cont?nuo pelo threshold do modelo presente
					if (bin == T) {
						bc_future_bin <- bc_future > tbc
					}
					# Resgata os valores continuos ao multiplicar o modelo binario pelo continuo
					if (mult == T) {
						bc_future_mult <- bc_future_bin * bc_future
						# Normaliza o modelo mult
						bc_future_mult <- bc_future_mult / maxValue(bc_future_mult)
					}
				} # Fecha o modelo futuro
			} # Fecha o algoritmo Bioclim
			
			if (Domain == T) {
				cat(paste("Domain", '\n'))
				
				# Constr?i o modelo no espa?o ambiental
				do <- domain (var, coord_pres_train)
				# Valida??o da performance
				edo <<- dismo::evaluate(coord_pres_teste, coord_abs_teste, do, var)
				# C?lculo do TSS
				doTSS <- max(edo@TPR + edo@TNR) - 1
				# Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
				tdo <- threshold (edo, 'spec_sens')
				# Projeta no espa?o geogr?fico o modelo cont?nuo
				do_cont <- predict (var, do, progress = 'text')
				do_cont_proj <- predict (var2, do, progress = 'text')
				
				# Transforma em bin?rio o modelo cont?nuo cortando-o pelo limiar tbc
				do_bin <- do_cont > tdo
				# Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
				do_mult <- do_bin * do_cont
				# Normaliza o modelo mult
				do_mult <- do_mult / maxValue(do_mult)
				# Faz os modelos futuros
				if (future.model == T) {
					# Projeta o modelo nas vari?veis futuras
					do_future <- predict(future.raster, do, progress = 'text')
					# Transforma em bin?rio o modelo futuro cont?nuo pelo threshold do modelo presente
					if (bin == T) {
						do_future_bin <- do_future > tdo
					}
					# Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
					if (mult == T) {
						do_future_mult <- do_future_bin * do_future
						# Normaliza o modelo mult
						do_future_mult <- do_future_mult / maxValue(do_future_mult)
					}
				} # Fecha o modelo futuro
			} # Fecha o algoritmo Bioclim
			
			if (maxent == T) {
				cat(paste("Maxent", '\n'))
				# Constr?i o modelo no espa?o ambiental
				mx <- maxent (var, coord_pres_train)
				# Valida??o da performance
				emx <- dismo::evaluate(coord_pres_teste, coord_abs_teste, mx, var)
				# C?lculo do TSS
				mxTSS <- max(emx@TPR + emx@TNR) - 1
				# Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
				tmx <- threshold (emx, 'spec_sens')
				# Projeta no espa?o geogr?fico o modelo cont?nuo
				mx_cont <- predict (var, mx, progress = 'text')
				mx_cont_proj <- predict (var2, mx, progress = 'text')
				
				# Transforma em bin?rio o modelo cont?nuo cortando-o pelo limiar tbc
				mx_bin <- mx_cont > tmx
				# Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
				mx_mult <- mx_bin * mx_cont
				# Normaliza o modelo mult
				mx_mult <- mx_mult / maxValue(mx_mult)
				if (future.model == T) {
					# Projeta o modelo nas vari?veis futuras
					mx_future <- predict(future.raster, mx, progress = 'text')
					# Transforma em bin?rio o modelo futuro cont?nuo pelo threshold do modelo presente
					if (bin == T) {
						mx_future_bin <- mx_future > tmx
					}
					# Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
					if (mult == T) {
						mx_future_mult <- mx_future_bin * mx_future
						# Normaliza o modelo mult
						mx_future_mult <- mx_future_mult / maxValue(mx_future_mult)
					}
				} # Fecha o modelo futuro
			} # Fecha o algoritmo Maxent
			
			if (GLM == T) {
				cat(paste("GLM", '\n'))
				# Constr?i o modelo no espa?o ambiental
				mglm <- glm(pre_abs ~ ., data = envtrain)
				# Valida??o da performance
				eglm <- dismo::evaluate(envtest_pre, envtest_abs, mglm)
				# C?lculo do TSS
				glmTSS <- max(eglm@TPR + eglm@TNR) - 1
				# Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
				tglm <- threshold (eglm, 'spec_sens')
				# Projeta no espa?o geogr?fico o modelo cont?nuo
				glm_cont <- predict (var, mglm, progress = 'text')
				glm_cont_proj <- predict (var2, mglm, progress = 'text')
				
				#plot(glm_cont)
				# Transforma em bin?rio o modelo cont?nuo cortando-o pelo limiar tbc
				glm_bin <- glm_cont > tglm
				# Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
				glm_mult <- glm_bin * glm_cont
				# Normaliza o modelo mult
				glm_mult <- glm_mult / maxValue(glm_mult)
				if (future.model == T) {
					# Projeta o modelo nas vari?veis futuras
					glm_future <- predict(future.raster, mglm, progress = 'text')
					# Transforma em bin?rio o modelo futuro cont?nuo pelo threshold do modelo presente
					if (bin == T) {
						glm_future_bin <- glm_future > tglm
					}
					# Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
					if (mult == T) {
						glm_future_mult <- glm_future_bin * glm_future
						# Normaliza o modelo mult
						glm_future_mult <- glm_future_mult / maxValue(glm_future_mult)
					}
				} # Fecha o modelo futuro
			} # Fecha o algoritmo GLM
			
			if (RF == T) {
				cat(paste("RF", '\n'))
				
				# Constr?i o modelo no espa?o ambiental
				##rf1 <- randomForest (pre_abs~.,data=envtrain) # porque da mensagem de aviso ao usar rf1(regression)?
				##envtrain
				##pre_abs
				rf1 <-
					randomForest (pre_abs ~ ., data = envtrain) # porque da mensagem de aviso ao usar rf1(regression)?
				#rf2 <- randomForest (factor(pre_abs) ~ ., data=envtrain) # faz classification e n?o d? mensagem de erro.
				# rf2 tem como output somente modelos bin?rios
				# Valida??o de performance
				erf1 <- dismo::evaluate(envtest_pre, envtest_abs, rf1)
				#erf2 <- dismo::evaluate(envtest_pre,envtest_abs, rf2)
				# C?lculo do TSS
				rfTSS1 <- max(erf1@TPR + erf1@TNR) - 1
				#rfTSS2 <- max(erf2@TPR + erf2@TNR)-1
				# Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
				trf1 <- threshold (erf1, 'spec_sens')
				#trf2 <- threshold (erf2,'spec_sens') # tbm d? mensagem de erro
				# Projeta no espa?o geogr?fico o modelo cont?nuo
				rf1_cont <- predict (var, rf1, progress = 'text')
				rf1_cont_proj <- predict (var2, rf1, progress = 'text')
				#rf_cont2 <- predict (var,rf2,progress='text') # o cont?nuo fica igual ao bin?rio!
				# Transforma em bin?rio o modelo cont?nuo cortando-o pelo limiar tbc
				rf1_bin <- rf1_cont > trf1
				#rf_bin2 <- rf_cont2>trf2
				# Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
				rf1_mult <- rf1_bin * rf1_cont
				#rf_mult2 <- rf_bin2*rf_cont2
				# Normaliza o modelo mult
				rf1_mult <- rf1_mult / maxValue(rf1_mult)
				
				if (future.model == T) {
					# Projeta o modelo nas vari?veis futuras
					rf1_future <- predict(future.raster, rf1, progress = 'text')
					# Transforma em bin?rio o modelo futuro cont?nuo pelo threshold do modelo presente
					if (bin == T) {
						rf1_future_bin <- rf1_future > trf1
					}
					# Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
					if (mult == T) {
						rf1_future_mult <- rf1_future_bin * rf1_future
						# Normaliza o modelo mult
						rf1_future_mult <- rf1_future_mult / maxValue(rf1_future_mult)
					}
				} # Fecha o modelo futuro
			} # Fecha o algoritmo RandomForest
			
			if (SVM == T) {
				cat(paste("SVM", '\n'))
				# Constr?i o modelo no espa?o ambiental
				msvm <- ksvm(pre_abs ~ ., data = envtrain)
				# Valida??o da performance
				esvm <- dismo::evaluate(envtest_pre, envtest_abs, msvm)
				# C?lculo do TSS
				svmTSS <- max(esvm@TPR + esvm@TNR) - 1
				# Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
				tsvm <- threshold (esvm, 'spec_sens')
				# Projeta no espa?o geogr?fico o modelo cont?nuo
				svm_cont <- predict (var, msvm, progress = 'text')
				svm_cont_proj <- predict (var2, msvm, progress = 'text')
				# Transforma em bin?rio o modelo cont?nuo cortando-o pelo limiar tbc
				svm_bin <- svm_cont > tsvm
				# Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
				svm_mult <- svm_bin * svm_cont
				# Normaliza o modelo mult
				svm_mult <- svm_mult / maxValue(svm_mult)
				if (future.model == T) {
					# Projeta o modelo nas vari?veis futuras
					svm_future <- predict(future.raster, msvm, progress = 'text')
					# Transforma em bin?rio o modelo futuro cont?nuo pelo threshold do modelo presente
					if (bin == T) {
						svm_future_bin <- svm_future > tsvm
					}
					# Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
					if (mult == T) {
						svm_future_mult <- svm_future_bin * svm_future
						# Normaliza o modelo mult
						svm_future_mult <- svm_future_mult / maxValue(svm_future_mult)
					}
				} # Fecha o modelo futuro
			} # Fecha o algoritmo SVM
			
			if (Mahal == T) {
				cat(paste("Mahal", '\n'))
				
				# Checa se o n?mero de registros de presen?a ? maior que o n?mero de vari?veis
				condicao_Mahal <- nrow(coord_pres_train) > length(names(var))
				if (condicao_Mahal == TRUE) {
					# Construi o modelo no espaco ambiental
					
					ma <- mahal (var, coord_pres_train)
					# validacao da performance
					ema <- dismo::evaluate(coord_pres_teste, coord_abs_teste, ma, var)
					# Calculo do TSS
					maTSS <- max(ema@TPR + ema@TNR) - 1
					# Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
					tma <- threshold (ema, 'spec_sens')
					# Projeta no espaco geografico o modelo continuo
					ma_cont <- predict (var, ma, progress = 'text')
					ma_cont_proj <- predict (var2, ma, progress = 'text')
					# Invertendo os valores dos pixel, porque o valor 0 corresponde ao maior valor de adequabilidade
					ma_cont_invert <- ma_cont + (-1 * minValue(ma_cont))
					# Transforma em binario o modelo continuo cortando-o pelo limiar tma
					ma_bin <- ma_cont > tma
					# Resgata os valores continuos ao multiplicar o modelo binario pelo continuo invertido
					ma_mult <- ma_bin * ma_cont_invert
					# Normaliza o modelo mult
					ma_mult <- ma_mult / maxValue(ma_mult)
					# Faz os modelos futuros
					if (future.model == T) {
						# Projeta o modelo nas variaveis futuras
						ma_future <- predict(future.raster, ma, progress = 'text')
						# Invertendo os valores dos pixel, porque o valor 0 corresponde ao maior valor de adequabilidade
						ma_future_invert <- ma_future + (-1 * minValue(ma_future))
						# Transforma em binario o modelo futuro continuo pelo threshold do modelo presente
						if (bin == T) {
							ma_future_bin <- ma_future > tma
						}
						# Resgata os valores continuos ao multiplicar o modelo binario pelo continuo
						if (mult == T) {
							ma_future_mult <- ma_future_bin * ma_future_invert
							# Normaliza o modelo mult
							ma_future_mult <- ma_future_mult / maxValue(ma_future_mult)
						}
					} # Fecha o modelo futuro
				} # Fecha o algoritmo Mahalanobis
				else {
					
				}
			}
			
			
			
			
			### ESCREVE OS MODELOS
			## Modelos continuos
			if (write.cont == T) {
				cat(paste("Salvando modelos continuos...", sp, i, '\n'))
				
				if (Bioclim == T) {
					writeRaster(
						x = bc_cont,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_bc_con", ".tif"),
						overwrite = T
					)
					png(filename = paste0("./www/", projeto, "/jpg/pre_", i, "_bc_con", ".jpg"))
					plot(bc_cont, main = paste("BioClim - ", i))
					
					dev.off()
					
					if (write.projecao == T)
					{
						writeRaster(
							x = bc_cont_proj,
							filename = paste0(
								"./www/",
								projeto,
								"/proj/pre_",
								i,
								"_bc_con_proj",
								".tif"
							),
							overwrite = T
						)
					}
					if (write.future == T) {
						writeRaster(
							x = bc_future,
							filename = paste0("./www/", projeto, "/futuro/fut_", i, "_bc_con", ".tif"),
							overwrite = T
						)
						png(filename = paste0("./www/", projeto, "/jpg/fut_", i, "_bc_con", ".jpg"))
						plot(bc_future, main = paste("BioClim - Fut ", i))
						dev.off()
					}
				}
				
				if (Domain == T) {
					writeRaster(
						x = do_cont,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_do_con", ".tif"),
						overwrite = T
					)
					
					png(filename = paste0("./www/", projeto, "/jpg/pre_", i, "_do_con", ".jpg"))
					plot(do_cont, main = paste("Domain - ", i))
					
					dev.off()
					
					if (write.projecao == T)
					{
						writeRaster(
							x = do_cont_proj,
							filename = paste0(
								"./www/",
								projeto,
								"/proj/pre_",
								i,
								"_do_con_proj",
								".tif"
							),
							overwrite = T
						)
					}
					if (write.future == T) {
						writeRaster(
							x = do_future,
							filename = paste0("./www/", projeto, "/futuro/fut_", i, "_do_con", ".tif"),
							overwrite = T
						)
						png(filename = paste0("./www/", projeto, "/jpg/fut_", i, "_do_con", ".jpg"))
						plot(bc_future, main = paste("Domain - Fut ", i))
						dev.off()
					}
				}
				
				if (maxent == T) {
					writeRaster(
						x = mx_cont,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_mx_con", ".tif"),
						overwrite = T
					)
					png(filename = paste0("./www/", projeto, "/jpg/pre_", i, "_mx_con", ".jpg"))
					plot(mx_cont, main = paste("Maxent - ", i))
					dev.off()
					
					if (write.projecao == T)
					{
						writeRaster(
							x = mx_cont_proj,
							filename = paste0(
								"./www/",
								projeto,
								"/proj/pre_",
								i,
								"_mx_con_proj",
								".tif"
							),
							overwrite = T
						)
					}
					if (write.future == T) {
						writeRaster(
							x = mx_future,
							filename = paste0("./www/", projeto, "/futuro/fut_", i, "_mx_con", ".tif"),
							overwrite = T
						)
						png(filename = paste0("./www/", projeto, "/jpg/fut_", i, "_mx_con", ".jpg"))
						plot(mx_future, main = paste("Maxent - Fut ", i))
						dev.off()
					}
				}
				
				if (GLM == T) {
					writeRaster(
						x = glm_cont,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_glm_con", ".tif"),
						overwrite = T
					)
					png(filename = paste0("./www/", projeto, "/jpg/pre_", i, "_glm_con", ".jpg"))
					plot(glm_cont, main = paste("GLM - ", i))
					dev.off()
					if (write.projecao == T)
					{
						writeRaster(
							x = glm_cont_proj,
							filename = paste0(
								"./www/",
								projeto,
								"/proj/pre_",
								i,
								"_glm_con_proj",
								".tif"
							),
							overwrite = T
						)
					}
					
					if (write.future == T) {
						writeRaster(
							x = glm_future,
							filename = paste0(
								"./www/",
								projeto,
								"/futuro/fut_",
								i,
								"_glm_con",
								".tif"
							),
							overwrite = T
						)
						png(filename = paste0("./www/", projeto, "/jpg/fut_", i, "_glm_con", ".jpg"))
						plot(glm_future, main = paste("GLM - Fut ", i))
						dev.off()
					}
				}
				
				if (RF == T) {
					writeRaster(
						x = rf1_cont,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_rf_con", ".tif"),
						overwrite = T
					)
					png(filename = paste0("./www/", projeto, "/jpg/pre_", i, "_rf_con", ".jpg"))
					plot(rf1_cont, main = paste("RF - ", i))
					
					dev.off()
					
					if (write.projecao == T)
					{
						writeRaster(
							x = rf1_cont_proj,
							filename = paste0(
								"./www/",
								projeto,
								"/proj/pre_",
								i,
								"_rf_con_proj",
								".tif"
							),
							overwrite = T
						)
					}
					if (write.future == T) {
						writeRaster(
							x = rf1_future,
							filename = paste0(
								"./www/",
								projeto,
								"/futuro/fut_",
								i,
								"_rf1_con",
								".tif"
							),
							overwrite = T
						)
						png(filename = paste0("./www/", projeto, "/jpg/fut_", i, "_rf1_con", ".jpg"))
						plot(rf1_future, main = paste("RF - Fut ", i))
						dev.off()
					}
				}
				
				if (SVM == T) {
					writeRaster(
						x = svm_cont,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_svm_con", ".tif"),
						overwrite = T
					)
					png(filename = paste0("./www/", projeto, "/jpg/pre_", i, "_svm_con", ".jpg"))
					plot(svm_cont, main = paste("SVM - ", i))
					dev.off()
					if (write.projecao == T)
					{
						writeRaster(
							x = svm_cont_proj,
							filename = paste0(
								"./www/",
								projeto,
								"/proj/pre_",
								i,
								"_svm_con_proj",
								".tif"
							),
							overwrite = T
						)
					}
					if (write.future == T) {
						writeRaster(
							x = svm_future,
							filename = paste0(
								"./www/",
								projeto,
								"/futuro/fut_",
								i,
								"_svm_con",
								".tif"
							),
							overwrite = T
						)
						png(filename = paste0("./www/", projeto, "/jpg/fut_", i, "_svm_con", ".jpg"))
						plot(svm_future, main = paste("SVM - Fut ", i))
						dev.off()
					}
				}
				
				if (Mahal == T && condicao_Mahal == TRUE) {
					writeRaster(
						x = ma_cont,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_ma_con", ".tif"),
						overwrite = T
					)
					png(filename = paste0("./www/", projeto, "/jpg/pre_", i, "_ma_con", ".jpg"))
					plot(ma_cont, main = paste("Mahalanobis - ", i))
					dev.off()
					if (write.projecao == T)
					{
						writeRaster(
							x = ma_cont_proj,
							filename = paste0(
								"./www/",
								projeto,
								"/proj/pre_",
								i,
								"_ma_con_proj",
								".tif"
							),
							overwrite = T
						)
					}
					if (write.future == T) {
						writeRaster(
							x = ma_future,
							filename = paste0("./www/", projeto, "/futuro/fut_", i, "_ma_con", ".tif"),
							overwrite = T
						)
						png(filename = paste0("./www/", projeto, "/jpg/fut_", i, "_ma_con", ".jpg"))
						plot(ma_future, main = paste("Mahalanobis - Fut ", i))
						dev.off()
					}
				}
				
			} # Fecha escrita de modelos cont?nuos
			
			## Modelos bin?rios
			if (write.bin == T) {
				cat(paste("Salvando modelos binários...", sp, i, '\n'))
				
				if (Bioclim == T) {
					writeRaster(
						x = bc_bin,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_bc_bin", ".tif"),
						overwrite = T
					)
					png(filename = paste0("./www/", projeto, "/jpg/pre_", i, "_bc_bin", ".jpg"))
					plot(bc_bin, main = paste("Bioclim - Bin ", i))
					dev.off()
					
					if (write.future == T) {
						writeRaster(
							x = bc_future_bin,
							filename = paste0("./www/", projeto, "/futuro/fut_", i, "_bc_bin", ".tif"),
							overwrite = T
						)
					}
				}
				
				if (Domain == T) {
					writeRaster(
						x = do_bin,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_do_bin", ".tif"),
						overwrite = T
					)
					png(filename = paste0("./www/", projeto, "/jpg/pre_", i, "_do_bin", ".jpg"))
					plot(do_bin, main = paste("Domain - Bin ", i))
					dev.off()
					
					if (write.future == T) {
						writeRaster(
							x = do_future_bin,
							filename = paste0("./www/", projeto, "/futuro/fut_", i, "_do_bin", ".tif"),
							overwrite = T
						)
					}
				}
				
				if (maxent == T) {
					writeRaster(
						x = mx_bin,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_mx_bin", ".tif"),
						overwrite = T
					)
					png(filename = paste0("./www/", projeto, "/jpg/pre_", i, "_mx_bin", ".jpg"))
					plot(mx_bin, main = paste("Maxent - Bin ", i))
					dev.off()
					
					if (write.future == T) {
						writeRaster(
							x = mx_future_bin,
							filename = paste0("./www/", projeto, "/futuro/fut_", i, "_mx_bin", ".tif"),
							overwrite = T
						)
					}
				}
				
				if (GLM == T) {
					writeRaster(
						x = glm_bin,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_glm_bin", ".tif"),
						overwrite = T
					)
					png(filename = paste0("./www/", projeto, "/jpg/pre_", i, "_glm_bin", ".jpg"))
					plot(glm_bin, main = paste("GLM - Bin ", i))
					dev.off()
					
					if (write.future == T) {
						writeRaster(
							x = glm_future_bin,
							filename = paste0(
								"./www/",
								projeto,
								"/futuro/fut_",
								i,
								"_glm_bin",
								".tif"
							),
							overwrite = T
						)
					}
				}
				
				if (RF == T) {
					writeRaster(
						x = rf1_bin,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_rf_bin", ".tif"),
						overwrite = T
					)
					png(filename = paste0("./www/", projeto, "/jpg/pre_", i, "_rf_bin", ".jpg"))
					plot(rf1_bin, main = paste("RF - Bin ", i))
					dev.off()
					if (write.future == T) {
						writeRaster(
							x = rf1_future_bin,
							filename = paste0("./www/", projeto, "/futuro/fut_", i, "_rf_bin", ".tif"),
							overwrite = T
						)
					}
				}
				
				if (SVM == T) {
					writeRaster(
						x = svm_bin,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_svm_bin", ".tif"),
						overwrite = T
					)
					png(filename = paste0("./www/", projeto, "/jpg/pre_", i, "_svm_bin", ".jpg"))
					plot(svm_bin, main = paste("SVM - Bin ", i))
					dev.off()
					if (write.future == T) {
						writeRaster(
							x = svm_future_bin,
							filename = paste0(
								"./www/",
								projeto,
								"/futuro/fut_",
								i,
								"_svm_bin",
								".tif"
							),
							overwrite = T
						)
					}
				}
				
				if (Mahal == T &&
						condicao_Mahal == TRUE) {
					writeRaster(
						x = ma_bin,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_ma_bin", ".tif"),
						overwrite = T
					)
					png(filename = paste0("./www/", projeto, "/jpg/pre_", i, "_ma_bin", ".jpg"))
					plot(ma_bin, main = paste("Mahalanobis - Bin ", i))
					dev.off()
					if (write.future == T) {
						writeRaster(
							x = ma_future_bin,
							filename = paste0("./www/", projeto, "/futuro/fut_", i, "_ma_bin", ".tif"),
							overwrite = T
						)
					}
				}
				
				
			} # Fecha escrita de modelos binarios
			
			## Modelos multiplicados
			if (write.mult == T) {
				cat(paste("Salvando modelos multiplicados...", sp, i, '\n'))
				
				if (Bioclim == T) {
					writeRaster(
						x = bc_mult,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_bc_mult", ".tif"),
						overwrite = T
					)
					if (write.future == T) {
						writeRaster(
							x = bc_future_mult,
							filename = paste0(
								"./www/",
								projeto,
								"/futuro/fut_",
								i,
								"_bc_mult",
								".tif"
							),
							overwrite = T
						)
					}
				}
				
				if (Domain == T) {
					writeRaster(
						x = do_mult,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_do_mult", ".tif"),
						overwrite = T
					)
					if (write.future == T) {
						writeRaster(
							x = do_future_mult,
							filename = paste0(
								"./www/",
								projeto,
								"/futuro/fut_",
								i,
								"_do_mult",
								".tif"
							),
							overwrite = T
						)
					}
				}
				
				if (maxent == T) {
					writeRaster(
						x = mx_mult,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_mx_mult", ".tif"),
						overwrite = T
					)
					if (write.future == T) {
						writeRaster(
							x = mx_future_mult,
							filename = paste0(
								"./www/",
								projeto,
								"/futuro/fut_",
								i,
								"_mx_mult",
								".tif"
							),
							overwrite = T
						)
					}
				}
				
				if (GLM == T) {
					writeRaster(
						x = glm_mult,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_glm_mult", ".tif"),
						overwrite = T
					)
					if (write.future == T) {
						writeRaster(
							x = glm_future_mult,
							filename = paste0(
								"./www/",
								projeto,
								"/futuro/fut_",
								i,
								"_glm_mult",
								".tif"
							),
							overwrite = T
						)
					}
				}
				
				if (RF == T) {
					writeRaster(
						x = rf1_mult,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_rf_mult", ".tif"),
						overwrite = T
					)
					if (write.future == T) {
						writeRaster(
							x = rf1_future_mult,
							filename = paste0(
								"./www/",
								projeto,
								"/futuro/fut_",
								i,
								"_rf_mult",
								".tif"
							),
							overwrite = T
						)
					}
				}
				
				if (SVM == T) {
					writeRaster(
						x = svm_mult,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_svm_mult", ".tif"),
						overwrite = T
					)
					if (write.future == T) {
						writeRaster(
							x = svm_future_mult,
							filename = paste0(
								"./www/",
								projeto,
								"/futuro/fut_",
								i,
								"_svm_mult",
								".tif"
							),
							overwrite = T
						)
					}
				}
				
				if (Mahal == T && condicao_Mahal == TRUE) {
					writeRaster(
						x = ma_mult,
						filename = paste0("./www/", projeto, "/models/pre_", i, "_ma_mult", ".tif"),
						overwrite = T
					)
					if (write.future == T) {
						writeRaster(
							x = ma_future_mult,
							filename = paste0(
								"./www/",
								projeto,
								"/models/fut_",
								i,
								"_ma_mult",
								".tif"
							),
							overwrite = T
						)
					}
				}
			} # Fecha escrita de modelos multiplicados
			
			
			### SALVA ARQUIVOS DE VALIDAÇÃO DE PERFORMANCE
			cat(paste("Saving validation file...", sp, i, '\n'))
			
			sink(
				file = paste0("./www/", projeto, "/models/evaluate_", sp, ".txt"),
				split = T,
				append = T
			)
			if (Bioclim == T) {
				print(
					paste(
						sp,
						spname,
						i,
						"BioClim",
						round(ebc@auc, 3),
						round(bcTSS, 3),
						round(tbc, 3),
						round(threshold(ebc)$kappa, 3),
						round(threshold(ebc)$equal_sens_spec, 3),
						round(threshold(ebc)$no_omission, 3),
						round(threshold(ebc)$prevalence, 3),
						round(threshold(ebc)$sensitivity, 3),
						ebc@np,
						ebc@na,
						round(ebc@cor, 3),
						sep = ","
					)
				)
			}
			if (Domain == T) {
				print(
					paste(
						sp,
						spname,
						i,
						"Domain",
						round(edo@auc, 3),
						round(doTSS, 3),
						round(tdo, 3),
						round(threshold(edo)$kappa, 3),
						round(threshold(edo)$equal_sens_spec, 3),
						round(threshold(edo)$no_omission, 3),
						round(threshold(edo)$prevalence, 3),
						round(threshold(edo)$sensitivity, 3),
						edo@np,
						edo@na,
						round(edo@cor, 3),
						sep = ","
					)
				)
			}
			if (maxent == T) {
				print(
					paste(
						sp,
						spname,
						i,
						"maxent",
						round(emx@auc, 3),
						round(mxTSS, 3),
						round(tmx, 3),
						round(threshold(emx)$kappa, 3),
						round(threshold(emx)$equal_sens_spec, 3),
						round(threshold(emx)$no_omission, 3),
						round(threshold(emx)$prevalence, 3),
						round(threshold(emx)$sensitivity, 3),
						emx@np,
						emx@na,
						round(emx@cor, 3),
						sep = ","
					)
				)
			}
			if (GLM == T) {
				print(
					paste(
						sp,
						spname,
						i,
						"GLM",
						round(eglm@auc, 3),
						round(glmTSS, 3),
						round(tglm, 3),
						round(threshold(eglm)$kappa, 3),
						round(threshold(eglm)$equal_sens_spec, 3),
						round(threshold(eglm)$no_omission, 3),
						round(threshold(eglm)$prevalence, 3),
						round(threshold(eglm)$sensitivity, 3),
						eglm@np,
						eglm@na,
						round(eglm@cor, 3),
						sep = ","
					)
				)
			}
			if (RF == T) {
				print(
					paste(
						sp,
						spname,
						i,
						"RF",
						round(erf1@auc, 3),
						round(rfTSS1, 3),
						round(trf1, 3),
						round(threshold(erf1)$kappa, 3),
						round(threshold(erf1)$equal_sens_spec, 3),
						round(threshold(erf1)$no_omission, 3),
						round(threshold(erf1)$prevalence, 3),
						round(threshold(erf1)$sensitivity, 3),
						erf1@np,
						erf1@na,
						round(erf1@cor, 3),
						sep = ","
					)
				)
			}
			if (SVM == T) {
				print(
					paste(
						sp,
						spname,
						i,
						"SVM",
						round(esvm@auc, 3),
						round(svmTSS, 3),
						round(tsvm, 3),
						round(threshold(esvm)$kappa, 3),
						round(threshold(esvm)$equal_sens_spec, 3),
						round(threshold(esvm)$no_omission, 3),
						round(threshold(esvm)$prevalence, 3),
						round(threshold(esvm)$sensitivity, 3),
						esvm@np,
						esvm@na,
						round(esvm@cor, 3),
						sep = ","
					)
				)
			}
			if (Mahal == T && condicao_Mahal == TRUE) {
				print(
					paste(
						sp,
						sp,
						i,
						"Mahal",
						round(ema@auc, 3),
						round(maTSS, 3),
						round(tma, 3),
						round(threshold(ema)$kappa, 3),
						round(threshold(ema)$equal_sens_spec, 3),
						round(threshold(ema)$no_omission, 3),
						round(threshold(ema)$prevalence, 3),
						round(threshold(ema)$sensitivity, 3),
						ema@np,
						ema@na,
						round(ema@cor, 3),
						sep = ","
					)
				)
			}
			sink()
			
			sink(
				file = paste0("./www/", projeto, "/models/evaluate_ALL_models.txt"),
				split = T,
				append = T
			)
			if (Bioclim == T) {
				print(
					paste(
						sp,
						spname,
						i,
						"BioClim",
						round(ebc@auc, 3),
						round(bcTSS, 3),
						round(tbc, 3),
						round(threshold(ebc)$kappa, 3),
						round(threshold(ebc)$equal_sens_spec, 3),
						round(threshold(ebc)$no_omission, 3),
						round(threshold(ebc)$prevalence, 3),
						round(threshold(ebc)$sensitivity, 3),
						ebc@np,
						ebc@na,
						round(ebc@cor, 3),
						sep = ","
					)
				)
			}
			if (Domain == T) {
				print(
					paste(
						sp,
						spname,
						i,
						"Domain",
						round(edo@auc, 3),
						round(doTSS, 3),
						round(tdo, 3),
						round(threshold(edo)$kappa, 3),
						round(threshold(edo)$equal_sens_spec, 3),
						round(threshold(edo)$no_omission, 3),
						round(threshold(edo)$prevalence, 3),
						round(threshold(edo)$sensitivity, 3),
						edo@np,
						edo@na,
						round(edo@cor, 3),
						sep = ","
					)
				)
			}
			if (maxent == T) {
				print(
					paste(
						sp,
						spname,
						i,
						"maxent",
						round(emx@auc, 3),
						round(mxTSS, 3),
						round(tmx, 3),
						round(threshold(emx)$kappa, 3),
						round(threshold(emx)$equal_sens_spec, 3),
						round(threshold(emx)$no_omission, 3),
						round(threshold(emx)$prevalence, 3),
						round(threshold(emx)$sensitivity, 3),
						emx@np,
						emx@na,
						round(emx@cor, 3),
						sep = ","
					)
				)
			}
			if (GLM == T) {
				print(
					paste(
						sp,
						spname,
						i,
						"GLM",
						round(eglm@auc, 3),
						round(glmTSS, 3),
						round(tglm, 3),
						round(threshold(eglm)$kappa, 3),
						round(threshold(eglm)$equal_sens_spec, 3),
						round(threshold(eglm)$no_omission, 3),
						round(threshold(eglm)$prevalence, 3),
						round(threshold(eglm)$sensitivity, 3),
						eglm@np,
						eglm@na,
						round(eglm@cor, 3),
						sep = ","
					)
				)
			}
			if (RF == T) {
				print(
					paste(
						sp,
						spname,
						i,
						"RF",
						round(erf1@auc, 3),
						round(rfTSS1, 3),
						round(trf1, 3),
						round(threshold(erf1)$kappa, 3),
						round(threshold(erf1)$equal_sens_spec, 3),
						round(threshold(erf1)$no_omission, 3),
						round(threshold(erf1)$prevalence, 3),
						round(threshold(erf1)$sensitivity, 3),
						erf1@np,
						erf1@na,
						round(erf1@cor, 3),
						sep = ","
					)
				)
			}
			if (SVM == T) {
				print(
					paste(
						sp,
						spname,
						i,
						"SVM",
						round(esvm@auc, 3),
						round(svmTSS, 3),
						round(tsvm, 3),
						round(threshold(esvm)$kappa, 3),
						round(threshold(esvm)$equal_sens_spec, 3),
						round(threshold(esvm)$no_omission, 3),
						round(threshold(esvm)$prevalence, 3),
						round(threshold(esvm)$sensitivity, 3),
						esvm@np,
						esvm@na,
						round(esvm@cor, 3),
						sep = ","
					)
				)
			}
			if (Mahal == T && condicao_Mahal == TRUE) {
				print(
					paste(
						sp,
						spname,
						i,
						"Mahal",
						round(ema@auc, 3),
						round(maTSS, 3),
						round(tma, 3),
						round(threshold(ema)$kappa, 3),
						round(threshold(ema)$equal_sens_spec, 3),
						round(threshold(ema)$no_omission, 3),
						round(threshold(ema)$prevalence, 3),
						round(threshold(ema)$sensitivity, 3),
						ema@np,
						ema@na,
						round(ema@cor, 3),
						sep = ","
					)
				)
			}
			
			sink()
			
			stats <-
				read.delim(
					file = paste0("./www/", projeto, "/models/evaluate_ALL_models.txt"),
					header = F,
					sep = ",",
					quote = "",
					col.names = c(
						"id",
						"sp",
						"part",
						"algorithm",
						"AUC",
						"TSS",
						"TSSth",
						"Kappa",
						"Equal_sens_spec",
						"No_omission",
						"Prevalence",
						"Sensitivity",
						"np",
						"na",
						"Cor"
					)
				)
			stats$Sensitivity <-
				as.numeric(sub(pattern = "\"", "", stats$Sensitivity))
			stats20 <- stats[order(stats$sp, stats$algorithm, stats$part), -1]
			
			write.table(stats20, paste0("./www/", projeto, "/models/statsALL.txt"))
			
			output$dbgridresultado <- renderDataTable({
				stats20
			}, options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
			
		} # Fecha o for loop
		
		
		output$dbgridresultado <- renderDataTable({
			cat(c(date(), "Exhibit stats20 results", '\n', '\n'))
			stats20
		}, options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
		
		# The sinked files are re-read and tranformed into a proper data frame...
		cat(c(date(),"====FIM====",'\n','\n'))
		
		conta_alg = 0;
		algoritmos = ''
		
		if (input$GLM==TRUE)
		{
			conta_alg = conta_alg + 1
			algoritmos <- paste(algoritmos,'GLM')
			glm_arquivos <- list.files(paste0("./www/",projeto,"/models/"),full.names=T,pattern=paste0("glm_con.tif"))
			glm_raster<-stack(glm_arquivos)
			ensemble.glm<-mean(glm_raster,glm_raster)
			writeRaster(ensemble.glm,filename=paste0("www/",projeto,"/final/","glm_ensemble.tif"), format='GTiff', overwrite=T)
			#plot( ensemble.glm, main=paste("(GLM - Ensemble)"))
			png(filename=paste0("./www/",projeto,"/jpg/glm_ensemble",".jpg"))
			plot(ensemble.glm,main=paste("GLM - Ensemble "))
			points(especie, bg='red', cex=1,pch=21)
			dev.off()
			
		}
		
		if (input$RF==TRUE)
		{
			conta_alg = conta_alg + 1
			algoritmos <- paste(algoritmos,'RF')
			rf_arquivos <- list.files(paste0("./www/",projeto,"/models/"),full.names=T,pattern=paste0("rf_con.tif"))
			rf_raster<-stack(rf_arquivos)
			ensemble.rf<-mean(rf_raster,rf_raster)
			writeRaster(ensemble.rf,filename=paste0("www/",projeto,"/final/","rf_ensemble.tif"), format='GTiff', overwrite=T)
			#plot( ensemble.rf, main=paste("(RF - Ensemble)"))
			png(filename=paste0("./www/",projeto,"/jpg/rf_ensemble",".jpg"))
			plot(ensemble.rf,main=paste("RF - Ensemble"))
			points(especie, bg='red', cex=1,pch=21)
			dev.off()
		}
		
		if (input$BIOCLIM==TRUE)
		{
			conta_alg = conta_alg + 1
			algoritmos <- paste(algoritmos,'Bioclim')
			bioclim_arquivos <- list.files(paste0("./www/",projeto,"/models/"),full.names=T,pattern=paste0("bc_con.tif"))
			bc_raster<-stack(bioclim_arquivos)
			ensemble.bc<-mean(bc_raster,bc_raster)
			writeRaster(ensemble.bc,filename=paste0("www/",projeto,"/final/","bc_ensemble.tif"), format='GTiff', overwrite=T)
			#plot( ensemble.bc, main=paste("BIOCLIM - Ensemble"))
			png(filename=paste0("./www/",projeto,"/jpg/bc_ensemble",".jpg"))
			plot(ensemble.bc,main=paste("BIOCLIM - Ensemble"))
			points(especie, bg='red', cex=1,pch=21)
			dev.off()
		}
		
		if (input$DOMAIN==TRUE)
		{
			conta_alg = conta_alg + 1
			algoritmos <- paste(algoritmos,'Domain')
			domain_arquivos <- list.files(paste0("./www/",projeto,"/models/"),full.names=T,pattern=paste0("do_con.tif"))
			do_raster<-stack(domain_arquivos)
			ensemble.do<-mean(do_raster,do_raster)
			writeRaster(ensemble.do,filename=paste0("www/",projeto,"/final/","do_ensemble.tif"), format='GTiff', overwrite=T)
			png(filename=paste0("./www/",projeto,"/jpg/do_ensemble",".jpg"))
			plot(ensemble.do,main=paste("Domain - Ensemble"))
			points(especie, bg='red', cex=1,pch=21)
			dev.off()
		}
		
		if (input$MAHALANOBIS==TRUE)
		{
			conta_alg = conta_alg + 1
			algoritmos <- paste(algoritmos,'Mahalanobis')
			maha_arquivos <- list.files(paste0("./www/",projeto,"/models/"),full.names=T,pattern=paste0("ma_con.tif"))
			ma_raster<-stack(maha_arquivos)
			ensemble.ma<-mean(ma_raster,ma_raster)
			writeRaster(ensemble.ma,filename=paste0("www/",projeto,"/final/","ma_ensemble.tif"), format='GTiff', overwrite=T)
			#plot( ensemble.ma, main=paste("(MAHALANOBIS - Ensemble)"))
			png(filename=paste0("./www/",projeto,"/jpg/ma_ensemble",".jpg"))
			plot(ensemble.ma,main=paste("MAHALANOBIS - Ensemble"))
			points(especie, bg='red', cex=1,pch=21)
			dev.off()
		}
		
		if (input$SVM==TRUE)
		{
			conta_alg = conta_alg + 1
			algoritmos <- paste(algoritmos,'SVM')
			svm_arquivos <- list.files(paste0("./www/",projeto,"/models/"),full.names=T,pattern=paste0("svm_con.tif"))
			svm_raster<-stack(svm_arquivos)
			ensemble.svm<-mean(svm_raster,svm_raster)
			writeRaster(ensemble.svm,filename=paste0("www/",projeto,"/final/","svm_ensemble.tif"), format='GTiff', overwrite=T)
			#plot( ensemble.svm, main=paste("(SVM - Ensemble)"))
			png(filename=paste0("./www/",projeto,"/jpg/svm_ensemble",".jpg"))
			plot(ensemble.svm,main=paste("SVM - Ensemble"))
			points(especie, bg='red', cex=1,pch=21)
			dev.off()
		}
		
		if (input$MAXENT==TRUE)
		{
			conta_alg = conta_alg + 1
			algoritmos <- paste(algoritmos,'Maxent')
			mx_arquivos <- list.files(paste0("./www/",projeto,"/models/"),full.names=T,pattern=paste0("mx_con.tif"))
			mx_raster<-stack(mx_arquivos)
			ensemble.mx<-mean(mx_raster,mx_raster)
			writeRaster(ensemble.mx,filename=paste0("www/",projeto,"/final/","mx_ensemble.tif"), format='GTiff', overwrite=T)
			#plot( ensemble.mx, main=paste("(MAXENT - Ensemble)"))
			png(filename=paste0("./www/",projeto,"/jpg/mx_ensemble",".jpg"))
			plot(ensemble.mx,main=paste("MAXENT - Ensemble"))
			points(especie, bg='red', cex=1,pch=21)
			dev.off()
		}
		
		ensemble_arquivos <- list.files(paste0("./www/",projeto,"/final/"),full.names=T,pattern=paste0("ensemble.tif"))
		if (conta_alg>1)
		{
			ensemble_raster<-stack(ensemble_arquivos)
			ensemble.geral<-mean(ensemble_raster,ensemble_raster)
			writeRaster(ensemble.geral,filename=paste0("www/",projeto,"/final/","ensemble_geral.tif"), format='GTiff', overwrite=T)
			png(filename=paste0("./www/",projeto,"/jpg/ensemble_geral",".jpg"))
			plot(ensemble.geral,main=paste("Ensemble ",algoritmos))
			points(especie, bg='red', cex=1,pch=21)
			dev.off()
		}
		
		if (future.model == T)
		{
			## ENSEMBLE FUTURO
			ensemble_futuro_arquivos <- list.files(paste0("./www/",projeto,"/futuro/"),full.names=T,pattern=paste0("con"))
			ensemble_futuro_raster<-stack(ensemble_futuro_arquivos)
			ensemble_futuro.geral<-mean(ensemble_futuro_raster,ensemble_futuro_raster)
			writeRaster(ensemble_futuro.geral,filename=paste0("www/",projeto,"/final/","ensemble_futuro_geral.tif"), format='GTiff', overwrite=T)
			plot(ensemble_futuro.geral, main=paste("Ensemble Futuro",''))
			png(filename=paste0("./www/",projeto,"/jpg/ensemble_futuro",".jpg"))
			plot(ensemble_futuro.geral,main=paste("Ensemble Futuro ",''))
			dev.off()
			## FIM ENSEMBLE FUTURO
		}
		
		if (write.projecao==T)
		{
			ensemble_arquivos_projecao <- list.files(paste0("./www/",projeto,"/proj/"),full.names=T,pattern=paste0("proj.tif"))
			ensemble_raster_projecao<-stack(ensemble_arquivos_projecao)
			ensemble.projecao<-mean(ensemble_raster_projecao,ensemble_raster_projecao)
			writeRaster(ensemble.projecao,filename=paste0("www/",projeto,"/final/","proj_ensemble.tif"), format='GTiff', overwrite=T)
			plot(ensemble.projecao, main=paste("Ensemble Projeção"))
			
			png(filename=paste0("./www/",projeto,"/jpg/ensemble_projecao",".jpg"))
			plot(ensemble.projecao,main=paste("Ensemble Projeção"))
			dev.off()
		}
		
	})# ISOLATE
	
	library("data.table")
	cat(paste("Reading the evaluation files","\n"))
	evall3<- list.files(path = paste0("./www/",projeto,"/models","/"),pattern=paste0("statsALL.txt"),full.names = T)
	lista3<-list()
	
	for (i in 1:length(evall3)) {
		lista3[[i]] <- read.table(file = evall3[i],
															header = T,
															row.names = 1)
	}
	stats3<-rbindlist(lista3)
	stats3<-as.data.frame(stats3)
	
	# Extracts only for the selected algorithm
	algoritmos <- unique(stats3$algorithm)
	
	## Algoritmos
	for (algo in algoritmos){
		stats2 <- stats3[stats3$algorithm==algo,]
		if (algo=="BioClim")
		{
			algo<-'bc'
		}
		if (algo=="GLM")
		{
			algo<-'glm'
		}
		if (algo=="SVM")
		{
			algo<-'svm'
		}
		if (algo=="Mahal")
		{
			algo<-'ma'
		}
		if (algo=="Maxent")
		{
			algo<-'mx'
		}
		if (algo=="RF")
		{
			algo<-'rf'
		}
		if (algo=="Domain")
		{
			algo<-'do'
		}
		
		#How many partitions were there
		part <- nrow(stats2)
		cat(paste("Reading models from .tif files","\n"))
		modelos <- list.files(path = paste0('./www/',projeto,'/models',"/"),full.names=T,pattern=paste0(algo,"_con"))
		mod<-stack(modelos)#(0)
		names(mod)<-paste0("Partition",1:part)
		
		#Binary by TSSth and Cut
		bin <- mod>stats2[,names(stats2)=="Equal_sens_spec"] #stack
		cut <- bin * mod #stack
		
		sel.index<- which(stats2[,"TSS"]>=TSS.value)
		mod.sel<- mod[[sel.index]]
		
		if (length(sel.index)==0) cat(paste("No partition was selected for","\n"))
		
		if (length(sel.index)>0){
			mod.sel <- mod[[sel.index]] #(1)
			bin.sel <-mod.sel > stats2[, names(stats2) == "Equal_sens_spec"][sel.index] #(5)
			cut.sel <- bin.sel * mod.sel#(8)
			th.mean <-
				mean(stats2[, names(stats2) == "Equal_sens_spec"][sel.index])
		}
		
		#en caso de que sea solo uno varios modelos son el mismo
		if (length(sel.index)==1){
			cat(paste(length(sel.index), "partitions was selected for",sp))
			
			final.sel.cont<-mod.sel#(1)(2)
			final.sel.bin<-bin.sel#(5)(3)(7) (8)
			final.sel.cut<-cut.sel#(4)(6)(9)(10)
			
			final <- stack(mod.sel,bin.sel,cut.sel,bin.sel,bin.sel,cut.sel,cut.sel)
			names(final) <- c("2_Final_cont_mean_","3_Final_bin_mean_","4_Final_cut_mean_","7_Final_mean_bin_","8_Final_inter_bin_","9_Mean_cut_sel_","10_inter_cut_sel_")
			
		}
		
		#en caso de que sean aplica el mapa
		if (length(sel.index) > 1) {
			cat(paste(length(sel.index), "partitions were selected for"))
			
			final.cont.mean <- mean(mod.sel)#(2)
			final.bin.mean <- (final.cont.mean > th.mean)#(3)
			final.cut.mean <- final.bin.mean * final.cont.mean #(4)
			
			final.sel.bin <- mean(bin.sel)#(7)
			final.inter <- prod(bin.sel)#(8)
			
			mean.cut.sel <- mean(cut.sel)#(9)
			inter.cut.sel <- prod(cut.sel)#(10)
			
			final <-
				stack(
					final.cont.mean,
					final.bin.mean,
					final.cut.mean,
					final.sel.bin,
					final.inter,
					mean.cut.sel,
					inter.cut.sel
				)
			names(final) <-
				c(
					"2_Final.cont.mean_",
					"3_Final.bin.mean_",
					"4_Final.cut.mean_",
					"7_Final.mean.bin_",
					"8_Final.inter.bin_",
					"9_Mean.cut.sel_",
					"10_inter.cut.sel_"
				)
			writeRaster(
				x = final.cont.mean,
				filename = paste0("./www/", projeto, "/final", "/2_Final_cont_mean_", algo),
				overwrite = T,
				format = "GTiff"
			)
			writeRaster(
				x = final.bin.mean,
				filename = paste0("./www/", projeto, "/final", "/3_Final_bin_mean_", algo),
				overwrite = T,
				format = "GTiff"
			)
			writeRaster(
				x = final.cut.mean,
				filename = paste0("./www/", projeto, "/final", "/4_Final_cut_mean_", algo),
				overwrite = T,
				format = "GTiff"
			)
			writeRaster(
				x = final.sel.bin,
				filename = paste0("./www/", projeto, "/final", "/7_Final_mean_bin_", algo),
				overwrite = T,
				format = "GTiff"
			)
			writeRaster(
				x = final.inter,
				filename = paste0("./www/", projeto, "/final", "/8_Final_inter_bin_", algo),
				overwrite = T,
				format = "GTiff"
			)
			writeRaster(
				x = mean.cut.sel,
				filename = paste0("./www/", projeto, "/final", "/9_Mean_cut_sel_", algo),
				overwrite = T,
				format = "GTiff"
			)
			writeRaster(
				x = inter.cut.sel,
				filename = paste0("./www/", projeto, "/final", "/10_inter_cut_sel_", algo),
				overwrite = T,
				format = "GTiff"
			)
		}
		
		#Escribe mean binary de los seleccionados
		if(exists("final")) {
			for (i in 1:dim(final)[[3]]) {
				png(filename = paste0(
					'./www/',
					projeto,
					'/final',
					"/",
					names(final)[i],
					algo,
					".png"
				))
				plot(final[[i]], main = paste0(names(final)[i], algo))
				dev.off()
			}
		}
		
	} # fecha Algoritmos
} # Fecha a função dismo.mod