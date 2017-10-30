############################
## MODEL-R                ##
## RAFAEL OLIVEIRA LIMA   ##
## ANDREA SÁNCHEZ TAPIA   ##
## FELIPE SODRÉ BARROS    ##
## 19 DE SETEMBRO DE 2017 ##
############################

# Thanks to Steven Worthington for function ipak https://gist.github.com/stevenworthington/3178163 (HT Karlo Guidoni Martins)

ipak <- function(pkg) {
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

ipak(c("shinydashboard",
       "leaflet",
       "R.utils",
       "raster",
       "rjson",
       "maps",
       "rgdal",
       "raster",
       "dismo",
       "rgbif",
       "XML",
       "randomForest",
       "kernlab",
       "rJava",
       "data.table"))

ARQUIVO_SAIDA <- ''
# server.R
rm(list = ls())
rm(list = setdiff(ls(), lsf.str()))

home <-'/'
## PARA A HOSPEDAGEM NO JARDIM ALTERAR PARA '/modelagem/'

t <- 7
ext1 <- -90
ext2 <- -33
ext3 <- -32
ext4 <- 23

ext12 <- -90
ext22 <- -33
ext32 <- -32
ext42 <- 23

arquivo <- list()
arquivo2 <- list()

ETAPA <- 0

spname <<- ''

# MaxEnt.jar#### baixa e descompacta o maxent java
jar <- paste0(system.file(package = "dismo"), "/java/maxent.jar")
if (file.exists(jar) != T) {
  url = "http://biodiversityinformatics.amnh.org/open_source/maxent/maxent.php?op=download"
  download.file(url, dest = "maxent.zip", mode = "wb")
  unzip("maxent.zip", files = "maxent.jar", exdir = system.file("java", package = "dismo"))
  unlink("maxent.zip")} 

#função para gerar os valores de correlação no gráfico da função pairs

panel.cor <- function(x, y, digits = 2, prefix = "", ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  text(0.5, 0.5, txt, cex = 1.5)}

#função para gerar os histogramas no gráfico da função pairs

panel.hist <- function(x, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "gray", ...)}


limparResultadosAnteriores<-function()({

  ## LIMPANDO OS RESULTADOS ANTERIORES

  lista <- list.files(paste0("www/",projeto,"/models/",full.names=T,pattern=paste0(".")))
  if (length(lista>0))
  {
    file.remove(paste0("www/",projeto,"/models/",lista))
  }

  lista <- list.files(paste0("www/",projeto,"/final/",full.names=T,pattern=paste0(".")))
  if (length(lista>0))
  {
    file.remove(paste0("www/",projeto,"/final/",lista))
  }

  lista <- list.files(paste0("www/",projeto,"/proj/",full.names=T,pattern=paste0(".")))
  if (length(lista>0))
  {
    file.remove(paste0("www/",projeto,"/proj/",lista))
  }

  lista <- list.files(paste0("www/",projeto,"/futuro/",full.names=T,pattern=paste0(".")))
  if (length(lista>0))
  {
    file.remove(paste0("www/",projeto,"/futuro/",lista))
  }

  lista <- list.files(paste0("www/",projeto,"/jpg/",full.names=T,pattern=paste0(".jpg")))
  if (length(lista>0))
  {
    file.remove(paste0("www/",projeto,"/jpg/",lista))
  }

})

getOcorrencia <-
  function(pTaxon){
    library("rjson")
    pTaxon = gsub(' ', '_', pTaxon)
    json_file <- paste("http://aplicacoes.jbrj.gov.br/jabot/v2/ws/server.php?coordenada=S&taxon=",pTaxon,sep="")
    json_data <- fromJSON(file=json_file,method = "C")

    final_data <- do.call(rbind, json_data)
    write.csv(final_data, "final_data.csv")
    y2 <- final_data[,c("taxoncompleto", "longitude", "latitude")]

    y2 <- cbind(
      as.numeric(y2[ ,2]),
      as.numeric(y2[ ,3])
    )

    colnames(y2) <- c("Longitude", "Latitude")
    y2 <- data.frame(y2)
    return (y2)
  }

options(shiny.maxRequestSize = 100*1024^2)

dirColors <-c("1"="#595490", "2"="#527525", "3"="#A93F35", "4"="#BA48AA")




function(input, output, session) {
  especie <<- NULL


  #rm(especie)
  library(maps)
  library(rgdal)
  library(raster)
  library(dismo)
  library(rgbif)
  library('XML')
  library('leaflet')

  dismo.mod <- function(sp,
                        occs=spp.filt,
                        var=expl,
                        var2=expl2,
                        maxent=F,
                        Bioclim=F,
                        GLM=F,
                        RF=F,
                        SVM=F,
                        Mahal=F,
                        Domain=F,
                        SVM2=F,
                        part=3,
                        numpontos = 500,
                        seed=123,
                        write.cont=T, # escreve modelos cont?nuos
                        bin=T, # faz modelos bin?rios
                        write.bin=T, # escreve modelos bin?rios
                        mult=T, # faz modelos multiplicados(bin*con)
                        write.mult=T, # escreve modelos multiplicados
                        TSS.value = 0.2,
                        future.model=F, # faz modelos futuros
                        future.raster=newdata, # vari?veis futuras
                        write.future=F, # escreve modelos futuros
                        write.projecao=F)


{
    ## Carregando bibliotecas
    library(dismo)
    library(randomForest)
    library(kernlab)
    library(XML)
    library(raster)
    library(rgdal)
    ##library(rJava)
    library(maps)

    cat(paste0("library(dismo)"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat(paste0("library(randomForest)"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat(paste0("library(kernlab)"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat(paste0("library(XML)"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat(paste0("library(raster)"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat(paste0("library(rgdal)"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat(paste0("library(maps)"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)


## DELETE OS ARQUIVOS ANTIGOS ANTES DE GERAR UM NOVO

    unlink(paste0("www/",projeto,'/models/evaluate_.txt'), recursive=TRUE)
    unlink(paste0("www/",projeto,'/models/statsALL.txt'), recursive=TRUE)
    unlink(paste0("www/",projeto,'/models/evaluate_ALL_models.txt'), recursive=TRUE)

    isolate({

    print(date())

    cat(paste("Modeling",sp,"...",'\n'))
    coord <- especie

    cat(paste0("coord<-especie"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    n <- nrow(coord)

    cat(paste0("n<-nrow(coord)"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    ## Extraindo os valores das vari?veis onde h? pontos de registros

    presvals<- raster::extract(var,coord)

    cat(paste0("presvals<-raster::extract(var,coord)"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    ## Seeting seed para sempre criar os mesmos pontos aleat?rios
    set.seed(seed)

    ## Gerando as pseudoaus?ncias aleatoriamente

    backgr <- randomPoints(var, numpontos)

    cat(paste0("backgr<-randomPoints(var,",numpontos,")"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    ## Determina os nomes da colunas de coordenadas para os pontos de background
    colnames(backgr) = c('Longitude', 'Latitude')

    cat(paste0("colnames(backgr)=c('Longitude','Latitude')"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    ## Extraindo os valores das vari?veis onde h? pseudoaus?ncias
    absvals <- raster::extract(var, backgr)

    cat(paste0("absvals <- raster::extract(var,backgr)"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    ## Cria um vetor contendo algarismo "1" e "0" correspondendo ao n?mero de registros presen?as e aus?ncias respectivamente.
    pre_abs <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))

    cat(paste0("pre_abs<-c(rep(1,nrow(presvals)),rep(0,nrow(absvals)))"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    ## N?mero de parti??es
    if (n<10) part<-n else part <- part

    cat(paste0("if (n<10) part<-n else part <- part"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    ## Setting seed para distribuir as presen?as sempre para as mesmas parti??es
    set.seed(seed)

    ## Separando as presen?as e pseudoaus?ncias nos grupos das parti??es
    group_pre <- kfold(coord,part)
    set.seed(seed)
    group_abs <- kfold(backgr,part)

    cat(paste0("group_pre <- kfold(coord,part)"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat(paste0("group_abs <- kfold(backgr,part)"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    # Cosntruindo o data.frame com todos os dados
    append_1 <- append(group_pre,group_abs)
    cbind_1 <- cbind(coord,presvals)
    cbind_2 <- cbind(backgr,absvals)
    rbind_1 <- rbind(cbind_1,cbind_2)
    sdmdata <- data.frame(cbind(append_1,pre_abs,rbind_1))
    sdmdata2 <- sdmdata[-1]
    sdmdata2 <- sdmdata2[-1]
    sdmdata2 <- sdmdata2[-1]
    sdmdata2 <- sdmdata2[-1]

    cat(paste0("append_1 <- append(group_pre,group_abs)"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat(paste0("cbind_1 <- cbind(coord,presvals)"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat(paste0("cbind_2 <- cbind(backgr,absvals)"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat(paste0("rbind_1 <- rbind(cbind_1,cbind_2)"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat(paste0("sdmdata <- data.frame(cbind(append_1,pre_abs,rbind_1))"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat(paste0("sdmdata2 <- sdmdata[-1]"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat(paste0("sdmdata2 <- sdmdata2[-1]"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat(paste0("sdmdata2 <- sdmdata2[-1]"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat(paste0("sdmdata2 <- sdmdata2[-1]"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    colnames(sdmdata)[1] <- "group"

    cat(paste0("colnames(sdmdata)[1] <- \"group\""),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    for (i in unique(group_pre)){

      if (i==1)
      {
      cat(paste0("for (i in unique(group_pre)){"),file=ARQUIVO_SAIDA,append=TRUE)
      cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
      cat(paste0("sdmdata_train <- subset(sdmdata,group!=i)"),file=ARQUIVO_SAIDA,append=TRUE)
      cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
      cat(paste0("sdmdata_teste <- subset(sdmdata,group ==i)"),file=ARQUIVO_SAIDA,append=TRUE)
      cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
      cat(paste0("envtrain <- subset(sdmdata_train,select= c(-group,-Longitude,-Latitude))"),file=ARQUIVO_SAIDA,append=TRUE)
      cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
      cat(paste0("envtest_pre <- subset(sdmdata_teste,pre_abs==1,select= c(-group,-Latitude,-Latitude,-pre_abs))"),file=ARQUIVO_SAIDA,append=TRUE)
      cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
      cat(paste0("envtest_abs <- subset(sdmdata_teste,pre_abs==0,select= c(-group,-Latitude,-Latitude,-pre_abs))"),file=ARQUIVO_SAIDA,append=TRUE)
      cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
      cat(paste0("coord_pres_train <- subset(sdmdata_train,pre_abs==1,select= c(Longitude,Latitude))"),file=ARQUIVO_SAIDA,append=TRUE)
      cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
      cat(paste0("coord_pres_teste <- subset(sdmdata_teste,pre_abs==1,select= c(Longitude,Latitude))"),file=ARQUIVO_SAIDA,append=TRUE)
      cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
      cat(paste0("coord_abs_train <- subset(sdmdata_train,pre_abs==0,select= c(Longitude,Latitude))"),file=ARQUIVO_SAIDA,append=TRUE)
      cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
      cat(paste0("coord_abs_teste <- subset(sdmdata_teste,pre_abs==0,select= c(Longitude,Latitude))"),file=ARQUIVO_SAIDA,append=TRUE)
      cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
      }

      progress$set(message = paste("Processando a modelagem",i), value = 0)
      # Separar o sdmdata entre teste e treino
      sdmdata_train <- subset(sdmdata,group!=i)
      sdmdata_teste <- subset(sdmdata,group ==i)

      # Preparando sdmdata train para fazer o modelo
      envtrain <- subset(sdmdata_train,select= c(-group,-Longitude,-Latitude))

      # Preparando sdmdata test para avaliar modelos que n?o usam s? presen?a
      envtest_pre <- subset(sdmdata_teste,pre_abs==1,select= c(-group,-Latitude,-Latitude,-pre_abs))
      envtest_abs <- subset(sdmdata_teste,pre_abs==0,select= c(-group,-Latitude,-Latitude,-pre_abs))

      ### Separando os dados (apenas coordenadas) para fazer BioClim, Maxent e Mahalanobis que usam s? presen?a
      coord_pres_train <- subset(sdmdata_train,pre_abs==1,select= c(Longitude,Latitude))
      coord_pres_teste <- subset(sdmdata_teste,pre_abs==1,select= c(Longitude,Latitude))
      coord_abs_train <- subset(sdmdata_train,pre_abs==0,select= c(Longitude,Latitude))
      coord_abs_teste <- subset(sdmdata_teste,pre_abs==0,select= c(Longitude,Latitude))

      ### FAZ OS MODELOS
      cat(paste("Modeling...",sp,"Partition",i,'\n'))

      if (Bioclim==T){
        cat(paste("#Bioclim",'\n'))
        # Constr?i o modelo no espa?o ambiental
        bc <- bioclim (var, coord_pres_train)

        if (i==1)
        {
        cat(paste0("#BIOCLIM"),file=ARQUIVO_SAIDA,append=TRUE)
        cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

        cat(paste0(" bc <- bioclim (var, coord_pres_train)"),file=ARQUIVO_SAIDA,append=TRUE)
        cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
        cat(paste0("ebc <<- dismo::evaluate(coord_pres_teste,coord_abs_teste,bc,var)"),file=ARQUIVO_SAIDA,append=TRUE)
        cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
        cat(paste0("bcTSS <- max(ebc@TPR + ebc@TNR)-1"),file=ARQUIVO_SAIDA,append=TRUE)
        cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
        cat(paste0("tbc <- threshold (ebc,'spec_sens')"),file=ARQUIVO_SAIDA,append=TRUE)
        cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
        cat(paste0("bc_cont <- predict (var,bc,progress='text')"),file=ARQUIVO_SAIDA,append=TRUE)
        cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
        cat(paste0("bc_cont_proj <- predict (var2,bc,progress='text')"),file=ARQUIVO_SAIDA,append=TRUE)
        cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
        cat(paste0("bc_bin <- bc_cont>tbc"),file=ARQUIVO_SAIDA,append=TRUE)
        cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
        cat(paste0(" bc_mult <- bc_bin*bc_cont"),file=ARQUIVO_SAIDA,append=TRUE)
        cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
        cat(paste0("bc_mult <- bc_mult/maxValue(bc_mult)"),file=ARQUIVO_SAIDA,append=TRUE)
        cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
        }

        # Validacao da performance
        ebc <<- dismo::evaluate(coord_pres_teste,coord_abs_teste,bc,var)
        # C?lculo do TSS
        bcTSS <- max(ebc@TPR + ebc@TNR)-1
        # Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
        tbc <- threshold (ebc,'spec_sens')
        # Projeta no espa?o geografico o modelo contnuo
        bc_cont <- predict (var,bc,progress='text')
        bc_cont_proj <- predict (var2,bc,progress='text')
        # Transforma em binario o modelo continuo cortando-o pelo limiar tbc
        bc_bin <- bc_cont>tbc
        # Resgata os valores continuos ao multiplicar o modelo binario pelo continuo
        bc_mult <- bc_bin*bc_cont
        # Normaliza o modelo mult
        bc_mult <- bc_mult/maxValue(bc_mult)
        # Faz os modelos futuros
        if (future.model==T){
          # Projeta o modelo nas variiveis futuras
          bc_future <- predict(future.raster,bc,progress='text')
          # Transforma em bin?rio o modelo futuro cont?nuo pelo threshold do modelo presente
          if (bin ==T) {bc_future_bin <- bc_future > tbc}
          # Resgata os valores continuos ao multiplicar o modelo binario pelo continuo
          if (mult==T) {
            bc_future_mult <- bc_future_bin * bc_future
            # Normaliza o modelo mult
            bc_future_mult <- bc_future_mult/maxValue(bc_future_mult)}
        } # Fecha o modelo futuro
      } # Fecha o algoritmo Bioclim


      if (Domain==T){
        cat(paste("Domain",'\n'))

        if (i==1)
        {

          cat(paste0("DOMAIN"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

          cat(paste0("  do <- domain (var, coord_pres_train)"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("edo <<- dismo::evaluate(coord_pres_teste,coord_abs_teste,do,var)"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("doTSS <- max(edo@TPR + edo@TNR)-1"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("tdo <- threshold (edo,'spec_sens')"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0(" do_cont <- predict (var,do,progress='text')"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("do_cont_proj <- predict (var2,do,progress='text')"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("do_bin <- do_cont>tdo"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("do_mult <- do_bin*do_cont"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("do_mult <- do_mult/maxValue(do_mult)"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
        }
        # Constr?i o modelo no espa?o ambiental
        do <- domain (var, coord_pres_train)
        # Valida??o da performance
        edo <<- dismo::evaluate(coord_pres_teste,coord_abs_teste,do,var)
        # C?lculo do TSS
        doTSS <- max(edo@TPR + edo@TNR)-1
        # Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
        tdo <- threshold (edo,'spec_sens')
        # Projeta no espa?o geogr?fico o modelo cont?nuo
        do_cont <- predict (var,do,progress='text')
        do_cont_proj <- predict (var2,do,progress='text')

        # Transforma em bin?rio o modelo cont?nuo cortando-o pelo limiar tbc
        do_bin <- do_cont>tdo
        # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
        do_mult <- do_bin*do_cont
        # Normaliza o modelo mult
        do_mult <- do_mult/maxValue(do_mult)
        # Faz os modelos futuros
        if (future.model==T){
          # Projeta o modelo nas vari?veis futuras
          do_future <- predict(future.raster,do,progress='text')
          # Transforma em bin?rio o modelo futuro cont?nuo pelo threshold do modelo presente
          if (bin ==T) {do_future_bin <- do_future > tdo}
          # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
          if (mult==T) {
            do_future_mult <- do_future_bin * do_future
            # Normaliza o modelo mult
            do_future_mult <- do_future_mult/maxValue(do_future_mult)}
        } # Fecha o modelo futuro
      } # Fecha o algoritmo Bioclim

      if (maxent==T){

        if (i==1)
        {
          cat(paste0("MAXENT"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)


          cat(paste0("mx <- maxent (var, coord_pres_train)"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("emx <- dismo::evaluate(coord_pres_teste,coord_abs_teste,mx,var)"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("mxTSS <- max(emx@TPR + emx@TNR)-1"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("tmx <- threshold (emx,'spec_sens')"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("mx_cont <- predict (var,mx,progress='text')"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("mx_cont_proj <- predict (var2,mx,progress='text')"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("mx_bin <- mx_cont>tmx"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("mx_mult <- mx_bin*mx_cont"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("mx_mult <- mx_mult/maxValue(mx_mult)"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
        }

        cat(paste("Maxent",'\n'))
        # Constr?i o modelo no espa?o ambiental
        mx <- maxent (var, coord_pres_train)
        # Valida??o da performance
        emx <- dismo::evaluate(coord_pres_teste,coord_abs_teste,mx,var)
        # C?lculo do TSS
        mxTSS <- max(emx@TPR + emx@TNR)-1
        # Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
        tmx <- threshold (emx,'spec_sens')
        # Projeta no espa?o geogr?fico o modelo cont?nuo
        mx_cont <- predict (var,mx,progress='text')
        mx_cont_proj <- predict (var2,mx,progress='text')

        # Transforma em bin?rio o modelo cont?nuo cortando-o pelo limiar tbc
        mx_bin <- mx_cont>tmx
        # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
        mx_mult <- mx_bin*mx_cont
        # Normaliza o modelo mult
        mx_mult <- mx_mult/maxValue(mx_mult)
        if (future.model==T){
          # Projeta o modelo nas vari?veis futuras
          mx_future <- predict(future.raster,mx,progress='text')
          # Transforma em bin?rio o modelo futuro cont?nuo pelo threshold do modelo presente
          if (bin ==T) {mx_future_bin <- mx_future > tmx}
          # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
          if (mult==T) {
            mx_future_mult <- mx_future_bin * mx_future
            # Normaliza o modelo mult
            mx_future_mult <- mx_future_mult/maxValue(mx_future_mult)}
        } # Fecha o modelo futuro
      } # Fecha o algoritmo Maxent

      if (GLM==T){
        cat(paste("GLM",'\n'))

        if (i==1)
        {
          cat(paste0("GLM"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

          cat(paste0("mglm <- glm(pre_abs~.,data=envtrain)"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("eglm <- dismo::evaluate(envtest_pre,envtest_abs,mglm)"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("glmTSS <- max(eglm@TPR + eglm@TNR)-1"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("tglm <- threshold (eglm,'spec_sens')"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("glm_cont <- predict (var,mglm,progress='text')"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("glm_cont_proj <- predict (var2,mglm,progress='text')"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("glm_bin <- glm_cont>tglm"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("glm_mult <- glm_bin*glm_cont "),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("glm_mult <- glm_mult/maxValue(glm_mult)"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
        }

        # Constr?i o modelo no espa?o ambiental
        mglm <- glm(pre_abs~.,data=envtrain)
        # Valida??o da performance
        eglm <- dismo::evaluate(envtest_pre,envtest_abs,mglm)
        # C?lculo do TSS
        glmTSS <- max(eglm@TPR + eglm@TNR)-1
        # Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
        tglm <- threshold (eglm,'spec_sens')
        # Projeta no espa?o geogr?fico o modelo cont?nuo
        glm_cont <- predict (var,mglm,progress='text')
        glm_cont_proj <- predict (var2,mglm,progress='text')

        #plot(glm_cont)
        # Transforma em bin?rio o modelo cont?nuo cortando-o pelo limiar tbc
        glm_bin <- glm_cont>tglm
        # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
        glm_mult <- glm_bin*glm_cont
        # Normaliza o modelo mult
        glm_mult <- glm_mult/maxValue(glm_mult)
        if (future.model==T){
          # Projeta o modelo nas vari?veis futuras
          glm_future <- predict(future.raster,mglm,progress='text')
          # Transforma em bin?rio o modelo futuro cont?nuo pelo threshold do modelo presente
          if (bin ==T) {glm_future_bin <- glm_future > tglm}
          # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
          if (mult==T) {
            glm_future_mult <- glm_future_bin * glm_future
            # Normaliza o modelo mult
            glm_future_mult <- glm_future_mult/maxValue(glm_future_mult)}
        } # Fecha o modelo futuro
      } # Fecha o algoritmo GLM

      if (RF==T){
        cat(paste("RF",'\n'))

        if (i==1)
        {
          cat(paste0("RF"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

          cat(paste0("rf1 <- randomForest (pre_abs~.,data=envtrain)"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("erf1 <- dismo::evaluate(envtest_pre,envtest_abs, rf1)"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("rfTSS1 <- max(erf1@TPR + erf1@TNR)-1"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("trf1 <- threshold (erf1,'spec_sens')"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("rf1_cont <- predict (var,rf1,progress='text')"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("rf1_cont_proj <- predict (var2,rf1,progress='text')"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("rf1_bin <- rf1_cont>trf1"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("rf1_mult <- rf1_bin*rf1_cont "),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("rf1_mult <- rf1_mult/maxValue(rf1_mult)"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
        }

        # Constr?i o modelo no espa?o ambiental
        ##rf1 <- randomForest (pre_abs~.,data=envtrain) # porque da mensagem de aviso ao usar rf1(regression)?
        ##envtrain
        ##pre_abs
        rf1 <- randomForest (pre_abs~.,data=envtrain) # porque da mensagem de aviso ao usar rf1(regression)?
        #rf2 <- randomForest (factor(pre_abs) ~ ., data=envtrain) # faz classification e n?o d? mensagem de erro.
        # rf2 tem como output somente modelos bin?rios
        # Valida??o de performance
        erf1 <- dismo::evaluate(envtest_pre,envtest_abs, rf1)
        #erf2 <- dismo::evaluate(envtest_pre,envtest_abs, rf2)
        # C?lculo do TSS
        rfTSS1 <- max(erf1@TPR + erf1@TNR)-1
        #rfTSS2 <- max(erf2@TPR + erf2@TNR)-1
        # Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
        trf1 <- threshold (erf1,'spec_sens')
        #trf2 <- threshold (erf2,'spec_sens') # tbm d? mensagem de erro
        # Projeta no espa?o geogr?fico o modelo cont?nuo
        rf1_cont <- predict (var,rf1,progress='text')
        rf1_cont_proj <- predict (var2,rf1,progress='text')
        #rf_cont2 <- predict (var,rf2,progress='text') # o cont?nuo fica igual ao bin?rio!
        # Transforma em bin?rio o modelo cont?nuo cortando-o pelo limiar tbc
        rf1_bin <- rf1_cont>trf1
        #rf_bin2 <- rf_cont2>trf2
        # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
        rf1_mult <- rf1_bin*rf1_cont
        #rf_mult2 <- rf_bin2*rf_cont2
        # Normaliza o modelo mult
        rf1_mult <- rf1_mult/maxValue(rf1_mult)

        if (future.model==T){
          # Projeta o modelo nas vari?veis futuras
          rf1_future <- predict(future.raster,rf1,progress='text')
          # Transforma em bin?rio o modelo futuro cont?nuo pelo threshold do modelo presente
          if (bin ==T) {rf1_future_bin <- rf1_future > trf1}
          # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
          if (mult==T) {
            rf1_future_mult <- rf1_future_bin * rf1_future
            # Normaliza o modelo mult
            rf1_future_mult <- rf1_future_mult/maxValue(rf1_future_mult)}
        } # Fecha o modelo futuro
      } # Fecha o algoritmo RandomForest

      if (SVM==T){

        if (i==1)
        {
          cat(paste0("SVM"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

          cat(paste0("msvm <- ksvm(pre_abs~.,data=envtrain)"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("esvm <- dismo::evaluate(envtest_pre,envtest_abs,msvm)"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("svmTSS <- max(esvm@TPR + esvm@TNR)-1"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("tsvm <- threshold (esvm,'spec_sens')"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("svm_cont <- predict (var,msvm,progress='text')"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("svm_cont_proj <- predict (var2,msvm,progress='text')"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("svm_bin <- svm_cont>tsvm"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("svm_mult <- svm_bin*svm_cont"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          cat(paste0("svm_mult <- svm_mult/maxValue(svm_mult)"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
        }
        cat(paste("SVM",'\n'))
        # Constr?i o modelo no espa?o ambiental
        msvm <- ksvm(pre_abs~.,data=envtrain)
        # Valida??o da performance
        esvm <- dismo::evaluate(envtest_pre,envtest_abs,msvm)
        # C?lculo do TSS
        svmTSS <- max(esvm@TPR + esvm@TNR)-1
        # Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
        tsvm <- threshold (esvm,'spec_sens')
        # Projeta no espa?o geogr?fico o modelo cont?nuo
        svm_cont <- predict (var,msvm,progress='text')
        svm_cont_proj <- predict (var2,msvm,progress='text')
        # Transforma em bin?rio o modelo cont?nuo cortando-o pelo limiar tbc
        svm_bin <- svm_cont>tsvm
        # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
        svm_mult <- svm_bin*svm_cont
        # Normaliza o modelo mult
        svm_mult <- svm_mult/maxValue(svm_mult)
        if (future.model==T){
          # Projeta o modelo nas vari?veis futuras
          svm_future <- predict(future.raster,msvm,progress='text')
          # Transforma em bin?rio o modelo futuro cont?nuo pelo threshold do modelo presente
          if (bin ==T) {svm_future_bin <- svm_future > tsvm}
          # Resgata os valores cont?nuos ao multiplicar o modelo bin?rio pelo cont?nuo
          if (mult==T) {
            svm_future_mult <- svm_future_bin * svm_future
            # Normaliza o modelo mult
            svm_future_mult <- svm_future_mult/maxValue(svm_future_mult)}
        } # Fecha o modelo futuro
      } # Fecha o algoritmo SVM

      if (Mahal==T){
        cat(paste("Mahal",'\n'))

        # Checa se o n?mero de registros de presen?a ? maior que o n?mero de vari?veis
        condicao_Mahal<- nrow(coord_pres_train)>length(names(var))
        if (condicao_Mahal==TRUE){
          # Construi o modelo no espaco ambiental

          if (i==1)
          {

            cat(paste0("MAHALANOBIS"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

            cat(paste0("ma <- mahal (var, coord_pres_train)"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
            cat(paste0("ema <- dismo::evaluate(coord_pres_teste,coord_abs_teste,ma,var)"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
            cat(paste0("maTSS <- max(ema@TPR + ema@TNR)-1"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
            cat(paste0("tma <- threshold (ema,'spec_sens')"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
            cat(paste0("ma_cont <- predict (var,ma,progress='text')"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
            cat(paste0("ma_cont_proj <- predict (var2,ma,progress='text')"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
            cat(paste0("ma_cont_invert <- ma_cont+(-1*minValue(ma_cont))"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
            cat(paste0("ma_bin <- ma_cont>tma"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
            cat(paste0("ma_mult <- ma_bin * ma_cont_invert"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
            cat(paste0("ma_mult <- ma_mult/maxValue(ma_mult)"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          }
          ma <- mahal (var, coord_pres_train)
          # validacao da performance
          ema <- dismo::evaluate(coord_pres_teste,coord_abs_teste,ma,var)
          # Calculo do TSS
          maTSS <- max(ema@TPR + ema@TNR)-1
          # Extrai o valor do limiar que maximiza a soma da especificidade e sensibilidade
          tma <- threshold (ema,'spec_sens')
          # Projeta no espaco geografico o modelo continuo
          ma_cont <- predict (var,ma,progress='text')
          ma_cont_proj <- predict (var2,ma,progress='text')
          # Invertendo os valores dos pixel, porque o valor 0 corresponde ao maior valor de adequabilidade
          ma_cont_invert <- ma_cont+(-1*minValue(ma_cont))
          # Transforma em binario o modelo continuo cortando-o pelo limiar tma
          ma_bin <- ma_cont>tma
          # Resgata os valores continuos ao multiplicar o modelo binario pelo continuo invertido
          ma_mult <- ma_bin * ma_cont_invert
          # Normaliza o modelo mult
          ma_mult <- ma_mult/maxValue(ma_mult)
          # Faz os modelos futuros
          if (future.model==T){
            # Projeta o modelo nas variaveis futuras
            ma_future <- predict(future.raster,ma,progress='text')
            # Invertendo os valores dos pixel, porque o valor 0 corresponde ao maior valor de adequabilidade
            ma_future_invert <- ma_future+(-1*minValue(ma_future))
            # Transforma em binario o modelo futuro continuo pelo threshold do modelo presente
            if (bin ==T) {ma_future_bin <- ma_future > tma}
            # Resgata os valores continuos ao multiplicar o modelo binario pelo continuo
            if (mult==T) {
              ma_future_mult <- ma_future_bin * ma_future_invert
              # Normaliza o modelo mult
              ma_future_mult <- ma_future_mult/maxValue(ma_future_mult)}
          } # Fecha o modelo futuro
        } # Fecha o algoritmo Mahalanobis
        else {

			}
      }

      ### ESCREVE OS MODELOS

      ## Modelos continuos
      if (write.cont==T){
        cat(paste("Salvando modelos continuos...",sp,i,'\n'))

        if(Bioclim==T){
          writeRaster(x=bc_cont,filename=paste0("./www/",projeto,"/models/pre_",i,"_bc_con",".tif"),overwrite=T)
          png(filename=paste0("./www/",projeto,"/jpg/pre_",i,"_bc_con",".jpg"))
          plot(bc_cont,main=paste("BioClim - ",i))
          if (i==1)
          {
          cat(paste0("plot(bc_cont,main=paste(\"BioClim - \",i))"),file=ARQUIVO_SAIDA,append=TRUE)
          cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          }
          dev.off()

          if (write.projecao==T)
          {
            writeRaster(x=bc_cont_proj,filename=paste0("./www/",projeto,"/proj/pre_",i,"_bc_con_proj",".tif"),overwrite=T)
          }
          if(write.future==T) {
            writeRaster(x=bc_future,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_bc_con",".tif"),overwrite=T)
            png(filename=paste0("./www/",projeto,"/jpg/fut_",i,"_bc_con",".jpg"))
            plot(bc_future,main=paste("BioClim - Fut ",i))

            if (i==1)
            {
              cat(paste0("plot(bc_future,main=paste(\"BioClim - Fut \",i))"),file=ARQUIVO_SAIDA,append=TRUE)
              cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
            }

            dev.off()
          }
        }

        if(Domain==T){
          writeRaster(x=do_cont,filename=paste0("./www/",projeto,"/models/pre_",i,"_do_con",".tif"),overwrite=T)

          png(filename=paste0("./www/",projeto,"/jpg/pre_",i,"_do_con",".jpg"))
          plot(do_cont,main=paste("Domain - ",i))

          if (i==1)
          {
            cat(paste0("plot(do_cont,main=paste(\"Domain - \",i))"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          }

          dev.off()

          if (write.projecao==T)
          {
            writeRaster(x=do_cont_proj,filename=paste0("./www/",projeto,"/proj/pre_",i,"_do_con_proj",".tif"),overwrite=T)
          }
          if(write.future==T) {
            writeRaster(x=do_future,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_do_con",".tif"),overwrite=T)
            png(filename=paste0("./www/",projeto,"/jpg/fut_",i,"_do_con",".jpg"))
            plot(bc_future,main=paste("Domain - Fut ",i))
            dev.off()
          }
        }

          if(maxent==T){
          writeRaster(x=mx_cont,filename=paste0("./www/",projeto,"/models/pre_",i,"_mx_con",".tif"),overwrite=T)
          png(filename=paste0("./www/",projeto,"/jpg/pre_",i,"_mx_con",".jpg"))
          plot(mx_cont,main=paste("Maxent - ",i))

          if (i==1)
          {
            cat(paste0("plot(mx_cont,main=paste(\"Maxent - \",i))"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          }
          dev.off()

          if (write.projecao==T)
          {
            writeRaster(x=mx_cont_proj,filename=paste0("./www/",projeto,"/proj/pre_",i,"_mx_con_proj",".tif"),overwrite=T)
          }
          if(write.future==T) {
            writeRaster(x=mx_future,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_mx_con",".tif"),overwrite=T)
            png(filename=paste0("./www/",projeto,"/jpg/fut_",i,"_mx_con",".jpg"))
            plot(mx_future,main=paste("Maxent - Fut ",i))
            dev.off()
          }
        }

        if(GLM==T){
          writeRaster(x=glm_cont,filename=paste0("./www/",projeto,"/models/pre_",i,"_glm_con",".tif"),overwrite=T)
          png(filename=paste0("./www/",projeto,"/jpg/pre_",i,"_glm_con",".jpg"))
          plot(glm_cont,main=paste("GLM - ",i))

          if (i==1)
          {
            cat(paste0("plot(glm_cont,main=paste(\"GLM - \",i))"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          }
          dev.off()
          if (write.projecao==T)
          {
            writeRaster(x=glm_cont_proj,filename=paste0("./www/",projeto,"/proj/pre_",i,"_glm_con_proj",".tif"),overwrite=T)
          }

          if(write.future==T) {
            writeRaster(x=glm_future,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_glm_con",".tif"),overwrite=T)
            png(filename=paste0("./www/",projeto,"/jpg/fut_",i,"_glm_con",".jpg"))
            plot(glm_future,main=paste("GLM - Fut ",i))
            dev.off()
          }
        }

        if(RF==T){
          writeRaster(x=rf1_cont,filename=paste0("./www/",projeto,"/models/pre_",i,"_rf_con",".tif"),overwrite=T)
          png(filename=paste0("./www/",projeto,"/jpg/pre_",i,"_rf_con",".jpg"))
          plot(rf1_cont,main=paste("RF - ",i))

          if (i==1)
          {
            cat(paste0("plot(rf1_cont,main=paste(\"RF - \",i))"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          }

          dev.off()

          if (write.projecao==T)
          {
            writeRaster(x=rf1_cont_proj,filename=paste0("./www/",projeto,"/proj/pre_",i,"_rf_con_proj",".tif"),overwrite=T)
          }
          if(write.future==T) {
            writeRaster(x=rf1_future,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_rf1_con",".tif"),overwrite=T)
            png(filename=paste0("./www/",projeto,"/jpg/fut_",i,"_rf1_con",".jpg"))
            plot(rf1_future,main=paste("RF - Fut ",i))
            dev.off()
          }
        }

        if(SVM==T){
          writeRaster(x=svm_cont,filename=paste0("./www/",projeto,"/models/pre_",i,"_svm_con",".tif"),overwrite=T)
          png(filename=paste0("./www/",projeto,"/jpg/pre_",i,"_svm_con",".jpg"))
          plot(svm_cont,main=paste("SVM - ",i))

          if (i==1)
          {
            cat(paste0("plot(svm_cont,main=paste(\"SVM - \",i))"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          }

          dev.off()
          if (write.projecao==T)
          {
            writeRaster(x=svm_cont_proj,filename=paste0("./www/",projeto,"/proj/pre_",i,"_svm_con_proj",".tif"),overwrite=T)
          }
          if(write.future==T) {
            writeRaster(x=svm_future,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_svm_con",".tif"),overwrite=T)
            png(filename=paste0("./www/",projeto,"/jpg/fut_",i,"_svm_con",".jpg"))
            plot(svm_future,main=paste("SVM - Fut ",i))
            dev.off()
          }
        }

        if(Mahal==T && condicao_Mahal==TRUE){
          writeRaster(x=ma_cont,filename=paste0("./www/",projeto,"/models/pre_",i,"_ma_con",".tif"),overwrite=T)
          png(filename=paste0("./www/",projeto,"/jpg/pre_",i,"_ma_con",".jpg"))
          plot(ma_cont,main=paste("Mahalanobis - ",i))

          if (i==1)
          {
            cat(paste0("plot(ma_cont,main=paste(\"Mahalanobis - \",i))"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          }

          dev.off()
          if (write.projecao==T)
          {
            writeRaster(x=ma_cont_proj,filename=paste0("./www/",projeto,"/proj/pre_",i,"_ma_con_proj",".tif"),overwrite=T)
          }
          if(write.future==T) {
            writeRaster(x=ma_future,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_ma_con",".tif"),overwrite=T)
            png(filename=paste0("./www/",projeto,"/jpg/fut_",i,"_ma_con",".jpg"))
            plot(ma_future,main=paste("Mahalanobis - Fut ",i))
            dev.off()
          }
        }

      } # Fecha escrita de modelos cont?nuos

      ## Modelos bin?rios
      if (write.bin==T){
        cat(paste("Salvando modelos binários...",sp,i,'\n'))

        if(Bioclim==T){
          writeRaster(x=bc_bin,filename=paste0("./www/",projeto,"/models/pre_",i,"_bc_bin",".tif"),overwrite=T)
          png(filename=paste0("./www/",projeto,"/jpg/pre_",i,"_bc_bin",".jpg"))
          plot(bc_bin,main=paste("Bioclim - Bin ",i))

          if (i==1)
          {
            cat(paste0("plot(bc_bin,main=paste(\"Bioclim - Bin \",i))"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          }
          dev.off()

          if(write.future==T) { writeRaster(x=bc_future_bin,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_bc_bin",".tif"),overwrite=T)}}

        if(Domain==T){
          writeRaster(x=do_bin,filename=paste0("./www/",projeto,"/models/pre_",i,"_do_bin",".tif"),overwrite=T)
          png(filename=paste0("./www/",projeto,"/jpg/pre_",i,"_do_bin",".jpg"))
          plot(do_bin,main=paste("Domain - Bin ",i))

          if (i==1)
          {
            cat(paste0("plot(do_bin,main=paste(\"Domain - Bin \",i))"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          }
          dev.off()

          if(write.future==T) { writeRaster(x=do_future_bin,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_do_bin",".tif"),overwrite=T)}}

        if(maxent==T){
          writeRaster(x=mx_bin,filename=paste0("./www/",projeto,"/models/pre_",i,"_mx_bin",".tif"),overwrite=T)
          png(filename=paste0("./www/",projeto,"/jpg/pre_",i,"_mx_bin",".jpg"))
          plot(mx_bin,main=paste("Maxent - Bin ",i))

          if (i==1)
          {
            cat(paste0("plot(mx_bin,main=paste(\"Maxent - Bin \",i))"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          }

          dev.off()

          if(write.future==T) { writeRaster(x=mx_future_bin,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_mx_bin",".tif"),overwrite=T)}}

        if(GLM==T){
          writeRaster(x=glm_bin,filename=paste0("./www/",projeto,"/models/pre_",i,"_glm_bin",".tif"),overwrite=T)
          png(filename=paste0("./www/",projeto,"/jpg/pre_",i,"_glm_bin",".jpg"))
          plot(glm_bin,main=paste("GLM - Bin ",i))

          if (i==1)
          {
            cat(paste0("plot(glm_bin,main=paste(\"GLM - Bin \",i))"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          }
          dev.off()

          if(write.future==T) { writeRaster(x=glm_future_bin,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_glm_bin",".tif"),overwrite=T)}}

        if(RF==T){
          writeRaster(x=rf1_bin,filename=paste0("./www/",projeto,"/models/pre_",i,"_rf_bin",".tif"),overwrite=T)
          png(filename=paste0("./www/",projeto,"/jpg/pre_",i,"_rf_bin",".jpg"))
          plot(rf1_bin,main=paste("RF - Bin ",i))

          if (i==1)
          {
            cat(paste0("plot(rf1_bin,main=paste(\"RF - Bin \",i))"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          }

          dev.off()
          if(write.future==T) { writeRaster(x=rf1_future_bin,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_rf_bin",".tif"),overwrite=T)}}

        if(SVM==T){
          writeRaster(x=svm_bin,filename=paste0("./www/",projeto,"/models/pre_",i,"_svm_bin",".tif"),overwrite=T)
          png(filename=paste0("./www/",projeto,"/jpg/pre_",i,"_svm_bin",".jpg"))
          plot(svm_bin,main=paste("SVM - Bin ",i))

          if (i==1)
          {
            cat(paste0("plot(svm_bin,main=paste(\"SVM - Bin \",i))"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          }

          dev.off()
          if(write.future==T) { writeRaster(x=svm_future_bin,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_svm_bin",".tif"),overwrite=T)}}

        if(Mahal==T && condicao_Mahal==TRUE){
          writeRaster(x=ma_bin,filename=paste0("./www/",projeto,"/models/pre_",i,"_ma_bin",".tif"),overwrite=T)
          png(filename=paste0("./www/",projeto,"/jpg/pre_",i,"_ma_bin",".jpg"))
          plot(ma_bin,main=paste("Mahalanobis - Bin ",i))

          if (i==1)
          {
            cat(paste0("plot(ma_bin,main=paste(\"Mahalanobis - Bin \",i))"),file=ARQUIVO_SAIDA,append=TRUE)
            cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
          }

          dev.off()
          if(write.future==T) { writeRaster(x=ma_future_bin,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_ma_bin",".tif"),overwrite=T)}}


      } # Fecha escrita de modelos binarios

      ## Modelos multiplicados
      if (write.mult==T){
        cat(paste("Salvando modelos multiplicados...",sp,i,'\n'))

        if(Bioclim==T){
          writeRaster(x=bc_mult,filename=paste0("./www/",projeto,"/models/pre_",i,"_bc_mult",".tif"),overwrite=T)
          if(write.future==T) { writeRaster(x=bc_future_mult,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_bc_mult",".tif"),overwrite=T)}}

        if(Domain==T){
          writeRaster(x=do_mult,filename=paste0("./www/",projeto,"/models/pre_",i,"_do_mult",".tif"),overwrite=T)
          if(write.future==T) { writeRaster(x=do_future_mult,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_do_mult",".tif"),overwrite=T)}}


        if(maxent==T){
          writeRaster(x=mx_mult,filename=paste0("./www/",projeto,"/models/pre_",i,"_mx_mult",".tif"),overwrite=T)
          if(write.future==T) { writeRaster(x=mx_future_mult,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_mx_mult",".tif"),overwrite=T)}}

        if(GLM==T){
          writeRaster(x=glm_mult,filename=paste0("./www/",projeto,"/models/pre_",i,"_glm_mult",".tif"),overwrite=T)
          if(write.future==T) { writeRaster(x=glm_future_mult,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_glm_mult",".tif"),overwrite=T)}}

        if(RF==T){
          writeRaster(x=rf1_mult,filename=paste0("./www/",projeto,"/models/pre_",i,"_rf_mult",".tif"),overwrite=T)
          if(write.future==T) { writeRaster(x=rf1_future_mult,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_rf_mult",".tif"),overwrite=T)}}

        if(SVM==T){
          writeRaster(x=svm_mult,filename=paste0("./www/",projeto,"/models/pre_",i,"_svm_mult",".tif"),overwrite=T)
          if(write.future==T) { writeRaster(x=svm_future_mult,filename=paste0("./www/",projeto,"/futuro/fut_",i,"_svm_mult",".tif"),overwrite=T)}}

        if(Mahal==T && condicao_Mahal==TRUE){
          writeRaster(x=ma_mult,filename=paste0("./www/",projeto,"/models/pre_",i,"_ma_mult",".tif"),overwrite=T)
          if(write.future==T) { writeRaster(x=ma_future_mult,filename=paste0("./www/",projeto,"/models/fut_",i,"_ma_mult",".tif"),overwrite=T)}}
      } # Fecha escrita de modelos multiplicados

      ### SALVA ARQUIVOS DE VALIDA??O DE PERFORMANCE
      ##
      cat(paste("Salavando o arquivo de valida??o...",sp,i,'\n'))
      sink(file=paste0("./www/",projeto,"/models/evaluate_",sp,".txt"),split=T,append=T)
      if(Bioclim==T){
        print(paste(sp,spname,i,"BioClim",round(ebc@auc,3),round(bcTSS,3),round(tbc,3),round(threshold(ebc)$kappa,3),round(threshold(ebc)$equal_sens_spec,3),round(threshold(ebc)$no_omission,3),round(threshold(ebc)$prevalence,3),round(threshold(ebc)$sensitivity,3),ebc@np,ebc@na,round(ebc@cor,3),sep=","))}
      if(Domain==T){
        print(paste(sp,spname,i,"Domain",round(edo@auc,3),round(doTSS,3),round(tdo,3),round(threshold(edo)$kappa,3),round(threshold(edo)$equal_sens_spec,3),round(threshold(edo)$no_omission,3),round(threshold(edo)$prevalence,3),round(threshold(edo)$sensitivity,3),edo@np,edo@na,round(edo@cor,3),sep=","))}
      if(maxent==T){
        print(paste(sp,spname,i,"maxent",round(emx@auc,3),round(mxTSS,3),round(tmx,3),round(threshold(emx)$kappa,3),round(threshold(emx)$equal_sens_spec,3),round(threshold(emx)$no_omission,3),round(threshold(emx)$prevalence,3),round(threshold(emx)$sensitivity,3),emx@np,emx@na,round(emx@cor,3),sep=","))}
      if(GLM==T){
        print(paste(sp,spname,i,"GLM",round(eglm@auc,3),round(glmTSS,3),round(tglm,3),round(threshold(eglm)$kappa,3),round(threshold(eglm)$equal_sens_spec,3),round(threshold(eglm)$no_omission,3),round(threshold(eglm)$prevalence,3),round(threshold(eglm)$sensitivity,3),eglm@np,eglm@na,round(eglm@cor,3),sep=","))}
      if(RF==T){
        print(paste(sp,spname,i,"RF",round(erf1@auc,3),round(rfTSS1,3),round(trf1,3),round(threshold(erf1)$kappa,3),round(threshold(erf1)$equal_sens_spec,3),round(threshold(erf1)$no_omission,3),round(threshold(erf1)$prevalence,3),round(threshold(erf1)$sensitivity,3),erf1@np,erf1@na,round(erf1@cor,3),sep=","))}
      if(SVM==T){
        print(paste(sp,spname,i,"SVM",round(esvm@auc,3),round(svmTSS,3),round(tsvm,3),round(threshold(esvm)$kappa,3),round(threshold(esvm)$equal_sens_spec,3),round(threshold(esvm)$no_omission,3),round(threshold(esvm)$prevalence,3),round(threshold(esvm)$sensitivity,3),esvm@np,esvm@na,round(esvm@cor,3),sep=","))}
      if(Mahal==T && condicao_Mahal==TRUE){
        print(paste(sp,sp,i,"Mahal",round(ema@auc,3),round(maTSS,3),round(tma,3),round(threshold(ema)$kappa,3),round(threshold(ema)$equal_sens_spec,3),round(threshold(ema)$no_omission,3),round(threshold(ema)$prevalence,3),round(threshold(ema)$sensitivity,3),ema@np,ema@na,round(ema@cor,3),sep=","))}
      sink()

 #     cat(paste("Salvando arquivos de validação para todas as espécies...",sp,i,'\n'))
      sink(file=paste0("./www/",projeto,"/models/evaluate_ALL_models.txt"),split=T,append=T)
      if(Bioclim==T){
        print(paste(sp,spname,i,"BioClim",round(ebc@auc,3),round(bcTSS,3),round(tbc,3),round(threshold(ebc)$kappa,3),round(threshold(ebc)$equal_sens_spec,3),round(threshold(ebc)$no_omission,3),round(threshold(ebc)$prevalence,3),round(threshold(ebc)$sensitivity,3),ebc@np,ebc@na,round(ebc@cor,3),sep=","))}
      if(Domain==T){
        print(paste(sp,spname,i,"Domain",round(edo@auc,3),round(doTSS,3),round(tdo,3),round(threshold(edo)$kappa,3),round(threshold(edo)$equal_sens_spec,3),round(threshold(edo)$no_omission,3),round(threshold(edo)$prevalence,3),round(threshold(edo)$sensitivity,3),edo@np,edo@na,round(edo@cor,3),sep=","))}
      if(maxent==T){
        print(paste(sp,spname,i,"maxent",round(emx@auc,3),round(mxTSS,3),round(tmx,3),round(threshold(emx)$kappa,3),round(threshold(emx)$equal_sens_spec,3),round(threshold(emx)$no_omission,3),round(threshold(emx)$prevalence,3),round(threshold(emx)$sensitivity,3),emx@np,emx@na,round(emx@cor,3),sep=","))}
      if(GLM==T){
        print(paste(sp,spname,i,"GLM",round(eglm@auc,3),round(glmTSS,3),round(tglm,3),round(threshold(eglm)$kappa,3),round(threshold(eglm)$equal_sens_spec,3),round(threshold(eglm)$no_omission,3),round(threshold(eglm)$prevalence,3),round(threshold(eglm)$sensitivity,3),eglm@np,eglm@na,round(eglm@cor,3),sep=","))}
      if(RF==T){
        print(paste(sp,spname,i,"RF",round(erf1@auc,3),round(rfTSS1,3),round(trf1,3),round(threshold(erf1)$kappa,3),round(threshold(erf1)$equal_sens_spec,3),round(threshold(erf1)$no_omission,3),round(threshold(erf1)$prevalence,3),round(threshold(erf1)$sensitivity,3),erf1@np,erf1@na,round(erf1@cor,3),sep=","))}
      if(SVM==T){
        print(paste(sp,spname,i,"SVM",round(esvm@auc,3),round(svmTSS,3),round(tsvm,3),round(threshold(esvm)$kappa,3),round(threshold(esvm)$equal_sens_spec,3),round(threshold(esvm)$no_omission,3),round(threshold(esvm)$prevalence,3),round(threshold(esvm)$sensitivity,3),esvm@np,esvm@na,round(esvm@cor,3),sep=","))}
      if(Mahal==T && condicao_Mahal==TRUE){
        print(paste(sp,spname,i,"Mahal",round(ema@auc,3),round(maTSS,3),round(tma,3),round(threshold(ema)$kappa,3),round(threshold(ema)$equal_sens_spec,3),round(threshold(ema)$no_omission,3),round(threshold(ema)$prevalence,3),round(threshold(ema)$sensitivity,3),ema@np,ema@na,round(ema@cor,3),sep=","))}
      sink()

      stats <- read.delim(file=paste0("./www/",projeto,"/models/evaluate_ALL_models.txt"),header=F,sep=",",quote="",col.names=c("id","sp","part","algorithm","AUC","TSS","TSSth", "Kappa","Equal_sens_spec","No_omission","Prevalence","Sensitivity","np","na","Cor" ))
      #stats
      stats$Sensitivity<-as.numeric(sub(pattern="\"","",stats$Sensitivity))
#      stats$Sensitivity<-stats$Sensitivity
      stats20 <- stats[order(stats$sp,stats$algorithm,stats$part),-1]
      write.table(stats20,paste0("./www/",projeto,"/models/statsALL.txt"))
      output$dbgridresultado <- renderDataTable({
        stats20
      }, options = list(lengthMenu = c(5, 30, 50), pageLength = 10)
      )

    } # Fecha o for loop
    cat(paste0("} # Fecha o for loop"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    output$dbgridresultado <- renderDataTable({
      cat(c(date(),"Mostrando o resultado de stats20",'\n','\n'))
      stats20
    }, options = list(lengthMenu = c(5, 30, 50), pageLength = 10)
    )

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
      #plot( ensemble.bc, main=paste("BIOCLIM - Ensemble"))
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
      lista3[[i]]<-read.table(file = evall3[i],header=T,row.names=1)
    }
    stats3<-rbindlist(lista3)
    stats3<-as.data.frame(stats3)

    #      Extracts only for the selected algorithm
    algoritmos <- unique(stats3$algorithm)

    #algoritmos
    for (algo in algoritmos){
      #algo
      stats2 <- stats3[stats3$algorithm==algo,]
      #stats2
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

      part <- nrow(stats2)#How many partitions were there
      #part

      cat(paste("Reading models from .tif files","\n"))
      modelos <- list.files(path = paste0('./www/',projeto,'/models',"/"),full.names=T,pattern=paste0(algo,"_con"))
      #        modelos
      mod<-stack(modelos)#(0)
      names(mod)<-paste0("Partition",1:part)

      #Binary by TSSth and Cut
      bin <- mod>stats2[,names(stats2)=="Equal_sens_spec"]#stack
      cut <- bin * mod#stack
      #TSS.value<-0.2

      sel.index<- which(stats2[,"TSS"]>=TSS.value)
      mod.sel<- mod[[sel.index]]
      if (length(sel.index)==0) cat(paste("No partition was selected for","\n"))
      if (length(sel.index)>0){

        mod.sel<- mod[[sel.index]] #(1)
        bin.sel<-mod.sel>stats2[,names(stats2)=="Equal_sens_spec"][sel.index] #(5)
        cut.sel<-bin.sel*mod.sel#(8)

        th.mean<-mean(stats2[,names(stats2)=="Equal_sens_spec"][sel.index])

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
      if (length(sel.index)>1){
        cat(paste(length(sel.index), "partitions were selected for"))
        final.cont.mean <- mean(mod.sel)#(2)
        final.bin.mean <- (final.cont.mean>th.mean)#(3)
        final.cut.mean <- final.bin.mean*final.cont.mean #(4)

        final.sel.bin <- mean(bin.sel)#(7)
        final.inter<-prod(bin.sel)#(8)

        mean.cut.sel <- mean(cut.sel)#(9)
        inter.cut.sel <- prod(cut.sel)#(10)

        final <- stack(final.cont.mean,final.bin.mean,final.cut.mean,final.sel.bin,final.inter,mean.cut.sel,inter.cut.sel)
        names(final) <- c("2_Final.cont.mean_","3_Final.bin.mean_","4_Final.cut.mean_","7_Final.mean.bin_","8_Final.inter.bin_","9_Mean.cut.sel_","10_inter.cut.sel_")

        writeRaster(x=final.cont.mean,filename=paste0("./www/",projeto,"/final","/2_Final_cont_mean_",algo),overwrite=T,format="GTiff")
        writeRaster(x=final.bin.mean,filename=paste0("./www/",projeto,"/final","/3_Final_bin_mean_",algo),overwrite=T,format="GTiff")

        writeRaster(x=final.cut.mean,filename=paste0("./www/",projeto,"/final","/4_Final_cut_mean_",algo),overwrite=T,format="GTiff")
        writeRaster(x=final.sel.bin,filename=paste0("./www/",projeto,"/final","/7_Final_mean_bin_",algo),overwrite=T,format="GTiff")
        writeRaster(x=final.inter,filename=paste0("./www/",projeto,"/final","/8_Final_inter_bin_",algo),overwrite=T,format="GTiff")
        writeRaster(x=mean.cut.sel,filename=paste0("./www/",projeto,"/final","/9_Mean_cut_sel_",algo),overwrite=T,format="GTiff")
        writeRaster(x=inter.cut.sel,filename=paste0("./www/",projeto,"/final","/10_inter_cut_sel_",algo),overwrite=T,format="GTiff")

      }
      if(exists("final")) {
        #Escribe mean binary de los seleccionados
        #writeRaster(x=final,filename=paste0("./www/",projeto,"/final","/",names(final),algo),bylayer=T,overwrite=T,format="GTiff")
        for (i in 1:dim(final)[[3]]){
          png(filename=paste0('./www/',projeto,'/final',"/",names(final)[i],algo,".png"))
          plot(final[[i]],main=paste0(names(final)[i],algo))
          dev.off()
        }
      }

    } # for
} # Fecha a fun??o dismo.mod

##############################
# INICIO FUNÃÃO MODELAGEM
##############################

    modelagem <- function() ({

    ## LIMPANDO OS RESULTADOS ANTERIORES
   limparResultadosAnteriores()

    library(raster)
    numpontos = input$edtnumpontos
    numparticoes <- input$edtnumgrupo

    ########################################
    ## INICIO UTILIZACAO FUNCAO DISMO.MOD ##
    ########################################

    ## RODANDO A FUNCAO DISMO.MOD modificada
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
    if (input$PROJETAR==T)
    {
      write.projecao = T
    }

    cat("library(maps)",file=ARQUIVO_SAIDA,sep="\n")
    cat("library(dismo)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat("library(rgdal)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat("library(raster)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat("library(rgbif)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat("library(XML)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat("# INFORME O CAMINHO DA APLICACAO",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat("path <- \"\"",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat("file <- paste(path,\"csv/dados.csv\", sep=\"\")",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat("especie <- read.csv(file)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
    cat("head(especie)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat("head(especie)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat("arquivo <- list()",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat("# ADICIONE OS RASTERS",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)


    if (length(arquivo2>0))
    {
      for (i in 1:length(arquivo2)){
        cat(paste0("arquivo <- c(arquivo,paste('",arquivo2[[i]],"',sep=''))"),file=ARQUIVO_SAIDA,append=TRUE)
        cat("\n",file=ARQUIVO_SAIDA,append=TRUE)
      }
    }

    cat("arquivo",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat("predictors <- stack(arquivo)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat("pred_nf <- predictors",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    #cat(paste0("ext <- extent(",input$edtextend1,",",input$edtextend2,",",input$edtextend3,",",input$edtextend4,")"),file=ARQUIVO_SAIDA,append=TRUE)
    cat(paste0("ext <- extent(",ext1,",",ext3,",",ext2,",",ext4,")"),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat("pred_nf <- crop(pred_nf, ext)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat("plot(pred_nf, 1)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat("especie <<- especie[,2:3]",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat("points(especie, bg='red', cex=1,pch=21)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat("presvals <- raster::extract(pred_nf, especie)",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat("var <- pred_nf",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat("var2 <- pred_nf2",file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    cat(paste0("part <- ",numparticoes),file=ARQUIVO_SAIDA,append=TRUE)
    cat("\n",file=ARQUIVO_SAIDA,append=TRUE)

    dismo.mod("",especie,pred_nf,pred_nf2,input$MAXENT,input$BIOCLIM,input$GLM,input$RF,input$SVM,input$MAHALANOBIS,input$DOMAIN,input$SVM2,numparticoes,numpontos,123,T,T,T,F,F,input$TSS,futuro,pred_nffuturo,futuro,write.projecao)

    progress$set(message = paste("Gerando script"), value = 0)

    # script gerado pelo sistema

    progress$set(message = "Salvando dados...", value = 0)

    write.csv(especie, file = paste0("www/",projeto,"/csv/dados.csv"))

    #######################################################################
    ## PARA CADA ALGORITMO SELECIONADO MONTO UM MAPA COM O RASTER GERADO
    #######################################################################

    # verificando se foi gerado o arquivo de projeto ensemble final
    # gero o arquivo raster para ser colocado no mapa
    if (file.exists(paste0('www/',projeto,'/final/proj_ensemble.tif')))
    {
      rproj <- raster::raster(paste0("www/",projeto,"/final/proj_ensemble.tif"))
    }

    output$maparesultadomax <- renderLeaflet({
      input$btnModelar
      if (file.exists(paste0('www/',projeto,'/final/mx_ensemble.tif')))
      {
        r <- raster::raster(paste0("www/",projeto,"/final/mx_ensemble.tif"))
        pal <- colorNumeric(c("#FFFFFF","#FDBB84","#31A354"),values(r),na.color = "transparent")
        if (file.exists(paste0('www/',projeto,'/final/proj_ensemble.tif')))
        {
          map = leaflet() %>% addTiles %>%
          addRasterImage(r,colors = pal,opacity = 0.8) %>%
          addRasterImage(rproj,colors = pal,opacity = 0.8) %>%
          addLegend(pal = pal, values = values(r),
                    title = "Maxent") %>%
            addCircles(color = "red", lat = especie[,2], lng = especie[,1])  %>%
            #addMarkers(especie[,1], especie[,2]) %>%
          addRectangles(
            ext1, ext3, ext2, ext4, color = 'green', fill = FALSE, dashArray = '5,5', weight = 3
          )
        }else
        {
          map = leaflet() %>% addTiles %>%
            addRasterImage(r,colors = pal,opacity = 0.8) %>%
            addLegend(pal = pal, values = values(r),
                      title = "Maxent") %>%
            addCircles(color = "red", lat = especie[,2], lng = especie[,1])  %>%
            #addMarkers(especie[,1], especie[,2]) %>%
            addRectangles(
              ext1, ext3, ext2, ext4, color = 'green', fill = FALSE, dashArray = '5,5', weight = 3
            )
        }
        map
      }
    })

    output$maparesultadosvm <- renderLeaflet({
      input$btnModelar
      if (file.exists(paste0('www/',projeto,'/final/svm_ensemble.tif')))
      {
        r <- raster::raster(paste0("www/",projeto,"/final/svm_ensemble.tif"))
        pal <- colorNumeric(c("#FFFFFF","#FDBB84","#31A354"),values(r),na.color = "transparent")
        if (file.exists(paste0('www/',projeto,'/final/proj_ensemble.tif')))
        {
          map = leaflet() %>% addTiles %>%
          addRasterImage(r,colors = pal,opacity = 0.8) %>%
          addRasterImage(rproj,colors = pal,opacity = 0.8) %>%
          addLegend(pal = pal, values = values(r),
                    title = "SVM") %>%
            addCircles(color = "red", lat = especie[,2], lng = especie[,1])  %>%
            #addMarkers(especie[,1], especie[,2]) %>%
          addRectangles(
            ext1, ext3, ext2, ext4, color = 'green', fill = FALSE, dashArray = '5,5', weight = 3
          )
        }
        else
        {
          map = leaflet() %>% addTiles %>%
            addRasterImage(r,colors = pal,opacity = 0.8) %>%
            addLegend(pal = pal, values = values(r),
                      title = "SVM") %>%
            addCircles(color = "red", lat = especie[,2], lng = especie[,1])  %>%
            #addMarkers(especie[,1], especie[,2]) %>%
            addRectangles(
              ext1, ext3, ext2, ext4, color = 'green', fill = FALSE, dashArray = '5,5', weight = 3
            )
        }
        map
      }
    })

    output$maparesultadomh <- renderLeaflet({
      input$btnModelar
      if (file.exists(paste0('www/',projeto,'/final/ma_ensemble.tif')))
      {
        r <- raster::raster(paste0("www/",projeto,"/final/ma_ensemble.tif"))
        pal <- colorNumeric(c("#FFFFFF","#FDBB84","#31A354"),values(r),na.color = "transparent")
        if (file.exists(paste0('www/',projeto,'/final/proj_ensemble.tif')))
        {
          map = leaflet() %>% addTiles %>%
          addRasterImage(r,colors = pal,opacity = 0.8) %>%
          addRasterImage(rproj,colors = pal,opacity = 0.8) %>%
          addLegend(pal = pal, values = values(r),
                    title = "Maha") %>%
            addCircles(color = "red", lat = especie[,2], lng = especie[,1])  %>%
            #addMarkers(especie[,1], especie[,2]) %>%
          addRectangles(
            ext1, ext3, ext2, ext4, color = 'green', fill = FALSE, dashArray = '5,5', weight = 3
          )
        }
        else
        {
          map = leaflet() %>% addTiles %>%
            addRasterImage(r,colors = pal,opacity = 0.8) %>%
            addLegend(pal = pal, values = values(r),
                      title = "Maha") %>%
            addCircles(color = "red", lat = especie[,2], lng = especie[,1])  %>%
            #addMarkers(especie[,1], especie[,2]) %>%
            addRectangles(
              ext1, ext3, ext2, ext4, color = 'green', fill = FALSE, dashArray = '5,5', weight = 3
            )

        }
        map
      }
    })

    output$maparesultadorf <- renderLeaflet({
      input$btnModelar
      if (file.exists(paste0('www/',projeto,'/final/rf_ensemble.tif')))
      {
        r <- raster::raster(paste0("www/",projeto,"/final/rf_ensemble.tif"))
        pal <- colorNumeric(c("#FFFFFF","#FDBB84","#31A354"),values(r),na.color = "transparent")
        if (file.exists(paste0('www/',projeto,'/final/proj_ensemble.tif')))
        {
          map = leaflet() %>% addTiles %>%
          addRasterImage(r,colors = pal,opacity = 0.8) %>%
          addRasterImage(rproj,colors = pal,opacity = 0.8) %>%
          addLegend(pal = pal, values = values(r),
                    title = "RF") %>%
            addCircles(color = "red", lat = especie[,2], lng = especie[,1])  %>%
            #addMarkers(especie[,1], especie[,2]) %>%
          addRectangles(
            ext1, ext3, ext2, ext4, color = 'green', fill = FALSE, dashArray = '5,5', weight = 3
          )
        }else
        {
          map = leaflet() %>% addTiles %>%
            addRasterImage(r,colors = pal,opacity = 0.8) %>%
            addLegend(pal = pal, values = values(r),
                      title = "RF") %>%
            addCircles(color = "red", lat = especie[,2], lng = especie[,1])  %>%
            #addMarkers(especie[,1], especie[,2]) %>%
            addRectangles(
              ext1, ext3, ext2, ext4, color = 'green', fill = FALSE, dashArray = '5,5', weight = 3
            )
        }
        map
      }
    })

    output$maparesultadoglm <- renderLeaflet({
      input$btnModelar
      if (file.exists(paste0('www/',projeto,'/final/glm_ensemble.tif')))
      {
        r <- raster::raster(paste0("www/",projeto,"/final/glm_ensemble.tif"))
        pal <- colorNumeric(c("#FFFFFF","#FDBB84","#31A354"),values(r),na.color = "transparent")
        if (file.exists(paste0('www/',projeto,'/final/proj_ensemble.tif')))
        {
          map = leaflet() %>% addTiles %>%
          addRasterImage(r,colors = pal,opacity = 0.8) %>%
          addRasterImage(rproj,colors = pal,opacity = 0.8) %>%
          addLegend(pal = pal, values = values(r),
                    title = "GLM") %>%
          addCircles(color = "red", lat = especie[,2], lng = especie[,1])  %>%
          #addMarkers(especie[,1], especie[,2]) %>%
          addRectangles(
            ext1, ext3, ext2, ext4, color = 'green', fill = FALSE, dashArray = '5,5', weight = 3
          )
        }
        else
        {
          map = leaflet() %>% addTiles %>%
            addRasterImage(r,colors = pal,opacity = 0.8) %>%
            addLegend(pal = pal, values = values(r),
                      title = "GLM") %>%
            addCircles(color = "red", lat = especie[,2], lng = especie[,1])  %>%
            #addMarkers(especie[,1], especie[,2]) %>%
            addRectangles(
              ext1, ext3, ext2, ext4, color = 'green', fill = FALSE, dashArray = '5,5', weight = 3
            )
        }
        map
      }
    })

    output$maparesultadobc <- renderLeaflet({
      input$btnModelar
      if (file.exists(paste0('www/',projeto,'/final/bc_ensemble.tif')))
      {
        r <- raster::raster(paste0("www/",projeto,"/final/bc_ensemble.tif"))
        crs(r) <- sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
        pal <- colorNumeric(c("#FFFFFF","#FDBB84","#31A354"),values(r),na.color = "transparent")
        if (file.exists(paste0('www/',projeto,'/final/proj_ensemble.tif')))
        {
          map = leaflet() %>% addTiles %>%
          addRasterImage(r,colors = pal,opacity = 0.9) %>%
          addRasterImage(rproj,colors = pal,opacity = 0.9) %>%
          addLegend(pal = pal, values = values(r),
                    title = "BioClim") %>%
          addCircles(color = "red", lat = especie[,2], lng = especie[,1])  %>%
            #addCircles(color = "red", lat = ~ especie[,1], lng = ~ especie[,2]) %>%
          #addMarkers(especie[,1], especie[,2]) %>%
          addRectangles(
            ext1, ext3, ext2, ext4, color = 'green', fill = FALSE, dashArray = '5,5', weight = 3
          )
        }else
        {
          map = leaflet() %>% addTiles %>%
            addRasterImage(r,colors = pal,opacity = 0.8) %>%
            addLegend(pal = pal, values = values(r),
                      title = "BioClim") %>%
            addCircles(color = "red", lat = especie[,2], lng = especie[,1])  %>%
            #addCircles(color = "red", lat = ~ especie[,1], lng = ~ especie[,2]) %>%
            #addMarkers(especie[,1], especie[,2]) %>%
            addRectangles(
              ext1, ext3, ext2, ext4, color = 'green', fill = FALSE, dashArray = '5,5', weight = 3
            )
        }
        map
      }
    })

    output$maparesultadodo <- renderLeaflet({
      input$btnModelar
      if (file.exists(paste0('www/',projeto,'/final/do_ensemble.tif')))
      {
        r <- raster::raster(paste0("www/",projeto,"/final/do_ensemble.tif"))
        pal <- colorNumeric(c("#FFFFFF","#FDBB84","#31A354"),values(r),na.color = "transparent")
        if (file.exists(paste0('www/',projeto,'/final/proj_ensemble.tif')))
        {
          map = leaflet() %>% addTiles %>%
            addRasterImage(r,colors = pal,opacity = 0.8) %>%
            addRasterImage(rproj,colors = pal,opacity = 0.8) %>%
            addLegend(pal = pal, values = values(r),
                      title = "BioClim") %>%
            addCircles(color = "red", lat = especie[,2], lng = especie[,1])  %>%
            #addMarkers(especie[,1], especie[,2]) %>%
            addRectangles(
              ext1, ext3, ext2, ext4, color = 'green', fill = FALSE, dashArray = '5,5', weight = 3
            )
        }else
        {
          map = leaflet() %>% addTiles %>%
            addRasterImage(r,colors = pal,opacity = 0.8) %>%
            addLegend(pal = pal, values = values(r),
                      title = "BioClim") %>%
            addCircles(color = "red", lat = especie[,2], lng = especie[,1])  %>%
            #addMarkers(especie[,1], especie[,2]) %>%
            addRectangles(
              ext1, ext3, ext2, ext4, color = 'green', fill = FALSE, dashArray = '5,5', weight = 3
            )
        }
        map
      }
    })


    #######################################################
    ## APRESENTO OS ARQUIVOS GERADOS NA PGINA DE DOWNLOAD
    ########################################################

    output$uiarquivosmodelos <- renderUI({
      lista_models <- list.files(paste0("www/",projeto,"/models"),full.names=F,pattern=paste0("pre_"))
      lapply(1:length(sort(lista_models)), function(i) {
        tags$div(
          tags$a(href=paste0(home,projeto,'/models/',lista_models[i]), paste0(lista_models[i]))
        )
      })
    })

    output$ui <- renderUI({
      lista_jpg <- list.files(paste0("www/",projeto,"/jpg"),full.names=F,pattern=paste0(".jpg"))
      lapply(1:length(order(lista_jpg)), function(i) {
        tags$a(href=paste0(home,projeto,'/jpg/',lista_jpg[i]), tags$img(src = paste0(projeto,'/jpg/',lista_jpg[i]), height = "200px"), target="_blank")
      })
    })


    output$uiscript <- renderUI({
      lista_txt <- list.files(paste0("www/",projeto,"/"),full.names=F,pattern=paste0("script.R"))
      lapply(1:length(lista_txt), function(i) {
        tags$div(
          tags$a(href=paste0(home,projeto,'/',lista_txt[i]), paste0(lista_txt[i]), target="_blank")
        )
      })
    })


    output$uiestatistica <- renderUI({
      lista_txt <- list.files(paste0("www/",projeto,"/models"),full.names=F,pattern=paste0("statsALL.txt"))
      lapply(1:length(lista_txt), function(i) {
        tags$div(
          tags$a(href=paste0(home,projeto,'/models/',lista_txt[i]), paste0(lista_txt[i]), target="_blank")
        )
      })
    })

    output$uiarquivosdados <- renderUI({
      lista_csv <- list.files(paste0("www/",projeto,"/csv"),full.names=F,pattern=paste0(".csv"))
      lapply(1:length(lista_csv), function(i) {
        tags$div(
          tags$a(href=paste0(home,projeto,'/csv/',lista_csv[i]), paste0(lista_csv[i]), target="_blank")
        )
      })
    })


    output$uiarquivosensemble <- renderUI({
      lista_final <- list.files(paste0("www/",projeto,"/final"),full.names=F,pattern=paste0(".tif"))
      lapply(1:length(sort(lista_final)), function(i) {
        tags$div(
          tags$a(href=paste0(home,projeto,'/final/',lista_final[i]), paste0(lista_final[i]), target="_blank")
        )
      })
    })


    output$uiarquivosprojecao <- renderUI({
      lista_proj <- list.files(paste0("www/",projeto,"/proj"),full.names=F,pattern=paste0(".tif"))
      lapply(1:length(sort(lista_proj)), function(i) {
        tags$div(
          tags$a(href=paste0(home,projeto,'/proj/',lista_proj[i]), paste0(lista_proj[i]), target="_blank")
        )
      })
    })

    output$uiarquivosprojecaofuturo <- renderUI({
      lista_futuro <- list.files(paste0("www/",projeto,"futuro"),full.names=F,pattern=paste0(".tif"))

      lapply(1:length(sort(lista_futuro)), function(i) {
        tags$div(
          tags$a(href=paste0(home,projeto,'/futuro/',lista_futuro[i]), paste0(lista_futuro[i]), target="_blank")
        )
      })
    })

  })

  ##############################
  # FIM FUNCAO MODELAGEM
  ##############################

  # FUNCAO TENTANDO AGRUPAR TODOS OS PROCESSOS DE MODELAGEM APOS CLICAR NO BOTao EXECUTAR
  output$plotmodelagem <- renderPlot({
    input$btnModelar
    isolate({
    if ((input$DOMAIN=='TRUE') || (input$MAXENT=='TRUE') || (input$BIOCLIM=='TRUE') || (input$GLM=='TRUE') || (input$RF=='TRUE') || (input$SVM=='TRUE') || (input$GLM=='TRUE'))
    {
      if (ETAPA>1)
      {
        if (exists("especie"))
        {
            progress <<- shiny::Progress$new()
            progress$set(message = "Processando...", value = 0)
            on.exit(progress$close())
            modelagem()
        }
      }
    }
    })
  })

  output$mapapontosextend <- renderLeaflet({

    if (!is.null(especie))
    {
      ext1 <<- input$edtextend1
      ext3 <<- input$edtextend3
      ext2 <<- input$edtextend2
      ext4 <<- input$edtextend4
      #setView(lng = -31.5, lat = -13.4, zoom = 1) %>%
      map = leaflet(especie) %>% addTiles %>% addMarkers(clusterOptions = markerClusterOptions()) %>%  addMarkers(~Longitude, ~Latitude) %>%
        addRectangles(
          input$edtextend1, input$edtextend3, input$edtextend2, input$edtextend4, color = 'red', fill = TRUE, dashArray = '5,5', weight = 3
        )
      map
    }
  })

  output$mapapontosextend2 <- renderLeaflet({
    if (!is.null(especie))
    {
      ext12 <<- input$edtextend12
      ext32 <<- input$edtextend32
      ext22 <<- input$edtextend22
      ext42 <<- input$edtextend42
      map = leaflet() %>% addTiles %>%
        addRectangles(
          input$edtextend12, input$edtextend32, input$edtextend22, input$edtextend42, color = 'green', fill = TRUE, dashArray = '5,5', weight = 3
        )
      map
    }
  })


  output$mapaabiotico <- renderPlot({
    input$btnAtualizaSelecaoVariaveis
    withProgress(message = '', value = 0, {
    n <- 3
    incProgress(1/n, detail = paste0("Carregando variáveis"))
    ETAPA <<- 3
    isolate({
      if (input$tipodadoabiotico=='CLIMA')
      {
      path = paste(getwd(),'/ex/clima/current/',input$resolucao,sep='')
      pathfuturo = paste(getwd(),'/ex/clima/',input$periodo,'/',input$resolucao,sep='')
      arquivo = list()
      arquivofuturo = list()
      selecionado = FALSE
      if (input$Bio1==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio1.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio2==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio2.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio3==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio3.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio4==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio4.bil',sep=''))
        selecionado = TRUE
      }

      if (input$Bio5==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio5.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio6==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio6.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio7==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio7.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio8==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio8.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio9==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio9.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio10==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio10.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio11==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio11.bil',sep=''))
        selecionado = TRUE
      }

      if (input$Bio12==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio12.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio13==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio13.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio14==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio14.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio15==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio15.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio16==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio16.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio17==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio17.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio18==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio18.bil',sep=''))
        selecionado = TRUE
      }
      if (input$Bio19==TRUE) {
        arquivo <- c(arquivo,paste(path,'/bio19.bil',sep=''))
        selecionado = TRUE
      }


      #############################################################################
      # SE FOI ESCOLHIDO ALGUM PERÃODO DIFERENTE DO CURRENT ENTAO PROJETO O FUTURO
      #############################################################################
      if (input$periodo != 'current')
      {
      if (input$Bio1==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio1.bil',sep=''))
      }
      if (input$Bio2==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio2.bil',sep=''))
      }
      if (input$Bio3==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio3.bil',sep=''))
      }
      if (input$Bio4==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio4.bil',sep=''))
      }

      if (input$Bio5==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio5.bil',sep=''))
      }
      if (input$Bio6==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio6.bil',sep=''))
      }
      if (input$Bio7==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio7.bil',sep=''))
      }
      if (input$Bio8==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio8.bil',sep=''))
      }
      if (input$Bio9==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio9.bil',sep=''))
      }
      if (input$Bio10==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio10.bil',sep=''))
      }
      if (input$Bio11==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio11.bil',sep=''))
      }

      if (input$Bio12==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio12.bil',sep=''))
      }
      if (input$Bio13==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio13.bil',sep=''))
      }
      if (input$Bio14==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio14.bil',sep=''))
      }
      if (input$Bio15==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio15.bil',sep=''))
      }
      if (input$Bio16==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio16.bil',sep=''))
      }
      if (input$Bio17==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio17.bil',sep=''))
      }
      if (input$Bio18==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio18.bil',sep=''))
      }
      if (input$Bio19==TRUE) {
        arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/bio19.bil',sep=''))
      }
      }  # IF CURRENT
      }

      # BIOORACLE
      if (input$tipodadoabiotico=='BIOORACLE')
      {
      path <- paste(getwd(),'/ex/biooracle/',sep='')
      #
      pathfuturo = paste(getwd(),'/ex/biooracle/',input$cenariobiooracle,'/',input$periodobiooracle,sep='')
      cat(paste("presente: ",path,'\n'))
      cat(paste("futuro: ",pathfuturo,'\n'))
      arquivo = list()
      arquivofuturo = list()
      selecionado = FALSE

      if (input$calcite==TRUE) {
        arquivo <- c(arquivo,paste(path,'/calcite.asc',sep=''))
        selecionado = TRUE
      }
      if (input$chlomin==TRUE) {
        arquivo <- c(arquivo,paste(path,'/chlomin.asc',sep=''))
        selecionado = TRUE
      }
      if (input$cloudmean==TRUE) {
        arquivo <- c(arquivo,paste(path,'/cloudmean.asc',sep=''))
        selecionado = TRUE
      }
      if (input$damean==TRUE) {
        arquivo <- c(arquivo,paste(path,'/damean.asc',sep=''))
        selecionado = TRUE
      }
      if (input$nitrate==TRUE) {
        arquivo <- c(arquivo,paste(path,'/nitrate.asc',sep=''))
        selecionado = TRUE
      }
      if (input$ph==TRUE) {
        arquivo <- c(arquivo,paste(path,'/ph.asc',sep=''))
        selecionado = TRUE
      }
      if (input$silicate==TRUE) {
        arquivo <- c(arquivo,paste(path,'/silicate.asc',sep=''))
        selecionado = TRUE
      }
      if (input$sstmin==TRUE) {
        arquivo <- c(arquivo,paste(path,'/sstmin.asc',sep=''))
        selecionado = TRUE
      }
      if (input$chlomax==TRUE) {
        arquivo <- c(arquivo,paste(path,'/chlomax.asc',sep=''))
        selecionado = TRUE
      }
      if (input$chlorange==TRUE) {
        arquivo <- c(arquivo,paste(path,'/chlorange.asc',sep=''))
        selecionado = TRUE
      }
      if (input$cloudmin==TRUE) {
        arquivo <- c(arquivo,paste(path,'/cloudmin.asc',sep=''))
        selecionado = TRUE
      }
      if (input$damin==TRUE) {
        arquivo <- c(arquivo,paste(path,'/damin.asc',sep=''))
        selecionado = TRUE
      }
      if (input$parmax==TRUE) {
        arquivo <- c(arquivo,paste(path,'/parmax.asc',sep=''))
        selecionado = TRUE
      }
      if (input$phosphate==TRUE) {
        arquivo <- c(arquivo,paste(path,'/phosphate.asc',sep=''))
        selecionado = TRUE
      }
      if (input$sstmax==TRUE) {
        arquivo <- c(arquivo,paste(path,'/sstmax.asc',sep=''))
        selecionado = TRUE
      }
      if (input$sstrange==TRUE) {
        arquivo <- c(arquivo,paste(path,'/sstrange.asc',sep=''))
        selecionado = TRUE
      }
      if (input$chlomean==TRUE) {
        arquivo <- c(arquivo,paste(path,'/chlomean.asc',sep=''))
        selecionado = TRUE
      }
      if (input$cloudmax==TRUE) {
        arquivo <- c(arquivo,paste(path,'/cloudmax.asc',sep=''))
        selecionado = TRUE
      }
      if (input$damax==TRUE) {
        arquivo <- c(arquivo,paste(path,'/damax.asc',sep=''))
        selecionado = TRUE
      }
      if (input$dissox==TRUE) {
        arquivo <- c(arquivo,paste(path,'/dissox.asc',sep=''))
        selecionado = TRUE
      }
      if (input$parmean==TRUE) {
        arquivo <- c(arquivo,paste(path,'/parmean.asc',sep=''))
        selecionado = TRUE
      }
      if (input$salinity==TRUE) {
        arquivo <- c(arquivo,paste(path,'/salinity.asc',sep=''))
        selecionado = TRUE
      }
      if (input$sstmean==TRUE) {
        arquivo <- c(arquivo,paste(path,'/sstmean.asc',sep=''))
        selecionado = TRUE
      }

      #############################################################################
      # SE FOI ESCOLHIDO ALGUM PERÃODO DIFERENTE DO CURRENT ENTAO PROJETO O FUTURO
      #############################################################################
      if (input$periodobiooracle != 'current')
      {
        if (input$calcite==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/calcite.asc',sep=''))
        }
        if (input$chlomin==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/chlomin.asc',sep=''))
        }
        if (input$cloudmean==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/cloudmean.asc',sep=''))
        }
        if (input$damean==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/damean.asc',sep=''))
        }
        if (input$nitrate==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/nitrate.asc',sep=''))
        }
        if (input$ph==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/ph.asc',sep=''))
        }
        if (input$silicate==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/silicate.asc',sep=''))
        }
        if (input$sstmin==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/sstmin.asc',sep=''))
        }
        if (input$chlomax==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/chlomax.asc',sep=''))
        }
        if (input$chlorange==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/chlorange.asc',sep=''))
        }
        if (input$cloudmin==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/cloudmin.asc',sep=''))
        }
        if (input$damin==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/damin.asc',sep=''))
        }
        if (input$parmax==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/parmax.asc',sep=''))
        }
        if (input$phosphate==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/phosphate.asc',sep=''))
        }
        if (input$sstmax==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/sstmax.asc',sep=''))
        }
        if (input$sstrange==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/sstrange.asc',sep=''))
        }
        if (input$chlomean==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/chlomean.asc',sep=''))
        }
        if (input$cloudmax==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/cloudmax.asc',sep=''))
        }
        if (input$damax==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/damax.asc',sep=''))
        }
        if (input$dissox==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/dissox.asc',sep=''))
        }
        if (input$parmean==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/parmean.asc',sep=''))
        }
        if (input$salinity==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/salinity.asc',sep=''))
        }
        if (input$sstmean==TRUE) {
          arquivofuturo <- c(arquivofuturo,paste(pathfuturo,'/sstmean.asc',sep=''))
        }
      }  # IF CURRENT

      } # VAR BIOORACLE


      if (input$tipodadoabiotico=='Others')
      {
        path <- paste(getwd(),'/ex/outros/',sep='')
        #
#        pathfuturo = paste(getwd(),'/ex/outros/',input$cenariobiooracle,'/',input$periodobiooracle,sep='')
        cat(paste("presente: ",path,'\n'))
        #cat(paste("futuro: ",pathfuturo,'\n'))
        arquivo = list()
        #arquivofuturo = list()
        selecionado = FALSE
        lista_outros <- list.files("ex/outros/",full.names=F,pattern=paste0(".*"))
        if (length(lista_outros>0))
        {
          #if (input$chboxoutro1==TRUE)
          #{
          arquivo <- list.files("ex/outros/",full.names=T,pattern=paste0(".*"))
          #c(arquivo,paste('ex/outros/','bio1.bil',sep=''))
          cat(paste("selecionado=true ",'\n'))
          selecionado = TRUE
          # //}
        }


      }



      incProgress(2/n, detail = paste0("Verificando correlacao"))

      arquivo2 <<- arquivo
      arquivo3 = arquivo
      cat(paste("Checando... ",'\n'))
      if (length(arquivo)>0)
      {
        if ((selecionado == TRUE) && (exists("especie")))
        {
          predictors <- stack(arquivo)
          predictors3 = stack(arquivo3)


          if (input$tipodadoabiotico!='Others')
          {
            cat(paste("Estou aqui 1 ",'\n'))

          if (input$periodo != 'current')
          {
            predictorsfuturo = stack(arquivofuturo)
          }
          if (input$periodobiooracle != 'current')
          {
            predictorsfuturo = stack(arquivofuturo)
          }
          }
          ext <<- extent(ext1, ext2, ext3, ext4)
          ext2 = extent(ext12, ext22, ext32, ext42)
          pred_nf <<- crop(predictors, ext)
          pred_nf2 <<-  crop(predictors3, ext2)


          if (input$tipodadoabiotico!='Others')
          {
            #cat(paste("Estou aqui 2 ",'\n'))

          if (input$periodo != 'current')
          {
            pred_nffuturo <<- crop(predictorsfuturo,ext)
          }
          if (input$periodobiooracle != 'current')
          {
            pred_nffuturo <<- crop(predictorsfuturo,ext)
          }
          }

          presvals <<- raster::extract(pred_nf, especie)
          plot(pred_nf)
          cat(paste("Estou aqui 3 ",'\n'))

          ############################################
          ## APRESENTO A CORRELAO ENTRE AS VARIaVEIS
          ############################################

          backgr <- randomPoints(pred_nf, 300)

          ## Determina os nomes da colunas de coordenadas para os pontos de background
          colnames(backgr) = c('Longitude', 'Latitude')

          ## Extraindo os valores das vari?veis onde h? pseudoaus?ncias
          absvals <- raster::extract(pred_nf, backgr)

          if (length(arquivo)>1)
          {
            sdmdata <- data.frame(cbind(absvals))
            #sdmdata <- data.frame(cbind(presvals))
            output$grafico_correlacao <- renderPlot({
              pairs(sdmdata, cex=0.1, fig=TRUE, lower.panel = panel.smooth, diag.panel= panel.hist, upper.panel = panel.cor)
            })
            output$dgbriddadoscorrelacao <- renderDataTable({
              round(cor(sdmdata),2)
            })
          }
          else
          {
            output$grafico_correlacao <- renderPlot({
              plot(0, 0)
            })
          }
        }

      }

    }) #FIM ISOLATE
      incProgress(3/n, detail = paste0("Plotando..."))

    }
    )
  })



  datasetInput <- reactive({
    if (exists("especie"))
    {
      switch('especie',
             "especie" = especie)
    }

  })

  output$dgbriddadosdatacleaning = renderDataTable({
    input$btnapagar
    input$btneliminarduplicatas
    input$btnbuscarespecieCSV
    input$btnbuscarespeciejabot
    input$btnbuscarespecie

  if (is.null(especie))
  {
   n <- 0
  }
  n <- nrow(especie)
  if (n>0)
  {

    if (exists("especie"))
    {
      if (input$btneliminarduplicatas > 0)
      {
        progress <- shiny::Progress$new()
        progress$set(message = "Excluindo duplicatas...", value = 0)
        # Close the progress when this reactive exits (even if there's an error)
        on.exit(progress$close())
        especie <<- unique(especie)
      }
      isolate({
        input$edtelemento
        if (input$edtelemento!='0')
        {
          if (input$btnapagar == 0)
            return()
          especie <<- especie[-input$edtelemento,]
        }
        rownames(especie) <- NULL
        especie$id = 1:nrow(especie)
        especie
      })
      especie
    }
  } #IF

  }
  , options = list(searching = FALSE,lengthMenu = c(5, 30, 50), pageLength = 5)
  )


  output$mapadistribuicaodatacleaning <- renderLeaflet({

    #input$btnAtualizaMapaDataCleaning
    input$btnapagar
    input$btneliminarduplicatas
    input$btnbuscarespecieCSV
    input$btnbuscarespeciejabot
    input$btnbuscarespecie
    if (!is.null(especie))
    {
      if (exists("especie"))
    {
      rownames(especie) <- NULL
      especie$id = 1:nrow(especie)
      #%>% setView(lng = -31.5, lat = -13.4, zoom = 3)
#      map = leaflet(especie) %>% addTiles  %>% addCircles(color = "red", lat = ~ Latitude, lng = ~ Longitude)  %>% addMarkers(especie[,1], especie[,2]) %>% addMarkers(especie[,1], especie[,2],popup =~paste('<b><a onclick="document.getElementById(\'edtelemento\').value=',especie[,3],'">ID: ',especie[,3],'</a>') )
      map = leaflet(especie) %>% addTiles %>% addCircles(color = "red", lat = ~ Latitude, lng = ~ Longitude) %>% addMarkers(clusterOptions = markerClusterOptions())  %>% addMarkers(~Longitude,~Latitude,popup = ~as.character(id) )
      map
    }
    }
    else
    {
      showModal(modalDialog(
        title = "Warning",
        "Inform occurrence data",
        easyClose = TRUE
      ))

    }
  })

  pegaDadosCSV <- eventReactive(input$btnbuscarespecieCSV, {
    ETAPA <<- 1
    inFile <<- input$file1
    if (is.null(inFile))
    {
      return(NULL)
    }
    else
    {
      especie<<- read.csv(inFile$datapath, header=input$header, sep=input$sep,
                          quote=input$quote)

      arquivo_path <<- inFile$datapath
      arquivo_header <<- input$header
      arquivo_sep <<- input$sep
      arquivo_quote <<- input$quote
      especie <<- especie[,2:3]
    }
  })


  pegaDadosGBif <- eventReactive(input$btnbuscarespecie, {
    ETAPA <<- 1
    spname <<-input$edtespecie
    key <- name_backbone(name=input$edtespecie)$speciesKey
    especie <<- occ_search(taxonKey=key, return='data', limit=1000)
    especie <<- subset(especie, !is.na(decimalLongitude) & !is.na(decimalLatitude))
    especie <<- especie[, c(4,3)]
    names(especie)<<-c("Longitude", "Latitude")
    dados <- especie
    especie
  })

  pegaDadosJabot <- eventReactive(input$btnbuscarespeciejabot, {
    ETAPA <<- 1
    especie <<- getOcorrencia(input$edtespeciejabot)
    especie
  })


  output$dgbriddados <- renderDataTable({
    ETAPA <<- 1
    progress <- shiny::Progress$new()
    progress$set(message = "Localizando dados...", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())

    if(input$tipodado=="csv")
    {
      pegaDadosCSV()
    }
    else
    {
      if (input$tipodado=='jabot')
      {
        pegaDadosJabot()
      }
      else
      {
        pegaDadosGBif()
      }
    }
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5)
  )

#########################################
## DOWNLOADS
#########################################

output$downloadData <- downloadHandler(

  # This function returns a string which tells the client
  # browser what name to use when saving the file.
  filename = function() {
    paste('especie2', 'csv', sep = ".")
#    paste('Script', 'R', sep = ".")
  },

  # This function should write data to a file given to it by
  # the argument 'file'.
  content = function(file) {
#    file.copy('Script.R', file, overwrite = TRUE)

    # Write to a file specified by the 'file' argument
    write.table(especie, file, sep = ';', row.names = FALSE)
  }
)

output$downloadscript <- downloadHandler(
  filename = function() {
    paste('Script', 'R', sep = ".")
  },
  content = function(file) {
    file.copy('Script.R', file, overwrite = TRUE)
  }
)

  output$mapadistribuicao <- renderLeaflet({

    progress <- shiny::Progress$new()
    progress$set(message = "Atualizando o mapa...", value = 0)
    # Close the progress when this reactive exits (even if there's an error)
    on.exit(progress$close())

    input$btnbuscarespecieCSV
    input$btnbuscarespecie
    input$btnbuscarespeciejabot

    if (!is.null(especie))
    {
      if(input$tipodado=="gbif")
      {
        map = leaflet(especie) %>% addTiles  %>% addCircles(color = "red", lat = ~ Latitude, lng = ~ Longitude) %>% addMarkers(clusterOptions = markerClusterOptions()) %>% setView(lng = -31.5, lat = -13.4, zoom = 3)
        #%>% addMarkers(especie[,1], especie[,2])
      }
      else
      {
        if(input$tipodado=="csv")
        {
          map = leaflet(especie) %>% addTiles  %>% addCircles(color = "red", lat = ~ Latitude, lng = ~ Longitude) %>%
            setView(lng = -31.5, lat = -13.4, zoom = 1)
          #%>% addMarkers(especie[,1], especie[,2])
        }
        else
        {
          if(input$tipodado=="jabot")
          {
            map = leaflet(especie) %>% addTiles  %>% addCircles(color = "red", lat = ~ Latitude, lng = ~ Longitude) %>%
              setView(lng = -31.5, lat = -13.4, zoom = 1)
            #%>% addMarkers(especie[,1], especie[,2])
          }
        }
      }
      map
    }
    else
  {
    showModal(modalDialog(
      title = "Warning",
      "Inform occurrence data",
      easyClose = TRUE
    ))

  }
#    map
  })




	obs <- observe({

	isolate({ projeto <<- paste0('projeto/',input$edtprojeto)
	  ARQUIVO_SAIDA <<- paste0("www/",projeto,"/script.R")

	})
	if (input$btnconsultarprojeto>0)
	{
	  ## COLOCO AQUI TODAS AS FUNÇÃO PARA LISTAR OS DADOS DO PROJETO CONSULTADO
	  ## PRECISO FAZER UMA FUNÇÃO PARA NÃO FICAR DUPLICANDO ISSO

	  projeto <<- paste0('projeto/',input$edtprojeto)

	  output$dbgridresultado <- renderDataTable({
	    read.table(paste0("./www/",projeto,"/models/statsALL.txt"))
	  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 10)
	  )

	    output$ui <- renderUI({
	    lista_jpg <- list.files(paste0("www/",projeto,"/jpg"),full.names=F,pattern=paste0(".jpg"))
	    lapply(1:length(order(lista_jpg)), function(i) {
	      tags$a(href=paste0(home,projeto,'/jpg/',lista_jpg[i]), tags$img(src = paste0(projeto,'/jpg/',lista_jpg[i]), height = "200px"), target="_blank")
	    })
	  })

	    output$uifinal <- renderUI({
	      lista_modelsfinal <- list.files(paste0("www/",projeto,"/final"),full.names=F,pattern=paste0(".png"))
	      lapply(1:length(sort(lista_modelsfinal)), function(i) {
	        tags$a(href=paste0(home,projeto,'/final/',lista_modelsfinal[i]), tags$img(src = paste0(projeto,'/final/',lista_modelsfinal[i]), height = "200px"), target="_blank")

	      })
	    })


	    output$uiarquivosmodelos <- renderUI({
	      lista_models <- list.files(paste0("www/",projeto,"/models"),full.names=F,pattern=paste0("pre_"))
	      lapply(1:length(sort(lista_models)), function(i) {
	        tags$div(
	          tags$a(href=paste0(home,projeto,'/models/',lista_models[i]), paste0(lista_models[i]))
	        )
	      })
	    })



	    output$ui <- renderUI({
	      lista_jpg <- list.files(paste0("www/",projeto,"/jpg"),full.names=F,pattern=paste0(".jpg"))
	      lapply(1:length(order(lista_jpg)), function(i) {
	        tags$a(href=paste0(home,projeto,'/jpg/',lista_jpg[i]), tags$img(src = paste0(projeto,'/jpg/',lista_jpg[i]), height = "200px"), target="_blank")
	      })
	    })


	    output$uiscript <- renderUI({
	      lista_txt <- list.files(paste0("www/",projeto,"/"),full.names=F,pattern=paste0("Script.R"))
	      lapply(1:length(lista_txt), function(i) {
	        tags$div(
	          tags$a(href=paste0(home,projeto,'/',lista_txt[i]), paste0(lista_txt[i]), target="_blank")
	        )
	      })
	    })


	    output$uiestatistica <- renderUI({
	      lista_txt <- list.files(paste0("www/",projeto,"/models"),full.names=F,pattern=paste0("statsALL.txt"))
	      lapply(1:length(lista_txt), function(i) {
	        tags$div(
	          tags$a(href=paste0(home,projeto,'/models/',lista_txt[i]), paste0(lista_txt[i]), target="_blank")
	        )
	      })
	    })

	    output$uiarquivosdados <- renderUI({
	      lista_csv <- list.files(paste0("www/",projeto,"/csv"),full.names=F,pattern=paste0(".csv"))
	      lapply(1:length(lista_csv), function(i) {
	        tags$div(
	          tags$a(href=paste0(home,projeto,'/csv/',lista_csv[i]), paste0(lista_csv[i]), target="_blank")
	        )
	      })
	    })


	    output$uiarquivosensemble <- renderUI({
	      lista_final <- list.files(paste0("www/",projeto,"/final"),full.names=F,pattern=paste0(".tif"))
	      lapply(1:length(sort(lista_final)), function(i) {
	        tags$div(
	          tags$a(href=paste0(home,projeto,'/final/',lista_final[i]), paste0(lista_final[i]), target="_blank")
	        )
	      })
	    })


	    output$uiarquivosprojecao <- renderUI({
	      lista_proj <- list.files(paste0("www/",projeto,"/proj"),full.names=F,pattern=paste0(".tif"))
	      lapply(1:length(sort(lista_proj)), function(i) {
	        tags$div(
	          tags$a(href=paste0(home,projeto,'/proj/',lista_proj[i]), paste0(lista_proj[i]), target="_blank")
	        )
	      })
	    })

	    output$uiarquivosprojecaofuturo <- renderUI({
	      lista_futuro <- list.files(paste0("www/",projeto,"futuro"),full.names=F,pattern=paste0(".tif"))

	      lapply(1:length(sort(lista_futuro)), function(i) {
	        tags$div(
	          tags$a(href=paste0(home,projeto,'/futuro/',lista_futuro[i]), paste0(lista_futuro[i]), target="_blank")
	        )
	      })
	    })

	}

	if (input$btncriarprojeto>0)
	{

	  unlink(paste0("www/",projeto), recursive=TRUE)
	  cat(projeto, '\n', file = 'teste.txt', append = TRUE)

	  if (projeto!='projeto/')
	  {
	  withProgress(message = '', value = 0, {
	   n <- 6

		mkdirs(paste0("www/",projeto))
		mkdirs(paste0("www/",projeto,'/csv'))
		incProgress(1/n, detail = paste0("Criando pasta ",projeto))
		Sys.sleep(0.6)
		mkdirs(paste0("www/",projeto,'/final'))
		incProgress(2/n, detail = paste0("Criando pasta ",projeto,"/final"))
		Sys.sleep(0.6)
		mkdirs(paste0("www/",projeto,'/futuro'))
		incProgress(3/n, detail = paste0("Criando pasta ",projeto,"/futuro"))
		Sys.sleep(0.6)
		mkdirs(paste0("www/",projeto,'/jpg'))
		incProgress(4/n, detail = paste0("Criando pasta ",projeto,"/jpg"))
		Sys.sleep(0.6)
		mkdirs(paste0("www/",projeto,'/models'))
		incProgress(5/n, detail = paste0("Criando pasta ",projeto,"/models"))
		Sys.sleep(0.6)
		mkdirs(paste0("www/",projeto,'/proj'))
		incProgress(6/n, detail = paste0("Criando pasta ",projeto,"/proj"))
		Sys.sleep(0.6)
	  }) # progress

	    showModal(modalDialog(
	      title = "Atenção!",
	      paste0("Project succesfully created!"," Project directory: ", projeto) ,
	      easyClose = TRUE
	    ))



	  }





	}




  })

  mkdirs <- function(fp) {
    if(!file.exists(fp)) {
        mkdirs(dirname(fp))
		    dir.create(fp, showWarnings = FALSE, recursive = FALSE, mode = "777")
    }
  }

}
