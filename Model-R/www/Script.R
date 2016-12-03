library(maps)
library(dismo)
library(rgdal)
library(raster)
library(rgbif)
library(XML)
# INFORME O CAMINHO DA APLICACAO
path <- ""
file <- paste(path,"csv/dados.csv", sep="")
especie <- read.csv(file)
head(especie)
head(especie)

arquivo <- list()
# ADICIONE OS RASTERS
arquivo <- c(arquivo,paste('C:/Users/Rafael/Documents/Modelagem/R/ex/current/bio_10m_bil/bio1.bil',sep=''))
arquivo
predictors <- stack(arquivo)
pred_nf <- predictors
ext <- extent(-90,-33,-32,23)
pred_nf <- crop(pred_nf, ext)
plot(pred_nf, 1)
especie <<- especie[,2:3]
points(especie, bg='red', cex=1,pch=21)
presvals <- extract(pred_nf, especie)
var <- pred_nf
var2 <- pred_nf2
part <- 3
library(dismo)
library(randomForest)
library(kernlab)
library(XML)
library(raster)
library(rgdal)
library(maps)
coord<-especie
n<-nrow(coord)
presvals<-extract(var,coord)
backgr<-randomPoints(var,1000)
colnames(backgr)=c('Longitude','Latitude')
absvals <- extract(var,backgr)
pre_abs<-c(rep(1,nrow(presvals)),rep(0,nrow(absvals)))
if (n<10) part<-n else part <- part
group_pre <- kfold(coord,part)
group_abs <- kfold(backgr,part)
append_1 <- append(group_pre,group_abs)
cbind_1 <- cbind(coord,presvals)
cbind_2 <- cbind(backgr,absvals)
rbind_1 <- rbind(cbind_1,cbind_2)
sdmdata <- data.frame(cbind(append_1,pre_abs,rbind_1))
sdmdata2 <- sdmdata[-1]
sdmdata2 <- sdmdata2[-1]
sdmdata2 <- sdmdata2[-1]
sdmdata2 <- sdmdata2[-1]
colnames(sdmdata)[1] <- "group"
for (i in unique(group_pre)){
sdmdata_train <- subset(sdmdata,group!=i)
sdmdata_teste <- subset(sdmdata,group ==i)
envtrain <- subset(sdmdata_train,select= c(-group,-Longitude,-Latitude))
envtest_pre <- subset(sdmdata_teste,pre_abs==1,select= c(-group,-Latitude,-Latitude,-pre_abs))
envtest_abs <- subset(sdmdata_teste,pre_abs==0,select= c(-group,-Latitude,-Latitude,-pre_abs))
coord_pres_train <- subset(sdmdata_train,pre_abs==1,select= c(Longitude,Latitude))
coord_pres_teste <- subset(sdmdata_teste,pre_abs==1,select= c(Longitude,Latitude))
coord_abs_train <- subset(sdmdata_train,pre_abs==0,select= c(Longitude,Latitude))
coord_abs_teste <- subset(sdmdata_teste,pre_abs==0,select= c(Longitude,Latitude))
#BIOCLIM
 bc <- bioclim (var, coord_pres_train)
ebc <<- evaluate (coord_pres_teste,coord_abs_teste,bc,var)
bcTSS <- max(ebc@TPR + ebc@TNR)-1
tbc <- threshold (ebc,'spec_sens')
bc_cont <- predict (var,bc,progress='text')
bc_cont_proj <- predict (var2,bc,progress='text')
bc_bin <- bc_cont>tbc
 bc_mult <- bc_bin*bc_cont
bc_mult <- bc_mult/maxValue(bc_mult)
plot(bc_cont,main=paste("BioClim - ",i))
plot(bc_bin,main=paste("Bioclim - Bin ",i))
} # Fecha o for loop
