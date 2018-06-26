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

ipak <- function(pkg) {
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}

ipak(c(
  "shinydashboard",
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
  "data.table",
  "DT",
  "shinyjs",
  "sdmpredictors"
))

jdk_version <- list.files("/Library/Java/JavaVirtualMachines/")
if (length(jdk_version) != 0) {
  dyn.load(paste0("/Library/Java/JavaVirtualMachines/", jdk_version, "/Contents/Home/lib/server/libjvm.dylib"))
  library("rJava")
} else {
  library("rJava")
}

library("shinydashboard")
library("leaflet")
library("R.utils")
library("raster")
library("rjson")
library("maps")
library("rgdal")
library("dismo")
library("rgbif")
library("XML")
library("randomForest")
library("kernlab")
library("data.table")
library("DT")
library("sdmpredictors")
library("shinyjs")

rm(list = ls())
rm(list = setdiff(ls(), lsf.str()))

home <- "/"

t <- 7
ext11 <<- -90
ext21 <<- -33
ext31 <<- -32
ext41 <<- 23

ext12 <<- ext11
ext22 <<- ext21
ext32 <<- ext31
ext42 <<- ext41

ETAPA <<- 0

spname <<- ""

# Download and decompress maxent.jar file
jar <- paste0(system.file(package = "dismo"), "/java/maxent.jar")
if (file.exists(jar) != T) {
  url <- "http://biodiversityinformatics.amnh.org/open_source/maxent/maxent.php?op=download"
  download.file(url, destfile = "maxent.zip", mode = "wb")
  unzip("maxent.zip", files = "maxent.jar", exdir = system.file("java", package = "dismo"))
  unlink("maxent.zip")
}

panel.reg <- function(x, y, bg = NA, cex = 1, col.regres = "red", ...) {
  points(x, y, cex = cex)
  abline(stats::lm(y ~ x), col = col.regres, ...)
}

panel.cor <- function(x, y, digits = 2, prefix = "", ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  text(0.5, 0.5, txt, cex = 1.5)
}

panel.hist <- function(x, ...) {
  usr <- par("usr")
  on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5))
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts
  y <- y / max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "gray", ...)
}

mkdirs <- function(fp) {
  if (!file.exists(fp)) {
    mkdirs(dirname(fp))
    dir.create(fp, showWarnings = FALSE, recursive = FALSE, mode = "777")
  }
}

limparResultadosAnteriores <- function() ({
  lista <- list.files(paste0("www/", projeto, "/models/", full.names = T, pattern = paste0(".")))
  if (length(lista > 0)) {
    file.remove(paste0("www/", projeto, "/models/", lista))
  }
  lista <- list.files(paste0("www/", projeto, "/final/", full.names = T, pattern = paste0(".")))
  if (length(lista > 0)) {
    file.remove(paste0("www/", projeto, "/final/", lista))
  }
  lista <- list.files(paste0("www/", projeto, "/proj/", full.names = T, pattern = paste0(".")))
  if (length(lista > 0)) {
    file.remove(paste0("www/", projeto, "/proj/", lista))
  }
  lista <- list.files(paste0("www/", projeto, "/proj_time/", full.names = T, pattern = paste0(".")))
  if (length(lista > 0)) {
    file.remove(paste0("www/", projeto, "/proj_time/", lista))
  }
  lista <- list.files(paste0("www/", projeto, "/jpg/", full.names = T, pattern = paste0(".jpg")))
  if (length(lista > 0)) {
    file.remove(paste0("www/", projeto, "/jpg/", lista))
  }
})

getOccurrences_gbif <- function(spname) {
  key <- name_backbone(name = spname)$speciesKey
  if (!is.null(key)) {
    gbif_data <- occ_search(taxonKey = key, return = "data", limit = 1000)
    gbif_data <- subset(gbif_data, !is.na(decimalLongitude) & !is.na(decimalLatitude))
    gbif_data <- subset(gbif_data, (decimalLongitude != 0) & (decimalLatitude != 0))
    occur.data <- cbind(gbif_data$name, gbif_data$decimalLongitude, gbif_data$decimalLatitude)
    colnames(occur.data) <- c("Name", "Longitude", "Latitude")
    occur.data <- as.data.frame(occur.data)
    return(occur.data)
  } else {
    showModal(modalDialog(
      title = "No results!",
      paste0("Please insert a valid species scientific name."),
      easyClose = TRUE
    ))
  }
}

getOccurrences_jabot <- function(spname) {
  library("rjson")
  pTaxon <- gsub(" ", "_", spname)
  json_file <- paste0 ("https://model-r.jbrj.gov.br/execjabot.php?especie=", pTaxon)
  json_data <- fromJSON(file = json_file, method = "C")
  final_data <- do.call(rbind, json_data)
  jabot_data <- final_data[, c("taxoncompleto", "longitude", "latitude")]
  occur.data <- cbind(as.character(jabot_data[, 1]), as.numeric(jabot_data[, 2]), as.numeric(jabot_data[, 3]))
  colnames(occur.data) <- c("Name", "Longitude", "Latitude")
  return(occur.data)
  # else {
  #   showModal(modalDialog(
  #     title = "No results!",
  #     paste0("Please insert a valid species scientific name."),
  #     easyClose = TRUE
  #   ))
  # }
}

clean <- function(coord, abio) {
  if (dim(coord)[2] == 2) {
    if (exists("abio")) {
      mask = abio[[1]]
      cell <- cellFromXY(mask, coord)  # get the cell number for each point
      dup <- duplicated(cell)
      pts1 <- coord[!dup, ]  # select the records that are not duplicated
      pts1 <- pts1[!is.na(raster::extract(mask, pts1)), ]  #selecionando apenas pontos que tem valor de raster
      cat(dim(coord)[1] - dim(pts1)[1], "points removed\n")
      cat(dim(pts1)[1], "spatially unique points\n")
      names(pts1) = c("Longitude", "Latitude")
      return(pts1)
    } else (cat("Indicate the object with the predictive variables"))
  } else (stop("Coordinate table has more than two columns.\n This table should only have longitude and latitude in this order."))
}



options(shiny.maxRequestSize = 100 * 1024^2)
dirColors <- c(`1` = "#595490", `2` = "#527525", `3` = "#A93F35", `4` = "#BA48AA")

function(input, output, session) {
  occurrences <<- NULL
  library(maps)
  library(rgdal)
  library(raster)
  library(dismo)
  library(rgbif)
  library(XML)
  library(leaflet)
  
  dismo.mod <- function(
    sp,
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
    write.cont = T,
    bin = T,
    write.bin = T,
    mult = T,
    write.mult = T,
    TSS.value  =  0.2,
    
    future.model = F,
    future.raster = newdata,
    write.future = F,
    write.projecao = F) {
    library(dismo)
    library(randomForest)
    library(kernlab)
    library(XML)
    library(raster)
    library(rgdal)
    library(maps)
    
    unlink(paste0("www/", projeto, "/models/evaluate_.txt"), recursive = TRUE)
    unlink(paste0("www/", projeto, "/models/statsALL.txt"), recursive = TRUE)
    unlink(paste0("www/", projeto, "/models/evaluate_ALL_models.txt"), recursive = TRUE)
    
    isolate({
      print(date())
      cat(paste("Modeling", sp, "...", "\n"))
      
      ## PREPARING SDM DATA -----------------------------------------------------
      coord <- occurrences
      n <- nrow(coord)
      
      ## Extracting variables at presence points
      presvals <- raster::extract(var, coord)
      
      ## Fixing seed to ensure the same random points
      set.seed(seed)
      
      ## Generating random pseudo-absences
      backgr <- randomPoints(var, numpontos)
      colnames(backgr) <- c("Longitude", "Latitude")
      
      ## Extracting variables at background points
      absvals <- raster::extract(var, backgr)
      
      ## Creating vector corresponding to presence (1) and pseudo-absences (0) points respectively.
      pre_abs <- c(rep(1, nrow(presvals)), rep(0, nrow(absvals)))
      
      ## Number of partitions
      if (n < 10) {
        part <- n
      } else {
        part <- part
      }
      
      ## Setting seed to distribute the presences always to the same partitions
      set.seed(seed)
      
      ## Separating presences and pseudo-absences in groups of partitions
      group_pre <- kfold(coord, part)
      set.seed(seed)
      group_abs <- kfold(backgr, part)
      
      ## Grouping sdm data in a single data.frame
      append_1 <- append(group_pre, group_abs)
      cbind_1 <- cbind(coord, presvals)
      cbind_2 <- cbind(backgr, absvals)
      rbind_1 <- rbind(cbind_1, cbind_2)
      sdmdata <- data.frame(cbind(append_1, pre_abs, rbind_1))
      colnames(sdmdata)[1] <- "group"
      
      # Fitting a model per partition
      for (i in unique(group_pre)) {
        progress$set(message = paste("Processing models", i), value = 0)
        
        ## Separate the sdmdata between test and train groups
        sdmdata_train <- subset(sdmdata, group != i)
        sdmdata_teste <- subset(sdmdata, group == i)
        
        # Preparing sdmdata train to build model
        envtrain <- subset(sdmdata_train, select = c(-group, -Longitude, -Latitude))
        
        # Prepring test data to evaluate both presence only and presence/absence models
        envtest_pre <- subset(sdmdata_teste, pre_abs == 1, select = c(-group, -Latitude, -Latitude, -pre_abs))
        envtest_abs <- subset(sdmdata_teste, pre_abs == 0, select = c(-group, -Latitude, -Latitude, -pre_abs))
        
        # Separating the data (only coordinates) to run presence only algorithms
        coord_pres_train <- subset(sdmdata_train, pre_abs == 1, select = c(
          Longitude,
          Latitude
        ))
        coord_pres_teste <- subset(sdmdata_teste, pre_abs == 1, select = c(
          Longitude,
          Latitude
        ))
        coord_abs_train <- subset(sdmdata_train, pre_abs == 0, select = c(
          Longitude,
          Latitude
        ))
        coord_abs_teste <- subset(sdmdata_teste, pre_abs == 0, select = c(
          Longitude,
          Latitude
        ))
        
        ## FITTING THE MODELS -------------------------------------------------------------
        cat(paste("Modeling...", sp, "Partition", i, "\n"))
        if (Bioclim == T) {
          cat(paste("Bioclim", "\n"))
          bc <- bioclim(var, coord_pres_train)
          ebc <- dismo::evaluate(coord_pres_teste, coord_abs_teste, bc, var)
          bcTSS <- max(ebc@TPR + ebc@TNR) - 1
          tbc <- threshold(ebc, "spec_sens")
          bc_cont <- predict(var, bc, progress = "text")
          bc_cont_proj <- predict(var2, bc, progress = "text")
          bc_bin <- bc_cont > tbc
          bc_mult <- bc_bin * bc_cont
          bc_mult <- bc_mult / maxValue(bc_mult)
          
          if (future.model == T) {
            names(future.raster) <- names(pred_nf)
            bc_future <- predict(future.raster, bc, progress = "text")
            
            if (bin == T) {
              bc_future_bin <- bc_future > tbc
            }
            
            if (mult == T) {
              bc_future_mult <- bc_future_bin * bc_future
              bc_future_mult <- bc_future_mult / maxValue(bc_future_mult)
            }
          }
        }
        
        if (Domain == T) {
          cat(paste("Domain", "\n"))
          do <- domain(var, coord_pres_train)
          edo <- dismo::evaluate(coord_pres_teste, coord_abs_teste, do, var)
          doTSS <- max(edo@TPR + edo@TNR) - 1
          tdo <- threshold(edo, "spec_sens")
          do_cont <- predict(var, do, progress = "text")
          do_cont_proj <- predict(var2, do, progress = "text")
          do_bin <- do_cont > tdo
          do_mult <- do_bin * do_cont
          do_mult <- do_mult / maxValue(do_mult)
          if (future.model == T) {
            names(future.raster) <- names(pred_nf)
            do_future <- predict(future.raster, do, progress = "text")
            if (bin == T) {
              do_future_bin <- do_future > tdo
            }
            if (mult == T) {
              do_future_mult <- do_future_bin * do_future
              do_future_mult <- do_future_mult / maxValue(do_future_mult)
            }
          }
        }
        
        if (maxent == T) {
          cat(paste("maxent", "\n"))
          mx <- maxent(var, coord_pres_train)
          emx <- dismo::evaluate(coord_pres_teste, coord_abs_teste, mx, var)
          mxTSS <- max(emx@TPR + emx@TNR) - 1
          tmx <- threshold(emx, "spec_sens")
          mx_cont <- predict(var, mx, progress = "text")
          mx_cont_proj <- predict(var2, mx, progress = "text")
          mx_bin <- mx_cont > tmx
          mx_mult <- mx_bin * mx_cont
          mx_mult <- mx_mult / maxValue(mx_mult)
          if (future.model == T) {
            names(future.raster) <- names(pred_nf)
            mx_future <- predict(future.raster, mx, progress = "text")
            if (bin == T) {
              mx_future_bin <- mx_future > tmx
            }
            if (mult == T) {
              mx_future_mult <- mx_future_bin * mx_future
              mx_future_mult <- mx_future_mult / maxValue(mx_future_mult)
            }
          }
        }
        
        if (GLM == T) {
          cat(paste("GLM", "\n"))
          mglm <- glm(pre_abs ~ ., data = envtrain)
          eglm <- dismo::evaluate(envtest_pre, envtest_abs, mglm)
          glmTSS <- max(eglm@TPR + eglm@TNR) - 1
          tglm <- threshold(eglm, "spec_sens")
          glm_cont <- predict(var, mglm, progress = "text")
          glm_cont_proj <- predict(var2, mglm, progress = "text")
          glm_bin <- glm_cont > tglm
          glm_mult <- glm_bin * glm_cont
          glm_mult <- glm_mult / maxValue(glm_mult)
          if (future.model == T) {
            names(future.raster) <- names(pred_nf)
            glm_future <- predict(future.raster, mglm, progress = "text")
            if (bin == T) {
              glm_future_bin <- glm_future > tglm
            }
            if (mult == T) {
              glm_future_mult <- glm_future_bin * glm_future
              glm_future_mult <- glm_future_mult / maxValue(glm_future_mult)
            }
          }
        }
        
        if (RF == T) {
          cat(paste("RF", "\n"))
          rf1 <- randomForest
          rf1 <- randomForest(pre_abs ~ ., data = envtrain)
          erf1 <- dismo::evaluate(envtest_pre, envtest_abs, rf1)
          rfTSS1 <- max(erf1@TPR + erf1@TNR) - 1
          trf1 <- threshold(erf1, "spec_sens")
          rf1_cont <- predict(var, rf1, progress = "text")
          rf1_cont_proj <- predict(var2, rf1, progress = "text")
          rf1_bin <- rf1_cont > trf1
          rf1_mult <- rf1_bin * rf1_cont
          rf1_mult <- rf1_mult / maxValue(rf1_mult)
          if (future.model == T) {
            names(future.raster) <- names(pred_nf)
            rf1_future <- predict(future.raster, rf1, progress = "text")
            if (bin == T) {
              rf1_future_bin <- rf1_future > trf1
            }
            if (mult == T) {
              rf1_future_mult <- rf1_future_bin * rf1_future
              rf1_future_mult <- rf1_future_mult / maxValue(rf1_future_mult)
            }
          }
        }
        
        if (SVM == T) {
          cat(paste("SVM", "\n"))
          msvm <- ksvm(pre_abs ~ ., data = envtrain)
          esvm <- dismo::evaluate(envtest_pre, envtest_abs, msvm)
          svmTSS <- max(esvm@TPR + esvm@TNR) - 1
          tsvm <- threshold(esvm, "spec_sens")
          svm_cont <- predict(var, msvm, progress = "text")
          svm_cont_proj <- predict(var2, msvm, progress = "text")
          svm_bin <- svm_cont > tsvm
          svm_mult <- svm_bin * svm_cont
          svm_mult <- svm_mult / maxValue(svm_mult)
          if (future.model == T) {
            names(future.raster) <- names(pred_nf)
            svm_future <- predict(future.raster, msvm, progress = "text")
            if (bin == T) {
              svm_future_bin <- svm_future > tsvm
            }
            if (mult == T) {
              svm_future_mult <- svm_future_bin * svm_future
              svm_future_mult <- svm_future_mult / maxValue(svm_future_mult)
            }
          }
        }
        
        if (Mahal == T) {
          cat(paste("Mahal", "\n"))
          condicao_Mahal <- nrow(coord_pres_train) > length(names(var))
          if (condicao_Mahal == TRUE) {
            ma <- mahal(var, coord_pres_train)
            ema <- dismo::evaluate(coord_pres_teste, coord_abs_teste, ma, var)
            maTSS <- max(ema@TPR + ema@TNR) - 1
            tma <- threshold(ema, "spec_sens")
            ma_cont <- predict(var, ma, progress = "text")
            ma_cont_proj <- predict(var2, ma, progress = "text")
            ma_cont_invert <- ma_cont + (-1 * minValue(ma_cont))
            ma_bin <- ma_cont > tma
            ma_mult <- ma_bin * ma_cont_invert
            ma_mult <- ma_mult / maxValue(ma_mult)
            
            if (future.model == T) {
              names(future.raster) <- names(pred_nf)
              ma_future <- predict(future.raster, ma, progress = "text")
              ma_future_invert <- ma_future + (-1 * minValue(ma_future))
              if (bin == T) {
                ma_future_bin <- ma_future > tma
              }
              if (mult == T) {
                ma_future_mult <- ma_future_bin * ma_future_invert
                ma_future_mult <- ma_future_mult / maxValue(ma_future_mult)
              }
            }
          } else {
          }
        }
        
        ### WRITING THE MODELS -----------------------------------------------------
        if (write.cont == T) {
          cat(paste("Saving models...", sp, i, "\n"))
          
          if (Bioclim == T) {
            writeRaster(
              x = bc_cont,
              filename = paste0("./www/", projeto, "/models/pre_", i, "_bc_con", ".tif"),
              overwrite = T
            )
            png(filename = paste0("./www/", projeto, "/jpg/pre_", i, "_bc_con", ".jpg"))
            plot(bc_cont, main = paste("BioClim - ", i))
            dev.off()
            
            if (write.projecao == T) {
              writeRaster(
                x = bc_cont_proj,
                filename = paste0("./www/", projeto, "/proj/pre_", i, "_bc_con_proj", ".tif"),
                overwrite = T
              )
            }
            
            if (write.future == T) {
              writeRaster(
                x = bc_future,
                filename = paste0("./www/", projeto, "/proj_time/", timescale, i, "_bc_con", ".tif"),
                overwrite = T
              )
              png(filename = paste0("./www/", projeto, "/jpg/", timescale, i, "_bc_con", ".jpg"))
              plot(bc_future, main = paste("BioClim - ", timescale, i))
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
            
            if (write.projecao == T) {
              writeRaster(
                x = do_cont_proj,
                filename = paste0("./www/", projeto, "/proj/pre_", i, "_do_con_proj", ".tif"),
                overwrite = T
              )
            }
            
            if (write.future == T) {
              writeRaster(
                x = do_future,
                filename = paste0("./www/", projeto, "/proj_time/fut_", i, "_do_con", ".tif"),
                overwrite = T
              )
              png(filename = paste0("./www/", projeto, "/jpg/fut_", i, "_do_con", ".jpg"))
              plot(do_future, main = paste("Domain -", timescale, "", i))
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
            plot(mx_cont, main = paste("maxent - ", i))
            dev.off()
            
            if (write.projecao == T) {
              writeRaster(
                x = mx_cont_proj,
                filename = paste0("./www/", projeto, "/proj/pre_", i, "_mx_con_proj", ".tif"),
                overwrite = T
              )
            }
            
            if (write.future == T) {
              writeRaster(
                x = mx_future,
                filename = paste0("./www/", projeto, "/proj_time/fut_", i, "_mx_con", ".tif"),
                overwrite = T
              )
              png(filename = paste0("./www/", projeto, "/jpg/fut_", i, "_mx_con", ".jpg"))
              plot(mx_future, main = paste("maxent -", timescale, "", i))
              dev.off()
            }
          }
          
          if (GLM == T) {
            writeRaster(
              x = glm_cont,
              filename = paste0(
                "./www/", projeto,
                "/models/pre_", i, "_glm_con", ".tif"
              ),
              overwrite = T
            )
            png(filename = paste0(
              "./www/", projeto, "/jpg/pre_", i, "_glm_con",
              ".jpg"
            ))
            plot(glm_cont, main = paste("GLM - ", i))
            dev.off()
            if (write.projecao == T) {
              writeRaster(x = glm_cont_proj, filename = paste0(
                "./www/",
                projeto, "/proj/pre_", i, "_glm_con_proj", ".tif"
              ), overwrite = T)
            }
            if (write.future == T) {
              writeRaster(
                x = glm_future,
                filename = paste0(
                  "./www/", projeto,
                  "/proj_time/fut_", i, "_glm_con", ".tif"
                ), overwrite = T
              )
              png(filename = paste0(
                "./www/", projeto, "/jpg/fut_", i, "_glm_con",
                ".jpg"
              ))
              plot(glm_future, main = paste("GLM -", timescale, "", i))
              dev.off()
            }
          }
          
          if (RF == T) {
            writeRaster(x = rf1_cont, filename = paste0(
              "./www/", projeto,
              "/models/pre_", i, "_rf_con", ".tif"
            ), overwrite = T)
            png(filename = paste0(
              "./www/", projeto, "/jpg/pre_", i, "_rf_con",
              ".jpg"
            ))
            plot(rf1_cont, main = paste("RF - ", i))
            dev.off()
            if (write.projecao == T) {
              writeRaster(x = rf1_cont_proj, filename = paste0(
                "./www/",
                projeto, "/proj/pre_", i, "_rf_con_proj", ".tif"
              ), overwrite = T)
            }
            if (write.future == T) {
              writeRaster(x = rf1_future, filename = paste0(
                "./www/", projeto,
                "/proj_time/fut_", i, "_rf1_con", ".tif"
              ), overwrite = T)
              png(filename = paste0(
                "./www/", projeto, "/jpg/fut_", i, "_rf1_con",
                ".jpg"
              ))
              plot(rf1_future, main = paste("RF -", timescale, "", i))
              dev.off()
            }
          }
          
          if (SVM == T) {
            writeRaster(x = svm_cont, filename = paste0(
              "./www/", projeto,
              "/models/pre_", i, "_svm_con", ".tif"
            ), overwrite = T)
            png(filename = paste0(
              "./www/", projeto, "/jpg/pre_", i, "_svm_con",
              ".jpg"
            ))
            plot(svm_cont, main = paste("SVM - ", i))
            dev.off()
            if (write.projecao == T) {
              writeRaster(x = svm_cont_proj, filename = paste0(
                "./www/",
                projeto, "/proj/pre_", i, "_svm_con_proj", ".tif"
              ), overwrite = T)
            }
            if (write.future == T) {
              writeRaster(x = svm_future, filename = paste0(
                "./www/", projeto,
                "/proj_time/fut_", i, "_svm_con", ".tif"
              ), overwrite = T)
              png(filename = paste0(
                "./www/", projeto, "/jpg/fut_", i, "_svm_con",
                ".jpg"
              ))
              plot(svm_future, main = paste("SVM -", timescale, "", i))
              dev.off()
            }
          }
          
          if (Mahal == T && condicao_Mahal == TRUE) {
            writeRaster(x = ma_cont, filename = paste0(
              "./www/", projeto,
              "/models/pre_", i, "_ma_con", ".tif"
            ), overwrite = T)
            png(filename = paste0(
              "./www/", projeto, "/jpg/pre_", i, "_ma_con",
              ".jpg"
            ))
            plot(ma_cont, main = paste("Mahalanobis - ", i))
            dev.off()
            if (write.projecao == T) {
              writeRaster(x = ma_cont_proj, filename = paste0(
                "./www/", projeto,
                "/proj/pre_", i, "_ma_con_proj", ".tif"
              ), overwrite = T)
            }
            if (write.future == T) {
              writeRaster(x = ma_future, filename = paste0(
                "./www/", projeto,
                "/proj_time/fut_", i, "_ma_con", ".tif"
              ), overwrite = T)
              png(filename = paste0(
                "./www/", projeto, "/jpg/fut_", i, "_ma_con",
                ".jpg"
              ))
              plot(ma_future, main = paste("Mahalanobis -", timescale, "", i))
              dev.off()
            }
          }
        }
        
        # Binary models
        if (write.bin == T) {
          cat(paste("Saving binary models...", sp, i, "\n"))
          
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
                filename = paste0("./www/", projeto, "/proj_time/", timescale, i, "_bc_bin", ".tif"),
                overwrite = T
              )
            }
          }
          
          if (Domain == T) {
            writeRaster(x = do_bin, filename = paste0(
              "./www/", projeto,
              "/models/pre_", i, "_do_bin", ".tif"
            ), overwrite = T)
            png(filename = paste0(
              "./www/", projeto, "/jpg/pre_", i, "_do_bin",
              ".jpg"
            ))
            plot(do_bin, main = paste("Domain - Bin ", i))
            dev.off()
            if (write.future == T) {
              writeRaster(
                x = do_future_bin,
                filename = paste0("./www/", projeto, "/proj_time/", timescale, i, "_do_bin", ".tif"),
                overwrite = T
              )
            }
          }
          
          if (maxent == T) {
            writeRaster(x = mx_bin, filename = paste0(
              "./www/", projeto,
              "/models/pre_", i, "_mx_bin", ".tif"
            ), overwrite = T)
            png(filename = paste0(
              "./www/", projeto, "/jpg/pre_", i, "_mx_bin",
              ".jpg"
            ))
            plot(mx_bin, main = paste("maxent - Bin ", i))
            dev.off()
            if (write.future == T) {
              writeRaster(
                x = mx_future_bin,
                filename = paste0("./www/", projeto, "/proj_time/", timescale, i, "_mx_bin", ".tif"),
                overwrite = T
              )
            }
          }
          
          if (GLM == T) {
            writeRaster(x = glm_bin, filename = paste0(
              "./www/", projeto,
              "/models/pre_", i, "_glm_bin", ".tif"
            ), overwrite = T)
            png(filename = paste0("./www/", projeto, "/jpg/pre_", i, "_glm_bin", ".jpg"))
            plot(glm_bin, main = paste("GLM - Bin ", i))
            dev.off()
            if (write.future == T) {
              writeRaster(
                x = glm_future_bin,
                filename = paste0("./www/", projeto, "/proj_time/", timescale, i, "_glm_bin", ".tif"),
                overwrite = T
              )
            }
          }
          
          if (RF == T) {
            writeRaster(x = rf1_bin, filename = paste0(
              "./www/", projeto,
              "/models/pre_", i, "_rf_bin", ".tif"
            ), overwrite = T)
            png(filename = paste0(
              "./www/", projeto, "/jpg/pre_", i, "_rf_bin",
              ".jpg"
            ))
            plot(rf1_bin, main = paste("RF - Bin ", i))
            dev.off()
            if (write.future == T) {
              writeRaster(
                x = rf1_future_bin,
                filename = paste0("./www/", projeto, "/proj_time/", timescale, i, "_rf_bin", ".tif"),
                overwrite = T
              )
            }
          }
          
          if (SVM == T) {
            writeRaster(x = svm_bin, filename = paste0(
              "./www/", projeto,
              "/models/pre_", i, "_svm_bin", ".tif"
            ), overwrite = T)
            png(filename = paste0(
              "./www/", projeto, "/jpg/pre_", i, "_svm_bin",
              ".jpg"
            ))
            plot(svm_bin, main = paste("SVM - Bin ", i))
            dev.off()
            if (write.future == T) {
              writeRaster(
                x = svm_future_bin,
                filename = paste0("./www/", projeto, "/proj_time/", timescale, i, "_svm_bin", ".tif"),
                overwrite = T
              )
            }
          }
          
          if (Mahal == T && condicao_Mahal == TRUE) {
            writeRaster(x = ma_bin, filename = paste0(
              "./www/", projeto,
              "/models/pre_", i, "_ma_bin", ".tif"
            ), overwrite = T)
            png(filename = paste0(
              "./www/", projeto, "/jpg/pre_", i, "_ma_bin",
              ".jpg"
            ))
            plot(ma_bin, main = paste("Mahalanobis - Bin ", i))
            dev.off()
            if (write.future == T) {
              writeRaster(
                x = ma_future_bin,
                filename = paste0("./www/", projeto, "/proj_time/", timescale, i, "_ma_bin", ".tif"),
                overwrite = T
              )
            }
          }
        }
        
        ## Mult models
        if (write.mult == T) {
          if (Bioclim == T) {
            writeRaster(x = bc_mult, filename = paste0(
              "./www/", projeto,
              "/models/pre_", i, "_bc_mult", ".tif"
            ), overwrite = T)
            if (write.future == T) {
              writeRaster(x = bc_future_mult, filename = paste0(
                "./www/",
                projeto, "/proj_time/", timescale, i, "_bc_mult", ".tif"
              ), overwrite = T)
            }
          }
          if (Domain == T) {
            writeRaster(x = do_mult, filename = paste0(
              "./www/", projeto,
              "/models/pre_", i, "_do_mult", ".tif"
            ), overwrite = T)
            if (write.future == T) {
              writeRaster(x = do_future_mult, filename = paste0(
                "./www/",
                projeto, "/proj_time/", timescale, i, "_do_mult", ".tif"
              ), overwrite = T)
            }
          }
          if (maxent == T) {
            writeRaster(x = mx_mult, filename = paste0(
              "./www/", projeto,
              "/models/pre_", i, "_mx_mult", ".tif"
            ), overwrite = T)
            if (write.future == T) {
              writeRaster(x = mx_future_mult, filename = paste0(
                "./www/",
                projeto, "/proj_time/", timescale, i, "_mx_mult", ".tif"
              ), overwrite = T)
            }
          }
          if (GLM == T) {
            writeRaster(x = glm_mult, filename = paste0(
              "./www/", projeto,
              "/models/pre_", i, "_glm_mult", ".tif"
            ), overwrite = T)
            if (write.future == T) {
              writeRaster(x = glm_future_mult, filename = paste0(
                "./www/",
                projeto, "/proj_time/", timescale, i, "_glm_mult", ".tif"
              ), overwrite = T)
            }
          }
          if (RF == T) {
            writeRaster(x = rf1_mult, filename = paste0(
              "./www/", projeto,
              "/models/pre_", i, "_rf_mult", ".tif"
            ), overwrite = T)
            if (write.future == T) {
              writeRaster(x = rf1_future_mult, filename = paste0(
                "./www/",
                projeto, "/proj_time/", timescale, i, "_rf_mult", ".tif"
              ), overwrite = T)
            }
          }
          if (SVM == T) {
            writeRaster(x = svm_mult, filename = paste0(
              "./www/", projeto,
              "/models/pre_", i, "_svm_mult", ".tif"
            ), overwrite = T)
            if (write.future == T) {
              writeRaster(x = svm_future_mult, filename = paste0(
                "./www/",
                projeto, "/proj_time/", timescale, i, "_svm_mult", ".tif"
              ), overwrite = T)
            }
          }
          if (Mahal == T && condicao_Mahal == TRUE) {
            writeRaster(x = ma_mult, filename = paste0(
              "./www/", projeto,
              "/models/pre_", i, "_ma_mult", ".tif"
            ), overwrite = T)
            if (write.future == T) {
              writeRaster(x = ma_future_mult, filename = paste0(
                "./www/",
                projeto, "/proj_time/", timescale, i, "_ma_mult", ".tif"
              ), overwrite = T)
            }
          }
        }
        
        ### Saving validation files (evaluate.txt)
        cat(paste("Saving validation files...", sp, i, "\n"))
        sink(file = paste0("./www/", projeto, "/models/evaluate_", sp, ".txt"), split = T, append = T)
        
        if (Bioclim == T) {
          print(paste(sp, spname, i, "BioClim", round(ebc@auc, 3), round(
            bcTSS,
            3
          ), round(tbc, 3), round(threshold(ebc)$kappa, 3), round(
            threshold(ebc)$equal_sens_spec,
            3
          ), round(threshold(ebc)$no_omission, 3), round(
            threshold(ebc)$prevalence,
            3
          ), round(threshold(ebc)$sensitivity, 3), ebc@np, ebc@na, round(
            ebc@cor,
            3
          ), sep = ","))
        }
        
        if (Domain == T) {
          print(paste(sp, spname, i, "Domain", round(edo@auc, 3), round(
            doTSS,
            3
          ), round(tdo, 3), round(threshold(edo)$kappa, 3), round(
            threshold(edo)$equal_sens_spec,
            3
          ), round(threshold(edo)$no_omission, 3), round(
            threshold(edo)$prevalence,
            3
          ), round(threshold(edo)$sensitivity, 3), edo@np, edo@na, round(
            edo@cor,
            3
          ), sep = ","))
        }
        
        if (maxent == T) {
          print(paste(sp, spname, i, "maxent", round(emx@auc, 3), round(
            mxTSS,
            3
          ), round(tmx, 3), round(threshold(emx)$kappa, 3), round(
            threshold(emx)$equal_sens_spec,
            3
          ), round(threshold(emx)$no_omission, 3), round(
            threshold(emx)$prevalence,
            3
          ), round(threshold(emx)$sensitivity, 3), emx@np, emx@na, round(
            emx@cor,
            3
          ), sep = ","))
        }
        
        if (GLM == T) {
          print(paste(sp, spname, i, "GLM", round(eglm@auc, 3), round(
            glmTSS,
            3
          ), round(tglm, 3), round(threshold(eglm)$kappa, 3), round(
            threshold(eglm)$equal_sens_spec,
            3
          ), round(threshold(eglm)$no_omission, 3), round(
            threshold(eglm)$prevalence,
            3
          ), round(threshold(eglm)$sensitivity, 3), eglm@np, eglm@na, round(
            eglm@cor,
            3
          ), sep = ","))
        }
        
        if (RF == T) {
          print(paste(sp, spname, i, "RF", round(erf1@auc, 3), round(
            rfTSS1,
            3
          ), round(trf1, 3), round(threshold(erf1)$kappa, 3), round(
            threshold(erf1)$equal_sens_spec,
            3
          ), round(threshold(erf1)$no_omission, 3), round(
            threshold(erf1)$prevalence,
            3
          ), round(threshold(erf1)$sensitivity, 3), erf1@np, erf1@na, round(
            erf1@cor,
            3
          ), sep = ","))
        }
        
        if (SVM == T) {
          print(paste(sp, spname, i, "SVM", round(esvm@auc, 3), round(
            svmTSS,
            3
          ), round(tsvm, 3), round(threshold(esvm)$kappa, 3), round(
            threshold(esvm)$equal_sens_spec,
            3
          ), round(threshold(esvm)$no_omission, 3), round(
            threshold(esvm)$prevalence,
            3
          ), round(threshold(esvm)$sensitivity, 3), esvm@np, esvm@na, round(
            esvm@cor,
            3
          ), sep = ","))
        }
        
        if (Mahal == T && condicao_Mahal == TRUE) {
          print(paste(sp, sp, i, "Mahal", round(ema@auc, 3), round(maTSS, 3),
            round(tma, 3), round(threshold(ema)$kappa, 3), round(
              threshold(ema)$equal_sens_spec,
              3
            ), round(threshold(ema)$no_omission, 3), round(
              threshold(ema)$prevalence,
              3
            ), round(threshold(ema)$sensitivity, 3), ema@np, ema@na, round(
              ema@cor,
              3
            ),
            sep = ","
          ))
        }
        
        sink()
        
        sink(file = paste0("./www/", projeto, "/models/evaluate_ALL_models.txt"), split = T, append = T)
        
        if (Bioclim == T) {
          print(paste(sp, spname, i, "BioClim", round(ebc@auc, 3), round(
            bcTSS,
            3
          ), round(tbc, 3), round(threshold(ebc)$kappa, 3), round(
            threshold(ebc)$equal_sens_spec,
            3
          ), round(
            threshold(ebc)$no_omission,
            3
          ), round(
            threshold(ebc)$prevalence,
            3
          ), round(
            threshold(ebc)$sensitivity,
            3
          ), ebc@np, ebc@na, round(
            ebc@cor,
            3
          ), sep = ","))
        }
        
        if (Domain == T) {
          print(paste(sp, spname, i, "Domain", round(edo@auc, 3), round(
            doTSS,
            3
          ), round(tdo, 3), round(threshold(edo)$kappa, 3), round(
            threshold(edo)$equal_sens_spec,
            3
          ), round(threshold(edo)$no_omission, 3), round(
            threshold(edo)$prevalence,
            3
          ), round(threshold(edo)$sensitivity, 3), edo@np, edo@na, round(
            edo@cor,
            3
          ), sep = ","))
        }
        
        if (maxent == T) {
          print(paste(sp, spname, i, "maxent", round(emx@auc, 3), round(
            mxTSS,
            3
          ), round(tmx, 3), round(threshold(emx)$kappa, 3), round(
            threshold(emx)$equal_sens_spec,
            3
          ), round(threshold(emx)$no_omission, 3), round(
            threshold(emx)$prevalence,
            3
          ), round(threshold(emx)$sensitivity, 3), emx@np, emx@na, round(
            emx@cor,
            3
          ), sep = ","))
        }
        
        if (GLM == T) {
          print(paste(sp, spname, i, "GLM", round(eglm@auc, 3), round(
            glmTSS,
            3
          ), round(tglm, 3), round(threshold(eglm)$kappa, 3), round(
            threshold(eglm)$equal_sens_spec,
            3
          ), round(threshold(eglm)$no_omission, 3), round(
            threshold(eglm)$prevalence,
            3
          ), round(threshold(eglm)$sensitivity, 3), eglm@np, eglm@na, round(
            eglm@cor,
            3
          ), sep = ","))
        }
        
        if (RF == T) {
          print(paste(sp, spname, i, "RF", round(erf1@auc, 3), round(
            rfTSS1,
            3
          ), round(trf1, 3), round(threshold(erf1)$kappa, 3), round(
            threshold(erf1)$equal_sens_spec,
            3
          ), round(threshold(erf1)$no_omission, 3), round(
            threshold(erf1)$prevalence,
            3
          ), round(threshold(erf1)$sensitivity, 3), erf1@np, erf1@na, round(
            erf1@cor,
            3
          ), sep = ","))
        }
        
        if (SVM == T) {
          print(paste(sp, spname, i, "SVM", round(esvm@auc, 3), round(
            svmTSS,
            3
          ), round(tsvm, 3), round(threshold(esvm)$kappa, 3), round(
            threshold(esvm)$equal_sens_spec,
            3
          ), round(threshold(esvm)$no_omission, 3), round(
            threshold(esvm)$prevalence,
            3
          ), round(threshold(esvm)$sensitivity, 3), esvm@np, esvm@na, round(
            esvm@cor,
            3
          ), sep = ","))
        }
        
        if (Mahal == T && condicao_Mahal == TRUE) {
          print(paste(sp, spname, i, "Mahal", round(ema@auc, 3), round(
            maTSS,
            3
          ), round(tma, 3), round(threshold(ema)$kappa, 3), round(
            threshold(ema)$equal_sens_spec,
            3
          ), round(threshold(ema)$no_omission, 3), round(
            threshold(ema)$prevalence,
            3
          ), round(threshold(ema)$sensitivity, 3), ema@np, ema@na, round(
            ema@cor,
            3
          ), sep = ","))
        }
        
        sink()
        
        stats <- read.delim(
          file = paste0("./www/", projeto, "/models/evaluate_ALL_models.txt"),
          header = F,
          sep = ",",
          quote = "",
          col.names = c(
            "id", "sp", "part",
            "algorithm", "AUC", "TSS", "TSSth", "Kappa", "Equal_sens_spec",
            "No_omission", "Prevalence", "Sensitivity", "np", "na", "Cor"
          )
        )
        
        stats$Sensitivity <- as.numeric(sub(pattern = "\"", "", stats$Sensitivity))
        stats20 <- stats[order(stats$sp, stats$algorithm, stats$part), -1]
        write.table(stats20, paste0("./www/", projeto, "/models/statsALL.txt"))
        
        output$dbgridresultado <- renderDataTable({
          stats20
        }, options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
      }
      
      output$dbgridresultado <- renderDataTable({
        cat(c(date(), "Exhibiting stats20 results", "\n", "\n"))
        stats20
      }, options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
      
      # The sinked files are re-read and transformed into a proper data frame...
      
      cat(c(date(), " == ==  FIM == ==  ", "\n", "\n"))
      
      # WRITING ENSEMBLE FILES -------------------------------------------------
      conta_alg <- 0
      algoritmos <- ""
      
      if (input$BIOCLIM == TRUE) {
        conta_alg <- conta_alg + 1
        algoritmos <- paste(algoritmos, "Bioclim")
        bioclim_arquivos <- list.files(paste0("./www/", projeto, "/models/"), full.names = T, pattern = paste0("bc_con.tif"))
        bc_raster <- stack(bioclim_arquivos)
        ensemble.bc <- mean(bc_raster, bc_raster)
        writeRaster(ensemble.bc, filename = paste0("www/", projeto, "/final/", "bc_ensemble.tif"), format = "GTiff", overwrite = T)
        png(filename = paste0("./www/", projeto, "/jpg/bc_ensemble", ".jpg"))
        #dev.off()
        plot(ensemble.bc, main = paste("Bioclim - Ensemble"))
        points(occurrences, bg = "red", cex = 1, pch = 21)
        dev.off()
      }
      if (input$GLM == TRUE) {
        conta_alg <- conta_alg + 1
        algoritmos <- paste(algoritmos, "GLM")
        glm_arquivos <- list.files(paste0("./www/", projeto, "/models/"),
          full.names = T,
          pattern = paste0("glm_con.tif")
        )
        glm_raster <- stack(glm_arquivos)
        ensemble.glm <- mean(glm_raster, glm_raster)
        writeRaster(ensemble.glm, filename = paste0(
          "www/", projeto, "/final/",
          "glm_ensemble.tif"
        ), format = "GTiff", overwrite = T)
        png(filename = paste0("./www/", projeto, "/jpg/glm_ensemble", ".jpg"))
        plot(ensemble.glm, main = paste("GLM - Ensemble "))
        points(occurrences, bg = "red", cex = 1, pch = 21)
        dev.off()
      }
      if (input$RF == TRUE) {
        conta_alg <- conta_alg + 1
        algoritmos <- paste(algoritmos, "RF")
        rf_arquivos <- list.files(paste0("./www/", projeto, "/models/"), full.names = T, pattern = paste0("rf_con.tif"))
        rf_raster <- stack(rf_arquivos)
        ensemble.rf <- mean(rf_raster, rf_raster)
        writeRaster(ensemble.rf,
          filename = paste0(
            "www/", projeto, "/final/",
            "rf_ensemble.tif"
          ),
          format = "GTiff",
          overwrite = T
        )
        png(filename = paste0("./www/", projeto, "/jpg/rf_ensemble", ".jpg"))
        plot(ensemble.rf, main = paste("RF - Ensemble"))
        points(occurrences, bg = "red", cex = 1, pch = 21)
        dev.off()
      }
      if (input$DOMAIN == TRUE) {
        conta_alg <- conta_alg + 1
        algoritmos <- paste(algoritmos, "Domain")
        domain_arquivos <- list.files(paste0("./www/", projeto, "/models/"),
          full.names = T, pattern = paste0("do_con.tif")
        )
        do_raster <- stack(domain_arquivos)
        ensemble.do <- mean(do_raster, do_raster)
        writeRaster(ensemble.do, filename = paste0(
          "www/", projeto, "/final/",
          "do_ensemble.tif"
        ), format = "GTiff", overwrite = T)
        png(filename = paste0("./www/", projeto, "/jpg/do_ensemble", ".jpg"))
        plot(ensemble.do, main = paste("Domain - Ensemble"))
        points(occurrences, bg = "red", cex = 1, pch = 21)
        dev.off()
      }
      if (input$MAHALANOBIS == TRUE) {
        conta_alg <- conta_alg + 1
        algoritmos <- paste(algoritmos, "Mahalanobis")
        maha_arquivos <- list.files(paste0("./www/", projeto, "/models/"),
          full.names = T,
          pattern = paste0("ma_con.tif")
        )
        ma_raster <- stack(maha_arquivos)
        ensemble.ma <- mean(ma_raster, ma_raster)
        writeRaster(ensemble.ma, filename = paste0(
          "www/", projeto, "/final/",
          "ma_ensemble.tif"
        ), format = "GTiff", overwrite = T)
        png(filename = paste0("./www/", projeto, "/jpg/ma_ensemble", ".jpg"))
        plot(ensemble.ma, main = paste("MAHALANOBIS - Ensemble"))
        points(occurrences, bg = "red", cex = 1, pch = 21)
        dev.off()
      }
      if (input$SVM == TRUE) {
        conta_alg <- conta_alg + 1
        algoritmos <- paste(algoritmos, "SVM")
        svm_arquivos <- list.files(paste0("./www/", projeto, "/models/"),
          full.names = T,
          pattern = paste0("svm_con.tif")
        )
        svm_raster <- stack(svm_arquivos)
        ensemble.svm <- mean(svm_raster, svm_raster)
        writeRaster(ensemble.svm, filename = paste0(
          "www/", projeto, "/final/",
          "svm_ensemble.tif"
        ), format = "GTiff", overwrite = T)
        png(filename = paste0("./www/", projeto, "/jpg/svm_ensemble", ".jpg"))
        plot(ensemble.svm, main = paste("SVM - Ensemble"))
        points(occurrences, bg = "red", cex = 1, pch = 21)
        dev.off()
      }
      if (input$MAXENT == TRUE) {
        conta_alg <- conta_alg + 1
        algoritmos <- paste(algoritmos, "maxent")
        mx_arquivos <- list.files(paste0("./www/", projeto, "/models/"),
          full.names = T,
          pattern = paste0("mx_con.tif")
        )
        mx_raster <- stack(mx_arquivos)
        ensemble.mx <- mean(mx_raster, mx_raster)
        writeRaster(ensemble.mx,
          filename = paste0("www/", projeto, "/final/", "mx_ensemble.tif"),
          format = "GTiff",
          overwrite = T
        )
        png(filename = paste0("./www/", projeto, "/jpg/mx_ensemble", ".jpg"))
        plot(ensemble.mx, main = paste("MAXENT - Ensemble"))
        points(occurrences, bg = "red", cex = 1, pch = 21)
        dev.off()
      }
      
      # Final Ensemble
      ensemble_arquivos <- list.files(paste0("./www/", projeto, "/final/"),
        full.names = T,
        pattern = paste0("ensemble.tif")
      )
      
      if (conta_alg > 1) {
        ensemble_raster <- stack(ensemble_arquivos)
        ensemble.geral <- mean(ensemble_raster, ensemble_raster)
        writeRaster(ensemble.geral,
          filename = paste0("www/", projeto, "/final/", "ensemble_geral.tif"),
          format = "GTiff",
          overwrite = T
        )
        png(filename = paste0("./www/", projeto, "/jpg/ensemble_geral", ".jpg"))
        plot(ensemble.geral, main = paste("Ensemble ", algoritmos))
        points(occurrences, bg = "red", cex = 1, pch = 21)
        dev.off()
      }
      
      # Future/Past proj.
      if (future.model == T) {
        ensemble_futuro_arquivos <- list.files(paste0("./www/", projeto, "/proj_time/"),
          full.names = T,
          pattern = paste0("Fut.*_con")
        )
        
        ensemble_past_arquivos <- list.files(paste0("./www/", projeto, "/proj_time/"),
          full.names = T,
          pattern = paste0("Past.*_con")
        )
        
        if (length(ensemble_futuro_arquivos) > 0) {
          ensemble_futuro_raster <- stack(ensemble_futuro_arquivos)
          ensemble_futuro.geral <- mean(ensemble_futuro_raster, ensemble_futuro_raster)
          writeRaster(ensemble_futuro.geral,
            filename = paste0("www/", projeto, "/final/", "ensemble_futuro_geral.tif"),
            format = "GTiff",
            overwrite = T
          )
          png(filename = paste0("./www/", projeto, "/jpg/ensemble_future", ".jpg"))
          plot(ensemble_futuro.geral, main = paste("Ensemble - Future proj."))
          dev.off()
        }
        
        if (length(ensemble_past_arquivos) > 0) {
          ensemble_past_raster <- stack(ensemble_past_arquivos)
          ensemble_past.geral <- mean(ensemble_past_raster, ensemble_past_raster)
          writeRaster(ensemble_past.geral,
            filename = paste0("www/", projeto, "/final/", "ensemble_past_geral.tif"),
            format = "GTiff",
            overwrite = T
          )
          png(filename = paste0("./www/", projeto, "/jpg/ensemble_past", ".jpg"))
          plot(ensemble_past.geral, main = paste("Ensemble - Past  proj."))
          dev.off()
        }
      }
      
      # Geographic proj.
      if (write.projecao == T) {
        ensemble_arquivos_projecao <- list.files(paste0("./www/", projeto, "/proj/"),
          full.names = T, pattern = paste0("proj.tif")
        )
        ensemble_raster_projecao <- stack(ensemble_arquivos_projecao)
        ensemble.projecao <- mean(ensemble_raster_projecao, ensemble_raster_projecao)
        writeRaster(ensemble.projecao,
          filename = paste0("www/", projeto, "/final/", "proj_ensemble.tif"),
          format = "GTiff",
          overwrite = T
        )
        png(filename = paste0("./www/", projeto, "/jpg/ensemble_projecao", ".jpg"))
        plot(ensemble.projecao, main = paste("Ensemble - Geo. proj."))
        dev.off()
      }
    })
    
    # Select partitions TSS
    library("data.table")
    cat(paste("Reading evaluation files", "\n"))
    evall3 <- list.files(path = paste0("./www/", projeto, "/models"), pattern = paste0("statsALL.txt"), full.names = T)
    lista3 <- list()
    for (i in 1:length(evall3)) {
      lista3[[i]] <- read.table(file = evall3[i], header = T, row.names = 1)
    }
    
    stats3 <- rbindlist(lista3)
    stats3 <- as.data.frame(stats3)
    algoritmos <- unique(stats3$algorithm)
    for (algo in algoritmos) {
      stats2 <- stats3[stats3$algorithm == algo, ]
      if (algo == "BioClim") {
        algo <- "bc"
      }
      if (algo == "GLM") {
        algo <- "glm"
      }
      if (algo == "SVM") {
        algo <- "svm"
      }
      if (algo == "Mahal") {
        algo <- "ma"
      }
      if (algo == "maxent") {
        algo <- "mx"
      }
      if (algo == "RF") {
        algo <- "rf"
      }
      if (algo == "Domain") {
        algo <- "do"
      }
      
      part <- nrow(stats2)
      cat(paste("Reading models from .tif files", "\n"))
      modelos <- list.files(
        path = paste0("./www/", projeto, "/models"), full.names = T,
        pattern = paste0(algo, "_con")
      )
      mod <- stack(modelos) # (0)
      names(mod) <- paste0("Partition", 1:part)
      
      # Binary by TSSth and Cut
      bin <- mod > stats2[, names(stats2) == "Equal_sens_spec"] # stack
      cut <- bin * mod # stack
      sel.index <- which(stats2[, "TSS"] >= TSS.value)
      mod.sel <- mod[[sel.index]]
      
      if (length(sel.index) == 0) {
        cat(paste("No partition was selected for", "\n"))
      }
      
      if (length(sel.index) > 0) {
        mod.sel <- mod[[sel.index]] # (1)
        bin.sel <- mod.sel > stats2[, names(stats2) == "Equal_sens_spec"][sel.index] # (5)
        cut.sel <- bin.sel * mod.sel # (8)
        th.mean <- mean(stats2[, names(stats2) == "Equal_sens_spec"][sel.index])
      }
      
      # In case just one partition is selected, several models are the same
      if (length(sel.index) == 1) {
        cat(paste(length(sel.index), "partition was selected for", sp))
        final.sel.cont <- mod.sel # (1)(2)
        final.sel.bin <- bin.sel # (5)(3)(7) (8)
        final.sel.cut <- cut.sel # (4)(6)(9)(10)
        final <- stack(
          mod.sel, bin.sel, cut.sel, bin.sel, bin.sel, cut.sel,
          cut.sel
        )
        names(final) <- c(
          "2_Final_cont_mean_", "3_Final_bin_mean_", "4_Final_cut_mean_",
          "7_Final_mean_bin_", "8_Final_inter_bin_", "9_Mean_cut_sel_", "10_inter_cut_sel_"
        )
      }
      
      if (length(sel.index) > 1) {
        cat(paste(length(sel.index), "partitions were selected for","\n"))
        final.cont.mean <- mean(mod.sel) # (2)
        final.bin.mean <- (final.cont.mean > th.mean) # (3)
        final.cut.mean <- final.bin.mean * final.cont.mean # (4)
        final.sel.bin <- mean(bin.sel) # (7)
        final.inter <- prod(bin.sel) # (8)
        mean.cut.sel <- mean(cut.sel) # (9)
        inter.cut.sel <- prod(cut.sel) # (10)
        final <- stack(
          final.cont.mean, final.bin.mean, final.cut.mean, final.sel.bin,
          final.inter, mean.cut.sel, inter.cut.sel
        )
        names(final) <- c(
          "2_Final.cont.mean_", "3_Final.bin.mean_", "4_Final.cut.mean_",
          "7_Final.mean.bin_", "8_Final.inter.bin_", "9_Mean.cut.sel_", "10_inter.cut.sel_"
        )
        
        writeRaster(x = final.cont.mean, filename = paste0(
          "./www/", projeto,
          "/final", "/2_Final_cont_mean_", algo
        ), overwrite = T, format = "GTiff")
        writeRaster(x = final.bin.mean, filename = paste0(
          "./www/", projeto,
          "/final", "/3_Final_bin_mean_", algo
        ), overwrite = T, format = "GTiff")
        writeRaster(x = final.cut.mean, filename = paste0(
          "./www/", projeto,
          "/final", "/4_Final_cut_mean_", algo
        ), overwrite = T, format = "GTiff")
        writeRaster(x = final.sel.bin, filename = paste0(
          "./www/", projeto, "/final",
          "/7_Final_mean_bin_", algo
        ), overwrite = T, format = "GTiff")
        writeRaster(x = final.inter, filename = paste0(
          "./www/", projeto, "/final",
          "/8_Final_inter_bin_", algo
        ), overwrite = T, format = "GTiff")
        writeRaster(x = mean.cut.sel, filename = paste0(
          "./www/", projeto, "/final",
          "/9_Mean_cut_sel_", algo
        ), overwrite = T, format = "GTiff")
        writeRaster(x = inter.cut.sel, filename = paste0(
          "./www/", projeto, "/final",
          "/10_inter_cut_sel_", algo
        ), overwrite = T, format = "GTiff")
      }
      # Writes mean binary of the selected partitions
      if (exists("final")) {
        for (i in 1:dim(final)[[3]]) {
          png(filename = paste0("./www/", projeto, "/final", "/", names(final)[i], algo, ".png"))
          plot(final[[i]], main = paste0(names(final)[i], algo))
          dev.off()
        }
      }
    }
  }
  
  # MODELING FUNCTION ----------------------------------------------------------
  modelagem <- function() ({
    limparResultadosAnteriores()
    library(raster)
    
    write.projecao <- FALSE
    if (input$project_ext == T) {
      write.projecao <- T
    }
    
    future.model <- FALSE
    if (write_timeproj == T) {
      future.model <- TRUE
    }
    occ_points <- coordinates(occurrences)
    occurrences <- clean(coord=occ_points, abio = pred_nf[[1]])
    write.table(occurrences, file=paste0(getwd(),"/www/",projeto,"/csv/occurrences.csv"), append = FALSE, col.names = TRUE, row.names = FALSE)
    
    dismo.mod(
      "", occurrences, pred_nf, pred_nf2,
      input$MAXENT, input$BIOCLIM, input$GLM, input$RF, input$SVM, input$MAHALANOBIS, input$DOMAIN, input$SVM2,
      input$edtnumgrupo, input$edtnumpontos, 123, T, T, T, F, F, input$edtTSS,
      future.model, pred_nffuturo, future.model,
      write.projecao
    )
    
    progress$set(message = "Saving output data...", value = 0)
    
    ## Function to plot model ensemble map of selected algorithm
    maparesultado_model <- function(
      model_ensemble = model_ensemble,
      model_title = model_title) {
      if (file.exists(paste0("www/", projeto, "/final/", model_ensemble, ".tif"))) {
        r <- raster::raster(paste0("www/", projeto, "/final/", model_ensemble, ".tif"))
        pal <- colorNumeric(c("#FFFFFF", "#FDBB84", "#31A354"), values(r), na.color = "transparent")
        occ_points <<- coordinates(occurrences)
        lng <<- occ_points[,1]
        lat <<-  occ_points[,2]
        map <- leaflet() %>%
          addTiles() %>%
          addRasterImage(r, colors = pal, opacity = 0.7) %>%
          addLegend(pal = pal, values = values(r), title = model_title) %>%
          addCircles(color = "red",lat = lat, lng =lng, weight = 2, fill = TRUE) %>%
          addRectangles(ext11, ext31, ext21, ext41, color = "red", fill = FALSE, dashArray = "5,5", weight = 2)
      }
    }
    
    ## Function to plot geographic projection of the ensemble maps
    maparesultado_model_proj <- function() {
      if (file.exists(paste0("www/", projeto, "/final/proj_ensemble.tif")) && input$project_ext == TRUE) {
        rproj <- raster::raster(paste0("www/", projeto, "/final/proj_ensemble.tif"))
        palproj <- colorNumeric (c("#FFFFFF", "#FDBB84", "#31A354"), values(rproj), na.color = "transparent")
        occ_points <<- coordinates(occurrences)
        lng <<- occ_points[,1]
        lat <<- occ_points[,2]
        map_proj <- leaflet() %>%
          addTiles() %>%
          addRasterImage(rproj, colors = palproj, opacity = 0.7) %>%
          addLegend(pal = palproj, values = values(rproj), title = "") %>%
          addCircles(color = "red", lat = lat, lng = lng, weight = 2, fill = TRUE) %>%
          addRectangles(ext12, ext32, ext22, ext42, color = "green", fill = FALSE, dashArray = "5,5", weight = 2)
      }
    }
    
    output$maparesultadomax <- renderLeaflet({
      input$btnModelar
      maparesultado_model(model_ensemble = "mx_ensemble", model_title = "Maxent")
    })
    
    output$maparesultadosvm <- renderLeaflet({
      input$btnModelar
      maparesultado_model(model_ensemble = "svm_ensemble", model_title = "SVM")
    })
    
    output$maparesultadomh <- renderLeaflet({
      input$btnModelar
      maparesultado_model(model_ensemble = "ma_ensemble", model_title = "Mahal")
    })
    
    output$maparesultadorf <- renderLeaflet({
      input$btnModelar
      maparesultado_model(model_ensemble = "rf_ensemble", model_title = "RF")
    })
    
    output$maparesultadoglm <- renderLeaflet({
      input$btnModelar
      maparesultado_model(model_ensemble = "glm_ensemble", model_title = "GLM")
    })
    
    output$maparesultadobc <- renderLeaflet({
      input$btnModelar
      maparesultado_model(model_ensemble = "bc_ensemble", model_title = "BioClim")
    })
    
    output$maparesultadodo <- renderLeaflet({
      input$btnModelar
      maparesultado_model(model_ensemble = "do_ensemble", model_title = "Domain")
    })
    
    #### Exhibit geo. projected model ensemble ####
    output$maparesultado_proj <- renderLeaflet({
      input$btnModelar
      maparesultado_model_proj()
    })
    
    #### Display results at the Outputs tab ####
    output$uiarquivosmodelos <- renderUI({
      lista_models <- list.files(paste0("www/", projeto, "/models"),
        full.names = F,
        pattern = paste0("pre_")
      )
      lapply(1:length(sort(lista_models)), function(i) {
        tags$div(tags$a(
          href = paste0(home, projeto, "/models/", lista_models[i]),
          paste0(lista_models[i])
        ))
      })
    })
    
    output$ui <- renderUI({
      lista_jpg <- list.files(paste0("www/", projeto, "/jpg"),
        full.names = F,
        pattern = paste0(".jpg")
      )
      lapply(1:length(order(lista_jpg)), function(i) {
        tags$a(href = paste0(home, projeto, "/jpg/", lista_jpg[i]), tags$img(src = paste0(
          projeto,
          "/jpg/", lista_jpg[i]
        ), height = "200px"), target = "_blank")
      })
    })
    
    output$uiscript <- renderUI({
      lista_txt <- list.files(paste0("www/", projeto, "/"), full.names = F, pattern = paste0("script.R"))
      lapply(1:length(lista_txt), function(i) {
        tags$div(tags$a(
          href = paste0(home, projeto, "/", lista_txt[i]), paste0(lista_txt[i]),
          target = "_blank"
        ))
      })
    })
    
    output$uiestatistica <- renderUI({
      lista_txt <- list.files(paste0("www/", projeto, "/models"),
        full.names = F,
        pattern = paste0("statsALL.txt")
      )
      lapply(1:length(lista_txt), function(i) {
        tags$div(tags$a(
          href = paste0(home, projeto, "/models/", lista_txt[i]),
          paste0(lista_txt[i]), target = "_blank"
        ))
      })
    })
    
    output$uiarquivosdados <- renderUI({
      lista_csv <- list.files(paste0("www/", projeto, "/csv"),
        full.names = F,
        pattern = paste0(".csv")
      )
      lapply(1:length(lista_csv), function(i) {
        tags$div(tags$a(
          href = paste0(home, projeto, "/csv/", lista_csv[i]),
          paste0(lista_csv[i]), target = "_blank"
        ))
      })
    })
    
    output$uiarquivosensemble <- renderUI({
      lista_final <- list.files(paste0("www/", projeto, "/final"),
        full.names = F,
        pattern = paste0(".tif")
      )
      lapply(1:length(sort(lista_final)), function(i) {
        tags$div(tags$a(
          href = paste0(home, projeto, "/final/", lista_final[i]),
          paste0(lista_final[i]), target = "_blank"
        ))
      })
    })
    
    output$uiarquivosprojecao <- renderUI({
      lista_proj <- list.files(paste0("www/", projeto, "/proj"),
        full.names = F,
        pattern = paste0(".tif")
      )
      lapply(1:length(sort(lista_proj)), function(i) {
        tags$div(tags$a(
          href = paste0(home, projeto, "/proj/", lista_proj[i]),
          paste0(lista_proj[i]), target = "_blank"
        ))
      })
    })
    
    output$uiarquivosprojecaofuturo <- renderUI({
      lista_futuro <- list.files(paste0("www/", projeto, "/proj_time"),
        full.names = F,
        pattern = paste0(".tif")
      )
      lapply(1:length(sort(lista_futuro)), function(i) {
        tags$div(tags$a(
          href = paste0(home, projeto, "/proj_time/", lista_futuro[i]),
          paste0(lista_futuro[i]), target = "_blank"
        ))
      })
    })
  })
  
  #### GROUPING ALL MODELING PROCESSES BY CLICKING THE EXECUTE BUTTON ####
  observeEvent(input$btnModelar, {
    if ((input$DOMAIN == "TRUE") || (input$MAXENT == "TRUE") ||
        (input$BIOCLIM == "TRUE") || (input$GLM == "TRUE") ||
        (input$RF == "TRUE") || (input$SVM == "TRUE") ||
        (input$GLM == "TRUE")) {
      
      if (ETAPA > 1) {
        if (exists("occurrences")) {
          progress <<- shiny::Progress$new()
          progress$set(message = "Processing...", value = 0)
          on.exit(progress$close()) 
          modelagem()
        }else{
          showModal(modalDialog(
            title = "Error!",
            "Please inform species occurrence data.",
            easyClose = TRUE
          ))
        }
      }
    }
  })
  
  #### Model Extent ####
  
  output$mapapontosextend <- renderLeaflet({
    input$btnapagar
    input$btneliminarduplicatas
    input$btnsearch_spdatacsv
    input$btnsearch_spdata
    
    if (!is.null(occurrences)) {
      ext11 <<- input$edtextend1
      ext31 <<- input$edtextend3
      ext21 <<- input$edtextend2
      ext41 <<- input$edtextend4
      
      ext12 <<- input$edtextend1
      ext32 <<- input$edtextend3
      ext22 <<- input$edtextend2
      ext42 <<- input$edtextend4
      
      occurrences <<- occurrences
      map <- leaflet(occurrences) %>%
        addTiles() %>%
        addMarkers(clusterOptions = markerClusterOptions()) %>%
        addMarkers(~ Longitude, ~ Latitude) %>%
        addRectangles(ext11,
          ext31, ext21, ext41,
          color = "red",
          fill = TRUE, dashArray = "5,5", weight = 3)
      map
    }
  })
  
  #### Geographic projection area ####

  output$mapapontosextend2 <- renderLeaflet({
    input$btnapagar
    input$btneliminarduplicatas
    input$btnsearch_spdatacsv
    input$btnsearch_spdata
    if (!is.null(occurrences)) {
      ext12 <<- input$edtextend12
      ext32 <<- input$edtextend32
      ext22 <<- input$edtextend22
      ext42 <<- input$edtextend42
      
      occurrences <<- occurrences
      map <- leaflet(occurrences) %>%
        addTiles() %>%
        addMarkers(clusterOptions = markerClusterOptions()) %>%
        addMarkers(~ Longitude, ~ Latitude) %>%
        addRectangles(ext12,
          ext32, ext22, ext42,
          color = "green", fill = TRUE, dashArray = "5,5",
          weight = 3
        )
      map
    }
  })
  
  ######### ENVIRONMENTAL VARIABLES #########
  observeEvent(input$btnAtualizaSelecaoVariaveis, {
    environmental_data <- reactiveValues(
      write_timeproj = FALSE,
      data_current = list(),
      data_timeproj = list()
    )
    
    ##  WorldClim variables 
    if (input$tipodadoabiotico == "CLIMA") {
      vars_selection <<- paste(input$pred_vars_wc)
      path_current <- paste0(getwd(), "/ex/clima/current/", input$resolution)
      checkfiles <- list()
      
      if (length(vars_selection) >= 1) {
        for (i in c(1:length(input$pred_vars_wc))) {
          layer <- paste0(path_current, "/", vars_selection[i], ".bil")
          environmental_data$data_current <- c(environmental_data$data_current, layer)
          checkfiles <- c(checkfiles, file.exists(layer))
        }
        
        # Check/Download layers - Current conditions
        if (length(vars_selection) >= 1 && any(checkfiles == F)) {
          if (input$resolution != "30s") {
            zip_current <- paste0("bio_", input$resolution, "_bil.zip")
            url <- paste0(
              "http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/",
              zip_current
            )
            download.file(url, zip_current, mode = "wb")
            unzip(zip_current, exdir = path_current)
            unlink(zip_current)
          }
          
          if (input$resolution == "30s") {
            group1 <- c(
              "bio1",
              "bio2",
              "bio3",
              "bio4",
              "bio5",
              "bio6",
              "bio7",
              "bio8",
              "bio9"
            )
            group2 <- c(
              "bio10",
              "bio11",
              "bio12",
              "bio13",
              "bio14",
              "bio15",
              "bio16",
              "bio17",
              "bio18",
              "bio19"
            )
            
            if (any(group1 %in% input$pred_vars_wc)) {
              download.file("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/bio1-9_30s_bil.zip",
                "bio_30s1.zip",
                mode = "wb"
              )
              unzip("bio_30s1.zip", exdir = path_current)
              unlink("bio_30s1.zip")
            }
            if (any(group2 %in% input$pred_vars_wc)) {
              download.file("http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/bio10-19_30s_bil.zip",
                "bio_30s2.zip",
                mode = "wb"
              )
              unzip("bio_30s2.zip", exdir = path_current)
              unlink("bio_30s2.zip")
            }
            
            files <- paste0(path_current, "/", list.files(path_current))
            file.rename(from = files, to = sub(pattern = "bio_", replacement = "bio", files))
          }
        }
        
        ### Include time projections ###
        if (input$forecasting_wc != "current_wc") {
          environmental_data$write_timeproj <- TRUE
          checkfiles_timeproj <- list()
          
          ## Include future projection
          if (input$forecasting_wc == "future_wc") {
            timescale <<- "Future"
            path_future <- paste0(
              getwd(),
              "/ex/clima/",
              input$future_dates_wc,
              "/",
              input$resolution,
              "/",
              input$gcm_future_wc,
              "/",
              input$rcp_wc
            )
            
            year <- sub(".*(\\d+{2}).*$", "\\1", input$future_dates_wc)
            
            for (i in c(1:length(input$pred_vars_wc))) {
              nbi <- sub("bio", "", vars_selection[i])
              layer <- paste0(
                path_future,
                "/",
                input$gcm_future_wc,
                input$rcp_wc,
                "bi",
                year,
                nbi,
                ".tif"
              )
              environmental_data$data_timeproj <- c(environmental_data$data_timeproj, layer)
              checkfiles_timeproj <- c(checkfiles_timeproj, file.exists(layer))
            }
          }
          
          ## Include paleo projection
          if (input$forecasting_wc == "past_wc") {
            timescale <<- "Past"
            
            if (input$past_dates_wc == "mid") {
              gcm_past <- input$gcm_past_wc_mid
            }
            
            if (input$past_dates_wc == "lgm") {
              gcm_past <- input$gcm_past_wc_lgm
            }
            
            path_past <- paste0(
              getwd(), "/ex/clima/",
              input$past_dates_wc,
              "/",
              input$resolution,
              "/",
              gcm_past
            )
            
            for (i in c(1:length(input$pred_vars_wc))) {
              nbi <- sub("bio", "", vars_selection[i])
              layer <- paste0(
                path_past,
                "/",
                gcm_past,
                input$past_dates_wc,
                "bi",
                nbi,
                ".tif"
              )
              environmental_data$data_timeproj <- c(environmental_data$data_timeproj, layer)
              checkfiles_timeproj <- c(checkfiles_timeproj, file.exists(layer))
            }
          }
          
          ### Check/Download layers - Future/Paleo Conditions ###
          if (any(checkfiles_timeproj == F)) {
            
            # Download future layers
            if (input$forecasting_wc == "future_wc") {
              zip <- paste0(input$gcm_future_wc, input$rcp_wc, "bi", year, ".zip")
              if (input$resolution == "2-5m") {
                url <- paste0(
                  "http://biogeo.ucdavis.edu/data/climate/cmip5/2_5m",
                  "/",
                  zip
                )
              }
              
              if (input$resolution != "2-5m") {
                url <- paste0(
                  "http://biogeo.ucdavis.edu/data/climate/cmip5/",
                  input$resolution,
                  "/",
                  zip
                )
              }
              
              download.file(url, zip, mode = "wb")
              unzip(zip, exdir = path_future)
              unlink(zip)
            }
            
            # Download paleo layers
            if (input$forecasting_wc == "past_wc") {
              zip <- paste0(
                gcm_past,
                input$past_dates_wc,
                "bi_",
                input$resolution,
                ".zip"
              )
              url <- paste0(
                "http://biogeo.ucdavis.edu/data/climate/cmip5/",
                input$past_dates_wc,
                "/",
                zip
              )
              download.file(url, zip, mode = "wb")
              unzip(zip, exdir = path_past)
              unlink(zip)
            }
          }
        }
      }
    }
    
    ## Bio-ORACLE variables 
    if (input$tipodadoabiotico == "BIOORACLE") {
      path_current <- paste0(getwd(), "/ex/biooracle/current")
      
      ## Include future projections ##
      if (input$forecasting_bo == "future_bo") {
        timescale <<- "Future"
        environmental_data$write_timeproj <- TRUE
        
        if (input$future_dates_bo == "2100") {
          scenario_bo <- input$scenario_bo_2100
        }
        if (input$future_dates_bo == "2200") {
          scenario_bo <- input$scenario_bo_2200
        }
        path_future <- paste0(getwd(), "/ex/biooracle/", input$future_dates_bo, "/", scenario_bo)
        vars_selection <<- paste(input$pred_vars_bo_fut)
        
        if (length(vars_selection) != 0) {
          for (i in c(1:length(vars_selection))) {
            # Future layers
            layer_code_future <- paste0("BO_", scenario_bo, "_", input$future_dates_bo, "_", vars_selection[i])
            environmental_data$data_timeproj <- c(environmental_data$data_timeproj, load_layers(layer_code_future, rasterstack = FALSE, datadir = path_future))
            # Current layers
            layer_code <- paste0("BO_", vars_selection[i])
            environmental_data$data_current <- c(environmental_data$data_current, load_layers(layer_code, rasterstack = FALSE, datadir = path_current))
          }
        }
        ## Do not include future projections ##
      } else {
        vars_selection <<- paste(input$pred_vars_bo)
        for (i in c(1:length(vars_selection))) {
          if (length(vars_selection) != 0) {
            layer_code <- paste0("BO_", vars_selection[i])
            environmental_data$data_current <- c(environmental_data$data_current, load_layers(layer_code, rasterstack = FALSE, datadir = path_current))
          }
        }
      }
    }
    
    ## Browse variables from local files 
    if (input$tipodadoabiotico == "Others") {
      path_current <- paste(getwd(), "/ex/outros/", sep = "")
      vars_selection <<- paste(input$pred_vars_other)
      for (i in c(1:length(input$pred_vars_other))) {
        layer <- paste0(path_current, vars_selection[i])
        environmental_data$data_current <- c(environmental_data$data_current, layer)
      }
    }
    
    if (length(vars_selection) == 0) {
      showModal(modalDialog(
        title = "Unable to load layers!",
        paste0("Please select one or more environmental variables"),
        easyClose = TRUE
      ))
    } else {
      withProgress(message = "", value = 0, {
        n <- 3
        incProgress(1 / n, detail = paste0("Loading variables..."))
        ETAPA <<- 3
        env_data <<- environmental_data$data_current
        write_timeproj <<- environmental_data$write_timeproj
        envdata_timeproj <<- environmental_data$data_timeproj
        
        if (!is.null(env_data) && exists("occurrences")) {
          
          # Stack and crop environmental layers
          predictors <- stack(env_data)
          ext <- extent(ext11, ext21, ext31, ext41)
          pred_nf <<- crop(predictors, ext)
          if (length(envdata_timeproj) >= 1) {
            predictorsfuturo <- stack(envdata_timeproj)
            pred_nffuturo <<- crop(predictorsfuturo, ext)
          }
          ext2 <- extent(ext12, ext22, ext32, ext42)
          pred_nf2 <<- crop(predictors, ext2)
          
          # Calculate correlation between selected variables (current conditions only)
          if (length(env_data) > 1) {
            incProgress(2 / n, detail = paste0("Calculating corelation..."))
            presvals <- raster::extract(pred_nf, occurrences)
            backgr <- randomPoints(pred_nf, 300)
            colnames(backgr) <- c("Longitude", "Latitude")
            absvals <- raster::extract(pred_nf, backgr)
            sdmdata <- data.frame(cbind(absvals))
            
            # Exhibit correlation panel plots
            output$grafico_correlacao <- renderPlot({
              if (is.null(env_data) | length(env_data) <= 1) {
                return(NULL)
              } else {
                pairs(sdmdata,
                  cex = 0.1,
                  fig = TRUE,
                  lower.panel = panel.reg,
                  diag.panel = panel.hist,
                  upper.panel = panel.cor
                )
                
                # Exhibit correlation matrix
                output$dgbriddadoscorrelacao <- renderDataTable({
                  round(cor(sdmdata), 2)
                }, options = list(
                  searching = FALSE,
                  rownames = TRUE,
                  colnames = c("Variable" = 1)
                ))
              }
            })
          }
        }
        incProgress(3 / n, detail = paste0("Ploting correlation..."))
      })
    }
  })
  
  
  output$mapaabiotico <- renderPlot({
    input$btnAtualizaSelecaoVariaveis
    if (is.null(env_data)) {
      return(NULL)
    } else {
      plot(pred_nf)
    }
  })
  
  # Update species occurrence dataset
  datasetInput <- reactive({
    if (exists("occurrences")) {
      switch("occurrences", occurrences = occurrences)
    }
  })
  
  #####  DATA CLEANING #####
  output$dgbriddadosdatacleaning <- renderDataTable({
    input$btnapagar
    input$btneliminarduplicatas
    input$btnsearch_spdatacsv
    input$btnsearch_spdata
    ETAPA<<-2
    if (is.null(occurrences)) {
      n <- 0
    }
    n <- nrow(occurrences)
    if (n > 0) {
      if (exists("occurrences")) {
        if (input$btneliminarduplicatas > 0) {
          progress <- shiny::Progress$new()
          progress$set(message = "Excluding duplicates...", value = 0)
          on.exit(progress$close())
          occurrences <<- unique(occurrences)
        }
        isolate({
          input$edtelemento
          if (input$edtelemento != "0") {
            if (input$btnapagar == 0) {
              return()
            }
            occurrences <<- occurrences[-input$edtelemento, ]
          }
          rownames(occurrences) <- NULL
          occurrences$id <- 1:nrow(occurrences)
        })
        occurrences
      }
    }
  }, options = list(searching = FALSE, lengthMenu = c(5, 30, 50), pageLength = 5))
  
  observeEvent(input$saveDataset, {
    write.csv(occurrences, file=paste0(getwd(),"/www/",projeto,"/csv/occurrences.csv"), row.names = FALSE)
    })
  
  output$mapadistribuicaodatacleaning <- renderLeaflet({
    input$btnapagar
    input$btneliminarduplicatas
    input$btnsearch_spdatacsv
    input$btnsearch_spdata
    
    if (!is.null(occurrences)) {
      if (exists("occurrences")) {
        rownames(occurrences) <- NULL
        occurrences$id <- 1:nrow(occurrences)
        map <- leaflet(occurrences) %>%
          addTiles() %>%
          addCircles(color = "red", lat = ~ Latitude, lng = ~ Longitude) %>%
          addMarkers(clusterOptions = markerClusterOptions()) %>%
          addMarkers(~ Longitude, ~ Latitude, popup = ~ as.character(id))
        map
      }
    } else {
      showModal(modalDialog(
        title = "Error!",
        "Please inform species occurrence data.",
        easyClose = TRUE
      ))
    }
  })
  
  #####  IMPORT SPECIES OCCURRENCE DATASET #####
  # Load species occurrence dataset from gbif/jabot databases
  loadspdata <- eventReactive(input$btnsearch_spdata, {
    ETAPA <<- 1
    progress <- shiny::Progress$new()
    progress$set(message = "Importing species occurrence data...", value = 0)
    on.exit(progress$close())
    input$btnsearch_spdata
    if (input$bio_datasource == "gbif") {
      occur.data <- getOccurrences_gbif(input$edtespecie)
      occur.data_gbif <- occur.data [, c(2, 3)]
      occurrences <<- occur.data_gbif
    }
    if (input$bio_datasource == "jabot") {
      occur.data <- getOccurrences_jabot(input$edtespecie)
      occur.data <- as.data.frame(occur.data, stringsAsFactors = F)
      occur.data_jabot <- occur.data[, c(2, 3)]
      occur.data_jabot[, 1] <- as.numeric(occur.data_jabot[, 1])
      occur.data_jabot[, 2] <- as.numeric(occur.data_jabot[, 2])
      occurrences <<- occur.data_jabot
    }
    occurrences
  })
  
  # Browse occurrence dataset from local csv file
  loadspdata_csv <- eventReactive(input$btnsearch_spdatacsv, {
    ETAPA <<- 1
    progress <- shiny::Progress$new()
    progress$set(message = "Importing species occurrence data...", value = 0)
    on.exit(progress$close())
    input$btnsearch_spdatacsv
    inFile <<- input$file1
    if (is.null(inFile)) {
      return(NULL)
    } else {
      sp_data <- read.csv(
        inFile$datapath,
        header = input$header,
        sep = input$sep,
        quote = input$quote
      )
      arquivo_path <- inFile$datapath
      arquivo_header <- input$header
      arquivo_sep <- input$sep
      arquivo_quote <- input$quote
      sp_data_csv <- sp_data [, 2:3]
      occurrences <<- sp_data_csv
    }
    occurrences
  })
  
  # Exhibit table with occurrence records
  output$spdata_table <- DT::renderDataTable({
    
    if (exists("occurrences")) {
    occurrences
    }
    observeEvent(input$btnsearch_spdatacsv,{
      loadspdata_csv()
      occurrences<<-occurrences
    })
     
    observeEvent(input$btnsearch_spdata,{
      loadspdata()
      occurrences<<-occurrences
    })
    
    
    
    
    # else {
    #   if (exists("occurrences")) {
    #     occurrences
    #   }
    # }
   # occurrences
  }, options = list(lengthMenu = c(5, 30, 50), pageLength = 5))
  
  # Display map with loaded occurrence records
  output$mapadistribuicao <- renderLeaflet({
    input$btnsearch_spdatacsv
    input$btnsearch_spdata
    
    progress <- shiny::Progress$new()
    progress$set(message = "Updating species occurrence map...", value = 0)
    on.exit(progress$close())
    
    if (!is.null(occurrences)) {
      latitude<-as.character(occurrences$Latitude)
      latitude<-as.numeric(latitude)
      longitude<-as.character(occurrences$Longitude)
      longitude<-as.numeric(longitude)
      if (input$bio_datasource == "csv") {
        map <- leaflet(occurrences) %>%
          addTiles() %>%
          addCircles(color = "red", lat = ~ latitude, lng = ~ longitude) %>%
          setView(lng = -31.5, lat = -13.4, zoom = 3)
      }
      
      if (input$bio_datasource == "gbif") {
        map <- leaflet(occurrences) %>%
          addTiles() %>%
          addCircles(color = "red", lat = ~ latitude, lng = longitude) %>%
          #addMarkers(clusterOptions = markerClusterOptions()) %>%
          setView(lng = -31.5, lat = -13.4, zoom = 3)
      }
      
      if (input$bio_datasource == "jabot") {
        map <- leaflet(occurrences) %>%
          addTiles() %>%
          addCircles(color = "red", lat = ~ latitude, lng = ~ longitude) %>%
          setView(lng = -31.5, lat = -13.4, zoom = 3)
      }
      map
      
      # In case no occurrence dataset is informed, exihibit error message
    } else {
      showModal(modalDialog(
        title = "Error!",
        "Please inform species occurrence dataset",
        easyClose = TRUE
      ))
    }
  })
  
  ##### CREATE NEW/LOAD PROJECT #####
  observeEvent(input$btnrefreshprojeto, {
    
    # Create new project
    if (input$select_project == "new_proj") {
      projeto <- paste0("projeto/", input$edtprojeto.create)
      rm(occurrences, envir = .GlobalEnv)
      
     
      if (projeto == "projeto/") {
        showModal(modalDialog(
          title = "Unable to create new project",
          paste0("Project name cannot be blank!", "Please enter a valid name."),
          easyClose = TRUE
        ))
      }
      
      # Check if project already exists in the directory
      if (projeto != "projeto/") {
        if (file.exists(paste0(getwd(), "/www/", projeto)) == TRUE) {
          showModal(modalDialog(
            title = "Unable to create new project",
            paste0("This name is alredy in use!", "Please insert a different name."),
            easyClose = TRUE
          ))
        }
        
        # If not, create project folder and sub-directories
        if (file.exists(paste0(getwd(), "/www/", projeto)) != TRUE) {
          projeto <<- paste0("projeto/", input$edtprojeto.create)
          
          withProgress(message = "", value = 0, {
            n <- 7
            
            mkdirs(paste0("www/", projeto))
            incProgress(1 / n, detail = paste0("Creating directory ", projeto))
            Sys.sleep(0.2)
            
            mkdirs(paste0("www/", projeto, "/final"))
            incProgress(2 / n, detail = paste0("Creating directory ", projeto, "/final"))
            Sys.sleep(0.2)
            
            mkdirs(paste0("www/", projeto, "/proj_time"))
            incProgress(3 / n, detail = paste0("Creating directory ", projeto, "/proj_time"))
            Sys.sleep(0.2)
            
            mkdirs(paste0("www/", projeto, "/jpg"))
            incProgress(4 / n, detail = paste0("Creating directory ", projeto, "/jpg"))
            Sys.sleep(0.2)
            
            mkdirs(paste0("www/", projeto, "/models"))
            incProgress(5 / n, detail = paste0("Creating directory ", projeto, "/models"))
            Sys.sleep(0.2)
            
            mkdirs(paste0("www/", projeto, "/proj"))
            incProgress(6 / n, detail = paste0("Creating directory ", projeto, "/proj"))
            Sys.sleep(0.2)
            
            mkdirs(paste0("www/", projeto, "/csv"))
            incProgress(7 / n, detail = paste0("Creating directory ", projeto, "/csv"))
            Sys.sleep(0.2)
          })
          showModal(modalDialog(
            title = "Project succesfully created!",
            paste0("Project directory: ", projeto),
            easyClose = TRUE
          ))
        }
      }
    }
    
    # Load output files from a previous project
    if (input$select_project == "load_proj") {
      projeto <- paste0("projeto/", input$edtprojeto.load)
      
      # If any of the listed projects is selected, load output results
      if (projeto != "projeto/") {
        if (file.exists(paste0(getwd(), "/www/", projeto)) == TRUE) {
          if (file.exists(paste0("www/", projeto, "/csv/occurrences.csv")) == TRUE) {
            occurrences <<- read.csv(paste0("www/", projeto, "/csv/occurrences.csv"), header = TRUE, sep=",")
          }
          
          # Display Stats results
          output$dbgridresultado <- renderDataTable({
            read.table(paste0("./www/", projeto, "/models/statsALL.txt"))
          }, options = list(lengthMenu = c(5, 30, 50), pageLength = 10))
          
          # Display continuous/binary jpg files
          output$ui <- renderUI({
            lista_jpg <- list.files(
              paste0("www/", projeto, "/jpg"),
              full.names = F,
              pattern = paste0(".jpg")
            )
            lapply(1:length(order(lista_jpg)), function(i) {
              tags$a(
                href = paste0(home, projeto, "/jpg/", lista_jpg[i]),
                tags$img(src = paste0(projeto, "/jpg/", lista_jpg[i]), height = "200px"),
                target = "_blank"
              )
            })
          })
          
          # list final models
          output$uifinal <- renderUI({
            lista_modelsfinal <- list.files(
              paste0("www/", projeto, "/final"),
              full.names = F,
              pattern = paste0(".png")
            )
            lapply(1:length(sort(lista_modelsfinal)), function(i) {
              tags$a(
                href = paste0(home, projeto, "/final/", lista_modelsfinal[i]),
                tags$img(src = paste0(projeto, "/final/", lista_modelsfinal[i]), height = "200px"),
                target = "_blank"
              )
            })
          })
          
          # List models
          output$uiarquivosmodelos <- renderUI({
            lista_models <- list.files(
              paste0("www/", projeto, "/models"),
              full.names = F,
              pattern = paste0("pre_")
            )
            lapply(1:length(sort(lista_models)), function(i) {
              tags$div(tags$a(
                href = paste0(home, projeto, "/models/", lista_models[i]),
                paste0(lista_models[i])
              ))
            })
          })
          
          # List jpeg files
          output$ui <- renderUI({
            lista_jpg <- list.files(paste0("www/", projeto, "/jpg"),
                                    full.names = F,
                                    pattern = paste0(".jpg")
            )
            lapply(1:length(order(lista_jpg)), function(i) {
              tags$a(
                href = paste0(home, projeto, "/jpg/", lista_jpg[i]),
                tags$img(src = paste0(projeto, "/jpg/", lista_jpg[i]), height = "200px"),
                target = "_blank"
              )
            })
          })
          
          # List script files
          output$uiscript <- renderUI({
            lista_txt <- list.files(
              paste0("www/", projeto, "/"),
              full.names = F,
              pattern = paste0("Script.R")
            )
            lapply(1:length(lista_txt), function(i) {
              tags$div(tags$a(
                href = paste0(home, projeto, "/", lista_txt[i]),
                paste0(lista_txt[i]), target = "_blank"
              ))
            })
          })
          
          # List statistics files
          output$uiestatistica <- renderUI({
            lista_txt <- list.files(
              paste0("www/", projeto, "/models"),
              full.names = F,
              pattern = paste0("statsALL.txt")
            )
            lapply(1:length(lista_txt), function(i) {
              tags$div(tags$a(
                href = paste0(home, projeto, "/models/", lista_txt[i]),
                paste0(lista_txt[i]),
                target = "_blank"
              ))
            })
          })
          
          # List species occurrence dataset file (.csv)
          output$uiarquivosdados <- renderUI({
            lista_csv <- list.files(
              paste0("www/", projeto, "/csv"),
              full.names = F,
              pattern = paste0("occurrences.csv")
            )
            lapply(1:length(lista_csv), function(i) {
              tags$div(tags$a(
                href = paste0(home, projeto, "/csv/", lista_csv[i]),
                paste0(lista_csv[i]),
                target = "_blank"
              ))
            })
          })
          
          # List ensemble files
          output$uiarquivosensemble <- renderUI({
            lista_final <- list.files(
              paste0("www/", projeto, "/final"),
              full.names = F,
              pattern = paste0(".tif")
            )
            lapply(1:length(sort(lista_final)), function(i) {
              tags$div(tags$a(
                href = paste0(home, projeto, "/final/", lista_final[i]),
                paste0(lista_final[i]),
                target = "_blank"
              ))
            })
          })
          
          # List "geographic projection" files
          output$uiarquivosprojecao <- renderUI({
            lista_proj <- list.files(
              paste0("www/", projeto, "/proj"),
              full.names = F,
              pattern = paste0(".tif")
            )
            lapply(1:length(sort(lista_proj)), function(i) {
              tags$div(tags$a(
                href = paste0(home, projeto, "/proj/", lista_proj[i]),
                paste0(lista_proj[i]),
                target = "_blank"
              ))
            })
          })
          
          # List "time projection" files
          output$uiarquivosprojecaofuturo <- renderUI({
            list_timeproj <- list.files(
              paste0("www/", projeto, "proj_time"),
              full.names = F,
              pattern = paste0(".tif")
            )
            lapply(1:length(sort(list_timeproj)), function(i) {
              tags$div(tags$a(
                href = paste0(home, projeto, "/proj_time/", list_timeproj [i]),
                paste0(list_timeproj [i]),
                target = "_blank"
              ))
            })
          })
          
          showModal(modalDialog(
            title = paste0("Project ", input$edtprojeto.load, " succefully loaded!"),
            paste0("Output files are dispalyed at the 'Outputs' tab."),
            easyClose = TRUE
          ))
          projeto <<- paste0("projeto/", input$edtprojeto.load)
        }
      }
      
      # In case none of the listed projects is selected, exhibit error dialog
      if (projeto == "projeto/") {
        showModal(modalDialog(
          title = "Error! Project name cannot be blank!",
          paste0("Please enter a valid name."),
          easyClose = TRUE
        ))
      }
    }
  })
}