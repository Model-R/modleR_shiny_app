## LOAD PACKAGES #####
pck <- c(
  "devtools",
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
  "data.table",
  "DT",
  "shinyjs",
  "sdmpredictors",
  "rJava"
)
sapply(pck, library, character.only = TRUE)
library(modleR)
#precisamos disto para testar
#devtools::load_all("../../modleR")

## SETWD ####
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## GLOBAL UI VARIABLES ####

select_project <- c("Create new project" = "new_proj",
                    "Open project" = "load_proj")

distance <- c("centroid",
              "mindist")

partition_type <- c("crossvalidation",
                    "bootstrap")

past_dates_wc <- c("Mid Holocene" = "mid",
                   "Last Glacial Maximum" = "lgm")

wc_forecasting_timescale <- c("Future conditions" = "future",
                              "Past conditions" = "past")

future_dates_wc <- c("2050" = "2050",
                     "2070" = "2070")

past_dates_wc <- c("Mid Holocene" = "mid",
                   "Last Glacial Maximum" = "lgm")

future_bo_dates <- c("2100" = "2100",
                     "2200" = "2200")

forecasting_bo <- c("Future" = "future_bo",
                    "None" = "current_bo")

weight_par <- c("TSS", "AUC", "pROC")

bio_datasource <- read.csv('./data/bio_datasource.csv')
bio_datasource <-
  setNames(as.character(bio_datasource$value), bio_datasource$key)

env_datasource <- read.csv('./data/env_datasource.csv')
env_datasource <-
  setNames(as.character(env_datasource$value), env_datasource$key)

resolution <- read.csv('./data/resolution.csv')
resolution <-
  setNames(as.character(resolution$value), resolution$key)

forecasting_wc <- read.csv('./data/forecasting_wc.csv')
forecasting_wc <-
  setNames(as.character(forecasting_wc$value), forecasting_wc$key)

pred_vars_bo_fut <- read.csv('./data/pred_vars_bo_fut.csv')
pred_vars_bo_fut <-
  setNames(as.character(pred_vars_bo_fut$value), pred_vars_bo_fut$key)

algorithms <- read.csv('./data/algorithms.csv')
algorithms <-
  setNames(as.character(algorithms$value), algorithms$key)

pred_vars_wc <- read.csv('./data/pred_vars_wc.csv')
pred_vars_wc <-
  setNames(as.character(pred_vars_wc$value), pred_vars_wc$key)

gcm_past_wc_lgm <- read.csv('./data/gcm_past_wc_lgm.csv')
gcm_past_wc_lgm <-
  setNames(as.character(gcm_past_wc_lgm$value), gcm_past_wc_lgm$key)

gcm_past_wc_mid <- read.csv('./data/gcm_past_wc_mid.csv')
gcm_past_wc_mid <-
  setNames(as.character(gcm_past_wc_mid$value), gcm_past_wc_mid$key)

gcm_future_wc <- read.csv('./data/gcm_future_wc.csv')
gcm_future_wc <-
  setNames(as.character(gcm_future_wc$value), gcm_future_wc$key)

rcp_wc <- read.csv('./data/rcp_wc.csv')
rcp_wc <- setNames(as.character(rcp_wc$value), rcp_wc$key)

gcm_past_wc_lgm <- read.csv('./data/gcm_past_wc_lgm.csv')
gcm_past_wc_lgm <-
  setNames(as.character(gcm_past_wc_lgm$value), gcm_past_wc_lgm$key)

pred_vars_bo <- read.csv('./data/pred_vars_bo.csv')
pred_vars_bo <-
  setNames(as.character(pred_vars_bo$value), pred_vars_bo$key)

which_models_final <- read.csv('./data/which_models_final.csv')
which_models_final <-
  setNames(as.character(which_models_final$value),
           which_models_final$key)



## GLOBAL SERVER FUNCTIONS ####

panel.reg <-
  function(x,
           y,
           bg = NA,
           cex = 1,
           col.regres = "red",
           ...) {
    points(x, y, cex = cex)
    abline(stats::lm(y ~ x), col = col.regres, ...)
  }

panel.cor <- function(x,
                      y,
                      digits = 2,
                      prefix = "",
                      ...) {
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

mk.dirs <- function(dir_path) {
  if (!file.exists(dir_path)) {
    mk.dirs(dirname(dir_path))
    dir.create(dir_path, recursive = FALSE, mode = "777")
  }
}

getOccurrences_gbif <- function(species_name) {
  key <- rgbif::name_backbone(name = species_name)$speciesKey
  if (!is.null(key)) {
    gbif_data <- rgbif::occ_search(
      hasCoordinate = TRUE,
      hasGeospatialIssue = F,
      taxonKey = key,
      return = "data"
    )
    gbif_data <-
      subset(gbif_data,
             !is.na(decimalLongitude) & !is.na(decimalLatitude))
    occur.data <-
      data.frame(gbif_data$name,
                 gbif_data$decimalLongitude,
                 gbif_data$decimalLatitude)
    colnames(occur.data) <- c("name", "lon", "lat")
    return(occur.data)
  } else {
    showModal(modalDialog(
      title = "No results!",
      paste0("Please insert a valid species scientific name."),
      easyClose = TRUE
    ))
  }
}

getOccurrences_jabot <- function(species_name) {
  pTaxon <- gsub(" ", "_", species_name)
  json_file <-
    paste0("https://model-r.jbrj.gov.br/execjabot.php?especie=",
           pTaxon)
  json_data <- rjson::fromJSON(file = json_file, method = "C")
  final_data <- do.call(rbind, json_data)
  jabot_data <-
    final_data[, c("taxoncompleto", "longitude", "latitude")]
  occur.data <-
    data.frame(as.character(jabot_data[, 1]),
               as.numeric(jabot_data[, 2]),
               as.numeric(jabot_data[, 3]))
  colnames(occur.data) <- c("name", "lon", "lat")
  return(occur.data)
}

MapPreview.partitions <- function(algorithm = algorithm, part = 1) {
  #finaldir <- list.files(paste0(modelsDir, "/", species_name, "/present/final_models/"), full.names = T, pattern = paste0(algorithm, "_raw_mean.tif^"))
  #tif_file <- finaldir[finaldir == paste0(species_name, "_", algorithm, "_raw_mean.tif")]
  part_file <-
    list.files(
      paste0(modelsDir, "/", species_name, "/present/partitions/"),
      pattern = paste0(algorithm, ".tif$"),
      full.names = T
    )[part]
  
  if (file.exists(raw_mean_file)) {
    r <- raster::raster(raw_mean_file)
    pal <-
      colorNumeric(c("#FFFFFF", "#FDBB84", "#31A354"),
                   raster::values(r),
                   na.color = "transparent")
    occ_points <<- coordinates(occurrences)
    lng <<- occ_points[, 1]
    lat <<- occ_points[, 2]
    map <- leaflet() %>%
      addTiles() %>%
      addRasterImage(r, colors = pal, opacity = 0.7) %>%
      addLegend(pal = pal,
                values = raster::values(r),
                title = "") %>%
      addCircles(
        color = "red",
        lat = lat,
        lng = lng,
        weight = 2,
        fill = TRUE
      ) %>%
      addRectangles(
        ext11,
        ext31,
        ext21,
        ext41,
        color = "red",
        fill = FALSE,
        dashArray = "5,5",
        weight = 2
      )
  } else {
    message(paste0("No paritions were found for", algorithm))
  }
}

MapPreview.final <- function(algorithm = algorithm) {
  #finaldir <- list.files(paste0(modelsDir, "/", species_name, "/present/final_models/"), full.names = T, pattern = paste0(algorithm, "_raw_mean.tif^"))
  #tif_file <- finaldir[finaldir == paste0(species_name, "_", algorithm, "_raw_mean.tif")]
  raw_mean_file <-
    list.files(
      paste0(modelsDir, "/", species_name, "/present/final_models/"),
      pattern = paste0(algorithm, "_raw_mean.tif$"),
      ##ö this has to be which, not only raw_mean
      full.names = T
    )
  
  if (file.exists(raw_mean_file)) {
    r <- raster::raster(raw_mean_file)
    pal <-
      colorNumeric(c("#FFFFFF", "#FDBB84", "#31A354"),
                   raster::values(r),
                   na.color = "transparent")
    occ_points <<- coordinates(occurrences)
    lng <<- occ_points[, 1]
    lat <<- occ_points[, 2]
    map <- leaflet() %>%
      addTiles() %>%
      addRasterImage(r, colors = pal, opacity = 0.7) %>%
      addLegend(pal = pal,
                values = raster::values(r),
                title = "") %>%
      addCircles(
        color = "red",
        lat = lat,
        lng = lng,
        weight = 2,
        fill = TRUE
      ) %>%
      addRectangles(
        ext11,
        ext31,
        ext21,
        ext41,
        color = "red",
        fill = FALSE,
        dashArray = "5,5",
        weight = 2
      )
  } else {
    message(paste0("No final_models were selected for", algorithm))
  }
}

MapPreview.ensemble <- function() {
  #finaldir <- list.files(paste0(modelsDir, "/", species_name, "/present/ensemble/"))
  #tif_file <- finaldir[finaldir == paste0(species_name, "_raw_mean_ensemble_mean.tif")]
  raw_mean_file <-
    list.files(
      paste0(modelsDir, "/", species_name, "/present/ensemble/"),
      pattern = "_raw_mean_median.tif$",
      full.names = T
    )
  if (file.exists(raw_mean_file)) {
    r <- raster::raster(raw_mean_file)
    pal <-
      colorNumeric(c("#FFFFFF", "#FDBB84", "#31A354"),
                   raster::values(r),
                   na.color = "transparent")
    occ_points <<- coordinates(occurrences)
    lng <<- occ_points[, 1]
    lat <<- occ_points[, 2]
    map <- leaflet() %>%
      addTiles() %>%
      addRasterImage(r, colors = pal, opacity = 0.7) %>%
      addLegend(pal = pal,
                values = raster::values(r),
                title = "") %>%
      addCircles(
        color = "red",
        lat = lat,
        lng = lng,
        weight = 2,
        fill = TRUE
      ) %>%
      addRectangles(
        ext11,
        ext31,
        ext21,
        ext41,
        color = "red",
        fill = FALSE,
        dashArray = "5,5",
        weight = 2
      )
  }
}

## GLOBAL OPTIONS ####
options(shiny.maxRequestSize = 100 * 1024 ^ 2)
dirColors <-
  c(
    `1` = "#595490",
    `2` = "#527525",
    `3` = "#A93F35",
    `4` = "#BA48AA"
  )
