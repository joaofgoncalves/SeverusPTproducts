

exportMetaToJSON <- function(x, outFilePath) {
  # Export the data.frame to a prettified JSON file
  json_text <- jsonlite::toJSON(x, pretty = TRUE)
  cat(json_text, file = outFilePath)
}


metaTableTemplate <- function(metaList) {
  metadf <- SPT_META_TABLE
  nms <- names(metaList)

  for (i in 1:length(metaList)) {
    # print(i)
    # print(nms[i])

    if (!(nms[i] %in% metadf$Parameter)) {
      warning(nms[i], " is not a valid parameter to metadata")
      next
    }

    metadf[metadf$Parameter == nms[i], "Value"] <- as.character(metaList[[i]])
  }
  return(metadf)
}


updateMetaTaskStatus <- function(task, state, taskTable = NULL) {
  # Acquire a lock over the file
  lck <- filelock::lock(paste0(
    SPT_TASK_TABLE_DIR, "/",
    SPT_TASK_TABLE_BASENAME,
    ".lock"
  ), timeout = 30000)

  if (is.null(taskTable)) {
    taskTable <- readTaskTable()
  }

  if (is.null(lck)) {
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  }


  idx <- taskTable$taskUID == task$taskUID
  taskTable[idx, "metadataTaskStatus"] <- state

  out <- try({
    writeTaskTable(taskTable)
    filelock::unlock(lck)
  })

  if (inherits(out, "try-error")) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


getSpectralIndexName <- function(spiAcronym) {
  spiNames <- list(
    NBR  = "Normalized Burn Ratio",
    NDVI = "Normalized Difference Vegetation Index",
    EVI  = "Enhanced Vegetation Index",
    TCTB = "Tasseled Cap Transformation - Brightness",
    TCTG = "Tasseled Cap Transformation - Greenness",
    TCTW = "Tasseled Cap Transformation - Wetness",
    LST = "Land Surface Temperature",
    LAI = "Leaf Area Index",
    GPP = "Gross Primary Productivity",
    NPP = "Net Primary Productivity",
    ALB = "Albedo",
    FVC = "Fractional Vegetation Cover"
  )

  return(spiNames[[spiAcronym]])
}


getSpecIndFormula <- function(spiAcronym) {
  spiForms <- list(
    NBR  = "(NIR - SWIR) / (NIR + SWIR)",
    NDVI = "(NIR - Red) / (NIR + Red)",
    EVI  = "2.5*((NIR - Red) / ((NIR + 6 * Red - 7.5 * Blue) + 1))",
    TCTB = "N/A",
    TCTG = "N/A",
    TCTW = "N/A",
    LST = "N/A",
    LAI = "N/A",
    GPP = "N/A",
    NPP = "N/A",
    ALB = "N/A",
    FVC = "N/A"
  )

  return(spiForms[[spiAcronym]])
}


getSatMissionName <- function(satCode) {
  satNames <- list(
    S2MSI  = "Sentinel-2a/b/MSI",
    MOD    = "Terra/MODIS",
    MYD    = "Aqua/MODIS",
    MCD    = "Combined Terra+Aqua/MODIS",
    L5TM   = "Landsat-5/TM",
    L7ETM  = "Landsat-7/ETM+",
    L8OLI  = "Landsat-8/OLI",
    L8TIRS = "Landsat-8/TIRS",
    LTH    = "Landsat Harmonized (LT-5,7,8)"
  )

  return(satNames[[satCode]])
}


getSeverityIndicatorForm <- function(spi, si) {
  if (si == "DELTA") {
    return(paste(si, " = ", spi, "_prefire - ", spi, "_postfire", sep = ""))
  } else {
    return("N/A")
  }
}


getBurntAreaDataURL <- function(baDataset) {
  if (baDataset == "EFFIS") {
    baDataURL <- "https://effis.jrc.ec.europa.eu/applications/data-and-services"
  } else if (baDataset == "ICNF") {
    baDataURL <- "https://geocatalogo.icnf.pt/catalogo_tema5.html"
  } else {
    baDataURL <- "N/A"
  }

  return(baDataURL)
}
