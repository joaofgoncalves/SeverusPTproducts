

spt_ba_dataset_code <- function(baDataset) {
  baCodes <- list(
    ICNF    = "I",
    EFFIS   = "E",
    MCD64   = "M",
    MICNF   = "C",
    FireCCI = "F",
    VIIRS   = "V"
  )

  return(baCodes[[baDataset]])
}

spt_export_meta_to_json <- function(x, outFilePath) {
  # Export the data.frame to a prettified JSON file
  json_text <- jsonlite::toJSON(x, pretty = TRUE)
  cat(json_text, file = outFilePath)
}


spt_fill_meta_template <- function(metaList) {
  metadf <- SPT_META_TABLE
  nms <- names(metaList)

  for (i in 1:length(metaList)) {

    if (!(nms[i] %in% metadf$Parameter)) {
      warning(nms[i], " is not a valid parameter to metadata")
      next
    }

    metadf[metadf$Parameter == nms[i], "Value"] <- as.character(metaList[[i]])
  }
  return(metadf)
}


spt_update_meta_task <- function(task, state, taskTable = NULL) {
  
  # Acquire a lock over the file
  lck <- filelock::lock(paste0(
    SPT_TASK_TABLE_DIR, "/",
    SPT_TASK_TABLE_BASENAME,
    ".lock"
  ), timeout = 30000)

  if (is.null(taskTable)) {
    taskTable <- spt_read_tasks_table()
  }

  if (is.null(lck)) {
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  }


  idx <- taskTable$taskUID == task$taskUID
  taskTable[idx, "metadataTaskStatus"] <- state

  out <- try({
    spt_write_tasks_table(taskTable)
    filelock::unlock(lck)
  })

  if (inherits(out, "try-error")) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


spt_spec_index_fullname <- function(spiAcronym) {
  spiNames <- list(
    NBR     = "Normalized Burn Ratio",
    NDVI    = "Normalized Difference Vegetation Index",
    EVI     = "Enhanced Vegetation Index",
    NBRSWIR = 'Normalized Burn Ratio SWIR (Liu, et al 2020)',
    MIRBI   = 'Mid-Infrared Burn Index',
    CSI  = 'Char Soil Index',
    NBRP = 'Normalized Burn Ratio plus (Alcaras, 2022)',
    TCTB = "Tasseled Cap Transformation - Brightness",
    TCTG = "Tasseled Cap Transformation - Greenness",
    TCTW = "Tasseled Cap Transformation - Wetness",
    LST  = "Land Surface Temperature",
    LAI  = "Leaf Area Index",
    GPP  = "Gross Primary Productivity",
    NPP  = "Net Primary Productivity",
    ALB  = "Albedo",
    FVC  = "Fractional Vegetation Cover"
  )

  return(spiNames[[spiAcronym]])
}


spt_spec_index_formula <- function(spiAcronym, satCode) {
  
  spiForms <- list(
    
    NBR     = "(NIR - SWIR) / (NIR + SWIR)",
    NDVI    = "(NIR - Red) / (NIR + Red)",
    EVI     = "2.5*((NIR - Red) / ((NIR + 6 * Red - 7.5 * Blue) + 1))",
    NBRSWIR = '(SWIR2 - SWIR1 - 0.02) / (SWIR2 + SWIR1 + 0.1)',
    MIRBI   = '10 * SWIR2 - 9.8 * SWIR1 + 2',
    CSI     = 'NIR / SWIR2',
    NBRP    = '(SWIR2 - NIR - GREEN - BLUE) / (SWIR2 + NIR + GREEN + BLUE)',
    
    TCTB = list(
      S2MSI = '(BLUE * 0.351) + (GREEN * 0.3813) + (RED * 0.3437) + (NIR * 0.7196) + (SWIR1 * 0.2396) + (SWIR2 * 0.1949)',
      L5TM  = '(BLUE * 0.2043) + (GREEN * 0.4158) + (RED * 0.5524) + (NIR * 0.5741) + (SWIR1 * 0.3124) + (SWIR2 * 0.2303)',
      L7ETM = '(BLUE * 0.3561) + (GREEN * 0.3972) + (RED * 0.3904) + (NIR * 0.6966) + (SWIR1 * 0.2286) + (SWIR2 * 0.1596)', 
      L8OLI = '(BLUE * 0.3029) + (GREEN * 0.2786) + (RED * 0.4733) + (NIR * 0.5599) + (SWIR1 * 0.508) + (SWIR2 * 0.1872)',
      L9OLI = '(BLUE * 0.3029) + (GREEN * 0.2786) + (RED * 0.4733) + (NIR * 0.5599) + (SWIR1 * 0.508) + (SWIR2 * 0.1872)',
      MOD   = '(0.4395 * RED) + (0.5945 * NIR) + (0.2460 * BLUE) + (0.3918 * GREEN) + (0.3506 * NIR2) + (0.2136 * SWIR1) + (0.2678 * SWIR2)',
      MYD   = '(0.4395 * RED) + (0.5945 * NIR) + (0.2460 * BLUE) + (0.3918 * GREEN) + (0.3506 * NIR2) + (0.2136 * SWIR1) + (0.2678 * SWIR2)'
    ),
    TCTG = list(
      S2MSI = '(BLUE * -0.3599) + (GREEN * -0.3533) + (RED * -0.4734) + (NIR * 0.6633) + (SWIR1 * 0.0087) + (SWIR2 * -0.2856)',
      L5TM  = '(BLUE * -0.1603) + (GREEN * -0.2819) + (RED * -0.4934) + (NIR * 0.7940) + (SWIR1 * -0.0002) + (SWIR2 * -0.1446)',
      L7ETM = '(BLUE * -0.3344) + (GREEN * -0.3544) + (RED * -0.4556) + (NIR * 0.6966) + (SWIR1 * -0.0242) + (SWIR2 * -0.2630)',
      L8OLI = '(BLUE * -0.2941) + (GREEN * -0.243) + (RED * -0.5424) + (NIR * 0.7276) + (SWIR1 * 0.0713) + (SWIR2 * -0.1608)',
      L9OLI = '(BLUE * -0.2941) + (GREEN * -0.243) + (RED * -0.5424) + (NIR * 0.7276) + (SWIR1 * 0.0713) + (SWIR2 * -0.1608)',
      MOD   = '(-0.4064 * RED) + (0.5129 * NIR) + (-0.2744 * BLUE) + (-0.2893 * GREEN) + (0.4882 * NIR2) + (-0.0036 * SWIR1) + (-0.4169 * SWIR2)',
      MYD   = '(-0.4064 * RED) + (0.5129 * NIR) + (-0.2744 * BLUE) + (-0.2893 * GREEN) + (0.4882 * NIR2) + (-0.0036 * SWIR1) + (-0.4169 * SWIR2)'
    ),
    TCTW = list(
      S2MSI = '(BLUE * 0.2578) + (GREEN * 0.2305) + (RED * 0.0883) + (NIR * 0.1071) + (SWIR1 * -0.7611) + (SWIR2 * -0.5308)', 
      L5TM  = '(BLUE * 0.0315) + (GREEN * 0.2021) + (RED * 0.3102) + (NIR * 0.1594) + (SWIR1 * -0.6806) + (SWIR2 * -0.6109)',
      L7ETM = '(BLUE * 0.2626) + (GREEN * 0.2141) + (RED * 0.0926) + (NIR * 0.0656) + (SWIR1 * -0.7629) + (SWIR2 * -0.5388)',
      L8OLI = '(BLUE * 0.1511) + (GREEN * 0.1973) + (RED * 0.3283) + (NIR * 0.3407) + (SWIR1 * -0.7117) + (SWIR2 * -0.4559)',
      L9OLI = '(BLUE * 0.1511) + (GREEN * 0.1973) + (RED * 0.3283) + (NIR * 0.3407) + (SWIR1 * -0.7117) + (SWIR2 * -0.4559)',
      MOD   = '(0.1147 * RED) + (0.2489 * NIR) + (0.2408 * BLUE) + (0.3132 * GREEN) + (-0.3122 * NIR2) + (-0.6416 * SWIR1) + (-0.5087 * SWIR2) ',
      MYD   = '(0.1147 * RED) + (0.2489 * NIR) + (0.2408 * BLUE) + (0.3132 * GREEN) + (-0.3122 * NIR2) + (-0.6416 * SWIR1) + (-0.5087 * SWIR2) '
    ),
    LST  = "N/A",
    LAI  = "N/A",
    GPP  = "N/A",
    NPP  = "N/A",
    ALB  = "N/A",
    FVC  = "N/A"
  )

  if(is.list(spiForms[[spiAcronym]])){
    return(spiForms[[spiAcronym]][[satCode]])
  }else{
    return(spiForms[[spiAcronym]])
  }
}


spt_sat_mission_fullname <- function(satCode) {
  satNames <- list(
    S2MSI  = "Sentinel-2a/b/MSI",
    MOD    = "Terra/MODIS",
    MYD    = "Aqua/MODIS",
    MCD    = "Combined Terra+Aqua/MODIS",
    L5TM   = "Landsat-5/TM",
    L7ETM  = "Landsat-7/ETM+",
    L8OLI  = "Landsat-8/OLI",
    L8TIRS = "Landsat-8/TIRS",
    L8OLI  = "Landsat-9/OLI",
    L9TIRS = "Landsat-9/TIRS",
    LTH    = "Landsat Harmonized (LT-5,7,8)"
  )

  return(satNames[[satCode]])
}


spt_severity_indicator_form <- function(spi, si) {
  if (si %in% c("DELTA", "DLT")) {
    return(paste(si, " = ", spi, "_prefire - ", spi, "_postfire", sep = ""))
  } 
  else if (si %in% c("RBR")) {
    return(paste(si, " = (", spi, "_prefire - ", spi, "_postfire",") / (", spi, "_prefire + 1.001)", sep = ""))
  }
  else if (si %in% c("RDELTA", "RDT")) {
    return(paste(si, " = (", spi, "_prefire - ", spi, "_postfire) / (sqrt(abs(",spi,"_prefire))", sep = ""))
  }
  else {
    return("N/A")
  }
}


spt_ba_data_url <- function(baDataset) {
  if (baDataset == "EFFIS") {
    baDataURL <- "https://effis.jrc.ec.europa.eu/applications/data-and-services"
  } else if (baDataset == "ICNF") {
    baDataURL <- "https://geocatalogo.icnf.pt/catalogo_tema5.html"
  } else {
    baDataURL <- "N/A"
  }

  return(baDataURL)
}

spt_proc_levels <- function(procLevel) {
  if (procLevel %in% c("L1C", "L1")) {
    return("Level L1/L1C: Top-of-the-atmosphere reflectance (TOAR)")
  } else if (procLevel %in% c("L2A", "L2")) {
    return("Level L2/L2A: Surface reflectance (SR)")
  } else {
    return("N/A")
  }
}


