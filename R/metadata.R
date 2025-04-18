

#' Export Metadata to JSON
#'
#' This function exports a data frame to a prettified JSON file.
#'
#' @param x The data frame to be exported.
#' @param outFilePath The file path for the JSON output file.
#'
#' @examples
#' spt_export_meta_to_json(myDataFrame, "path/to/output.json")
#'
#' @export
#'

spt_export_meta_to_json <- function(x, outFilePath) {
  # Export the data.frame to a prettified JSON file
  json_text <- jsonlite::toJSON(x, pretty = TRUE)
  cat(json_text, file = outFilePath)
}


#' Fill Metadata Template
#'
#' This function fills in a metadata template with values from a metadata list.
#'
#' @param metaTemplate The metadata template data frame.
#' @param metaList The metadata list containing parameter-value pairs.
#'
#' @return The filled metadata template with updated values.
#'
#' @examples
#' filledTemplate <- spt_fill_meta_template(metaTemplate, metaList)
#'
#' @export
#'
#'

spt_fill_meta_template <- function(metaTemplate, metaList) {

  #metaTemplate <- SPT_META_TABLE
  nms <- names(metaList)

  for (i in 1:length(metaList)) {

    if (!(nms[i] %in% metaTemplate$Parameter)) {
      warning(nms[i], " is not a valid parameter to metadata")
      next
    }

    metaTemplate[metaTemplate$Parameter == nms[i], "Value"] <- as.character(metaList[[i]])
  }
  return(metaTemplate)
}

#' Burned Area Dataset Code
#'
#' This function retrieves the code for a given burned area dataset.
#'
#' @param baDataset The burnt area dataset name.
#'
#' @return Character. The dataset code corresponding to the given
#' burned area dataset.
#'
#' @examples
#' code <- spt_ba_dataset_code("ICNF")
#'
#' @export
#'

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


#' Spectral Index Full Name
#'
#' This function retrieves the full name of a spectral index based on its acronym.
#'
#' @param spiAcronym The acronym of the spectral index.
#'
#' @return Character. The full name of the spectral index corresponding to
#' the given acronym.
#'
#' @examples
#' fullname <- spt_spec_index_fullname("NBR")
#'
#' @export
#'

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
    FVC  = "Fractional Vegetation Cover",

    # New indices added from Duarte Velho contributed code:
    BAI   = "Burned Area Index",
    DBSI  = "Dry Bareness Index",
    NBR2  = "Normalized Burn Ratio 2",
    GEMI  = "Global Environment Monitoring Index",
    SAVI  = "Soil-Adjusted Vegetation Index",
    MSAVI = "Modified Soil Adjusted Vegetation Index",
    NDWI  = "Normalized Difference Water Index"
  )

  return(spiNames[[spiAcronym]])
}


#' Spectral Index Formula
#'
#' This function retrieves the formula of a spectral index based on its
#' acronym and satellite code.
#'
#' @param spiAcronym The acronym of the spectral index.
#' @param satCode The satellite code.
#'
#' @return Character. The formula of the spectral index corresponding to the given
#' acronym and satellite code.
#'
#' @examples
#' formula <- spt_spec_index_formula("TCTB", "MOD")
#'
#' @export
#'

spt_spec_index_formula <- function(spiAcronym, satCode) {

  spiForms <- list(

    NBR     = "(NIR - SWIR) / (NIR + SWIR)",
    NDVI    = "(NIR - Red) / (NIR + Red)",
    EVI     = "2.5*((NIR - Red) / ((NIR + 6 * Red - 7.5 * Blue) + 1))",
    NBRSWIR = '(SWIR2 - SWIR1 - 0.02) / (SWIR2 + SWIR1 + 0.1)',
    MIRBI   = '10 * SWIR2 - 9.8 * SWIR1 + 2',
    CSI     = 'NIR / SWIR2',
    NBRP    = '(SWIR2 - NIR - GREEN - BLUE) / (SWIR2 + NIR + GREEN + BLUE); Note: for Sentinel2-2 NIR is replaced by RE4 - band L8A',

    # New indices added from Duarte Velho contributed code:
    BAI   = "1 / (pow(0.1 - RED,2) + pow(0.06 - NIR,2))",
    DBSI  = "((SWIR1 - GREEN) / (SWIR1 + GREEN)) - ((NIR - RED) / (NIR + RED))",
    NBR2  = "(SWIR1 - SWIR2) / (SWIR1 + SWIR2)",
    GEMI  = "((2.0*((NIR ** 2.0)-(RED ** 2.0)) + 1.5*NIR + 0.5*RED)/(NIR + RED + 0.5))*(1.0 - 0.25*((2.0 * ((NIR ** 2.0) - (RED ** 2)) + 1.5 * NIR + 0.5 * RED)/(NIR + RED + 0.5)))-((RED - 0.125)/(1 - RED))",
    SAVI  = "(1 + L) * (NIR - RED) / (NIR + RED + L); L=0.5",
    MSAVI = "(2 * NIR + 1 - sqrt(pow((2 * NIR + 1), 2) - 8 * (NIR - RED))) / 2",
    NDWI  = "(NIR - SWIR1) / (NIR + SWIR1)",

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


#' Satellite Mission Full Name
#'
#' This function retrieves the full name of a satellite mission based on its code.
#'
#' @param satCode The code of the satellite
#'
#' @return The full name of the satellite mission corresponding to the given code.
#'
#' @examples
#' fullname <- spt_sat_mission_fullname("S2MSI")
#'
#' @export
#'

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


#' Get the Severity Indicator Formula
#'
#' This function generates the formula for a severity indicator based on the
#' selected spectral index (spi) and severity indicator (si). Available options
#' are delta, Relative delta and Relativized Burn Ratio.
#'
#' @param spi The selected spectral index.
#' @param si The selected severity indicator.
#'
#' @return The formula for the severity indicator based on the selected
#' spectral index and severity indicator.
#'
#' @examples
#' formula <- spt_severity_indicator_form("NDVI", "DELTA")
#'
#' @export
#'

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


#' Burned Area Dataset URL
#'
#' This function retrieves the URL of the specified burned area dataset.
#'
#' @param baDataset The burned area dataset code (eg., EFFIS, ICNF).
#'
#' @return The URL of the specified burned area dataset.
#'
#' @examples
#' url <- spt_ba_data_url("EFFIS")
#'
#' @export
#'

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


#' Processing Levels description
#'
#' This function provides a description of the processing levels for satellite data.
#'
#' @param procLevel The processing level of the satellite data.
#'
#' @return A description of the processing level.
#'
#' @examples
#' levelDesc <- spt_proc_levels("L1C")
#'
#' @export
#'

spt_proc_levels <- function(procLevel) {
  if (procLevel %in% c("L1C", "L1")) {
    return("Level L1/L1C: Top-of-the-atmosphere reflectance (TOAR)")
  } else if (procLevel %in% c("L2A", "L2")) {
    return("Level L2/L2A: Surface reflectance (SR)")
  } else {
    return("N/A")
  }
}


