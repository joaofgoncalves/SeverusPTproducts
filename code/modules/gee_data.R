

## ----------------------------------------------------------------------------------- ##
## Make datasets ----
## ----------------------------------------------------------------------------------- ##

# Burned area datasets


getGEEburntAreaDataset <- function(baDataset, referenceYear, minFireSizeHa = 5) {
  if (baDataset == "ICNF") {
    aa_icnf <- ee$FeatureCollection(SPT_ICNF_GEE_ASSET)

    baData <- aa_icnf %>%
      
      #ee$FeatureCollection$filter(ee$Filter$eq(SPT_ICNF_YEAR_FIELD, referenceYear)) %>%
      ee$FeatureCollection$filter(ee$Filter$Or(
        ee$Filter$eq(SPT_ICNF_YEAR_FIELD, as.character(referenceYear)), 
        ee$Filter$eq(SPT_ICNF_YEAR_FIELD, as.integer(referenceYear))
        )
      ) %>% 

      ee$FeatureCollection$filter(ee$Filter$gte(SPT_ICNF_AREA_FIELD, minFireSizeHa)) %>%
      ee$FeatureCollection$filter(ee$Filter$neq(SPT_ICNF_DATE_FIELD, "")) %>%
      ee$FeatureCollection$filter(ee$Filter$neq(SPT_ICNF_DATE_FIELD, NULL))

    return(baData)
  }

  if (baDataset == "EFFIS") {
    aa_effis <- ee$FeatureCollection(SPT_EFFIS_GEE_ASSET)

    baData <- aa_effis %>%
      
      #ee$FeatureCollection$filter(ee$Filter$eq(SPT_EFFIS_YEAR_FIELD, referenceYear)) %>%
      ee$FeatureCollection$filter(ee$Filter$Or(
        ee$Filter$eq(SPT_EFFIS_YEAR_FIELD, as.character(referenceYear)), 
        ee$Filter$eq(SPT_EFFIS_YEAR_FIELD, as.integer(referenceYear))
        )
      ) %>% 
      
      ee$FeatureCollection$filter(ee$Filter$gte(SPT_EFFIS_AREA_FIELD, minFireSizeHa)) %>%
      ee$FeatureCollection$filter(ee$Filter$neq(SPT_EFFIS_DATE_FIELD, "")) %>%
      ee$FeatureCollection$filter(ee$Filter$neq(SPT_EFFIS_DATE_FIELD, NULL))

    return(baData)
  }
}



getGEEsatImageCollection <- function(satCode = "S2MSI",
                                     procLevel = "L2A", modisProduct = NULL) {
  
  
  ## ----------------------------------------------------------------------- ##
  ## Sentinel-2a/b mission ----
  ## ----------------------------------------------------------------------- ##
  
  if (satCode == "S2MSI") {
    if (procLevel %in% c("L2A", "L2")) {
      
      # Harmonized Sentinel-2 MSI: MultiSpectral Instrument, Level-2A
      imCol = ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED") %>% 
              ee$ImageCollection$select(c("B2", "B3", "B4", "B5", "B6", "B7",
                                          "B8", "B8A", "B11", "B12", "QA60"),
                                        # Renamed bands
                                        c("Blue", "Green", "Red", "RE1", "RE2", "RE3",
                                          "NIR", "RE4", "SWIR1", "SWIR2", "QA60"))
      return(imCol)
      
    } else if (procLevel %in% c("L1C", "L1")) {
      
      # Harmonized Sentinel-2 MSI: MultiSpectral Instrument, Level-1C
      imCol = ee$ImageCollection("COPERNICUS/S2_HARMONIZED") %>% 
              ee$ImageCollection$select(c("B2","B3","B4","B5","B6","B7",
                                    "B8","B8A","B11","B12","QA60"),
                                   # Renamed bands
                                  c("Blue", "Green", "Red","RE1","RE2",
                                    "RE3","NIR","RE4","SWIR1", "SWIR2","QA60"))
      return(imCol)
      
    } else {
      stop("Sentinel-2 - Processing level value in procLevel is not supported")
    }
  }

  ## ----------------------------------------------------------------------- ##
  ## MODIS missions ----
  ## ----------------------------------------------------------------------- ##
  
  # Terra/MODIS mission ----------------------------------------------------- #
  
  else if (satCode == "MOD") {
    if (is.null(modisProduct)) {
      stop("A MODIS product must be defined")
    } else if (modisProduct == "MOD09A1") {
      
      # MOD09A1.006 Terra Surface Reflectance 8-Day Global 500m
      imCol = ee$ImageCollection("MODIS/061/MOD09A1") %>% 
        ee$ImageCollection$select(c("sur_refl_b01","sur_refl_b02","sur_refl_b03",
                                    "sur_refl_b04","sur_refl_b05","sur_refl_b06",
                                    "sur_refl_b07","QA","StateQA"),
                                  # Renamed bands
                                  c("Red", "NIR", "Blue",
                                    "Green", "NIR2", "SWIR1", 
                                    "SWIR2","QA","StateQA"))
      return(imCol)
      
    } else if (modisProduct == "MOD13Q1") {
      
      # MOD13Q1.006 Terra Vegetation Indices 16-Day Global 250m
      imCol = ee$ImageCollection("MODIS/061/MOD13Q1") %>% 
        ee$ImageCollection$select(c("sur_refl_b01", "sur_refl_b02", "sur_refl_b03",
                                    "sur_refl_b07", "NDVI", "EVI",
                                    "DetailedQA", "SummaryQA"),
                                  # Renamed bands
                                  c("Red", "NIR", "Blue",
                                    "SWIR2", "NDVI", "EVI", 
                                    "DetailedQA", "SummaryQA"))
      return(imCol)
      
    } else if (modisProduct == "MOD09GQ") {
      
      # MOD09GQ.006 Terra Surface Reflectance Daily Global 250m
      imCol = ee$ImageCollection("MODIS/061/MOD09GQ") %>% 
        ee$ImageCollection$select(c("sur_refl_b01", "sur_refl_b02", "QC_250m"),
                                  # Renamed bands
                                  c("Red", "NIR", "QC_250m"))
      return(imCol)

    } else if (modisProduct == "MOD09Q1") {
      
      # MOD09Q1.006 Terra Surface Reflectance 8-Day Global 250m
      imCol = ee$ImageCollection("MODIS/061/MOD09Q1") %>% 
        ee$ImageCollection$select(c("sur_refl_b01", "sur_refl_b02", "State", "QA"),
                                  # Renamed bands
                                  c("Red", "NIR", "State", "QA"))
      return(imCol)

    }
  }

  # Aqua/MODIS mission ---------------------------------------------------- #
  
  else if (satCode == "MYD") {
    if (is.null(modisProduct)) {
      stop("A MODIS product must be defined")
    } else if (modisProduct == "MYD09A1") {
      
      # MYD09A1.061 Aqua Surface Reflectance 8-Day Global 500m
      imCol = ee$ImageCollection("MODIS/061/MYD09A1") %>% 
        ee$ImageCollection$select(c("sur_refl_b01","sur_refl_b02","sur_refl_b03",
                                    "sur_refl_b04","sur_refl_b05","sur_refl_b06",
                                    "sur_refl_b07","QA","StateQA"),
                                  # Renamed bands
                                  c("Red", "NIR", "Blue",
                                    "Green", "NIR2", "SWIR1", 
                                    "SWIR2","QA","StateQA"))
      return(imCol)

    } else if (modisProduct == "MYD09GQ") {
      
      # MYD09GQ.061 Aqua Surface Reflectance Daily Global 250m
      imCol = ee$ImageCollection("MODIS/061/MYD09GQ") %>% 
        ee$ImageCollection$select(c("sur_refl_b01", "sur_refl_b02", "QC_250m"),
                                  # Renamed bands
                                  c("Red", "NIR", "QC_250m"))
      return(imCol)

    } else if (modisProduct == "MYD09Q1") {
      
      # MYD09Q1.061 Aqua Surface Reflectance 8-Day Global 250m
      imCol = ee$ImageCollection("MODIS/061/MYD09Q1") %>% 
        ee$ImageCollection$select(c("sur_refl_b01", "sur_refl_b02", "State", "QA"),
                                  # Renamed bands
                                  c("Red", "NIR", "State", "QA"))
      return(imCol)
      
    } else if (modisProduct == "MYD13Q1") {
      
      
      # MYD13Q1.061 Aqua Vegetation Indices 16-Day Global 250m
      imCol = ee$ImageCollection("MODIS/061/MYD13Q1") %>% 
        ee$ImageCollection$select(c("sur_refl_b01", "sur_refl_b02", "sur_refl_b03",
                                    "sur_refl_b07", "NDVI", "EVI",
                                    "DetailedQA", "SummaryQA"),
                                  # Renamed bands
                                  c("Red", "NIR", "Blue",
                                    "SWIR2", "NDVI", "EVI", 
                                    "DetailedQA", "SummaryQA"))
      return(imCol)
      
    }
  }

  # Combined Aqua/Terra --------------------------------------------------- #
  
  else if (satCode == "MCD") {
    if(modisProduct == "MCD43A4"){
      
      # MCD43A4.061 MODIS Nadir BRDF-Adjusted Reflectance Daily 500m
      imCol = ee$ImageCollection("MODIS/061/MCD43A4") %>% 
        ee$ImageCollection$select(c("Nadir_Reflectance_Band1",
                                    "Nadir_Reflectance_Band2",
                                    "Nadir_Reflectance_Band3",
                                    "Nadir_Reflectance_Band4",
                                    "Nadir_Reflectance_Band5",
                                    "Nadir_Reflectance_Band6",
                                    "Nadir_Reflectance_Band7",
                                    "BRDF_Albedo_Band_Mandatory_Quality_Band1",
                                    "BRDF_Albedo_Band_Mandatory_Quality_Band2",
                                    "BRDF_Albedo_Band_Mandatory_Quality_Band3",
                                    "BRDF_Albedo_Band_Mandatory_Quality_Band4",
                                    "BRDF_Albedo_Band_Mandatory_Quality_Band5",
                                    "BRDF_Albedo_Band_Mandatory_Quality_Band6",
                                    "BRDF_Albedo_Band_Mandatory_Quality_Band7"),
                                  # Renamed bands
                                  c("Red", "NIR", "Blue",
                                    "Green", "NIR2", "SWIR1","SWIR2",
                                    "QA_B1","QA_B2","QA_B3","QA_B4",
                                    "QA_B5","QA_B6","QA_B7"))
      return(imCol)
      
    }
  }

  ## ----------------------------------------------------------------------- ##
  ## Landsat missions ----
  ## ----------------------------------------------------------------------- ##
  
  else if (satCode == "L5TM") {
    if (procLevel %in% c("L2A", "L2")) {
      
      # USGS Landsat 5 Level 2, Collection 2, Tier 1
      imCol = ee$ImageCollection("LANDSAT/LT05/C02/T1_L2") %>% 
        ee$ImageCollection$select(c("SR_B1", "SR_B2", "SR_B3",
                                    "SR_B4", "SR_B5", "SR_B7", 
                                    "SR_CLOUD_QA", "QA_PIXEL"),
                                  # Renamed bands
                                  c("Blue", "Green", "Red",
                                    "NIR", "SWIR1", "SWIR2",
                                    "SR_CLOUD_QA", "QA_PIXEL"))
      return(imCol)
      
    } else if (procLevel %in% c("L1C", "L1")) {
      
      # USGS Landsat 5 Top of Atmosphere, Collection 2, Tier 1
      imCol = ee$ImageCollection("LANDSAT/LT05/C02/T1_TOA") %>% 
        ee$ImageCollection$select(c("B1", "B2", "B3",
                                    "B4", "B5", "B7",
                                    "QA_PIXEL"),
                                  # Renamed bands
                                  c("Blue", "Green", "Red",
                                    "NIR", "SWIR1", "SWIR2", 
                                    "QA_PIXEL"))
      return(imCol)
      
    } else {
      stop("Landsat-5: Processing level value in procLevel is not supported")
    }
  }

  else if (satCode == "L7ETM") {
    if (procLevel %in% c("L2A", "L2")) {

      # USGS Landsat 7 Level 2, Collection 2, Tier 1
      imCol = ee$ImageCollection("LANDSAT/LE07/C02/T1_L2") %>% 
        ee$ImageCollection$select(c("SR_B1", "SR_B2", "SR_B3",
                                    "SR_B4", "SR_B5", "SR_B7",
                                    "SR_CLOUD_QA", "QA_PIXEL"),
                                  # Renamed bands
                                  c("Blue", "Green", "Red",
                                    "NIR", "SWIR1", "SWIR2",
                                    "SR_CLOUD_QA", "QA_PIXEL"))
      return(imCol)
      
    } else if (procLevel %in% c("L1C", "L1")) {
      
      # USGS Landsat 7 Tof of Atmosphere, Collection 2, Tier 1
      imCol = ee$ImageCollection("LANDSAT/LE07/C02/T1_TOA") %>% 
        ee$ImageCollection$select(c("B1", "B2", "B3",
                                    "B4", "B5", "B7",
                                    "QA_PIXEL"),
                                  # Renamed bands
                                  c("Blue", "Green", "Red",
                                    "NIR", "SWIR1", "SWIR2", 
                                    "QA_PIXEL"))
      return(imCol)

    } else {
      stop("Landsat-7: Processing level value in procLevel is not supported")
    }
  }

  else if (satCode == "L8OLI") {
    if (procLevel %in% c("L2A", "L2")) {
      
      # USGS Landsat 8 Level 2, Collection 2, Tier 1
      imCol = ee$ImageCollection("LANDSAT/LC08/C02/T1_L2") %>% 
        ee$ImageCollection$select(c("SR_B1", "SR_B2", "SR_B3",
                                    "SR_B4", "SR_B5", "SR_B6", "SR_B7",
                                    "SR_QA_AEROSOL", "QA_PIXEL"),
                                  # Renamed bands
                                  c("Coastal","Blue", "Green", 
                                    "Red", "NIR", "SWIR1", "SWIR2",
                                    "SR_QA_AEROSOL", "QA_PIXEL"))
      return(imCol)
      
    } else if (procLevel %in% c("L1C", "L1")) {
      
      # USGS Landsat 8 Collection 2 Tier 1 TOA Reflectance
      imCol = ee$ImageCollection("LANDSAT/LC08/C02/T1_TOA") %>% 
        ee$ImageCollection$select(c("B1","B2","B3",
                                    "B4","B5","B6",
                                    "B7","QA_PIXEL"),
                                  # Renamed bands
                                  c("Coastal","Blue", "Green", "Red",
                                    "NIR", "SWIR1", "SWIR2", 
                                    "QA_PIXEL"))
      return(imCol)
      
    } else {
      stop("Landsat-8: Processing level value in procLevel is not supported")
    }
  }

  else if (satCode == "L9OLI") {
    if (procLevel %in% c("L2A", "L2")) {
      
      # USGS Landsat 9 Level 2, Collection 2, Tier 1
      imCol = ee$ImageCollection("LANDSAT/LC09/C02/T1_L2") %>% 
        ee$ImageCollection$select(c("SR_B1", "SR_B2", "SR_B3",
                                    "SR_B4", "SR_B5", "SR_B6", "SR_B7",
                                    "SR_QA_AEROSOL", "QA_PIXEL"),
                                  # Renamed bands
                                  c("Coastal", "Blue", "Green", 
                                    "Red", "NIR", "SWIR1", "SWIR2",
                                    "SR_QA_AEROSOL", "QA_PIXEL"))
      return(imCol)
      
    } else if (procLevel %in% c("L1C", "L1")) {
     
      # USGS Landsat 9 Collection 2 Tier 1 TOA Reflectance
      imCol = ee$ImageCollection("LANDSAT/LC09/C02/T1_TOA") %>% 
        ee$ImageCollection$select(c("B1","B2","B3",
                                    "B4","B5","B6", "B7",
                                    "QA_PIXEL"),
                                  # Renamed bands
                                  c("Coastal", "Blue", "Green", 
                                    "Red", "NIR", "SWIR1", "SWIR2", 
                                    "QA_PIXEL"))
      return(imCol)

    } else {
      stop("Landsat-9: Processing level value in procLevel is not supported")
    }
  }

  else if (satCode == "LTH") {
    stop("LTH is not yet available!")
  }
  
  else{
    stop("Satellite code not available or malformed!")
  }
}

spt_etm_to_oli <- function(img){
  
  itcps = ee$Image$constant(c(0.0003, 0.0088, 0.0061, 0.0412, 0.0254, 0.0172))
  slopes = ee$Image$constant(c(0.8474, 0.8483, 0.9047, 0.8462, 0.8937, 0.9071))
  
  out <- img$select(c('Blue', 'Green', 'Red', 'NIR', 'SWIR1', 'SWIR2')) %>% 
    ee$Image$multiply(slopes) %>% 
    ee$Image$add(itcps)
  
  return(out)
}


