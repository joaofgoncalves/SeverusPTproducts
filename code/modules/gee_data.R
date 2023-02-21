

## ----------------------------------------------------------------------------------- ##
## Make datasets ----
## ----------------------------------------------------------------------------------- ##

# Burned area datasets


getGEEburntAreaDataset <- function(baDataset, referenceYear, minFireSizeHa = 5){
  
  
  if(baDataset == 'ICNF'){
    
    aa_icnf = ee$FeatureCollection(SPT_ICNF_GEE_ASSET);
    
    baData = aa_icnf %>% 
      ee$FeatureCollection$filter(ee$Filter$eq(SPT_ICNF_YEAR_FIELD, referenceYear)) %>% 
      ee$FeatureCollection$filter(ee$Filter$gte(SPT_ICNF_AREA_FIELD, minFireSizeHa)) %>% 
      ee$FeatureCollection$filter(ee$Filter$neq(SPT_ICNF_DATE_FIELD, '')) %>% 
      ee$FeatureCollection$filter(ee$Filter$neq(SPT_ICNF_DATE_FIELD, NULL))
    
    return(baData)
  }
  
  if(baDataset == 'EFFIS'){
    
    aa_effis = ee$FeatureCollection(SPT_EFFIS_GEE_ASSET);
    
    baData = aa_effis %>% 
      ee$FeatureCollection$filter(ee$Filter$eq(SPT_EFFIS_YEAR_FIELD, referenceYear)) %>% 
      ee$FeatureCollection$filter(ee$Filter$gte(SPT_EFFIS_AREA_FIELD, minFireSizeHa)) %>% 
      ee$FeatureCollection$filter(ee$Filter$neq(SPT_EFFIS_DATE_FIELD, '')) %>% 
      ee$FeatureCollection$filter(ee$Filter$neq(SPT_EFFIS_DATE_FIELD, NULL))
    
    return(baData)
    
  }
}

getGEEsatImageCollection <- function(satCode = "S2MSI", procLevel = "L2A"){
  
  ## Satellite time series 
  if(satCode == 'S2MSI'){
    
    if(procLevel == "L2A"){
      return(ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED"))
    }else if(procLevel == "L1C"){
      return(ee$ImageCollection("COPERNICUS/S2_HARMONIZED"))
    }else{
      return(ee$ImageCollection("COPERNICUS/S2_HARMONIZED"))
    }
  }
  
  # TODO: Add Landsat data collections
  
  # TODO: Add MODIS data collections
}



