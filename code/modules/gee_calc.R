

getSpecIndexFun <- function(baseIndex, satCode=NULL, modisProduct=NULL){
  
  if((baseIndex %in% c("TCTB","TCTG","TCTW","NBRP")) && is.null(satCode)){
    stop("The spectral index is specific to a satellite which has not been defined. Check satCode")
  }
  
  if((baseIndex %in% c("TCTB","TCTG","TCTW")) && (satCode %in% c("MOD","MYD"))){
    if(!(modisProduct %in% c("MOD09A1","MYD09A1"))){
      stop("MODIS product not supported")
    }
  }
  
  indexFunctions <- list(
    
    # General indices
    NBR     = calc_NBR,
    NDVI    = calc_NDVI,
    EVI     = calc_EVI,
    NBRSWIR = calc_NBRSWIR,
    MIRBI   = calc_MIRBI,
    CSI     = calc_CSI,
    
    # Satellite specific indices
    NBRP = list(
      S2MSI = calc_NBRP_S2,
      L5TM  = calc_NBRP,
      L7ETM = calc_NBRP,
      L8OLI = calc_NBRP,
      L9OLI = calc_NBRP,
      MOD   = calc_NBRP,
      MYD   = calc_NBRP
    ),
    
    TCTB = list(
      S2MSI = calc_TCTB_S2,
      L5TM  = calc_TCTB_L5,
      L7ETM = calc_TCTB_L7,
      L8OLI = calc_TCTB_L8,
      L9OLI = calc_TCTB_L8,
      MOD   = calc_TCTB_MOD09A1,
      MYD   = calc_TCTB_MOD09A1
    ),
    
    TCTG = list(
      S2MSI = calc_TCTG_S2,
      L5TM  = calc_TCTG_L5,
      L7ETM = calc_TCTG_L7,
      L8OLI = calc_TCTG_L8,
      L9OLI = calc_TCTG_L8,
      MOD   = calc_TCTG_MOD09A1,
      MYD   = calc_TCTG_MOD09A1
    ),
    
    TCTW = list(
      S2MSI = calc_TCTW_S2,
      L5TM  = calc_TCTW_L5,
      L7ETM = calc_TCTW_L7,
      L8OLI = calc_TCTW_L8,
      L9OLI = calc_TCTW_L8,
      MOD   = calc_TCTW_MOD09A1,
      MYD   = calc_TCTW_MOD09A1
    )
  )
  
  if(baseIndex %in% c("TCTB","TCTG","TCTW","NBRP")){
    return(indexFunctions[[baseIndex]][[satCode]])
  }else{
    return(indexFunctions[[baseIndex]])
  }
}


getCloudMaskFun <- function(satCode, modisProduct=NULL){
  
  
  if((satCode %in% c("MOD","MYD")) && is.null(modisProduct)){
    stop("modisProduct must be defined")
  }
  
  maskFuns <- list(
    
    S2MSI = maskClouds_S2,
    
    L5TM  = maskClouds_LT5,
    L7ETM = maskClouds_LT7,
    L8OLI = maskClouds_LT8,
    L9OLI = maskClouds_LT9,
    
    MOD   = list(
      MOD09A1 = maskClouds_MOD09A1,
      MOD13Q1 = maskClouds_MOD13Q1
    ),
    MYD   = list(
      MYD09A1 = maskClouds_MOD09A1,
      MYD13Q1 = maskClouds_MOD13Q1
    )
  )
  
  if(satCode %in% c("MOD","MYD")){
    return(maskFuns[[satCode]][[modisProduct]])
  }else{
    return(maskFuns[[satCode]])
  }
}


getScaleDataFun <- function(satCode, procLevel=NULL, modisProduct=NULL){
  
  
  if((satCode %in% c("MOD","MYD")) && is.null(modisProduct)){
    stop("MODIS product name (modisProduct) must be defined")
  }
  
  if((satCode %in% c("L5TM", "L7ETM", "L8OLI", "L9OLI")) && is.null(procLevel)){
    stop("Processing level (procLevel) must be defined")
  }
  
  scaleFuns <- list(
    
    S2MSI = scaleData_S2,
    
    L5TM  = list(
      L1  = scaleData_LT_TOA,
      L1C = scaleData_LT_TOA,
      L2  = scaleData_LT_SR,
      L2A = scaleData_LT_SR
    ),
    
    L7ETM = list(
      L1  = scaleData_LT_TOA,
      L1C = scaleData_LT_TOA,
      L2  = scaleData_LT_SR,
      L2A = scaleData_LT_SR
    ),
    
    L8OLI = list(
      L1  = scaleData_LT_TOA,
      L1C = scaleData_LT_TOA,
      L2  = scaleData_LT_SR,
      L2A = scaleData_LT_SR
    ),
    
    L9OLI = list(
      L1  = scaleData_LT_TOA,
      L1C = scaleData_LT_TOA,
      L2  = scaleData_LT_SR,
      L2A = scaleData_LT_SR
    ),
    
    MOD   = list(
      MOD09A1 = scaleData_MODIS,
      MOD13Q1 = scaleData_MODIS
    ),
    
    MYD   = list(
      MYD09A1 = scaleData_MODIS,
      MYD13Q1 = scaleData_MODIS
    )
  )
  
  if(satCode %in% c("MOD","MYD")){
    return(scaleFuns[[satCode]][[modisProduct]])
  }
  else if(satCode %in% c("L5TM", "L7ETM", "L8OLI", "L9OLI")){
    return(scaleFuns[[satCode]][[procLevel]])
  }
  else{
    return(scaleFuns[[satCode]])
  }
}



spt_process_gee_task <- function(task, outFolder = "GEE", boundBox, 
                           coordRefSys = 'EPSG:32629'){
  
  # Get run parameters
  mainTaskStatus    = spt_get_taskStatus(task,taskStep = "main")
  
  satCode           = spt_sat_code(task)                 # Satellite code
  procLevel         = spt_proc_level(task)               # Data processing level L1, L2, ..
  modisProduct      = spt_modis_product(task)            # MODIS product name, eg MOD09A1
  baseIndex         = spt_base_index(task)               # Base index name, eg NBR
  severityIndicator = spt_severity_indicator(task)       # Severity indicator name, eg, DELTA
  
  burntAreaDataset  = spt_ba_dataset(task)        # Burned area dataset used in calculations eg, ICNF, EFFIS
  referenceYear     = spt_reference_year(task)           # Reference year for calculations

  minFireSizeHa          = spt_min_fire_size(task)        # Minimum size of the fire (hectares)
  fixedPreFireWindowSize = spt_pre_fire_ref(task)      
  preFireWindowType      = spt_pre_fire_type(task)
  postFireWindowEndMonths= spt_post_fire_ref(task)
  postFireWindowDays     = spt_post_window_days(task)

  
  refPeriods = paste0(# Pre-fire ref period
                      ifelse(preFireWindowType %in% c("moving","mov","m"),"R","S"),
                      spt_pad_number(fixedPreFireWindowSize),
                      # Post-fire ref period
                      "P",spt_pad_number(postFireWindowEndMonths))
  

  prodName = spt_product_name(ProjectAccronym   = SPT_PROJ_ACRONYM, 
                              SeverityIndicator = severityIndicator, 
                              BaseIndex         = baseIndex, 
                              SatCode           = satCode, 
                              BurntAreaDataset  = burntAreaDataset, 
                              ReferenceYear = referenceYear, 
                              RefPeriods    = refPeriods, 
                              addCalcDate   = TRUE, 
                              VersionNumber = SPT_VERSION)
  
  if(burntAreaDataset=="ICNF"){
    fireDateFieldName = SPT_ICNF_DATE_FIELD
  } else if(burntAreaDataset=="EFFIS"){
    fireDateFieldName = SPT_EFFIS_DATE_FIELD
  } else{
    stop("Unsupported burnt area dataset name in burntAreaDataset!")
  }
  
  # Get the spatial resolution of the data
  spatialRes <- spt_spatial_resolution(task)
  
  # Create the burned area feature collection  
  baData = spt_get_ba_dataset(burntAreaDataset, referenceYear, minFireSizeHa)
  
  ## Convert burned area polygons to a list to iterate more easily
  baList = ee$FeatureCollection$toList(baData, baData$size())
  
  
  if(satCode != "LTH"){
      
    # Get the Image Collection object to handle in GEE
    sits = spt_get_sat_imgcol(satCode    = satCode, 
                            procLevel    = procLevel,
                            modisProduct = modisProduct)
    
    # Get the cloud mask function
    cloud_mask_fun = spt_cloud_mask_fun(satCode, modisProduct)
    
    # Get the scale data function
    scale_data_fun = spt_scale_fun(satCode, procLevel, modisProduct)
    
    # Get the spectral index function
    base_index_fun = spt_spectral_index_fun(baseIndex, satCode, modisProduct)
    
  }else{
    
    # Get inputs adjusted to the Landsat-5,7,8 Harmonized collection
    
    # LT5
    lt5_sits           = spt_get_sat_imgcol(satCode   = "L5TM", procLevel = procLevel)
    cloud_mask_fun_lt5 = spt_cloud_mask_fun("L5TM")
    scale_data_fun_lt5 = spt_scale_fun("L5TM", procLevel)
    base_index_fun_lt5 = spt_spectral_index_fun(baseIndex, "L5TM")
    
    # LT5
    lt7_sits           = spt_get_sat_imgcol(satCode   = "L7ETM", procLevel = procLevel)
    cloud_mask_fun_lt7 = spt_cloud_mask_fun("L7ETM")
    scale_data_fun_lt7 = spt_scale_fun("L7ETM", procLevel)
    base_index_fun_lt7 = spt_spectral_index_fun(baseIndex, "L7ETM")
    
    # LT5
    lt8_sits           = spt_get_sat_imgcol(satCode   = "L8OLI",  procLevel = procLevel)
    cloud_mask_fun_lt8 = spt_cloud_mask_fun("L8OLI")
    scale_data_fun_lt8 = spt_scale_fun("L8OLI", procLevel)
    base_index_fun_lt8 = spt_spectral_index_fun(baseIndex, "L8OLI")
  }

  
  # Set the start and end of the post-fire window
  timeWindowStart = ee$Number(postFireWindowDays[1])
  timeWindowEnd   = ee$Number(postFireWindowDays[2])
  
  
  # Function to iterate through each burned area polygon
  #
  #
  accumulate = function(currentListItem, previousImgResult){
    
    # Get a single feature from the iterated list
    selFeat = ee$Feature(currentListItem)
    
    # Extract the date as string
    # Fire ignition date
    dt = ee$String(selFeat$get(fireDateFieldName))

    fd = ee$Date(dt$slice(0, 11)$trim())
    
    # Post-fire window:
    # Start date
    postfireDate_sta = ee$Date(fd$advance(timeWindowStart,'day'))
    # End date
    postfireDate_end = ee$Date(fd$advance(timeWindowEnd,'day'))
    
    # Pre-fire window by type
    # Fixed window made n days before the ignition date
    if(preFireWindowType %in% c("fixed","fix","f")){
      
      prefireDate_sta = ee$Date(fd$advance(ee$Number((-1)*fixedPreFireWindowSize),'day'))
      prefireDate_end = fd
      
      # Moving window referencing the same period one year before
    }else if(preFireWindowType %in% c("moving","mov","m")){
      
      prefireDate_sta = postfireDate_sta$advance(-1, 'year')
      prefireDate_end = postfireDate_end$advance(-1, 'year')
    }
    
    # Pre-fire reference image
    preFire = sits %>% 
      ee$ImageCollection$filterDate(prefireDate_sta, prefireDate_end) %>% 
      ee$ImageCollection$filterBounds(selFeat$geometry()) %>% 
      ee$ImageCollection$map(cloudMaskFun) %>% 
      ee$ImageCollection$map(scaleDataFun) %>% 
      ee$ImageCollection$map(baseIndexFun) %>% 
      #ee$ImageCollection$select(baseIndex) %>% 
      ee$ImageCollection$median() #%>% 
      # ee$Image$multiply(10000) %>% 
      # ee$Image$toInt()
    
    # Post-fire reference image
    postFire = sits %>% 
      ee$ImageCollection$filterDate(postfireDate_sta, postfireDate_end) %>% 
      ee$ImageCollection$filterBounds(selFeat$geometry()) %>% 
      ee$ImageCollection$map(cloudMaskFun) %>% 
      ee$ImageCollection$map(scaleDataFun) %>% 
      ee$ImageCollection$map(baseIndexFun) %>% 
      #ee$ImageCollection$select(baseIndex) %>% 
      ee$ImageCollection$median() #%>% 
      # ee$Image$multiply(10000) %>% 
      # ee$Image$toInt()
    
    if(severityIndicator %in% c("DELTA", "DLT")){
      # Calculate delta
      imgDiffClip = 
        ee$Image$subtract(ee$Image(preFire), ee$Image(postFire)) %>% 
        ee$Image$rename(severityIndicator) %>% 
        ee$Image$clip(selFeat$geometry()) %>% 
        ee$Image$multiply(10000) %>% 
        ee$Image$toInt()
    }
    else if(severityIndicator == "RBR"){
      # Calculate the relativized burn ratio
      # Parks et al (2014) Remote Sensing https://www.mdpi.com/2072-4292/6/3/1827#
      #  A New Metric for Quantifying Burn Severity: The Relativized Burn Ratio 
      
      imgDiffClip = 
        ee$Image$divide(
          ee$Image$subtract(ee$Image(preFire), ee$Image(postFire)),
          ee$Image$add(ee$Image(preFire), 1.001)
        ) %>%
        ee$Image$rename(severityIndicator) %>% 
        ee$Image$clip(selFeat$geometry()) %>% 
        ee$Image$multiply(10000) %>% 
        ee$Image$toInt()

    }
    else if(severityIndicator %in% c("RDELTA","RDT")){
      
      # Calculate relative difference delta
      imgDiffClip = 
        ee$Image$divide(
          ee$Image$subtract(ee$Image(preFire), ee$Image(postFire)),
          ee$Image$sqrt(ee$Image$abs(ee$Image(preFire)))
        )%>% 
        ee$Image$rename(severityIndicator) %>% 
        ee$Image$clip(selFeat$geometry()) %>% 
        ee$Image$multiply(10000) %>% 
        ee$Image$toInt()
      
    }
    else{
      stop("Wrong value in severityIndicator for method processGEEtask")
    }
      
    imColClip = ee$ImageCollection(list(imgDiffClip))
    
    imColPrevious = ee$ImageCollection(list(ee$Image(ee$List(previousImgResult)$get(0))))
    
    added = imColPrevious$merge(imColClip)$sum()$toInt()
    
    return(ee$List(list(added)))
  }
  
  
  ## ----------------------------------------------------------------------------------- ##
  
  # Default image to start the accumulation
  imgFirst = ee$List(list(ee$Image$constant(0)$rename(severityIndicator)$toInt()));
  
  accumRaster = baList$iterate(ee_utils_pyfunc(accumulate), imgFirst);
  
  accumRasterClip = ee$Image(ee$List(accumRaster)$get(0))$clipToCollection(baData);
  
  
  # Move results from Earth Engine to Drive
  geeProcTask = ee_image_to_drive(
    image       = accumRasterClip, 
    description = prodName,
    folder      = outFolder,
    region      = boundBox,
    scale       = spatialRes,
    crs         = coordRefSys,
    maxPixels   = 1E12
  )
  
  geeProcTask$start()
  
  return(geeProcTask)
  
}

