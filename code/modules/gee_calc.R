

processGEEtask <- function(task, outFolder = "GEE", boundBox, 
                           coordRefSys = 'EPSG:32629'){
  
  # Get run parameters
  mainTaskStatus    = getTaskStatus(task,taskStep = "main")
  
  satCode           = getSatCode(task)
  procLevel         = getProcLevel(task)
  modisProduct      = getModisProduct(task)
  baseIndex         = getBaseIndex(task)
  severityIndicator = getSeverityIndicator(task)
  
  burntAreaDataset  = getBurntAreaDataset(task)
  referenceYear     = getReferenceYear(task)

  minFireSizeHa             = getMinFireSize(task)
  fixedPreFireWindowSize    = getPreFireRef(task)
  preFireWindowType         = getPreFireType(task)
  postFireWindowEndMonths   = getPostFireRef(task)
  postFireWindowDays        = getPostWindowDays(task)

  
  refPeriods = paste0(# Pre-fire ref period
                      ifelse(preFireWindowType %in% c("moving","mov","m"),"R","S"),
                      padNumber(fixedPreFireWindowSize),
                      # Post-fire ref period
                      "P",padNumber(postFireWindowEndMonths))

  prodName = getProductName(SPT_PROJ_ACRONYM, satCode, baseIndex, severityIndicator,
                             burntAreaDataset, referenceYear, refPeriods, addCalcDate=FALSE)
  
  
  if(burntAreaDataset=="ICNF"){
    fireDateFieldName = SPT_ICNF_DATE_FIELD
  }
  else if(burntAreaDataset=="EFFIS"){
    fireDateFieldName = SPT_EFFIS_DATE_FIELD
  }
  else{
    stop("Unsupported burnt area dataset name in burntAreaDataset!")
  }
  
  # Cloud mask & scale data functions
  #
  
  # SENTINEL-2
  if(satCode == "S2MSI"){
    cloudMaskFun = maskClouds_S2
    scaleDataFun = scaleData_S2
  }
  
  # LANDSAT MISSIONS
  else if(satCode %in% c("L5TM", "L7ETM", "L8OLI", "L9OLI")){
    
    if(satCode == "L5TM"){
      cloudMaskFun = maskClouds_LT5
    }else if(satCode == "L7ETM"){
      cloudMaskFun = maskClouds_LT7
    }else if(satCode == "L8OLI"){
      cloudMaskFun = maskClouds_LT8
    }else if(satCode == "L9OLI"){
      cloudMaskFun = maskClouds_LT9
    }else{
      stop("Invalid satCode value in processGEEtask function")
    }
    
    if(procLevel %in% c("L1", "L1C")){
      scaleDataFun = scaleData_LT_TOA
    }else if(procLevel %in% c("L2", "L2A")){
      scaleDataFun = scaleData_LT_SR
    }else{
      stop("Invalid procLevel value in processGEEtask function")
    }
  }
  else{
    stop("Unsupported satellite code in satCode!")
  }
  
  
  # Spectral indices
  if(baseIndex == "NBR"){
    baseIndexFun = calc_NBR
  }
  else if(baseIndex == "NDVI"){
    baseIndexFun = calc_NDVI
  } 
  else if(baseIndex == "EVI"){
    baseIndexFun = calc_EVI
  } 
  
  ## ---- TASSELED CAPS TRANSFORMATIONS ---- ## 
  ##
  else if(baseIndex == "TCTB"){
    if(satCode == "S2MSI"){
      baseIndexFun = calc_TCTB_S2 
    }
  }
  else if(baseIndex == "TCTG"){
    if(satCode == "S2MSI"){
      baseIndexFun = calc_TCTG_S2 
    }
  }
  else if(baseIndex == "TCTW"){
    if(satCode == "S2MSI"){
      baseIndexFun = calc_TCTW_S2 
    }
  }
  ## END TCT
  
  else{
    stop("Unsupported spectral index in baseIndex!")
  }
  
  # Get the spatial resolution of the data
  spatialRes <- getSpatialResolution(task)
  

  # Create the burned area feature collection  
  baData = getGEEburntAreaDataset(burntAreaDataset, referenceYear, minFireSizeHa)
  
  ## Convert burned area polygons to a list to iterate more easily
  baList = ee$FeatureCollection$toList(baData, baData$size())
  
  # Get the Image Collection object to handle in GEE
  sits = getGEEsatImageCollection(satCode   = satCode, 
                                  procLevel = procLevel,
                                  modisProduct = modisProduct)
  
  
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
      ee$ImageCollection$select(baseIndex) %>% 
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
      ee$ImageCollection$select(baseIndex) %>% 
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

