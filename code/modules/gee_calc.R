
# TODO: Add Relative Delta calcs

# TODO: Add other methods?? RBR??

# TODO: Add indices to other satellites Landsat and MODIS



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
  if(burntAreaDataset=="EFFIS"){
    fireDateFieldName = SPT_EFFIS_DATE_FIELD
  }
  

  if(satCode == "S2MSI"){
    
    # Cloud mask function
    cloudMaskFun = maskS2clouds
    
    # Spectral indices
    if(baseIndex == "NBR"){
      baseIndexFun = calc_NBR
    }
    if(baseIndex == "NDVI"){
      baseIndexFun = calc_NDVI
    }
    if(baseIndex == "EVI"){
      baseIndexFun = calc_EVI
    }
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
      ee$ImageCollection$map(baseIndexFun) %>% 
      ee$ImageCollection$select(baseIndex) %>% 
      ee$ImageCollection$median() %>% 
      ee$Image$multiply(10000) %>% 
      ee$Image$toInt()
    
    # Post-fire reference image
    postFire = sits %>% 
      ee$ImageCollection$filterDate(postfireDate_sta, postfireDate_end) %>% 
      ee$ImageCollection$filterBounds(selFeat$geometry()) %>% 
      ee$ImageCollection$map(cloudMaskFun) %>% 
      ee$ImageCollection$map(baseIndexFun) %>% 
      ee$ImageCollection$select(baseIndex) %>% 
      ee$ImageCollection$median() %>% 
      ee$Image$multiply(10000) %>% 
      ee$Image$toInt()
    
    # Calculate delta 
    imgDiffClip = 
      ee$Image$subtract(ee$Image(preFire), ee$Image(postFire)) %>% 
      ee$Image$rename(severityIndicator) %>% 
      ee$Image$clip(selFeat$geometry()) %>% 
      ee$Image$toInt()
    
    
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

