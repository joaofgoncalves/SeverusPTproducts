

spt_spectral_index_fun <- function(baseIndex, satCode=NULL, modisProduct=NULL){
  
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
    NBR     = spt_calc_nbr,
    NDVI    = spt_calc_ndvi,
    EVI     = spt_calc_evi,
    NBRSWIR = spt_calc_nbrswir,
    MIRBI   = spt_calc_mirbi,
    CSI     = spt_calc_csi,
    
    # Satellite specific indices
    NBRP = list(
      S2MSI = spt_calc_nbrp_s2,
      L5TM  = spt_calc_nbrp,
      L7ETM = spt_calc_nbrp,
      L8OLI = spt_calc_nbrp,
      L9OLI = spt_calc_nbrp,
      MOD   = spt_calc_nbrp,
      MYD   = spt_calc_nbrp
    ),
    
    TCTB = list(
      S2MSI = spt_calc_tctb_s2,
      L5TM  = spt_calc_tctb_l5,
      L7ETM = spt_calc_tctb_l7,
      L8OLI = spt_calc_tctb_l8,
      L9OLI = spt_calc_tctb_l8,
      MOD   = spt_calc_tctb_mod09a1,
      MYD   = spt_calc_tctb_mod09a1
    ),
    
    TCTG = list(
      S2MSI = spt_calc_tctg_s2,
      L5TM  = spt_calc_tctg_l5,
      L7ETM = spt_calc_tctg_l7,
      L8OLI = spt_calc_tctg_l8,
      L9OLI = spt_calc_tctg_l8,
      MOD   = spt_calc_tctg_mod09a1,
      MYD   = spt_calc_tctg_mod09a1
    ),
    
    TCTW = list(
      S2MSI = spt_calc_tctw_s2,
      L5TM  = spt_calc_tctw_l5,
      L7ETM = spt_calc_tctw_l7,
      L8OLI = spt_calc_tctw_l8,
      L9OLI = spt_calc_tctw_l8,
      MOD   = spt_calc_tctw_mod09a1,
      MYD   = spt_calc_tctw_mod09a1
    )
  )
  
  if(baseIndex %in% c("TCTB","TCTG","TCTW","NBRP")){
    return(indexFunctions[[baseIndex]][[satCode]])
  }else{
    return(indexFunctions[[baseIndex]])
  }
}


spt_cloud_mask_fun <- function(satCode, modisProduct=NULL){
  
  
  if((satCode %in% c("MOD","MYD")) && is.null(modisProduct)){
    stop("modisProduct must be defined")
  }
  
  maskFuns <- list(
    
    S2MSI = spt_mask_clouds_s2,
    L5TM  = spt_mask_clouds_lt5,
    L7ETM = spt_mask_clouds_lt7,
    L8OLI = spt_mask_clouds_lt8,
    L9OLI = spt_mask_clouds_lt9,
    
    MOD   = list(
      MOD09A1 = spt_mask_clouds_mod09a1,
      MOD13Q1 = spt_mask_clouds_mod13q1
    ),
    MYD   = list(
      MYD09A1 = spt_mask_clouds_mod09a1,
      MYD13Q1 = spt_mask_clouds_mod13q1
    )
  )
  
  if(satCode %in% c("MOD","MYD")){
    return(maskFuns[[satCode]][[modisProduct]])
  }else{
    return(maskFuns[[satCode]])
  }
}


spt_scale_fun <- function(satCode, procLevel=NULL, modisProduct=NULL){
  
  
  if((satCode %in% c("MOD","MYD")) && is.null(modisProduct)){
    stop("MODIS product name (modisProduct) must be defined")
  }
  
  if((satCode %in% c("L5TM", "L7ETM", "L8OLI", "L9OLI")) && is.null(procLevel)){
    stop("Processing level (procLevel) must be defined")
  }
  
  scaleFuns <- list(
    
    S2MSI = scaleData_S2,
    
    L5TM  = list(
      L1  = spt_scale_lt_toa,
      L1C = spt_scale_lt_toa,
      L2  = spt_scale_lt_tm_sr,
      L2A = spt_scale_lt_tm_sr
    ),
    
    L7ETM = list(
      L1  = spt_scale_lt_toa,
      L1C = spt_scale_lt_toa,
      L2  = spt_scale_lt_tm_sr,
      L2A = spt_scale_lt_tm_sr
    ),
    
    L8OLI = list(
      L1  = spt_scale_lt_toa,
      L1C = spt_scale_lt_toa,
      L2  = spt_scale_lt_oli_sr,
      L2A = spt_scale_lt_oli_sr
    ),
    
    L9OLI = list(
      L1  = spt_scale_lt_toa,
      L1C = spt_scale_lt_toa,
      L2  = spt_scale_lt_oli_sr,
      L2A = spt_scale_lt_oli_sr
    ),
    
    MOD   = list(
      MOD09A1 = spt_scale_mod,
      MOD13Q1 = spt_scale_mod
    ),
    
    MYD   = list(
      MYD09A1 = spt_scale_mod,
      MYD13Q1 = spt_scale_mod
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



spt_process_gee_task <- function(task,boundBox, coordRefSys, baGEEasset, 
                                 dateField, yearField, areaField, outFolder){
  
  # Get run parameters
  mainTaskStatus    = spt_get_taskStatus(task,taskStep = "main")
  
  satCode           = spt_sat_code(task)                 # Satellite code
  procLevel         = spt_proc_level(task)               # Data processing level L1, L2, ..
  modisProduct      = spt_modis_product(task)            # MODIS product name, eg MOD09A1
  baseIndex         = spt_base_index(task)               # Base index name, eg NBR
  severityIndicator = spt_severity_indicator(task)       # Severity indicator name, eg, DELTA
  
  burntAreaDataset  = spt_ba_dataset(task)               # Burned area dataset used in calculations eg, ICNF, EFFIS
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
  
  # if(burntAreaDataset=="ICNF"){
  #   dateField = SPT_ICNF_DATE_FIELD
  # } else if(burntAreaDataset=="EFFIS"){
  #   dateField = SPT_EFFIS_DATE_FIELD
  # } else{
  #   stop("Unsupported burnt area dataset name in burntAreaDataset!")
  # }
  
  # Get the spatial resolution of the data
  spatialRes <- spt_spatial_resolution(task)
  
  # Create the burned area feature collection  
  baData = spt_get_ba_dataset(
    baDataset     = burntAreaDataset, 
    baGEEasset    = baGEEasset, 
    referenceYear = referenceYear, 
    minFireSizeHa = minFireSizeHa, 
    dateField     = dateField, 
    yearField     = yearField, 
    areaField     = areaField)
  
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
    lt5_sits           = spt_get_sat_imgcol(satCode = "L5TM", procLevel = procLevel)
    cloud_mask_fun_lt5 = spt_cloud_mask_fun("L5TM")
    scale_data_fun_lt5 = spt_scale_fun("L5TM", procLevel)
    base_index_fun_lt5 = spt_spectral_index_fun(baseIndex, "L5TM")
    
    # LT5
    lt7_sits           = spt_get_sat_imgcol(satCode = "L7ETM", procLevel = procLevel)
    cloud_mask_fun_lt7 = spt_cloud_mask_fun("L7ETM")
    scale_data_fun_lt7 = spt_scale_fun("L7ETM", procLevel)
    base_index_fun_lt7 = spt_spectral_index_fun(baseIndex, "L7ETM")
    
    # LT5
    lt8_sits           = spt_get_sat_imgcol(satCode = "L8OLI",  procLevel = procLevel)
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
    dt = ee$String(selFeat$get(dateField))

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
    
    #### Interpolate images for Landsat-7/ETM ----
    
    if(satCode == "L7ETM"){
      
      # Pre-fire reference image
      preFire = sits %>% 
        ee$ImageCollection$filterDate(prefireDate_sta, prefireDate_end) %>% 
        ee$ImageCollection$filterBounds(selFeat$geometry()) %>% 
        ee$ImageCollection$map(cloud_mask_fun) %>% 
        ee$ImageCollection$map(scale_data_fun) %>% 
        ee$ImageCollection$map(spt_lt7_interpolate) %>% 
        ee$ImageCollection$map(base_index_fun) %>% 
        ee$ImageCollection$median()
      
      # Post-fire reference image
      postFire = sits %>% 
        ee$ImageCollection$filterDate(postfireDate_sta, postfireDate_end) %>% 
        ee$ImageCollection$filterBounds(selFeat$geometry()) %>% 
        ee$ImageCollection$map(cloud_mask_fun) %>% 
        ee$ImageCollection$map(scale_data_fun) %>% 
        ee$ImageCollection$map(spt_lt7_interpolate) %>% 
        ee$ImageCollection$map(base_index_fun) %>% 
        ee$ImageCollection$median()
      
    }else if(satCode == "LTH"){
      
      #### LTH Pre-fire reference image ----
      
      preFire_lt5 = lt5_sits %>% 
        ee$ImageCollection$filterDate(prefireDate_sta, prefireDate_end) %>% 
        ee$ImageCollection$filterBounds(selFeat$geometry()) %>% 
        ee$ImageCollection$map(cloud_mask_fun_lt5) %>% 
        ee$ImageCollection$map(scale_data_fun_lt5) %>% 
        ee$ImageCollection$map(spt_etm_to_oli) %>% 
        ee$ImageCollection$map(base_index_fun_lt5) 
      
      preFire_lt7 = lt7_sits %>% 
        ee$ImageCollection$filterDate(prefireDate_sta, prefireDate_end) %>% 
        ee$ImageCollection$filterBounds(selFeat$geometry()) %>% 
        ee$ImageCollection$map(cloud_mask_fun_lt7) %>% 
        ee$ImageCollection$map(scale_data_fun_lt7) %>% 
        ee$ImageCollection$map(spt_lt7_interpolate) %>% 
        ee$ImageCollection$map(spt_etm_to_oli) %>% 
        ee$ImageCollection$map(base_index_fun_lt7) 
      
      preFire_lt8 = lt8_sits %>% 
        ee$ImageCollection$filterDate(prefireDate_sta, prefireDate_end) %>% 
        ee$ImageCollection$filterBounds(selFeat$geometry()) %>% 
        ee$ImageCollection$map(cloud_mask_fun_lt8) %>% 
        ee$ImageCollection$map(scale_data_fun_lt8) %>% 
        ee$ImageCollection$map(base_index_fun_lt8) 
      
      #preFire_combn = ee$ImageCollection(preFire_lt5)$merge(ee$ImageCollection(preFire_lt7))
      #$merge(ee$ImageCollection(preFire_lt8))
      
      preFire_combn = ee$ImageCollection$merge(preFire_lt5, preFire_lt7) %>% 
                      ee$ImageCollection$merge(preFire_lt8)
      
      preFire = preFire_combn %>% 
                ee$ImageCollection$median()
      
      
      #### LTH Post-fire reference image ----
      
      postFire_lt5 = lt5_sits %>% 
        ee$ImageCollection$filterDate(postfireDate_sta, postfireDate_end) %>% 
        ee$ImageCollection$filterBounds(selFeat$geometry()) %>% 
        ee$ImageCollection$map(cloud_mask_fun_lt5) %>% 
        ee$ImageCollection$map(scale_data_fun_lt5) %>% 
        ee$ImageCollection$map(spt_etm_to_oli) %>% 
        ee$ImageCollection$map(base_index_fun_lt5) 
      
      postFire_lt7 = lt7_sits %>% 
        ee$ImageCollection$filterDate(postfireDate_sta, postfireDate_end) %>% 
        ee$ImageCollection$filterBounds(selFeat$geometry()) %>% 
        ee$ImageCollection$map(cloud_mask_fun_lt7) %>% 
        ee$ImageCollection$map(scale_data_fun_lt7) %>% 
        ee$ImageCollection$map(spt_lt7_interpolate) %>% 
        ee$ImageCollection$map(spt_etm_to_oli) %>% 
        ee$ImageCollection$map(base_index_fun_lt7) 
      
      postFire_lt8 = lt8_sits %>% 
        ee$ImageCollection$filterDate(postfireDate_sta, postfireDate_end) %>% 
        ee$ImageCollection$filterBounds(selFeat$geometry()) %>% 
        ee$ImageCollection$map(cloud_mask_fun_lt8) %>% 
        ee$ImageCollection$map(scale_data_fun_lt8) %>% 
        ee$ImageCollection$map(base_index_fun_lt8) 
      
      #postFire_combn = ee$ImageCollection(postFire_lt5)$merge(ee$ImageCollection(postFire_lt7))
      #$merge(ee$ImageCollection(postFire_lt8))
      
      postFire_combn = ee$ImageCollection$merge(postFire_lt5, postFire_lt7) %>% 
                       ee$ImageCollection$merge(postFire_lt8)
      
      postFire = postFire_combn %>% 
                 ee$ImageCollection$median()
      
      
    }else{
      
      # Pre-fire reference image
      preFiltCol = sits %>% 
        ee$ImageCollection$filterDate(prefireDate_sta, prefireDate_end) %>% 
        ee$ImageCollection$filterBounds(selFeat$geometry())
      
      # Post-fire reference image
      postFiltCol = sits %>% 
        ee$ImageCollection$filterDate(postfireDate_sta, postfireDate_end) %>% 
        ee$ImageCollection$filterBounds(selFeat$geometry()) 
      
      preFiltColSize  = ee$Number(preFiltCol$size())
      postFiltColSize = ee$Number(postFiltCol$size())
      
      preFire = ee$Image(ee$Algorithms$If(
        
        #ee$Filter$Or(preFiltColSize$eq(0), postFiltColSize$eq(0)),
        preFiltColSize$eq(0)$Or(postFiltColSize$eq(0)),
        
        #ee$Number(preFiltColSize$add(postFiltColSize))$eq(0),
        
        ee$Image$constant(0),
        
        preFiltCol %>% 
          ee$ImageCollection$map(cloud_mask_fun) %>% 
          ee$ImageCollection$map(scale_data_fun) %>% 
          ee$ImageCollection$map(base_index_fun) %>% 
          ee$ImageCollection$median()
      ))
      
      
      postFire = ee$Image(ee$Algorithms$If( 
        
        #ee$Filter$Or(preFiltColSize$eq(0), postFiltColSize$eq(0)),
        preFiltColSize$eq(0)$Or(postFiltColSize$eq(0)),
        
        #ee$Number(preFiltColSize$add(postFiltColSize))$eq(0),
        
        ee$Image$constant(0),
        
        postFiltCol %>% 
          ee$ImageCollection$map(cloud_mask_fun) %>% 
          ee$ImageCollection$map(scale_data_fun) %>% 
          ee$ImageCollection$map(base_index_fun) %>% 
          ee$ImageCollection$median()
      ))
    }
    
    
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
      stop("Wrong value in severityIndicator for method spt_process_gee_task")
    }
      
    imColClip = ee$ImageCollection(list(imgDiffClip))
    
    imColPrevious = ee$ImageCollection(list(ee$Image(ee$List(previousImgResult)$get(0))))
    
    added = imColPrevious$merge(imColClip)$sum()$toInt()
    
    return(ee$List(list(added)))
  }
  
  
  ## ----------------------------------------------------------------------------------- ##
  
  # Default image to start the accumulation
  imgFirst = ee$List(list(ee$Image$constant(0)$rename(severityIndicator)$toInt()));
  
  # Iterate through each fire polygon
  accumRaster = baList$iterate(ee_utils_pyfunc(accumulate), imgFirst);
  
  # Clip data
  accumRasterClip = ee$Image(ee$List(accumRaster)$get(0))$clipToCollection(baData);
  
  ### Process the GEE task to google drive

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

