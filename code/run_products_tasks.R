


## --------------------------------------------------------------------------- ##
## LOAD LIBS ----
## --------------------------------------------------------------------------- ##


# library(foreach)
# library(doParallel)
# library(doFuture)
# library(progressr)

source("globvars.r")
source("./CODE/modules/r_utils.R")
source("./CODE/modules/task_generator.R")
source("./CODE/modules/task_management.R")
source("./CODE/modules/gee_data.R")
source("./CODE/modules/gee_indices.R")
source("./CODE/modules/gee_utils.R")
source("./CODE/modules/gee_calc.R")
source("./CODE/modules/post_proc.R")
source("./CODE/modules/metadata.R")

# 
# cl <- makeCluster(4)
# registerDoParallel(cl)


ee_Initialize("joaofgo", gcs = TRUE, drive=TRUE)

# CAOP + buffer 5km
ptbox = ee$Geometry$Polygon(c(c(
  c(-9.59969546814, 42.20058580090),
  c(-9.55585216180, 36.92056016040),
  c(-6.31411752273, 36.89149009860),
  c(-6.10259922138, 42.16554977760))))


logToFile <- FALSE

xs = 1:nrow(readTaskTable())


#foreach(i = xs) %do% {
for(tidx in xs){
    
  # tidx = i
  # 
  # setwd("C:/MyFiles/R-dev/SeverusPT_Products")
  # 
  # source("globvars.r")
  # #ee_Initialize()
  # 
  # 
  # source("./CODE/modules/r_utils.R")
  # source("./CODE/modules/task_generator.R")
  # source("./CODE/modules/task_management.R")
  # source("./CODE/modules/gee_data.R")
  # source("./CODE/modules/gee_indices.R")
  # source("./CODE/modules/gee_utils.R")
  # source("./CODE/modules/gee_calc.R")
  # source("./CODE/modules/post_proc.R")
  # source("./CODE/modules/metadata.R")
  



  # Read the main task table
  ttb <- readTaskTable()
  
  # Select the target task
  task <- getTask(ttb, taskUID = ttb$taskUID[tidx])
    # Generate the task filename
  taskFn <- getTaskFileName(task)
  
  
  cat(blue("\n-----------------------------------------------------------------------\n"))
  cat(blue(bold(symbol$check, "TASK STARTING\n")))
  cat(blue(bold(symbol$arrow_right,"Task ID/UID:",task$taskID,"/",task$taskUID,"\n")))
  cat(blue(bold(symbol$arrow_right,"Task filename: [",taskFn,"]\n")))
  cat(blue("-----------------------------------------------------------------------\n\n"))
  
  cat(green(bold(
    symbol$arrow_right, "Reading task data and selecting target task...\n\n")))
  
  # Make a Log file to register progress
  logFile <- paste0(SPT_LOG_PATH,"/log_TaskID_",padNumber(task$taskID),"-UID_",
                    substr(task$taskUID,1,8),"-",taskFn,".txt")
  
  if(logToFile){
    sink(logFile)
    on.exit(sink())
    
  }
  
  # Print task params
  taskMd <- dataframeToMarkdown(taskToDataframe(task))
  print(taskMd)
  cat("\n\n")
  
  # Check if the main task is already completed
  
  if(task$mainStatus=="COMPLETED"){
    
    cat(green(bold(
      symbol$tick, "Task completed! Skipping to the next...\n\n")))
    #return(NA)
    next
  }
  
  
  ## --------------------------------------------------------------------------- ##
  ## 1. GEE CLOUD PROCESSING STAGE ----
  ## --------------------------------------------------------------------------- ##
  
  
  if(task$geeTaskStatus != "GEE COMPLETED"){
    
    cat(green(bold(
      symbol$arrow_right, "Processing Google Earth Engine task...\n\n")))
    
    # Process task in GEE
    #
    #
    geeTaskObj <- processGEEtask(task, 
                                 outFolder   = "GEE", 
                                 boundBox    = ptbox, 
                                 coordRefSys = "EPSG:32629")
    
    # Get the GEE task status in the server
    taskStatusList <- getGEEtaskStatus(geeTaskObj)
    
    cat(green(bold(
      symbol$arrow_right, "Update GEE task status...\n\n")))
    
    # Wait for a couple of seconds and then update the taskTable status for GEE
    Sys.sleep(time = 20)
    updateGEEtaskStatus(geeTaskObj, task, taskTable=NULL)
    
    # Refresh the target task status by re-loading it
    task <- getTask(taskUID = ttb$taskUID[tidx]) 
    
    cat(green(bold(
      symbol$arrow_right, "Checking GEE task completion ...\n\n")))
    
    # Check the task status continuously 
    # This check stops if the task fails or completes 
    geeProcCheck <- checkGEEtaskCompletion(geeTaskObj, verbose = TRUE)
    
    # Save the GEE status object
    
    cat(green(bold(
      symbol$arrow_right, "Saving GEE task list object...\n\n")))
    
    # Get the GEE status
    geeStatusList <- getGEEtaskStatus(geeTaskObj)
    
    # Save the GEE Status List object
    saveGEEstatusList(task, geeStatusList)

    
    if(geeProcCheck$state=="FAILED"){
      
      # Update the taskTable status for GEE
      updateGEEtaskStatus(geeTaskObj, task)
      # Refresh the target task status by re-loading it
      task <- getTask(taskUID = ttb$taskUID[tidx])
      
      stop(symbol$cross,"GEE Process ID:", geeProcCheck$id, "failed")
    }
    
    cat(green(bold(
      symbol$arrow_right, "Update GEE task final status...\n\n")))
    
    # Update the taskTable status for GEE
    updateGEEtaskStatus(geeTaskObj, task)
    
    # Refresh the target task status by re-loading it
    task <- getTask(taskUID = ttb$taskUID[tidx])
    
    cat("\n---- TASK UPDATE ----\n\n")
    taskMd <- dataframeToMarkdown(taskToDataframe(task))
    print(taskMd)
    cat("\n\n")
    
    
  }else{
    
    cat(green(bold(
      symbol$tick, "GEE sub-task completed! Skipping to the next...\n\n")))
    
    
  }
  
  
  ## --------------------------------------------------------------------------- ##
  ## 2. POST-PROCESSING STAGE ----
  ## --------------------------------------------------------------------------- ##
  
  
  if(task$postProcTaskStatus != "POST COMPLETED"){
    
    cat(green(bold(
      symbol$arrow_right, "Running post-processing tasks...\n\n")))
    
    
    # Update the taskTable status for post-processing
    updatePostProcTaskStatus(task, state="POST PROCESSING")
    # Refresh the target task status by re-loading it
    task <- getTask(taskUID = ttb$taskUID[tidx])
    
    cat(green(bold(
      symbol$arrow_right, "Downloading data from Google Drive folder...\n\n")))
    
    
    # Download data from Google Drive folder
    #
    #
    outFilePath <- try(downloadGEEdata(geeTaskObj))
    
    if(inherits(outFilePath,"try-error")){
      
      cat(red(bold(
        symbol$cross,"Unable to download data for GEE task:", getGEEtaskStatus(geeTaskObj)$id,"\n")))
      
      
      ### RECOVERY OF DATA DOWNLOAD !!! ####
      cat(yellow(bold(
        symbol$warning,"Trying to recover data from GEE Task List\n\n")))
      
      geeStatusList <- readGEEstatusList(task)
      
      # Download data from GDrive
      #
      #
      outFilePath <- try(downloadGEEdata(geeStatusList))
      
      if(inherits(outFilePath, "try-error")){
        
        cat(red(bold(
          symbol$cross,"Unable to download data for GEE task:", getGEEtaskStatus(geeTaskObj)$id,"\n")))
        
        stop("Unable to download data for GEE task: ", getGEEtaskStatus(geeTaskObj)$id,"\n")
      }
    }
    
    cat(green(bold(
      symbol$arrow_right, "Doing a backup on the downloaded raster file...\n\n")))
    
    # Create a copy of the downloaded file
    #
    #
    bk <- try(backupFile(outFilePath))
    
    if(inherits(bk,"try-error")){
      cat(red(bold(
        symbol$cross,"Unable to backup data for GEE task!\n\n"
      )))
      stop("Unable to backup data for GEE task!")
    }
    
    cat(green(bold(
      symbol$arrow_right, "Converting zeros to NA's...\n\n")))
    
      
    # Convert zeros no NA (zeros are the default null/no-data value exported by GEE)
    #
    #
    r0 <- try(rasterZerosToNA(outFilePath, writeRast=TRUE, datatype ="INT2S"))
    
    cat(green(bold(
      symbol$arrow_right, "Projecting raster data to ETRS-1989/PT-TM06...\n\n")))
    
    
    # Project data to ETRS 1989 / PT TM 06 CRS
    #
    #
    r1 <- try(projPTTM06(outFilePath, datatype ="INT2S", method="near"))
    
    if(inherits(r0,"try-error") || inherits(r1,"try-error")){
      
      # Update the taskTable status for post-processing
      updatePostProcTaskStatus(task, state="POST PROCESSING ERROR")
      # Refresh the target task status by re-loading it
      task <- getTask(taskUID = ttb$taskUID[tidx])
      
      stop("An error occurred while performing post-processing tasks!")
      
    }else{
      
      # Update the taskTable status for post-processing
      updatePostProcTaskStatus(task, state="POST COMPLETED")
      # Refresh the target task status by re-loading it
      task <- getTask(taskUID = ttb$taskUID[tidx])
    }
    
    cat("\n---- TASK UPDATE ----\n\n")
    taskMd <- dataframeToMarkdown(taskToDataframe(task))
    print(taskMd)
    cat("\n\n")
    
  }else{
    
    cat(green(bold(
      symbol$tick, "Post-processing sub-task completed! Skipping to the next...\n\n")))
    
    
  }
  
  
  ## --------------------------------------------------------------------------- ##
  ## 3. METADATA STAGE ----
  ## --------------------------------------------------------------------------- ##
  
  
  if(task$metadataTaskStatus != "METADATA COMPLETED"){
    
    
    cat(green(bold(
      symbol$arrow_right, "Creating metadata...\n\n")))
    
  
    
    # Generate metadata
    #
    #
    meta <- metaTableTemplate(list(
      
      ProductType          = "Observed/historical severity",
      ProductName          = paste("Fire/burn severity / Indicator:",
                                   task$severityIndicator,task$baseIndex,
                                   "/",task$preFireType,"window",task$preFireRef,"months composite"),
      SpatialResolution    = paste(res(r0)[1], "meters"),
      TemporalResolution   = paste(task$preFireRef,"months composite"),
      CoordRefSystem       = "Primary CRS: ETRS1989/PTTM06 / Secondary CRS: WGS 1984/UTM 29N",
      CalculationDate      = getCurrentDatetime(),
      CalculationPlatforms = "Google Earth Engine; R/RStudio",
      BurntAreaDataset     = task$burntAreaDataset,
      BurntAreaDatasetURL  = getBurntAreaDataURL(task$burntAreaDataset),
      ReferenceYear        = task$referenceYear,
      MinFireSize          = paste(task$minFireSize,"hectares"),
      SatCollectionData    = paste(task$satCode,getSatMissionName(task$satCode),sep=" - "),
      SatProcLevel         = "Surface reflectance (L2A)",
      #SatColVersion        = 
      CloudMask            = "Yes",
      BaseIndex            = paste(task$baseIndex, getSpectralIndexName(task$baseIndex),sep=" - "),
      BaseIndexFormula     = getSpecIndFormula(task$baseIndex),
      SeverityIndicator    = task$severityIndicator,
      SeverityIndicatorForm = getSeverityIndicatorForm(task$baseIndex, task$severityIndicator),
      CompAggMeasure        = "Median",
      PreFireType           = task$preFireType,
      PreFireRefPeriod      = paste(task$preFireRef,"months"),
      PostFireType          = "moving",
      PostFireRefPeriod     = paste("Start post-fire window:",
                                    task$postFireRef-task$preFireRef,"months",
                                   "/ End:",task$postFireRef,"months",", i.e.,", 
                                   paste(getPostWindowDays(task),collapse=" to "),
                                   "days after ignition date")
      ))
    
    # Export metadata to Markdown txt and JSON files
    out <- try({
      
      exportToMarkdown(meta, paste0(outFilePath,".meta.txt"))
      write.csv(meta, paste0(outFilePath,".meta.csv"), row.names = FALSE, quote = TRUE)
      exportMetaToJSON(meta, paste0(outFilePath,".meta.json"))
      
    })
    
    
    if(inherits(out,"try-error")){
      # Update the taskTable status for post-processing
      updateMetaTaskStatus(task, state="METADATA PROCESSING ERROR")
      # Refresh the target task status by re-loading it
      task <- getTask(taskUID = ttb$taskUID[tidx])
      
      stop("An error occurred while performing metadata tasks!")
      
    }else{
      # Update the taskTable status for post-processing
      updateMetaTaskStatus(task, state="METADATA COMPLETED")
      # Refresh the target task status by re-loading it
      task <- getTask(taskUID = ttb$taskUID[tidx])
    }
    
    cat("\n---- TASK UPDATE ----\n\n")
    taskMd <- dataframeToMarkdown(taskToDataframe(task))
    print(taskMd)
    cat("\n\n")
  }else{
    
    cat(green(bold(
      symbol$tick, "Metadata sub-task completed! Skipping to the next...\n\n")))
    
    
  }
  
  
  ## --------------------------------------------------------------------------- ##
  ## 4. CLOSING TASKS STAGE ----
  ## --------------------------------------------------------------------------- ##
  

  if(task$closingTaskStatus != "CLOSING COMPLETED"){
    
    cat(green(bold(
      symbol$arrow_right, "Performing closing tasks...\n\n")))
    
    updateClosingTaskStatus(task, "CLOSING COMPLETED")
    task <- getTask(taskUID = ttb$taskUID[tidx])
    
  }else{
    
    cat(green(bold(
      symbol$tick, "Closing sub-task completed! Skipping to the next...\n\n")))
  
  }
  
  # Close the task! Fully completed status
  if(task$geeTaskStatus     == "GEE COMPLETED" &&
    task$postProcTaskStatus == "POST COMPLETED" &&
    task$metadataTaskStatus == "METADATA COMPLETED" &&
    task$closingTaskStatus  == "CLOSING COMPLETED"){
    
    updateMainTaskStatus(task, "COMPLETED")
    task <- getTask(taskUID = ttb$taskUID[tidx])
    
  }
  
  # Save task metadata
  exportToMarkdown(taskToDataframe(task), paste0(outFilePath,".task.txt"))
  

  cat("\n---- TASK UPDATE ----\n\n")
  taskMd <- dataframeToMarkdown(taskToDataframe(task))
  print(taskMd)
  cat("\n\n")
  
  if(logToFile){
    sink()
  }
  
  cat(green(bold("\n-----------------------------------------------------------------------\n")))
  cat(green(bold(symbol$check, "TASK COMPLETED\n")))
  cat(green(bold("-----------------------------------------------------------------------\n\n")))
  
  
  #TRUE    
}




