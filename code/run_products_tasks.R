

## --------------------------------------------------------------------------- ##
## LOAD LIBS ----
## --------------------------------------------------------------------------- ##



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
source("./CODE/modules/gee_prepare.R")
source("./CODE/modules/post_proc.R")
source("./CODE/modules/metadata.R")

# library(foreach)
# library(doParallel)
# if(!exists("cl")){
#   cl <- makeCluster(3)
#   registerDoParallel(cl)
# }



if(!spt_file_dates("gee_last_init.log", tdelta = 10)){
  
  cat("\nRefreshing previous GEE initialization at:",
      readr::read_lines("gee_last_init.log",n_max = 1),"\n\n")
  
  ee_Initialize(gcs = TRUE, drive=TRUE)
  spt_create_file("gee_last_init.log")
  
}else{
  cat("\nLast GEE initialization is OK:",
      readr::read_lines("gee_last_init.log",n_max = 1),"\n\n")
}


SPT_BOUND_BOX_POLYGON <<- ee$Geometry$Polygon(c(c(
  # CAOP + buffer 5km
  c(-9.59969546814, 42.20058580090),
  c(-9.55585216180, 36.92056016040),
  c(-6.31411752273, 36.89149009860),
  c(-6.10259922138, 42.16554977760))))


logToFile <- FALSE

xs = 1:nrow(spt_read_tasks_table(SPT_TASK_TABLE_PATH))


#foreach(i = xs) %dopar% {
for(tidx in xs){
    
  # tidx = i
  # 
  # setwd("C:/MyFiles/R-dev/SeverusPT_Products")
  # 
  # source("globvars.r")
  # ee_Initialize("joaofgo@gmail.com", gcs = TRUE, drive=TRUE)
  # 
  # source("./CODE/modules/r_utils.R")
  # source("./CODE/modules/task_generator.R")
  # source("./CODE/modules/task_management.R")
  # source("./CODE/modules/gee_data.R")
  # source("./CODE/modules/gee_indices.R")
  # source("./CODE/modules/gee_utils.R")
  # source("./CODE/modules/gee_calc.R")
  # source("./CODE/modules/gee_prepare.R")
  # source("./CODE/modules/post_proc.R")
  # source("./CODE/modules/metadata.R")
  

  # Read the main task table - update task list
  ttb <- spt_read_tasks_table(SPT_TASK_TABLE_PATH)
  
  # Select the target task
  task <- spt_get_task(ttb, taskUID = ttb$taskUID[tidx],
                       taskTablePath = SPT_TASK_TABLE_PATH)
  
  # Generate the task filename
  taskFn <- spt_task_filename(task            = task,
                              projectAccronym = SPT_PROJ_ACRONYM, 
                              versionNumber   = SPT_VERSION)
  
  
  cat(blue("\n-----------------------------------------------------------------------\n"))
  cat(blue(bold(symbol$check, "TASK STARTING\n")))
  cat(blue(bold(symbol$arrow_right,"Task ID/UID:",task$taskID,"/",task$taskUID,"\n")))
  cat(blue(bold(symbol$arrow_right,"Task filename: [",taskFn,"]\n")))
  cat(blue("-----------------------------------------------------------------------\n\n"))
  
  cat(green(bold(
    symbol$arrow_right, "Reading task data and selecting target task...\n\n")))
  
  # Make a Log file to register progress
  logFile <- paste0(SPT_LOG_PATH,"/log_TaskID_",spt_pad_number(task$taskID),"-UID_",
                    substr(task$taskUID,1,8),"-",taskFn,".txt")
  
  if(logToFile){
    sink(logFile)
    on.exit(sink())
    
  }
  
  # Print task params
  taskMd <- spt_df_to_md(spt_task_to_dataframe(task))
  print(taskMd)
  cat("\n\n")
  
  # Check if the main task is already completed
  
  if(task$mainStatus=="COMPLETED"){
    
    cat(green(bold(
      symbol$tick, "Task completed! Skipping to the next...\n\n")))
    #return(NA)
    next
  }
  
  
  ## TODO: Add two operational mode called: "SEQUENTIAL"and "PARALLEL" (multiple jobs)
  
  ## If "PARALLEL" check any of the jobs and if any part of it has been started 
  ## just skip the whole job
  
  ## If "SEQUENTIAL" then allow to follow the normal sequence of jobs which can eventually be
  ## used to run parts that were not completed
   
  ## --------------------------------------------------------------------------- ##
  ## 1. GEE CLOUD PROCESSING STAGE ----
  ## --------------------------------------------------------------------------- ##
  
  
  if(task$geeTaskStatus != "GEE COMPLETED"){
    
    cat(green(bold(
      symbol$arrow_right, "Processing Google Earth Engine task...\n\n")))
    
    
    ### Process task in GEE ----
    #
    #
    
    # Get the field names in the ba dataset used 
    # Retrieve this metadata from global variables
    #
    if(task$burntAreaDataset == "ICNF"){
      
      baAsset   = SPT_ICNF_GEE_ASSET 
      dateField = SPT_ICNF_DATE_FIELD
      yearField = SPT_ICNF_YEAR_FIELD
      areaField = SPT_ICNF_AREA_FIELD
      
    } else if(task$burntAreaDataset == "EFFIS"){
      
      ## TODO: Add different operational modes for EFFIS? Historical data vs current
      
      
      #SPT_EFFIS_HIST_GEE_ASSET
      #SPT_EFFIS_CURR_GEE_ASSET
      
      baAsset   = SPT_EFFIS_GEE_ASSET 
      dateField = SPT_EFFIS_DATE_FIELD
      yearField = SPT_EFFIS_YEAR_FIELD
      areaField = SPT_EFFIS_AREA_FIELD
      
    } else{
      stop("Unsupported burnt area dataset name in burntAreaDataset!")
    }
    
    
    geeTaskObj <- spt_process_gee_task(task        = task, 
                                       outFolder   = SPT_GDRIVE_FOLDER, 
                                       boundBox    = SPT_BOUND_BOX_POLYGON, 
                                       coordRefSys = SPT_PROJ_COORD_SYSTEM_CODE,
                                       baGEEasset  = baAsset,
                                       dateField   = dateField,
                                       yearField   = yearField, 
                                       areaField   = areaField)
    
    # Get the GEE task status in the server
    taskStatusList <- spt_gee_task_status(geeTaskObj)
    
    cat(green(bold(
      symbol$arrow_right, "Update GEE task status...\n\n")))
    
    # Wait for a couple of seconds and then update the taskTable status for GEE
    Sys.sleep(time = 30)
    
    spt_update_gee_task(geeTaskObj, task, taskTable = NULL, 
                        taskTablePath = SPT_TASK_TABLE_PATH)
    
                        # lockfilePath = paste0(SPT_TASK_TABLE_DIR, "/",
                        #                       SPT_TASK_TABLE_BASENAME, ".lock")
    
    # Refresh the target task status by re-loading it
    task <- spt_get_task(taskUID = ttb$taskUID[tidx],
                         taskTablePath = SPT_TASK_TABLE_PATH) 
    
    cat(green(bold(
      symbol$arrow_right, "Checking GEE task completion ...\n\n")))
    
    # Check the task status continuously 
    # This check stops if the task fails or completes 
    geeProcCheck <- spt_check_gee_task(geeTaskObj, verbose = TRUE)
    
    # Save the GEE status object
    cat(green(bold(
      symbol$arrow_right, "Saving GEE task list object...\n\n")))
    
    # Get the GEE status
    geeStatusList <- spt_gee_task_status(geeTaskObj)
    
    # Save the GEE Status List object
    spt_save_gee_status_list(task          = task, 
                             geeStatusList = geeStatusList, 
                             geeTaskPath   = SPT_GEE_TASK_PATH)

    
    if(geeProcCheck$state=="FAILED"){
      
      # Update the taskTable status for GEE
      spt_update_gee_task(geeTaskObj, task,
                          taskTablePath = SPT_TASK_TABLE_PATH)
                          # lockfilePath = paste0(SPT_TASK_TABLE_DIR, "/",
                          #                       SPT_TASK_TABLE_BASENAME, ".lock")
                          
      # Refresh the target task status by re-loading it
      task <- spt_get_task(taskUID = ttb$taskUID[tidx],
                           taskTablePath = SPT_TASK_TABLE_PATH)
      
      stop(symbol$cross,"GEE Process ID: ", geeProcCheck$id, " failed")
    }
    
    cat(green(bold(
      symbol$arrow_right, "Update GEE task final status...\n\n")))
    
    # Update the taskTable status for GEE
    spt_update_gee_task(geeTaskObj, task,
                        taskTablePath = SPT_TASK_TABLE_PATH)
                        # lockfilePath = paste0(SPT_TASK_TABLE_DIR, "/",
                        #                       SPT_TASK_TABLE_BASENAME, ".lock")
    
    # Refresh the target task status by re-loading it
    task <- spt_get_task(taskUID = ttb$taskUID[tidx],
                         taskTablePath = SPT_TASK_TABLE_PATH)
    
    cat("\n---- TASK UPDATE ----\n\n")
    taskMd <- spt_df_to_md(spt_task_to_dataframe(task))
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
    spt_update_post_task(task, state="POST PROCESSING", 
                         taskTablePath = SPT_TASK_TABLE_PATH)
                         #filelockPath = paste0(SPT_TASK_TABLE_DIR, "/", SPT_TASK_TABLE_BASENAME, ".lock")
    
    # Refresh the target task status by re-loading it
    task <- spt_get_task(taskUID = ttb$taskUID[tidx],
                         taskTablePath = SPT_TASK_TABLE_PATH)
    
    cat(green(bold(
      symbol$arrow_right, "Downloading data from Google Drive folder...\n\n")))
    
    
    # Download data from Google Drive folder
    #
    #
    outFilePath <- try(spt_download_gdrive(geeTask   = geeTaskObj, 
                                           outFolder = SPT_GEE_PRODUCTS_PATH))
    
    ### --- RECOVERY MODE OF DATA DOWNLOAD !!! --- ####
    
    if(inherits(outFilePath,"try-error")){
      
      cat(red(bold(
        symbol$cross,"Unable to download data for GEE task:", spt_gee_task_status(geeTaskObj)$id,"\n")))
      
      
      
      cat(yellow(bold(
        symbol$warning,"Trying to recover data from GEE Task List\n\n")))
      
      geeStatusList <- spt_read_gee_status_list(task = task, 
                                                geeTaskPath = SPT_GEE_TASK_PATH)
      
      # Download data from GDrive
      #
      #
      # outFilePath <- try(spt_download_gdrive(geeStatusList))
      outFilePath <- try(spt_download_gdrive(geeTask   = geeTaskObj, 
                                             outFolder = SPT_GEE_PRODUCTS_PATH))
      
      if(inherits(outFilePath, "try-error")){
        
        cat(red(bold(
          symbol$cross,"Unable to download data for GEE task:", spt_gee_task_status(geeTaskObj)$id,"\n")))
        
        stop("Unable to download data for GEE task: ", spt_gee_task_status(geeTaskObj)$id,"\n")
      }
    }
    
    cat(green(bold(
      symbol$arrow_right, "Doing a backup on the downloaded raster file...\n\n")))
    
    # Create a copy of the downloaded file
    #
    #
    bk <- try(spt_backup_file(outFilePath))
    
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
    r0 <- try(spt_raster_zeros_to_na(outFilePath, writeRast=TRUE, datatype ="INT4S"))
    
    cat(green(bold(
      symbol$arrow_right, "Projecting raster data to ETRS-1989/PT-TM06...\n\n")))
    
    
    # Project data to ETRS 1989 / PT TM 06 CRS
    #
    #
    r1 <- try(spt_project_to_pttm06(outFilePath, datatype ="INT4S", method="near"))
    
    if(inherits(r0,"try-error") || inherits(r1,"try-error")){
      
      # Update the taskTable status for post-processing
      spt_update_post_task(task, state="POST PROCESSING ERROR", 
                           taskTablePath = SPT_TASK_TABLE_PATH)
                           #filelockPath = paste0(SPT_TASK_TABLE_DIR, "/", SPT_TASK_TABLE_BASENAME, ".lock"))
      
      # Refresh the target task status by re-loading it
      task <- spt_get_task(taskUID = ttb$taskUID[tidx],
                           taskTablePath = SPT_TASK_TABLE_PATH)
      
      stop("An error occurred while performing post-processing tasks!")
      
    }else{
      
      # Update the taskTable status for post-processing
      spt_update_post_task(task, state="POST COMPLETED", 
                           taskTablePath = SPT_TASK_TABLE_PATH)
                           #filelockPath = paste0(SPT_TASK_TABLE_DIR, "/", SPT_TASK_TABLE_BASENAME, ".lock"))
      
      # Refresh the target task status by re-loading it
      task <- spt_get_task(taskUID = ttb$taskUID[tidx],
                           taskTablePath = SPT_TASK_TABLE_PATH)
    }
    
    cat("\n---- TASK UPDATE ----\n\n")
    taskMd <- spt_df_to_md(spt_task_to_dataframe(task))
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
    
    # Generate metadata based  on the pre-defined template
    #
    #
    meta <- spt_fill_meta_template(
      metaTemplate = SPT_META_TABLE,
      metaList = list(
        ProductType          = "Observed/historical severity",
        ProductName          = paste("Fire/burn severity / Indicator:",
                                     task$severityIndicator,task$baseIndex,
                                     "/",task$preFireType,"window",task$preFireRef,"months composite"),
        SpatialResolution    = paste(res(r0)[1], "meters"),
        TemporalResolution   = paste(task$preFireRef,"months composite"),
        CoordRefSystem       = "Primary CRS: ETRS1989/PTTM06 / Secondary CRS: WGS 1984/UTM 29N",
        CalculationDate      = paste(spt_current_date_time(),"Lisbon GMT +00:00"),
        CalculationPlatforms = paste("Google Earth Engine; R/RStudio; EE-API-version:", rgee::ee_version(),
                                     "/ rgee-version:",packageVersion("rgee")),
        BurntAreaDataset     = task$burntAreaDataset,
        BurntAreaDatasetURL  = spt_ba_data_url(task$burntAreaDataset),
        ReferenceYear        = task$referenceYear,
        MinFireSize          = paste(task$minFireSize,"hectares"),
        SatCollectionData    = paste(task$satCode," / ",spt_sat_mission_fullname(task$satCode),
                                     ifelse(task$satCode %in% SPT_VALUES$satCode_md, 
                                            paste(" / Product:",task$modisProduct), ""), sep=""),
        SatProcLevel         = spt_proc_levels(task$procLevel),
        #SatColVersion       =
        CloudMaskUsed        = "Yes",
        CloudMaskMethod      = "Pixel QA band: Cirrus, clouds and/or cloud shadows removed",
        BaseIndex            = paste(task$baseIndex, spt_spec_index_fullname(task$baseIndex),sep=" - "),
        BaseIndexFormula     = spt_spec_index_formula(task$baseIndex, task$satCode),
        SeverityIndicator    = task$severityIndicator,
        SeverityIndicatorForm = spt_severity_indicator_form(task$baseIndex, task$severityIndicator),
        CompAggMeasure        = "Median",
        PreFireType           = paste(task$preFireType, "/ considers the year before fire, i.e., homologous year"),
        PreFireRefPeriod      = paste(task$preFireRef,"months"),
        PostFireType          = "moving",
        PostFireRefPeriod     = paste("Start post-fire window:",
                                      task$postFireRef-task$preFireRef,"months",
                                      "/ End:",task$postFireRef,"months",", i.e.,",
                                      paste(spt_post_window_days(task),collapse=" to "),
                                      "days after ignition date"),
        VersionNumber         = SPT_VERSION,
        VersionFullNumber     = SPT_FULL_VERSION_NR
      )
    )
    
    # Export metadata to Markdown txt and JSON files
    out <- try({
      
      spt_export_to_md(meta, paste0(outFilePath,".meta.txt"))
      write.csv(meta, paste0(outFilePath,".meta.csv"), row.names = FALSE, quote = TRUE)
      spt_export_meta_to_json(meta, paste0(outFilePath,".meta.json"))
      
    })
    
    
    if(inherits(out,"try-error")){
      
      # Update the taskTable status for post-processing
      spt_update_meta_task(task, state = "METADATA PROCESSING ERROR", 
                           taskTablePath = SPT_TASK_TABLE_PATH)
                           #lockfilePath = paste0(SPT_TASK_TABLE_DIR, "/", SPT_TASK_TABLE_BASENAME, ".lock"))
      
      # Refresh the target task status by re-loading it
      task <- spt_get_task(taskUID = ttb$taskUID[tidx],
                           taskTablePath = SPT_TASK_TABLE_PATH)
      
      stop("An error occurred while performing metadata tasks!")
      
    }else{
      # Update the taskTable status for post-processing
      spt_update_meta_task(task, state = "METADATA COMPLETED", 
                           taskTablePath = SPT_TASK_TABLE_PATH)
                           #lockfilePath = paste0(SPT_TASK_TABLE_DIR, "/", SPT_TASK_TABLE_BASENAME, ".lock"))
      
      # Refresh the target task status by re-loading it
      task <- spt_get_task(taskUID = ttb$taskUID[tidx],
                           taskTablePath = SPT_TASK_TABLE_PATH)
    }
    
    cat("\n---- TASK UPDATE ----\n\n")
    taskMd <- spt_df_to_md(spt_task_to_dataframe(task))
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
    
    spt_update_closing_task(task, "CLOSING COMPLETED", 
                            taskTablePath = SPT_TASK_TABLE_PATH)
    
    task <- spt_get_task(taskUID = ttb$taskUID[tidx],
                         taskTablePath = SPT_TASK_TABLE_PATH)
    
  }else{
    
    cat(green(bold(
      symbol$tick, "Closing sub-task completed! Skipping to the next...\n\n")))
  
  }
  
  if(task$mainStatus != "COMPLETED"){
    
    # Close the task! Fully completed status
    if(task$geeTaskStatus     == "GEE COMPLETED" &&
      task$postProcTaskStatus == "POST COMPLETED" &&
      task$metadataTaskStatus == "METADATA COMPLETED" &&
      task$closingTaskStatus  == "CLOSING COMPLETED"){
      
      spt_update_main_task(task, "COMPLETED", 
                           taskTablePath = SPT_TASK_TABLE_PATH)
      
      task <- spt_get_task(taskUID = ttb$taskUID[tidx],
                           taskTablePath = SPT_TASK_TABLE_PATH)
      
    }
  }

  # Save task metadata
  spt_export_to_md(spt_task_to_dataframe(task), paste0(outFilePath,".task.txt"))
  

  cat("\n---- TASK UPDATE ----\n\n")
  taskMd <- spt_df_to_md(spt_task_to_dataframe(task))
  print(taskMd)
  cat("\n\n")
  
  if(logToFile){
    sink()
  }
  
  cat(green(bold("\n-----------------------------------------------------------------------\n")))
  cat(green(bold(symbol$check, "TASK COMPLETED\n")))
  cat(green(bold("-----------------------------------------------------------------------\n\n")))
  
}


