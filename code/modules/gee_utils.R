

## Ancillary GEE functions (rgee)


## ----------------------------------------------------------------------------------- ##
## TASK MANAGEMENT FUNCTIONS ----
## ----------------------------------------------------------------------------------- ##

getGEEtaskStatus <- function(geeTask){
  
  # Check task Status and convert back to a R list object
  out <- ee_utils_py_to_r(ee$batch$Task$status(geeTask))
  
  # Add extra parameters to feed the download function without using the
  # actual GEE task object (which is non-storable)
  out$ExportOptions  <- geeTask[["config"]][["fileExportOptions"]]
  out$filenamePrefix <- out$ExportOptions[["driveDestination"]][["filenamePrefix"]]
  out$fileFormat     <- out$ExportOptions[["fileFormat"]]
  
  # Get URI for google drive if the calculation is finished
  if("destination_uris" %in% names(out)){
    out$basename_uri   <- basename(out[["destination_uris"]])
  }
  
  return(out)
} 


updateGEEtaskStatus <- function(geeTask, task, taskTable=NULL){
  

  if(is.null(taskTable)){
    taskTable <- readTaskTable()
  }
  
  # Acquire a lock over the file
  lck <- filelock::lock(paste0(SPT_TASK_TABLE_DIR, "/",
                               SPT_TASK_TABLE_BASENAME,
                               ".lock"), timeout = 30000)
  
  if(is.null(lck))
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  
  taskStatusList <- try(getGEEtaskStatus(geeTask))
  
  if(inherits(taskStatusList,"try-error")){
    
    return(FALSE)
    
  }else{
    
    if(taskStatusList$state %in% c("RUNNING","READY")){
      geeTaskStatus <- "GEE PROCESSING"
    }
    else if(taskStatusList$state == "FAILED"){
      geeTaskStatus <- "GEE PROCESSING ERROR"
    }
    else if(taskStatusList$state == "COMPLETED"){
      geeTaskStatus <- "GEE COMPLETED"
    }
    else if(taskStatusList$state == "CANCELLED"){
      geeTaskStatus <- "GEE CANCELLED"
    }
    else{
      geeTaskStatus <- "GEE STATUS UNKNOWN"
    }
    
    idx <- taskTable$taskUID == task$taskUID
    taskTable[idx, "geeTaskCode"]   <- taskStatusList$id
    taskTable[idx, "geeTaskStatus"] <- geeTaskStatus
    taskTable[idx, "fileName"]      <- taskStatusList$description
  
  }
  
  out <- try({
    writeTaskTable(taskTable)
    filelock::unlock(lck)
  })
  
  if(inherits(out,"try-error")){
    return(FALSE)
  }else{
    return(TRUE)
  }
  
}


checkGEEtaskCompletion <- function(geeTask, sleep=30, verbose=FALSE){
  
  
  # initialize the status variable
  status <- "RUNNING"
  ti <- Sys.time()
  
  # use a while loop to check the status every n seconds
  repeat{
    
    # call the getGEEtaskStatus function and store the result in the status variable
    geeStatus <- getGEEtaskStatus(geeTaskObj)
    
    status <- geeStatus$state
    
    tf = Sys.time()
    diff <- as.numeric(difftime(tf, ti, units = "secs"))
    
    # Calculate the number of minutes and seconds in the time difference
    mins <- floor(diff / 60)
    secs <- diff %% 60
    
    if(verbose){
      cat("\nGEE Process ID:",geeStatus$id,"|\nStatus:",geeStatus$state,
          "| approx. run time:",mins,"mins",round(secs),"seconds\n")
    }
    
    if(status %in% c("COMPLETED", "FAILED", "CANCELLED")){
      break
    }else{
      Sys.sleep(sleep)
      if(verbose) cat("\014")
    }
  }
  
  if(verbose){
    if(status=="COMPLETED"){
      if(verbose) message("GEE Process ID: ",geeStatus$id," completed")
    }
    if(status=="FAILED"){
      if(verbose) warning("GEE Process ID: ",geeStatus$id," failed")
    }
    if(status=="CANCELLED"){
      if(verbose) warning("GEE Process ID: ",geeStatus$id," cancelled")
    }
  }
  return(geeStatus)
}


downloadGEEdata <- function(geeTask, dataFormat="tif"){
  
  # Use a ee.batch.Task / python.builtin.object
  
  if(inherits(geeTask,"ee.batch.Task")){
    
    taskStatusList <- getGEEtaskStatus(geeTask)
    
    if(taskStatusList$state == "COMPLETED"){
      # 
      # outFile <- paste0(SPT_GEE_PRODUCTS_PATH,"/",
      #                   taskStatusList$description,"_",getDateString(),".",dataFormat)
      
      outFile <- paste0(SPT_GEE_PRODUCTS_PATH,"/",
                        taskStatusList$description,".",dataFormat)
      
      out <- try(ee_drive_to_local(task = geeTask, dsn = outFile, 
                                   overwrite = TRUE, consider = "last"))
      
    }else{
      return(NULL)
    }
    
  # Use a GEE status list instead 
    
  }else if(inherits(geeTask, "list")){
    
    # outFile <- paste0(SPT_GEE_PRODUCTS_PATH,"/",
    #                   geeTask$description,"_",getDateString(),".",dataFormat)
    
    outFile <- paste0(SPT_GEE_PRODUCTS_PATH,"/",
                      geeTask$description,".",dataFormat)
    
    out <- try(ee_drive_to_local(task = geeTask, dsn = outFile, 
                                 overwrite = TRUE, consider = "last"))
    
  }else{
    stop("Non-supported object type in geeTask for method downloadGEEdata.")
  }
  
  if(inherits(out,"try-error")){
    return(NULL)
  }else{
    return(outFile)
  }
}


getTaskFileName <- function(task){
  
  satCode           = getSatCode(task)
  baseIndex         = getBaseIndex(task)
  severityIndicator = getSeverityIndicator(task)
  
  burntAreaDataset  = getBurntAreaDataset(task)
  referenceYear     = getReferenceYear(task)
  
  fixedPreFireWindowSize    = getPreFireRef(task)
  preFireWindowType         = getPreFireType(task)
  postFireWindowEndMonths   = getPostFireRef(task)

  refPeriods = paste0(
    # Pre-fire ref period
    ifelse(preFireWindowType %in% c("moving","mov","m"),"R","S"),padNumber(fixedPreFireWindowSize),
    # Post-fire ref period
    "P",padNumber(postFireWindowEndMonths))
  
  # prodName = getProductName(SPT_PROJ_ACRONYM, satCode, baseIndex, severityIndicator, 
  #                           burntAreaDataset, referenceYear, refPeriods, addCalcDate=FALSE)
  
  prodName = getProductName(ProjectAccronym   = SPT_PROJ_ACRONYM, 
                            SeverityIndicator = severityIndicator, 
                            BaseIndex         = baseIndex, 
                            SatCode           = satCode, 
                            BurntAreaDataset  = burntAreaDataset, 
                            ReferenceYear = referenceYear, 
                            RefPeriods    = refPeriods, 
                            addCalcDate   = FALSE, 
                            VersionNumber = SPT_VERSION)
  
  return(prodName)
}


saveGEEstatusList <- function(task, geeStatusList){
  
  saveRDS(geeStatusList, paste0(SPT_GEE_TASK_PATH,"/geeStatusList_TaskID_",
                                padNumber(task$taskID),"-UID_",
                                substr(task$taskUID,1,8),".rds"))
  
  write.csv(geeStatusList, paste0(SPT_GEE_TASK_PATH,"/geeStatusList_TaskID_",
                                  padNumber(task$taskID),"-UID_",
                                  substr(task$taskUID,1,8),".csv"), row.names = FALSE)
  
}


readGEEstatusList <- function(task){
  
  geeStatusList <- readRDS(paste0(SPT_GEE_TASK_PATH,"/geeStatusList_TaskID_",
                                padNumber(task$taskID),"-UID_",
                                substr(task$taskUID,1,8),".rds"))
  return(geeStatusList)
}



ee_drive_to_local_mod <- function (task, dsn, overwrite = TRUE, consider = TRUE, 
                                   public = FALSE, metadata = FALSE, quiet = FALSE){
  
  
  rgee:::ee_check_packages("ee_drive_to_local", "googledrive")
  ee_user <- rgee:::ee_exist_credentials()
  
  if (is.na(ee_user[["drive_cre"]])) {
    drive_credential <- rgee:::ee_create_credentials_drive(ee_user$email)
    rgee:::ee_save_credential(pdrive = drive_credential)
    message("Google Drive credentials were not loaded.", 
            " Running ee_Initialize(user = '", ee_user[["email"]], 
            "', drive = TRUE)", " to fix.")
  }
  

  gd_folder <- task$basename_uri
  gd_filename <- task$filenamePrefix
  count <- 1
  
  files_gd <- try(googledrive::drive_find(q = sprintf("'%s' in parents", 
                                                      gd_folder), 
                                          q = sprintf("name contains '%s'", gd_filename)), 
                  silent = TRUE)
  
  while (any(class(files_gd) %in% "try-error") & count < 5) {
    files_gd <- try(googledrive::drive_find(q = sprintf("'%s' in parents", 
                                                        gd_folder), 
                                            q = sprintf("name contains '%s'", gd_filename)), 
                    silent = TRUE)
    count <- count + 1
  }
  
  if (public) {
    googledrive::with_drive_quiet({
      files_gd <- googledrive::drive_share_anyone(file = files_gd)
    })
  }
  
  if (nrow(files_gd) > 0) {
    ee_getTime <- function(x) {
      gd_file_date <- files_gd[["drive_resource"]][[x]][["createdTime"]]
      as.POSIXct(gd_file_date)
    }
    
    createdTime <- vapply(seq_len(nrow(files_gd)), ee_getTime, 0)
    
    files_gd <- files_gd[order(createdTime, decreasing = TRUE), ]
    
    if (isTRUE(consider)) {
      choices <- c(files_gd[["name"]], "last", "all")
      if (nrow(files_gd) == 1) {
        file_selected <- 1
      }
      else {
        file_selected <- menu(choices = choices, 
                              title = paste0("Multiple files with the same name", 
                                             " (sorted according to the created time argument):"))
      }
      if (choices[file_selected] == "last") {
        files_gd <- files_gd[1, ]
      }
      else if (choices[file_selected] == "all") {
        files_gd <- files_gd
      }
      else {
        files_gd <- files_gd[file_selected, ]
      }
    }
    else if (consider == "last") {
      files_gd <- files_gd[1, ]
    }
    else if (consider == "all") {
      files_gd <- files_gd
    }
    else {
      stop("consider argument was not defined properly.")
    }
  }
  else {
    stop("File does not exist in Google Drive.", 
         " Please verify if the task finished properly.")
  }
  
  #fileformat <- toupper(gd_ExportOptions[["fileFormat"]])
  fileformat <- toupper(task$fileFormat)
  
  
  if (missing(dsn)) {
    ee_tempdir <- tempdir()
    filenames_local <- sprintf("%s/%s", ee_tempdir, basename(files_gd$name))
  }
  else {
    pattern <- "(.*)(\\..*)$"
    element_len <- length(files_gd$name)
    if (task$task_type == "EXPORT_IMAGE" & element_len > 
        1) {
      file_ft <- sprintf("-%04d%s", seq_len(element_len), 
                         sub(pattern, "\\2", files_gd$name))
    }
    else {
      file_ft <- sub(pattern, "\\2", files_gd$name)
    }
    dsn_n <- sub(pattern, "\\1", basename(dsn))
    filenames_local <- sprintf("%s/%s%s", dirname(dsn), dsn_n, 
                               file_ft)
  }
  
  filenames_local <- rgee:::ee_sort_localfiles(filenames_local, fileformat)
  to_download <- rgee:::sort_drive_files(files_gd, fileformat)
  
  for (index in seq_len(nrow(to_download))) {
    if (!quiet) {
      googledrive::with_drive_quiet({
        googledrive::drive_download(file = to_download[index, 
        ], path = filenames_local[index], overwrite = overwrite)
      })
    }
    else {
      googledrive::drive_download(file = to_download[index, 
      ], path = filenames_local[index], overwrite = overwrite)
    }
  }
  
  if (metadata) {
    list(dsn = filenames_local, 
         metadata = list(ee_id = task$id, 
                         drive_name = to_download$name, drive_id = to_download$id, 
                         drive_download_link = sprintf("https://drive.google.com/uc?id=%s&export=download", 
                                                       to_download$id)))
  }
  else {
    filenames_local
  }
}

