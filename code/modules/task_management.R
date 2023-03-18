
#' Get a single task by its identifiers


spt_get_task <- function(taskTable      = NULL, 
                         taskID         = NULL, 
                         taskUID        = NULL, 
                         geeTaskID      = NULL,
                         geeTaskCode    = NULL, 
                         postProcTaskID = NULL, 
                         closingTaskID  = NULL) {
  
  if (is.null(taskID) && is.null(taskUID) && is.null(geeTaskID) &&
    is.null(geeTaskCode) && is.null(postProcTaskID) && is.null(metadataTaskID) &&
    is.null(closingTaskID)) {
    stop("At least one UID or ID needed")
  }

  if (is.null(taskTable)) {
    taskTable <- spt_read_tasks_table()
  }

  if (!is.null(taskID)) {
    outTask <- taskTable %>% filter(taskID == !!taskID)
  }
  if (!is.null(taskUID)) {
    outTask <- taskTable %>% filter(taskUID == !!taskUID)
  }
  if (!is.null(geeTaskID)) {
    outTask <- taskTable %>% filter(geeTaskID == !!geeTaskID)
  }
  if (!is.null(geeTaskCode)) {
    outTask <- taskTable %>% filter(geeTaskCode == !!geeTaskCode)
  }
  if (!is.null(taskID)) {
    outTask <- taskTable %>% filter(postProcTaskID == !!postProcTaskID)
  }
  if (!is.null(taskID)) {
    outTask <- taskTable %>% filter(closingTaskID == !!closingTaskID)
  }

  class(outTask) <- c("task", class(outTask))

  return(outTask)
}

#' Define the generic methods to access task parameters


spt_sat_code <- function(x) UseMethod("spt_sat_code", x)

spt_base_index <- function(x) UseMethod("spt_base_index", x)

spt_severity_indicator <- function(x) UseMethod("spt_severity_indicator", x)

spt_ba_dataset <- function(x) UseMethod("spt_ba_dataset", x)

spt_reference_year <- function(x) UseMethod("spt_reference_year", x)

spt_pre_fire_ref <- function(x) UseMethod("spt_pre_fire_ref", x)

spt_pre_fire_type <- function(x) UseMethod("spt_pre_fire_type", x)

spt_post_fire_ref <- function(x) UseMethod("spt_post_fire_ref", x)

spt_get_taskStatus <- function(x, ...) UseMethod("spt_get_taskStatus", x)

spt_post_window_days <- function(x, ...) UseMethod("spt_post_window_days", x)

spt_min_fire_size <- function(x) UseMethod("spt_min_fire_size", x)

spt_file_name <- function(x) UseMethod("spt_file_name", x)

spt_proc_level <- function(x) UseMethod("spt_proc_level", x)

spt_modis_product <- function(x) UseMethod("spt_modis_product", x)

spt_spatial_resolution <- function(x) UseMethod("spt_spatial_resolution", x)

#' Define the get functions to access task parameters


spt_sat_code.task <- function(x) as.character(x$satCode)

spt_base_index.task <- function(x) as.character(x$baseIndex)

spt_severity_indicator.task <- function(x) as.character(x$severityIndicator)

spt_ba_dataset.task <- function(x) as.character(x$burntAreaDataset)

spt_reference_year.task <- function(x) as.integer(x$referenceYear)

spt_pre_fire_ref.task <- function(x) as.integer(x$preFireRef)

spt_pre_fire_type.task <- function(x) as.character(x$preFireType)

spt_post_fire_ref.task <- function(x) as.integer(x$postFireRef)

spt_min_fire_size.task <- function(x) as.double(x$minFireSize)

spt_proc_level.task <- function(x) as.character(x$procLevel)

spt_modis_product.task <- function(x) as.character(x$modisProduct)


spt_get_taskStatus.task <- function(x, taskStep) {
  if (taskStep == "main") {
    return(as.character(x$mainStatus))
  }
  if (taskStep == "gee") {
    return(as.character(x$geeTaskStatus))
  }
  if (taskStep == "post") {
    return(as.character(x$postProcTaskStatus))
  }
  if (taskStep == "close") {
    return(as.character(x$closingTaskStatus))
  }
}

spt_spatial_resolution.task <- function(x) {
  if (x$satCode %in% SPT_VALUES$satCode_lt) {
    spatialRes <- 30
  } else if (x$satCode %in% SPT_VALUES$satCode_md) {
    if (x$modisProduct %in% SPT_VALUES$modisProduct[-c(10:12)]) {
      spatialRes <- 250
    } else if (x$modisProduct %in% c("MOD09A1", "MYD09A1", "MCD43A4")) {
      spatialRes <- 500
    } else {
      stop("Cannot find spatial resolution - unsupported MODIS product")
    }
  } else if (x$satCode == SPT_VALUES$satCode_s2) {
    spatialRes <- 20
  }
  else{
    stop("Satellite code not supported in spt_spatial_resolution")
  }

  return(spatialRes)
}

spt_post_window_days.task <- function(x) {
  return(c(
    (spt_post_fire_ref(x) - spt_pre_fire_ref(x)) * 30 + 1,
    spt_post_fire_ref(x) * 30
  ))
}


spt_task_filename <- function(task, projectAccronym, versionNumber){
  
  satCode           = spt_sat_code(task)
  baseIndex         = spt_base_index(task)
  severityIndicator = spt_severity_indicator(task)
  
  burntAreaDataset  = spt_ba_dataset(task)
  referenceYear     = spt_reference_year(task)
  
  fixedPreFireWindowSize    = spt_pre_fire_ref(task)
  preFireWindowType         = spt_pre_fire_type(task)
  postFireWindowEndMonths   = spt_post_fire_ref(task)
  
  refPeriods = paste0(
    # Pre-fire ref period
    ifelse(preFireWindowType %in% c("moving","mov","m"),"R","S"),spt_pad_number(fixedPreFireWindowSize),
    # Post-fire ref period
    "P",spt_pad_number(postFireWindowEndMonths))
  
  # prodName = spt_product_name(SPT_PROJ_ACRONYM, satCode, baseIndex, severityIndicator, 
  #                           burntAreaDataset, referenceYear, refPeriods, addCalcDate=FALSE)
  
  prodName = spt_product_name(ProjectAccronym = projectAccronym, 
                              SeverityIndicator = severityIndicator, 
                              BaseIndex         = baseIndex, 
                              SatCode           = satCode, 
                              BurntAreaDataset  = burntAreaDataset, 
                              ReferenceYear     = referenceYear, 
                              RefPeriods        = refPeriods, 
                              addCalcDate       = FALSE, 
                              VersionNumber     = versionNumber)
  
  return(prodName)
}


## ------------------------------------------------------------------------------------- ##
## TRASNFORM FUNCTIONS ----
## ------------------------------------------------------------------------------------- ##


spt_task_to_dataframe <- function(task) {
  data.frame(
    taskParam = colnames(task),
    taskValue = as.character(task[1, ])
  )
}


## ------------------------------------------------------------------------------------- ##
## UPDATE SUB-TASKS STATUS ----
## ------------------------------------------------------------------------------------- ##


spt_update_gee_task <- function(geeTask, task, taskTable=NULL, taskTablePath){
  
  # Acquire a lock over the file
  #lck <- filelock::lock(lockfilePath, timeout = 30000)
  lck <- filelock::lock(paste0(tools::file_path_sans_ext(taskTablePath),
                               ".lock"), timeout = 30000)
  
  if(is.null(taskTable)){
    taskTable <- spt_read_tasks_table(taskTablePath)
  }
  
  if(is.null(lck))
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  
  taskStatusList <- try(spt_gee_task_status(geeTask))
  
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
    spt_write_tasks_table(taskTable, taskTablePath)
    filelock::unlock(lck)
  })
  
  if(inherits(out,"try-error")){
    return(FALSE)
  }else{
    return(TRUE)
  }
  
}


spt_update_post_task <- function(task, state, taskTable=NULL, taskTablePath){
  
  # # Acquire a lock over the file
  # lck <- filelock::lock(paste0(SPT_TASK_TABLE_DIR, "/",
  #                              SPT_TASK_TABLE_BASENAME,
  #                              ".lock"), timeout = 30000)
  #lck <- filelock::lock(filelockPath, timeout = 30000)
  
  lck <- filelock::lock(paste0(tools::file_path_sans_ext(taskTablePath),
                               ".lock"), timeout = 30000)
  
  if(is.null(taskTable)){
    taskTable <- spt_read_tasks_table(taskTablePath)
  }
  
  if(is.null(lck))
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  
  
  idx <- taskTable$taskUID == task$taskUID
  taskTable[idx, "postProcTaskStatus"] <- state
  
  out <- try({
    #spt_write_tasks_table(taskTable)
    spt_write_tasks_table(taskTable, taskTablePath)
    
    filelock::unlock(lck)
  })
  
  if(inherits(out,"try-error")){
    return(FALSE)
  }else{
    return(TRUE)
  }
  
}


spt_update_meta_task <- function(task, state, taskTable = NULL, taskTablePath) {
  
  # Acquire a lock over the file
  # lck <- filelock::lock(paste0(
  #   SPT_TASK_TABLE_DIR, "/",
  #   SPT_TASK_TABLE_BASENAME,
  #   ".lock"
  # ), timeout = 30000)
  #lck <- filelock::lock(lockfilePath, timeout = 30000)
  
  lck <- filelock::lock(paste0(tools::file_path_sans_ext(taskTablePath),
                               ".lock"), timeout = 30000)
  
  if (is.null(taskTable)) {
    taskTable <- spt_read_tasks_table(taskTablePath)
  }
  
  if (is.null(lck)) {
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  }
  
  
  idx <- taskTable$taskUID == task$taskUID
  taskTable[idx, "metadataTaskStatus"] <- state
  
  out <- try({
    #spt_write_tasks_table(taskTable)
    spt_write_tasks_table(taskTable, taskTablePath)
    
    filelock::unlock(lck)
  })
  
  if (inherits(out, "try-error")) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}



spt_update_closing_task <- function(task, state, taskTable = NULL, taskTablePath) {
  # Acquire a lock over the file
  # lck <- filelock::lock(paste0(
  #   SPT_TASK_TABLE_DIR, "/",
  #   SPT_TASK_TABLE_BASENAME,
  #   ".lock"
  # ), timeout = 30000)
  lck <- filelock::lock(paste0(tools::file_path_sans_ext(taskTablePath),
                               ".lock"), timeout = 30000)

  if (is.null(taskTable)) {
    taskTable <- spt_read_tasks_table(taskTablePath)
  }

  if (is.null(lck)) {
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  }

  idx <- taskTable$taskUID == task$taskUID
  taskTable[idx, "closingTaskStatus"] <- state

  out <- try({
    spt_write_tasks_table(taskTable)
    filelock::unlock(lck)
  })

  if (inherits(out, "try-error")) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}

spt_update_main_task <- function(task, state, taskTable = NULL) {
  # Acquire a lock over the file
  lck <- filelock::lock(paste0(
    SPT_TASK_TABLE_DIR, "/",
    SPT_TASK_TABLE_BASENAME,
    ".lock"
  ), timeout = 30000)

  if (is.null(taskTable)) {
    taskTable <- spt_read_tasks_table()
  }

  if (is.null(lck)) {
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  }

  idx <- taskTable$taskUID == task$taskUID
  taskTable[idx, "mainStatus"] <- state

  out <- try({
    spt_write_tasks_table(taskTable)
    filelock::unlock(lck)
  })

  if (inherits(out, "try-error")) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


spt_rm_uncompleted_tasks <- function(taskTable = NULL) {
  # Acquire a lock over the file
  lck <- filelock::lock(paste0(
    SPT_TASK_TABLE_DIR, "/",
    SPT_TASK_TABLE_BASENAME,
    ".lock"
  ), timeout = 30000)
  
  if (is.null(taskTable)) {
    taskTable <- spt_read_tasks_table()
  }
  
  if (is.null(lck)) {
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  }
  
  idx <- taskTable$mainStatus == "COMPLETED"
  taskTable <- taskTable[idx, ]
  
  out <- try({
    spt_write_tasks_table(taskTable)
    filelock::unlock(lck)
  })
  
  if (inherits(out, "try-error")) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


