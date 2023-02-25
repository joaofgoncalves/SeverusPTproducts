

statusList <-function(){
  
  return(c(
    "NOT COMPLETED",
    "COMPLETED",
    
    "GEE NOT COMPLETED",
    "GEE TASKED",
    "GEE PROCESSING",
    "GEE PROCESSING ERROR",
    "GEE COMPLETED",
    "GEE CANCELLED",
    "GEE STATUS UNKNOWN",
    
    "POST NOT COMPLETED",
    "POST TASKED",
    "POST PROCESSING",
    "POST PROCESSING ERROR",
    "POST COMPLETED",
    
    "METADATA NOT COMPLETED",
    "METADATA TASKED",
    "METADATA PROCESSING",
    "METADATA PROCESSING ERROR",
    "METADATA COMPLETED",
    
    "CLOSING NOT COMPLETED",
    "CLOSING TASKED",
    "CLOSING PROCESSING",
    "CLOSING PROCESSING ERROR",
    "CLOSING COMPLETED"
   )
  )
}

checkValues <- function(value, what, verbose=TRUE) {
  
  # Validate numeric values
  if(what %in% c("preFireRef", "postFireRef", "minFireSize")){
    if(all(is.numeric(value)) && all((value > 0))){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  
  # Validate other params with specific values/bounded
  vals <- list(
    satCode = c(
      "S2MSI", "MOD", "MYD", "MCD", "L5TM", "L7ETM",
      "L8OLI", "L8TIRS", "LTH"
    ),
    procLevel = c("L1", "L1C", "L2", "L2A",
                  "NA","N/A",NA), # This may be undefined if MODIS is selected!
    modisProduct = c(
      "MOD09A1", "MOD13Q1", "MOD09GQ", "MOD09Q1", "MYD09A1",
      "MYD09GQ", "MYD09Q1", "MYD13Q1", "MCD43A4",
      "NA","N/A",NA # This may be undefined if S2/Landsat are selected!
    ),
    baseIndex = c(
      "NBR", "NDVI", "EVI", "TCTB", "TCTG", "TCTW", "LST",
      "LAI", "GPP", "NPP", "ALB", "FVC"
    ),
    severityIndicator = c("DELTA", "DLT", "RDELTA", "RDT", "S95"),
    burntAreaDataset = c(
      "ICNF", "EFFIS", "MCD64", "MICNF",
      "FireCCI", "VIIRS"
    ),
    referenceYear = 2001:as.integer(format(Sys.Date(), "%Y")),
    # preFireRef = c(),
    preFireType = c("moving", "fixed", "m", "f", "mov", "fix")
  )
  
  if (!(what %in% names(vals))) {
    if(verbose) message("Parameter ",what," cannot be checked by method checkValues")
    return(TRUE)
  } else {
    if (all(value %in% vals[[what]])){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}

makeTasksTable <- function(n){
  
  taskTable <- data.frame(
    
    # Task identifiers & status
    taskID          = integer(n),
    taskUID         = character(n),
    mainStatus      = character(n),
    fileName        = character(n),
    
    # Analysis parameters
    satCode           = character(n),
    procLevel         = character(n),
    modisProduct      = character(n),
    baseIndex         = character(n),
    severityIndicator = character(n),
    burntAreaDataset  = character(n),
    referenceYear     = integer(n),
    preFireRef        = integer(n), # in months
    preFireType       = character(n), # either single/static or moving/relative to the homologous year -1
    postFireRef       = integer(n), # in months
    minFireSize       = double(n),
    
    # GEE task code & status
    geeTaskID       = character(n),
    geeTaskCode     = character(n), # The Task ID from GEE platform
    geeTaskStatus   = character(n),
    
    # Post-processing task code & status 
    postProcTaskID      = character(n),
    postProcTaskStatus  = character(n),
    
    # Metadata task code & status
    metadataTaskID      = character(n),
    metadataTaskStatus  = character(n),
    
    # Closing task code & status
    closingTaskID       = character(n),
    closingTaskStatus   = character(n)
  )
  
  return(taskTable)
}


generateTasks <- function(taskTable = NULL, satCode, procLevel, modisProduct, 
                          baseIndex, severityIndicator, burntAreaDataset,
                          referenceYear, preFireRef, preFireType, 
                          postFireRef, minFireSize){
  
  
  params <- as.list(match.call())
  params <- params[-c(1,2)]
  
  for(nm in names(params)){
    
    if(inherits(params[[nm]],"call")){
      if(!checkValues(eval(params[[nm]]), nm)){
        stop("The ",nm," value is invalid. Check function call parameters.")
      }
    }else{
      if(!checkValues(params[[nm]], nm)){
        stop("The ",nm," value is invalid. Check function call parameters.")
      }
    }
  }
  
  # Generate combinations of parameters
  taskCombns <- expand.grid(satCode, procLevel, modisProduct, baseIndex, severityIndicator, 
                            burntAreaDataset, referenceYear, preFireRef, 
                            preFireType, postFireRef, minFireSize)
  
  # Name columns for indexing
  colnames(taskCombns) <- c("satCode","procLevel", "modisProduct", "baseIndex", 
                            "severityIndicator", "burntAreaDataset","referenceYear", 
                            "preFireRef", "preFireType", "postFireRef", "minFireSize")
  
  # Generate a new task table 
  nr <- nrow(taskCombns)
  newTaskTable <- makeTasksTable(nr)
  
  if(is.null(taskTable)){
    taskID_start <- 1
    taskID_end <- nr
  }else{
    taskID_start <- nrow(taskTable) + 1
    taskID_end <- nrow(taskTable) + nr
  }
  
  ### Save the task parameters to the task table
  
  # Sat data and indices
  newTaskTable$satCode           <- taskCombns$satCode
  newTaskTable$procLevel         <- taskCombns$procLevel
  newTaskTable$modisProduct      <- taskCombns$modisProduct
  
  # Add indices and severity indicators
  newTaskTable$baseIndex         <- taskCombns$baseIndex
  newTaskTable$severityIndicator <- taskCombns$severityIndicator
  newTaskTable$minFireSize       <- taskCombns$minFireSize
  
  # Burned dataset and reference year
  newTaskTable$burntAreaDataset  <- taskCombns$burntAreaDataset
  newTaskTable$referenceYear     <- taskCombns$referenceYear
  
  # Reference periods
  newTaskTable$preFireRef        <- taskCombns$preFireRef
  newTaskTable$preFireType       <- taskCombns$preFireType
  newTaskTable$postFireRef       <- taskCombns$postFireRef
  
  # Main task identifiers
  newTaskTable$taskID          <- taskID_start:taskID_end
  newTaskTable$taskUID         <- uuid::UUIDgenerate(n = nr)
  newTaskTable$mainStatus      <- "NOT COMPLETED"
  
  # GEE task codes & status
  newTaskTable$geeTaskID       <- uuid::UUIDgenerate(n = nr)
  newTaskTable$geeTaskCode     <- NA # The Task ID from GEE platform
  newTaskTable$geeTaskStatus   <- "GEE NOT COMPLETED"
  
  # Post-processing tasks code & status 
  newTaskTable$postProcTaskID      <- uuid::UUIDgenerate(n = nr)
  newTaskTable$postProcTaskStatus  <- "POST NOT COMPLETED"
  
  # Metadata tasks code & status
  newTaskTable$metadataTaskID      <- uuid::UUIDgenerate(n = nr)
  newTaskTable$metadataTaskStatus  <- "METADATA NOT COMPLETED"
  
  # Closing tasks code & status
  newTaskTable$closingTaskID       <- uuid::UUIDgenerate(n = nr)
  newTaskTable$closingTaskStatus   <- "CLOSING NOT COMPLETED"
  
  
  if(is.null(taskTable)){
    return(newTaskTable)
  }else{
    return(rbind(taskTable, newTaskTable))
  }
  
}

readTaskTable <- function(){
  
  # Acquire a lock over the file
  lck <- filelock::lock(paste0(SPT_TASK_TABLE_DIR, "/",
                               SPT_TASK_TABLE_BASENAME,
                               ".lock"), timeout = 30000)
  if(is.null(lck))
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  
  
  tb <- read.csv(SPT_TASK_TABLE_PATH)
  
  filelock::unlock(lck)
  
  return(tb)
  
}

writeTaskTable <- function(taskTable){
  
  # Acquire a lock over the file
  lck <- filelock::lock(paste0(SPT_TASK_TABLE_DIR, "/",
                        SPT_TASK_TABLE_BASENAME,
                        ".lock"), timeout = 30000)
  if(is.null(lck))
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  
  # After getting the lock write the file
  write.csv(taskTable, 
            file      = SPT_TASK_TABLE_PATH,
            row.names = FALSE)
  
  # Unlock to the file to make it available 
  # to other processes if needed
  filelock::unlock(lck)
  return(TRUE)
}




