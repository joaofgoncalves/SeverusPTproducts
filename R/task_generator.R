
#' Get List of Status Values
#'
#' This function returns a character vector containing a list of status values 
#' for a specific process.
#'
#' @return A character vector containing the list of status values.
#' @export
#' 

spt_status_list <- function(){
  
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


#' Check Validity of Parameter Values
#'
#' This function checks the validity of parameter values based on predefined criteria or value ranges.
#'
#' @param value The value(s) to be checked.
#' @param what A character string specifying the parameter name or type.
#' @param verbose A logical value indicating whether to display a message when a parameter cannot be checked. Default is TRUE.
#' @return A logical value indicating whether the value(s) pass the validity check.
#' @export
#' 

spt_check_values <- function(value, what, verbose=TRUE) {
  
  # Validate numeric values
  if(what %in% c("preFireRef", "postFireRef", "minFireSize")){
    if(all(is.numeric(value)) && all((value > 0))){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }
  
  # Validate other params with specific values/bounded
  #vals <- SPT_VALUES
  
  if (!(what %in% names(SPT_VALUES))) {
    if(verbose) message("Parameter ",what," cannot be checked by method spt_check_values")
    return(TRUE)
  } else {
    if (all(value %in% SPT_VALUES[[what]])){
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}


#' Create Tasks Table
#'
#' This function creates an empty tasks table with predefined columns to store task-related information.
#'
#' @param n The number of rows to create in the tasks table.
#' @return A data frame representing the tasks table with empty columns.
#' @export
#' 

spt_make_tasks_table <- function(n){
  
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
    closingTaskStatus   = character(n),
    
    # CRS information
    primaryCRScode      = character(n),
    secondaryCRScode    = character(n),
    primaryCRS          = character(n),
    secondaryCRS        = character(n)
  )
  
  return(taskTable)
}


#' Generate Tasks
#'
#' This function generates a table of tasks based on the provided analysis parameters.
#'
#' @param taskTable An optional existing tasks table to append the generated tasks to.
#' @param satCode The satellite code.
#' @param procLevel The processing level.
#' @param modisProduct The MODIS product.
#' @param baseIndex The base index.
#' @param severityIndicator The severity indicator.
#' @param burntAreaDataset The burnt area dataset.
#' @param referenceYear The reference year.
#' @param preFireRef The pre-fire reference period in months.
#' @param preFireType The type of pre-fire reference period.
#' @param postFireRef The post-fire reference period in months.
#' @param minFireSize The minimum fire size.
#' @return A data frame representing the tasks table with appended rows for the generated tasks.
#' @export
#' 

spt_generate_tasks <- function(taskTable = NULL, satCode, procLevel, modisProduct, 
                          baseIndex, severityIndicator, burntAreaDataset,
                          referenceYear, preFireRef, preFireType, 
                          postFireRef, minFireSize){
  
  
  params <- as.list(match.call())
  params <- params[-c(1,2)]
  
  for(nm in names(params)){
    
    if(inherits(params[[nm]],"call")){
      if(!spt_check_values(eval(params[[nm]]), nm)){
        stop("The ",nm," value is invalid. Check function call parameters.")
      }
    }else{
      if(!spt_check_values(params[[nm]], nm)){
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
  newTaskTable <- spt_make_tasks_table(nr)
  
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
  
  # CRS information
  newTaskTable$primaryCRScode <- "3763"
  newTaskTable$secondaryCRScode <- "32629"
  newTaskTable$primaryCRS <- "ETRS1989/PTTM06 (EPSG: 3763)"
  newTaskTable$secondaryCRS <- "WGS 1984/UTM 29N (EPSG: 32629)"
  
  
  if(is.null(taskTable)){
    return(newTaskTable)
  }else{
    return(rbind(taskTable, newTaskTable))
  }
  
}


#' Read Tasks Table
#'
#' This function reads a task table from a specified file path and returns the table as a data frame.
#' Uses a file lock.
#' 
#' @param taskTablePath The file path of the task table.
#' @return A data frame representing the read task table.
#' @export
#' 

spt_read_tasks_table <- function(taskTablePath){
  
  # Acquire a lock over the file
  lck <- filelock::lock(paste0(tools::file_path_sans_ext(taskTablePath),
                               ".lock"), timeout = 30000)
  
  if(is.null(lck))
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  
  #tb <- read.csv(SPT_TASK_TABLE_PATH)
  tb <- read.csv(taskTablePath)
  
  filelock::unlock(lck)
  
  return(tb)
  
}


#' Write Tasks Table
#'
#' This function writes a task table to a specified file path.
#' Uses a file lock over the to ensure concurrence management.
#' 
#' @param taskTable The task table to be written (data frame).
#' @param taskTablePath The file path where the task table should be written.
#' @return TRUE if the writing is successful, FALSE otherwise.
#' @export
#' 

spt_write_tasks_table <- function(taskTable, taskTablePath){
  
  # Acquire a lock over the file
  lck <- filelock::lock(paste0(tools::file_path_sans_ext(taskTablePath),
                               ".lock"), timeout = 30000)
  
  if(is.null(lck))
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  
  # After getting the lock write the file
  write.csv(taskTable, 
            file      = taskTablePath,
            row.names = FALSE)
  
  # Unlock to the file to make it available 
  # to other processes if needed
  filelock::unlock(lck)
  return(TRUE)
}




