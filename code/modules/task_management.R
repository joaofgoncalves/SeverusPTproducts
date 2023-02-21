
#' Get a single task by its identifiers

#`%>%` <- magrittr::`%>%`

getTask <- function(taskTable = NULL, taskID = NULL, taskUID = NULL, geeTaskID = NULL,
                    geeTaskCode = NULL, postProcTaskID = NULL, closingTaskID = NULL){
  
  if(is.null(taskID) && is.null(taskUID) && is.null(geeTaskID) &&
     is.null(geeTaskCode) && is.null(postProcTaskID) && is.null(metadataTaskID ) &&
     is.null(closingTaskID)){
    stop("At least one UID/ID needed")
  }
  
  if(is.null(taskTable)){
    taskTable <- readTaskTable()
  }
  
  if(!is.null(taskID)){
    outTask <- taskTable %>% filter(taskID == !!taskID)
  }
  if(!is.null(taskUID)){
    outTask <- taskTable %>% filter(taskUID == !!taskUID)
  }
  if(!is.null(geeTaskID)){
    outTask <- taskTable %>% filter(geeTaskID == !!geeTaskID)
  }
  if(!is.null(geeTaskCode)){
    outTask <- taskTable %>% filter(geeTaskCode == !!geeTaskCode)
  }
  if(!is.null(taskID)){
    outTask <- taskTable %>% filter(postProcTaskID == !!postProcTaskID)
  }
  if(!is.null(taskID)){
    outTask <- taskTable %>% filter(closingTaskID == !!closingTaskID)
  }
  
  class(outTask) <- c("task",class(outTask))
  
  return(outTask)
  
}

#' Define the generic methods to access task parameters


getSatCode <- function(x) UseMethod("getSatCode", x)

getBaseIndex <- function(x) UseMethod("getBaseIndex", x)

getSeverityIndicator <- function(x) UseMethod("getSeverityIndicator", x)

getBurntAreaDataset <- function(x) UseMethod("getBurntAreaDataset", x)

getReferenceYear <- function(x) UseMethod("getReferenceYear", x)

getPreFireRef <- function(x) UseMethod("getPreFireRef", x)

getPreFireType <- function(x) UseMethod("getPreFireType", x)

getPostFireRef <- function(x) UseMethod("getPostFireRef", x)

getTaskStatus <- function(x, ...) UseMethod("getTaskStatus", x)

getPostWindowDays <- function(x, ...) UseMethod("getPostWindowDays", x)

getMinFireSize <- function(x) UseMethod("getMinFireSize", x)

getFileName <- function(x) UseMethod("getFileName", x)

#' Define the get functions to access task parameters


getSatCode.task <- function(x) as.character(x$satCode)

getBaseIndex.task <- function(x) as.character(x$baseIndex)

getSeverityIndicator.task <- function(x) as.character(x$severityIndicator)

getBurntAreaDataset.task <- function(x) as.character(x$burntAreaDataset)

getReferenceYear.task <- function(x) as.integer(x$referenceYear)

getPreFireRef.task <- function(x) as.integer(x$preFireRef)

getPreFireType.task <- function(x) as.character(x$preFireType)

getPostFireRef.task <- function(x) as.integer(x$postFireRef)

getMinFireSize.task <- function(x) as.double(x$minFireSize)

getTaskStatus.task <- function(x, taskStep){
  
  if(taskStep == "main")
    return(as.character(x$mainStatus))
  if(taskStep == "gee")
    return(as.character(x$geeTaskStatus))
  if(taskStep == "post")
    return(as.character(x$postProcTaskStatus))
  if(taskStep == "close")
    return(as.character(x$closingTaskStatus))
}

getPostWindowDays.task <- function(x){
  return(c(
    (getPostFireRef(x) - getPreFireRef(x))*30+1,
    getPostFireRef(x)*30))
}

taskToDataframe <- function(task){
  data.frame(taskParam = colnames(task),
             taskValue = as.character(task[1,]))
}

updateClosingTaskStatus <- function(task, state, taskTable=NULL){
  
  # Acquire a lock over the file
  lck <- filelock::lock(paste0(SPT_TASK_TABLE_DIR, "/",
                               SPT_TASK_TABLE_BASENAME,
                               ".lock"), timeout = 30000)
  
  if(is.null(taskTable)){
    taskTable <- readTaskTable()
  }
  
  if(is.null(lck))
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  
  
  idx <- taskTable$taskUID == task$taskUID
  taskTable[idx, "closingTaskStatus"] <- state
  
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

updateMainTaskStatus <- function(task, state, taskTable=NULL){
  
  # Acquire a lock over the file
  lck <- filelock::lock(paste0(SPT_TASK_TABLE_DIR, "/",
                               SPT_TASK_TABLE_BASENAME,
                               ".lock"), timeout = 30000)
  
  if(is.null(taskTable)){
    taskTable <- readTaskTable()
  }
  
  if(is.null(lck))
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  
  
  idx <- taskTable$taskUID == task$taskUID
  taskTable[idx, "mainStatus"] <- state
  
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
