

## ------------------------------------------------------------------------------------- ##
## GET TASKS AND THEIR PROPERTIES ----
## ------------------------------------------------------------------------------------- ##


#' Get a single task by its identifiers
#'
#' This function retrieves a specific task from a task table based on the provided parameters.
#' It can search for a task using taskID, taskUID, geeTaskID, geeTaskCode, postProcTaskID, or
#' closingTaskID.
#'
#' @param taskTable A data frame representing the task table. If not provided, it will be
#' read from the specified taskTablePath.
#' @param taskID The ID of the task to retrieve.
#' @param taskUID The UID of the task to retrieve.
#' @param geeTaskID The GEE task ID of the task to retrieve.
#' @param geeTaskCode The GEE task code of the task to retrieve.
#' @param postProcTaskID The post-processing task ID of the task to retrieve.
#' @param closingTaskID The closing task ID of the task to retrieve.
#' @param taskTablePath The path to the task table file.
#'
#' @return A data frame representing the retrieved task, with an additional "task" class.
#'
#' @examples
#' spt_get_task(taskTable = myTaskTable, taskID = "12345")
#'
#' @export
#'

spt_get_task <- function(taskTable      = NULL,
                         taskID         = NULL,
                         taskUID        = NULL,
                         geeTaskID      = NULL,
                         geeTaskCode    = NULL,
                         postProcTaskID = NULL,
                         closingTaskID  = NULL,
                         taskTablePath) {

  if (is.null(taskID) && is.null(taskUID) && is.null(geeTaskID) &&
    is.null(geeTaskCode) && is.null(postProcTaskID) && is.null(metadataTaskID) &&
    is.null(closingTaskID)) {
    stop("At least one UID or ID needed")
  }

  if (is.null(taskTable)) {
    taskTable <- spt_read_tasks_table(taskTablePath)
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
#'
#' This function is a dispatch method that performs various satellite analysis
#' operations based on the input object. The specific method to be executed is determined
#' by the class of the input object.
#'
#' @param x An object for which the satellite analysis operation is to be performed.
#' @param ... Additional arguments to be passed to the dispatched method.
#'
#' @export
#'

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

spt_primary_crs_code <- function(x) UseMethod("spt_primary_crs_code", x)

spt_secondary_crs_code <- function(x) UseMethod("spt_secondary_crs_code", x)

spt_primary_crs <- function(x) UseMethod("spt_primary_crs", x)

spt_secondary_crs <- function(x) UseMethod("spt_secondary_crs", x)


## ------------------------------------------------------------------------------------- ##
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

# Added get functions for CRS codes
spt_primary_crs_code.task <- function(x) as.character(x$primaryCRScode)

spt_secondary_crs_code.task <- function(x) as.character(x$secondaryCRScode)

spt_primary_crs.task <- function(x) as.character(x$primaryCRS)

spt_secondary_crs.task <- function(x) as.character(x$secondaryCRS)


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


## ------------------------------------------------------------------------------------- ##
## OTHER PROPERTIES ----
## ------------------------------------------------------------------------------------- ##

#' Spatial Resolution for Task
#'
#' This function determines the spatial resolution for a given task based on the satellite
#' code and MODIS product (if applicable).
#'
#' @param x A task object containing information about the satellite code and the MODIS
#' product (if applicable).
#'
#' @return The spatial resolution in meters.
#'
#' @examples
#' task <- spt_get_task(taskTable = myTaskTable, taskID = "12345")
#' spt_spatial_resolution.task(task)
#'
#' @export
#'

spt_spatial_resolution.task <- function(x) {
  if (x$satCode %in% c("L5TM", "L7ETM", "L8OLI", "L8TIRS", "LTH")) {

    spatialRes <- 30

  } else if (x$satCode %in% c("MOD", "MYD", "MCD")) {
    if (x$modisProduct %in% c("MOD13Q1", "MOD09GQ", "MOD09Q1",
                              "MYD09GQ", "MYD09Q1", "MYD13Q1")) {
      spatialRes <- 250

    } else if (x$modisProduct %in% c("MOD09A1", "MYD09A1", "MCD43A4")) {

      spatialRes <- 500

    } else {
      stop("Cannot find spatial resolution - unsupported MODIS product")
    }
  } else if (x$satCode == "S2MSI") {

    spatialRes <- 20

  }
  else{
    stop("Satellite code not supported in spt_spatial_resolution")
  }

  return(spatialRes)
}


#' Post-Fire Window Days for Task
#'
#' This function calculates the number of days in the post-fire window for a given task,
#' based on the pre-fire and post-fire references.
#'
#' @param x A task object containing information about the pre-fire and post-fire references.
#'
#' @return A numeric vector with two elements: the number of days in the post-fire window
#' from pre-fire reference to post-fire reference, and the number of days in the post-fire
#' reference.
#'
#' @examples
#' task <- spt_get_task(taskTable = myTaskTable, taskID = "12345")
#' spt_post_window_days.task(task)
#'
#' @export
#'

spt_post_window_days.task <- function(x) {
  return(c(
    (spt_post_fire_ref(x) - spt_pre_fire_ref(x)) * 30 + 1,
    spt_post_fire_ref(x) * 30
  ))
}


#' Generate Task Filename
#'
#' This function generates a filename for a given task based on its properties and the
#' specified project acronym and version number.
#'
#' @param task A task object containing information about the task.
#' @param projectAcronym A character string specifying the project acronym.
#' @param versionNumber A numeric value specifying the version number.
#'
#' @return A character string representing the generated filename for the task.
#'
#' @examples
#' task <- spt_get_task(taskTable = myTaskTable, taskID = "12345")
#' spt_task_filename(task, projectAcronym = "ABC", versionNumber = 1.0)
#'
#' @export
#'

spt_task_filename <- function(task, projectAccronym, versionNumber){

  satCode           = spt_sat_code(task)
  baseIndex         = spt_base_index(task)
  severityIndicator = spt_severity_indicator(task)

  burntAreaDataset  = spt_ba_dataset(task)
  referenceYear     = spt_reference_year(task)

  fixedPreFireWindowSize    = spt_pre_fire_ref(task)
  preFireWindowType         = spt_pre_fire_type(task)
  postFireWindowEndMonths   = spt_post_fire_ref(task)
  crsCode                   = spt_secondary_crs_code(task) # Added secondary CRS code

  refPeriods = paste0(
    # Pre-fire ref period
    ifelse(preFireWindowType %in% c("moving","mov","m"),"R","S"),spt_pad_number(fixedPreFireWindowSize),
    # Post-fire ref period
    "P",spt_pad_number(postFireWindowEndMonths))

  prodName = spt_product_name(ProjectAccronym   = projectAccronym,
                              SeverityIndicator = severityIndicator,
                              BaseIndex         = baseIndex,
                              SatCode           = satCode,
                              BurntAreaDataset  = burntAreaDataset,
                              ReferenceYear     = referenceYear,
                              RefPeriods        = refPeriods,
                              CRScode           = crsCode, # Added primary CRS code
                              addCalcDate       = FALSE,
                              VersionNumber     = versionNumber)

  return(prodName)
}


## ------------------------------------------------------------------------------------- ##
## TRASNFORM FUNCTIONS ----
## ------------------------------------------------------------------------------------- ##


#' Convert Task to Data Frame
#'
#' This function converts a task object into a data frame, where each column represents a
#' task parameter and the corresponding value.
#'
#' @param task A task object containing information about the task.
#'
#' @return A data frame with two columns: "taskParam" representing the task parameter names,
#' and "taskValue" representing the corresponding task values.
#'
#' @examples
#' task <- spt_get_task(taskTable = myTaskTable, taskID = "12345")
#' spt_task_to_dataframe(task)
#'
#' @export
#'

spt_task_to_dataframe <- function(task) {
  data.frame(
    taskParam = colnames(task),
    taskValue = as.character(task[1, ])
  )
}


## ------------------------------------------------------------------------------------- ##
## UPDATE SUB-TASKS STATUS ----
## ------------------------------------------------------------------------------------- ##

#' Update GEE Task
#'
#' This function updates the status and information of a Google Earth Engine (GEE) task in
#' the task table.
#'
#' @param geeTask The GEE task object to update.
#' @param task The task object containing information about the task.
#' @param taskTable The task table to update (optional). If not provided, it will be
#' read from the specified \code{taskTablePath}.
#' @param taskTablePath The file path to the task table.
#'
#' @return \code{TRUE} if the update is successful, \code{FALSE} otherwise.
#'
#' @examples
#' task <- spt_get_task(taskTable = myTaskTable, taskID = "12345")
#' geeTask <- spt_create_gee_task(task)
#' spt_update_gee_task(geeTask, task, taskTable = myTaskTable, taskTablePath = "path/to/taskTable.csv")
#'
#' @export
#'

spt_update_gee_task <- function(geeTask, task, taskTable=NULL, taskTablePath){

  # Acquire a lock over the file
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


#' Start GEE Task status
#'
#' This function updates the status of a task in the task table to indicate that the associated
#' Google Earth Engine (GEE) task has started.
#'
#' @param task The task object containing information about the task.
#' @param taskTable The task table to update (optional). If not provided, it will be read
#' from the specified \code{taskTablePath}.
#' @param taskTablePath The file path to the task table.
#'
#' @return \code{TRUE} if the update is successful, \code{FALSE} otherwise.
#'
#' @examples
#' task <- spt_get_task(taskTable = myTaskTable, taskID = "12345")
#' spt_start_gee_task(task, taskTable = myTaskTable, taskTablePath = "path/to/taskTable.csv")
#'
#' @export
#'

spt_start_gee_task <- function(task, taskTable=NULL, taskTablePath){

  # Acquire a lock over the file
  lck <- filelock::lock(paste0(tools::file_path_sans_ext(taskTablePath),
                               ".lock"), timeout = 30000)

  if(is.null(taskTable)){
    taskTable <- spt_read_tasks_table(taskTablePath)
  }

  if(is.null(lck))
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)

  idx <- taskTable$taskUID == task$taskUID
  taskTable[idx, "geeTaskStatus"] <- "GEE STARTED"

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


#' Update Post-Processing Task status
#'
#' This function updates the status of a post-processing task in the task table.
#'
#' @param task The task object containing information about the task.
#' @param state The new state of the post-processing task.
#' @param taskTable The task table to update (optional). If not provided, it will
#' be read from the specified \code{taskTablePath}.
#' @param taskTablePath The file path to the task table.
#'
#' @return \code{TRUE} if the update is successful, \code{FALSE} otherwise.
#'
#' @examples
#' task <- spt_get_task(taskTable = myTaskTable, taskID = "12345")
#' spt_update_post_task(task, state = "COMPLETED", taskTable = myTaskTable,
#' taskTablePath = "path/to/taskTable.csv")
#'
#' @export
#'

spt_update_post_task <- function(task, state, taskTable=NULL, taskTablePath){
  # Acquire a lock over the file
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


#' Update Metadata Task status
#'
#' This function updates the status of a metadata task in the task table.
#'
#' @param task The task object containing information about the task.
#' @param state The new state of the metadata task.
#' @param taskTable The task table to update (optional). If not provided,
#' it will be read from the specified \code{taskTablePath}.
#' @param taskTablePath The file path to the task table.
#'
#' @return \code{TRUE} if the update is successful, \code{FALSE} otherwise.
#'
#' @examples
#' task <- spt_get_task(taskTable = myTaskTable, taskID = "12345")
#' spt_update_meta_task(task, state = "COMPLETED", taskTable = myTaskTable,
#' taskTablePath = "path/to/taskTable.csv")
#'
#' @export
#'

spt_update_meta_task <- function(task, state, taskTable = NULL, taskTablePath) {
  # Acquire a lock over the file
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


#' Update Closing Task status
#'
#' This function updates the status of a closing task in the task table.
#'
#' @param task The task object containing information about the task.
#' @param state The new state of the closing task.
#' @param taskTable The task table to update (optional). If not provided,
#' it will be read from the specified \code{taskTablePath}.
#' @param taskTablePath The file path to the task table.
#'
#' @return \code{TRUE} if the update is successful, \code{FALSE} otherwise.
#'
#' @examples
#' task <- spt_get_task(taskTable = myTaskTable, taskID = "12345")
#' spt_update_closing_task(task, state = "COMPLETED", taskTable = myTaskTable,
#' taskTablePath = "path/to/taskTable.csv")
#'
#' @export
#'

spt_update_closing_task <- function(task, state, taskTable = NULL, taskTablePath) {
  # Acquire a lock over the file
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


#' Update Main Task status
#'
#' This function updates the status of a main task in the task table.
#'
#' @param task The task object containing information about the task.
#' @param state The new state of the main task.
#' @param taskTable The task table to update (optional). If not provided,
#' it will be read from the specified \code{taskTablePath}.
#' @param taskTablePath The file path to the task table.
#'
#' @return \code{TRUE} if the update is successful, \code{FALSE} otherwise.
#'
#' @examples
#' task <- spt_get_task(taskTable = myTaskTable, taskID = "12345")
#' spt_update_main_task(task, state = "COMPLETED", taskTable = myTaskTable,
#' taskTablePath = "path/to/taskTable.csv")
#'
#' @export
#'

spt_update_main_task <- function(task, state, taskTable = NULL, taskTablePath) {
  # Acquire a lock over the file
  lck <- filelock::lock(paste0(tools::file_path_sans_ext(taskTablePath),
                               ".lock"), timeout = 30000)

  if (is.null(taskTable)) {
    taskTable <- spt_read_tasks_table(taskTablePath)
  }

  if (is.null(lck)) {
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  }

  idx <- taskTable$taskUID == task$taskUID
  taskTable[idx, "mainStatus"] <- state

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


#' Remove completed tasks
#'
#' This function removes completed tasks from the task table.
#'
#' @param taskTable The task table to remove completed tasks from (optional).
#' If not provided, it will be read from the specified \code{taskTablePath}.
#' @param taskTablePath The file path to the task table.
#'
#' @return \code{TRUE} if the removal is successful, \code{FALSE} otherwise.
#'
#' @examples
#' spt_rm_completed_tasks(taskTable = myTaskTable, taskTablePath = "path/to/taskTable.csv")
#'
#' @export
#'

spt_rm_completed_tasks <- function(taskTable = NULL, taskTablePath) {
  # Acquire a lock over the file
  lck <- filelock::lock(paste0(tools::file_path_sans_ext(taskTablePath),
                               ".lock"), timeout = 30000)

  if (is.null(taskTable)) {
    taskTable <- spt_read_tasks_table(taskTablePath)
  }

  if (is.null(lck)) {
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  }

  idx <- taskTable$mainStatus == "COMPLETED"
  taskTable <- taskTable[-idx, ]

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



#' Remove not completed tasks
#'
#' This function removes uncompleted tasks from the task table.
#'
#' @param taskTable The task table to remove not completed tasks from (optional).
#' If not provided, it will be read from the specified \code{taskTablePath}.
#' @param taskTablePath The file path to the task table.
#'
#' @return \code{TRUE} if the removal is successful, \code{FALSE} otherwise.
#'
#' @examples
#' spt_rm_not_completed_tasks(taskTable = myTaskTable, taskTablePath = "path/to/taskTable.csv")
#'
#' @export
#'

spt_rm_not_completed_tasks <- function(taskTable = NULL, taskTablePath) {
  # Acquire a lock over the file
  lck <- filelock::lock(paste0(tools::file_path_sans_ext(taskTablePath),
                               ".lock"), timeout = 30000)

  if (is.null(taskTable)) {
    taskTable <- spt_read_tasks_table(taskTablePath)
  }

  if (is.null(lck)) {
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  }

  idx <- taskTable$mainStatus == "NOT COMPLETED"
  taskTable <- taskTable[-idx, ]

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



#' Remove all tasks
#'
#' This function removes all tasks from the task table.
#'
#' @param taskTable The task table to remove all tasks from (optional).
#' If not provided, it will be read from the specified \code{taskTablePath}.
#' @param taskTablePath The file path to the task table.
#'
#' @return \code{TRUE} if the removal is successful, \code{FALSE} otherwise.
#'
#' @examples
#' spt_rm_all_tasks(taskTable = myTaskTable, taskTablePath = "path/to/taskTable.csv")
#'
#' @export
#'

spt_rm_all_tasks <- function(taskTable = NULL, taskTablePath) {
  # Acquire a lock over the file
  lck <- filelock::lock(paste0(tools::file_path_sans_ext(taskTablePath),
                               ".lock"), timeout = 30000)

  if (is.null(taskTable)) {
    taskTable <- spt_read_tasks_table(taskTablePath)
  }

  if (is.null(lck)) {
    stop("Failed to acquire a lock over the task table file!", call. = TRUE)
  }

  idx <- 1:nrow(taskTable)
  taskTable <- taskTable[-idx, ]

  out <- try({
    spt_write_tasks_table(taskTable, taskTablePath)

    filelock::unlock(lck)
  })

  if (inherits(out, "try-error")) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}
