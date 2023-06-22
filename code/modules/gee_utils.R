

## ----------------------------------------------------------------------------------- ##
## TASK MANAGEMENT FUNCTIONS ----
## ----------------------------------------------------------------------------------- ##


#' Retrieve the status of a Google Earth Engine (GEE) task and convert it to an R list object
#'
#' This function retrieves the status of a GEE task and converts it to an R list object.
#'
#' @param geeTask The GEE task object.
#' @return A list containing the task status and additional parameters for the download function.
#'
#' @details
#' The function checks the status of the GEE task using \code{ee$batch$Task$status(geeTask)} and converts
#' the result from Python to an R list object using the \code{ee_utils_py_to_r} function. It then adds
#' extra parameters to the output list, such as the file export options, filename prefix, and file format,
#' which are needed for the download function but cannot be stored directly from the GEE task object.
#' 
#' If the task has finished and the \code{"destination_uris"} field exists in the output list, the function
#' retrieves the basename of the URI for the Google Drive file and stores it as \code{basename_uri}.
#'
#' @seealso
#' \code{\link{ee_utils_py_to_r}}, \code{\link{basename}}
#'
#' @examples
#' # Assuming 'geeTask' is a valid GEE task object
#' spt_gee_task_status(geeTask)
#'
#' @importFrom ee.batch.Task
#' @importFrom utils::basename
#'

spt_gee_task_status <- function(geeTask){
  
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


#' Check the status of a Google Earth Engine (GEE) task and wait until completion
#'
#' This function checks the status of a GEE task at regular intervals and waits until the 
#' task is completed, failed, or cancelled.
#'
#' @param geeTask The GEE task object.
#' @param sleep The number of seconds to sleep between status checks (default is 60 seconds).
#' @param verbose If TRUE, verbose output will be printed during the status check (default 
#' is FALSE).
#' 
#' @return The final status of the GEE task as a list.
#'
#' @details
#' The function initializes the `status` variable as "RUNNING" and starts a timer. It then 
#' enters a `repeat` loop
#' where it calls the `spt_gee_task_status` function to retrieve the current status of the GEE 
#' task. The loop continues until the task status is one of "COMPLETED", "FAILED", "CANCELLED", 
#' or "CANCEL_REQUESTED". During each iteration, the function calculates the time difference and 
#' prints the GEE process ID, task status, and approximate run time if `verbose` is set to TRUE.
#'
#' Once the loop is exited, the function checks the final status and prints appropriate 
#' messages if `verbose` is TRUE. It then returns the final status of the GEE task as a list.
#'
#' @seealso
#' \code{\link{spt_gee_task_status}}
#'
#' @examples
#' # Assuming 'geeTask' is a valid GEE task object
#' spt_check_gee_task(geeTask)
#'

spt_check_gee_task <- function(geeTask, sleep=60, verbose=FALSE){
  
  
  # initialize the status variable
  status <- "RUNNING"
  ti <- Sys.time()
  
  # use a while loop to check the status every n seconds
  repeat{
    
    # call the spt_gee_task_status function and store the result in the status variable
    geeStatus <- spt_gee_task_status(geeTaskObj)
    
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
    
    if(status %in% c("COMPLETED", "FAILED", "CANCELLED", "CANCEL_REQUESTED")){
      break
    }else{
      Sys.sleep(sleep)
      #if(verbose) cat("\014")
    }
  }
  
  if(verbose){
    if(status=="COMPLETED"){
      if(verbose) message("GEE Process ID: ",geeStatus$id," completed")
    }
    if(status=="FAILED"){
      if(verbose) warning("GEE Process ID: ",geeStatus$id," failed")
    }
    if(status %in% c("CANCELLED","CANCEL_REQUESTED")){
      if(verbose) warning("GEE Process ID: ",geeStatus$id," cancelled")
    }
  }
  return(geeStatus)
}


#' Download a Google Earth Engine (GEE) task result from Google Drive
#'
#' This function downloads a GEE task result from Google Drive to a local folder.
#'
#' @param geeTask The GEE task object or GEE status list.
#' @param outFolder The output folder to save the downloaded file.
#' @param dataFormat The format of the downloaded file (default is "tif").
#' @return The path to the downloaded file if successful, NULL otherwise.
#'
#' @details
#' The function first checks the status of the GEE task using the \code{spt_gee_task_status} 
#' function. If the task is in the "COMPLETED" state, it proceeds to download the result from Google 
#' Drive to the specified \code{outFolder}. The downloaded file is named based on the task description 
#' and the specified \code{dataFormat}.
#'
#' If the input \code{geeTask} is a GEE task object of class \code{"ee.batch.Task"}, the 
#' function calls \code{ee_drive_to_local} the task has already been completed
#' and calls \code{ee_drive_to_local} directly.
#'
#' The function returns the path to the downloaded file if the download is successful. Otherwise, 
#' it returns NULL.
#'
#' @seealso
#' \code{\link{spt_gee_task_status}}, \code{\link{ee_drive_to_local}}
#'

spt_download_gdrive <- function(geeTask, outFolder, dataFormat="tif"){
  
  # Use a ee.batch.Task / python.builtin.object
  
  if(inherits(geeTask,"ee.batch.Task")){
    
    taskStatusList <- spt_gee_task_status(geeTask)
    
    if(taskStatusList$state == "COMPLETED"){
      
      outFile <- paste0(outFolder,"/",taskStatusList$description,".",dataFormat)
      
      out <- try(ee_drive_to_local(task = geeTask, dsn = outFile, 
                                   overwrite = TRUE, consider = "last"))
      
    }else{
      return(NULL)
    }
    
  # Use a GEE status list instead 
    
  }else if(inherits(geeTask, "list")){

    outFile <- paste0(outFolder,"/",geeTask$description,".",dataFormat)
    
    out <- try(ee_drive_to_local(task = geeTask, dsn = outFile, 
                                 overwrite = TRUE, consider = "last"))
    
  }else{
    stop("Non-supported object type in geeTask for method spt_download_gdrive.")
  }
  
  if(inherits(out,"try-error")){
    return(NULL)
  }else{
    return(outFile)
  }
}


#' Save a Google Earth Engine (GEE) status list to disk.
#'
#' This function saves a GEE status list to disk as RDS and CSV files.
#'
#' @param task The GEE task object.
#' @param geeStatusList The GEE status list to be saved.
#' @param geeTaskPath The path to the GEE task folder where the files will be saved.
#'
#' 
#' @details
#' The function saves the GEE status list to disk in two formats: RDS and CSV. It uses the \code{saveRDS} 
#' function to save the status list as an RDS file, and \code{write.csv} to save it as a CSV file. 
#' The files are named based on the task ID andtask UID to ensure uniqueness.
#'

spt_save_gee_status_list <- function(task, geeStatusList, geeTaskPath){
  
  saveRDS(geeStatusList, paste0(geeTaskPath,"/geeStatusList_TaskID_",
                                spt_pad_number(task$taskID),"-UID_",
                                substr(task$taskUID,1,8),".rds"))
  
  write.csv(geeStatusList, paste0(geeTaskPath,"/geeStatusList_TaskID_",
                                  spt_pad_number(task$taskID),"-UID_",
                                  substr(task$taskUID,1,8),".csv"), row.names = FALSE)
  
}

#' Read a Google Earth Engine (GEE) status list from disk.
#'
#' This function reads a GEE status list from disk in RDS format.
#'
#' @param task The GEE task object.
#' @param geeTaskPath The path to the GEE task folder where the status list is stored.
#' @return The GEE status list read from disk.
#'
#' @details
#' The function reads the GEE status list from disk using the \code{readRDS} function. It assumes 
#' that the status list was previously saved in the specified \code{geeTaskPath} using the 
#' \code{spt_save_gee_status_list} function.
#'
#'

spt_read_gee_status_list <- function(task, geeTaskPath){
  
  geeStatusList <- readRDS(paste0(geeTaskPath,"/geeStatusList_TaskID_",
                                  spt_pad_number(task$taskID),"-UID_",
                                  substr(task$taskUID,1,8),".rds"))
  return(geeStatusList)
}


#' Modified function to download files from Google Drive to local storage.
#'
#' This function downloads files from Google Drive to the local storage.
#'
#' @param task The GEE task object.
#' @param dsn The destination file path where the files will be downloaded. If not specified, a 
#' temporary directory will be used.
#' @param overwrite Logical indicating whether to overwrite existing files with the same name. 
#' Default is \code{TRUE}.
#' @param consider Logical or character indicating how to handle multiple files with the same 
#' name in Google Drive.
#'                 If \code{TRUE}, it prompts the user to select one file. If \code{"last"}, 
#'                 it selects the most recent file.
#'                 If \code{"all"}, it downloads all files. Default is \code{TRUE}.
#' @param public Logical indicating whether to make the downloaded files publicly accessible. 
#' Default is \code{FALSE}.
#' @param metadata Logical indicating whether to include metadata information in the output. 
#' Default is \code{FALSE}.
#' @param quiet Logical indicating whether to suppress progress messages. Default is \code{FALSE}.
#' @return A character vector of file paths to the downloaded files, or a list with file paths and 
#' metadata information if \code{metadata = TRUE}.
#'
#' @details
#' This function is a modified version of the \code{ee_drive_to_local} function from the \pkg{rgee} 
#' package. It allows downloading files from Google Drive based on a GEE task object. The files are 
#' downloaded to the specified destination file path (\code{dsn}) or to a temporary directory if \code{dsn} 
#' is not provided. The function provides options for handling multiple files with the same name in Google Drive,
#' including selecting one file, selecting the most recent file, or downloading all files. It 
#' also supports making the downloaded files publicly accessible and including metadata information 
#' in the output.
#'
#' @importFrom rgee ee_check_packages ee_exist_credentials ee_create_credentials_drive ee_save_credential ee_getTime
#' @import googledrive drive_find drive_share_anyone with_drive_quiet drive_download
#'
#'
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

