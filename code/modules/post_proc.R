


updatePostProcTaskStatus <- function(task, state, taskTable=NULL){
  
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
  taskTable[idx, "postProcTaskStatus"] <- state
  
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


rasterZerosToNA <- function(rstPath, writeRast=TRUE, ...) {
  
  # Read the raster file
  rst <- rast(rstPath)
  
  # Convert all zeros to NA
  rst[rst == 0] <- NA
  
  if(writeRast)
    writeRaster(rst, rstPath, overwrite=TRUE, ...)
  
  # Return the raster object
  return(rast(rstPath))
}

projPTTM06 <- function(x, writeData=TRUE, outPath=NULL, ...) {
  
  if(is.character(x)){
    filePath <- x
    x <- rast(x)
    
    if(writeData){
      # Get the file name and extension
      file_ext <- tools::file_ext(filePath)
      file_name <- tools::file_path_sans_ext(filePath)
      
      # Create the backup file name with the suffix _bkp
      outPath <- paste0(file_name, "_ETRS89TM06.", file_ext)
    }
  }else{
    if(writeData && is.null(outPath)){
      stop("outPath cannot be null")
    }
  }
  
  # Transform the raster to the target coordinate reference system
  if(writeData){
    rst <- project(x, crs("EPSG:3763"), res=res(x)[1], overwrite=TRUE, filename=outPath, ...)
  }else{
    rst <- project(x, crs("EPSG:3763"), res=res(x)[1], overwrite=TRUE)
  }
  
  # Return the new raster object
  return(rst)
}
