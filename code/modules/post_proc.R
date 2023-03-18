


spt_raster_zeros_to_na <- function(rstPath, writeRast=TRUE, ...) {
  
  # Read the raster file
  rst <- rast(rstPath)
  
  # Convert all zeros to NA
  rst[rst == 0] <- NA
  
  if(writeRast)
    writeRaster(rst, rstPath, overwrite=TRUE, ...)
  
  # Return the raster object
  return(rast(rstPath))
}


spt_project_to_pttm06 <- function(x, writeData=TRUE, outPath=NULL, ...) {
  
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
