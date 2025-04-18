

#' Convert Zeros to NA in Raster
#'
#' This function reads a raster file, converts all zero values to NA (missing values),
#' and optionally writes the modified raster to the same file.
#'
#' @param rstPath A character string representing the path to the raster file.
#' @param writeRast A logical value indicating whether to write the modified raster
#' to the same file. Default is TRUE.
#' @param COG Make Cloud optimized GeoTIFF? (default: FALSE)
#' @param ... Additional arguments to be passed to the writeRaster function if
#' writeRast is TRUE.
#' @return If writeRast is TRUE, the function returns a raster object representing
#' the modified raster. Otherwise, it returns NULL.
#' @importFrom terra rast writeRaster
#' @export
#'

spt_raster_zeros_to_na <- function(rstPath, writeRast=TRUE, COG=FALSE, ...) {

  # Read the raster file
  rst <- rast(rstPath)

  # Convert all zeros to NA
  rst[rst == 0] <- NA

  if(writeRast){

    # Add overviews?
    if(COG){

      writeRaster(rst, rstPath, overwrite=TRUE,
                  gdal = c("TILED=YES", "COMPRESS=LZW"), ...)

      cat("Writing raster overviews...\n")
      gdal_addo(rstPath, clean=TRUE)
      gdal_addo(rstPath)
      cat("done!\n\n")

    }else{

      writeRaster(rst, rstPath, overwrite=TRUE,
                  gdal = c("COMPRESS=LZW"), ...)
    }
  }

  # Return the raster object
  return(rast(rstPath))
}


#' Project Raster to ETRS89/PT TM06 Coordinate Reference System
#'
#' This function projects a raster object or a raster file to the ETRS89/PT TM06 (EPSG:3763)
#' coordinate reference system.
#' It can optionally write the projected raster to a new file with the updated CRS code in
#' the filename.
#'
#' @param x A raster object or a character string representing the path to the raster file.
#' @param writeData A logical value indicating whether to write the projected raster to a
#' new file. Default is TRUE.
#' @param outPath A character string representing the output file path for the projected
#' raster. Ignored if writeData is FALSE.
#' @param COG Make Cloud optimized GeoTIFF? (default: FALSE)
#' @param ... Additional arguments to be passed to the project function.
#' @return If writeData is TRUE, the function returns a raster object representing the
#' projected raster. Otherwise, it returns NULL.
#' @importFrom raster rast project res crs
#' @export
#'

spt_project_to_pttm06 <- function(x, writeData=TRUE, outPath=NULL, COG=FALSE, ...) {

  if(is.character(x)){
    filePath <- x
    x <- rast(x)

    if(writeData){
      # Get the file name and extension
      file_ext <- tools::file_ext(filePath)
      file_name <- tools::file_path_sans_ext(filePath)

      # Create the backup file name with the suffix _bkp
      #outPath <- paste0(file_name, "_ETRS89TM06.", file_ext)
      # Replace the filename CRS code by the ETRS89/PT TM06 (EPSG: 3763)
      outPath <- spt_replace_crs_code(filePath)
    }
  }else{
    if(writeData && is.null(outPath)){
      stop("outPath cannot be null")
    }
  }

  # Transform the raster to the target coordinate reference system
  if(writeData){


    # Add overviews?
    if(COG){

      rst <- project(x,
                     crs("EPSG:3763"),
                     res=res(x)[1],
                     overwrite=TRUE,
                     filename=outPath,
                     gdal = c("TILED=YES", "COMPRESS=LZW"), ...)

      cat("Writing raster overviews...\n")
      gdal_addo(outPath, clean=TRUE)
      gdal_addo(outPath)
      cat("done!\n\n")
    }else{

      rst <- project(x,
                     crs("EPSG:3763"),
                     res=res(x)[1],
                     overwrite=TRUE,
                     filename=outPath,
                     gdal = c("COMPRESS=LZW"), ...)
    }

  }else{
    rst <- project(x, crs("EPSG:3763"), res=res(x)[1])
  }

  # Return the new raster object
  return(rst)
}
