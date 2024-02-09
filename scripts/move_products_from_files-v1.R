
library(cli)
library(tools)



## ---------------------------------------------------------------------- ##
## Copy product files into adequate folders properly named following
## SeverusPT naming conventions
## ---------------------------------------------------------------------- ##



data_dir <- "D:/DATA/SeverusPT/DATASETS/SPT_Products_v02/"

data_move<-"D:/DATA/SeverusPT/DATASETS/SPT_Products_v02_vf/"


fl <- list.files(data_dir, pattern=".tif$")
fl_all <- list.files(data_dir, full.names=TRUE)


#' Extract Product Name from Filename
#'
#' This function parses a given filename that follows a specific pattern and constructs
#' a product name by rearranging and modifying parts of the filename. It is specifically
#' designed for filenames that contain underscore-separated values, where each segment
#' represents a different component of the product name. The function replaces "LT" with
#' "LH" in the fourth segment and constructs the final product name using selected parts
#' of the filename.
#'
#' @param x A character string representing the filename from which to extract the
#' product name. The filename is expected to follow a pattern where segments are
#' separated by underscores.
#'
#' @return A character string representing the constructed product name. The product
#' name is constructed by concatenating the first segment, "D", the second and third
#' segments, a modified version of the fourth segment (where "LT" is replaced with
#' "LH"), and the first character of the fifth and ninth segments, separated by dashes.
#'
#' @examples
#' filename <- "product_segment1_segment2_LT_segment4_segment5_segment6_segment7_segment8_123.tif"
#' get_product_name(filename)
#' # Returns something like "product-D-segment1segment2-LH-s-1"

get_product_name <- function(x){
  fni <- unlist(strsplit(x, split = "_"))
  pname <- paste(fni[1], "D", fni[2], fni[3], gsub("LT","LH",substr(fni[4],1,2)), substr(fni[5],1,1),
        substr(fni[9],1,3), sep="-")
  #gsub("LT","LH",pname)
  }


# List all products in the data
cat(unique(sapply(fl, FUN = get_product_name, USE.NAMES = FALSE)),sep = "\n")


cli_progress_bar("Moving files", total = length(fl))

for(i in 1:length(fl)){

  # Get the product name for the target file
  fni <- fl[i]
  prod_name <- get_product_name(fni)

  # Folder/product name where to move the files
  out_dir <- paste(data_move,prod_name,sep="")

  if(!dir.exists(out_dir)){
    dir.create(out_dir)
  }

  # Get the main file name
  fp <- file_path_sans_ext(fni)

  # List all data and metadata files with the same names
  from_files <- fl_all[grepl(fp, fl_all)]

  # Copy files to new folder
  fcopy <- file.copy(from_files, to = out_dir)

  if(!all(fcopy)){
    message("Error on file:", fp)
  }

  cli_progress_update()
}

## ---------------------------------------------------------------------- ##
## Compress products to zip files
## ---------------------------------------------------------------------- ##


library(zip)

# Define the source and destination directories
source_dir <- "D:/DATA/SeverusPT/DATASETS/SPT_Products_v02_vf/"
dest_dir <- "D:/DATA/SeverusPT/DATASETS/"

# Get all the subdirectories within the source directory
subdirs <- list.dirs(path = source_dir, full.names = TRUE,
                     recursive = FALSE)

cli_progress_bar("Zipping files", total = length(subdirs))

# Loop through each subdirectory and compress it
for (subdir in subdirs) {
  # Extract the name of the folder to name the zip file
  folder_name <- basename(subdir)
  zip_file_path <- file.path(dest_dir, paste0(folder_name, ".zip"))

  # Use zipr to compress the folder and save it to the destination directory
  zip::zipr(zipfile = zip_file_path, files = subdir, recurse = TRUE)
  cli_progress_update()
}




