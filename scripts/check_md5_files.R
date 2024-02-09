
## ------------------------------------------------------------------------------- ##
## Check the MD5 checksum for a list of files by name
## ------------------------------------------------------------------------------- ##


# Load necessary library
library(tools)

#' Compute MD5 Checksum for a File
#'
#' This function calculates the MD5 checksum of a specified file. The MD5 checksum is a
#' 128-bit hash value that can be used to verify the integrity of files. This function
#' is a simple wrapper around the `md5sum` function, providing an easy-to-use interface
#' for obtaining a file's MD5 checksum.
#'
#' @param file A character string specifying the path to the file for which the MD5
#' checksum is to be computed.
#'
#' @return A character string representing the MD5 checksum of the file. The checksum
#' is typically used to verify file integrity.
#'
#' @examples
#' # Assuming 'example.txt' exists in the current working directory
#' compute_md5("example.txt")

compute_md5 <- function(file) {
  md5sum(file)
}

# Define the directory containing the zip files
zip_dir <- "D:/DATA/SeverusPT/DATASETS/zenodo_uploaded"

# List all zip files in the directory
zip_files <- list.files(path = zip_dir, pattern = "\\.zip$", full.names = TRUE)

# Provided MD5 checksums
md5_list <- list(
  "SPT-D-DELTA-NBR-L8-E-v02.zip" = "8afd3c8c6d5c031b32b14dfa73aa4cea",
  "SPT-D-DELTA-NBR-MO-E-v02.zip" = "7c746bd7b14888e90c6ddd3232bffdf3",
  "SPT-D-DELTA-NBR-LH-E-v02.zip" = "aea7358e44fc2a21655af0a57ccb0e19",
  "SPT-D-RBR-NBR-S2-E-v02.zip" = "e723662b089d3c8c620ee63bfcf1c154",
  "SPT-D-RDT-NBR-L8-E-v02.zip" = "21945692b6495a88c1dfba69950d43f4",
  "SPT-D-RBR-NBR-L8-E-v02.zip" = "defe0aa271e80f6048fbe0381ce55804",
  "SPT-D-RBR-NBR-LH-E-v02.zip" = "eb1befe5527ee885c8d5c6b86f42ff5e",
  "SPT-D-DELTA-NBR-S2-E-v02.zip" = "2ef83aed882db922a991d3221e39c233",
  "SPT-D-RBR-NBR-MO-E-v02.zip" = "8724ea82e97b83e79b849a8f0e2c2d0e",
  "SPT-D-RDT-NBR-MO-E-v02.zip" = "4aab4c4b39c07ddfcb0ba9e61928864d",
  "SPT-D-RDT-NBR-LH-E-v02.zip" = "0486123f386e0cab0a3687bdcd33f81f",
  "SPT-D-RDT-NBR-S2-E-v02.zip" = "e73b4ef7c8e061aa070c815d975c2f3d"
)



# Check each file's MD5
for (file_path in zip_files) {
  file_name <- basename(file_path)
  file_md5 <- compute_md5(file_path)

  if (file_name %in% names(md5_list)) {
    expected_md5 <- md5_list[[file_name]]
    if (file_md5 == expected_md5) {
      message(file_name, " MD5 matches: ", expected_md5)
    } else {
      message(file_name, " MD5 does not match. Expected: ", expected_md5, ", Found: ", file_md5)
    }
  } else {
    message(file_name, " is not in the provided MD5 list.")
  }
}

