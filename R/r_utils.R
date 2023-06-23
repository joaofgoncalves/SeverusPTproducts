
## ----------------------------------------------------------------------------------- ##
## Ancillary R functions ----
## ----------------------------------------------------------------------------------- ##

#' Calculate the Number of Periods from a Date to Today
#'
#' This function calculates the number of periods from a given date to today, based on a specified number of days per period.
#'
#' @param date A character or Date object representing the starting date.
#' @param ndays An integer specifying the number of days per period.
#' @return An integer indicating the number of periods from the starting date to today.
#' @examples
#' spt_periods_to_today("2022-01-01", 7)
#' spt_periods_to_today(as.Date("2021-12-15"), 30)
#' @export
#' 

spt_periods_to_today <- function(date, ndays) {
  today <- Sys.Date()
  d <- as.Date(date)
  timeDiff <- as.numeric(difftime(today, d, units = "days"))
  daysDiff <- ceiling(timeDiff)
  return(floor(daysDiff / ndays))
}


#' Pad a Number with Leading Zeros
#'
#' This function pads a number with leading zeros to a specified width.
#'
#' @param x A numeric or character value to be padded.
#' @param width An integer specifying the desired width of the padded number. Default is 3.
#' @return A character string representing the padded number.
#' @examples
#' spt_pad_number(7, width = 5)
#' spt_pad_number(123, width = 8)
#' spt_pad_number("42", width = 4)
#' @importFrom stringr str_pad
#' @export
#' 

spt_pad_number <- function(x, width = 3){
  stringr::str_pad(x, width = width, side = "left", pad = 0)
} 


#' Get the Current Date in YYYYMMDD Format
#'
#' This function returns the current date in the format "YYYYMMDD".
#'
#' @return A character string representing the current date in YYYYMMDD format.
#' @examples
#' spt_ymd_date()
#' @export
#' 

spt_ymd_date <- function() {
  date <- Sys.Date()
  year <- format(date, "%Y")
  month <- format(date, "%m")
  day <- format(date, "%d")
  if (nchar(month) == 1) {
    month <- paste0("0", month)
  }
  if (nchar(day) == 1) {
    day <- paste0("0", day)
  }
  return(paste0(year, month, day))
}


#' Get the Current Date and Time
#'
#' This function returns the current date and time in the format "YYYY-MM-DD HH:MM:SS".
#'
#' @return A character string representing the current date and time in "YYYY-MM-DD HH:MM:SS" format.
#' @examples
#' spt_current_date_time()
#' @export
#' 

spt_current_date_time <- function() {
  current_time <- Sys.time()
  datetime_str <- format(current_time, "%Y-%m-%d %H:%M:%S")
  return(datetime_str)
}


#' Get the Current Year
#'
#' This function returns the current year as a character string.
#'
#' @return A character string representing the current year.
#' @examples
#' spt_current_year()
#' @export
#' 

spt_current_year <- function() {
  return(format(Sys.time(), "%Y"))
}


#' Generate the Product Name
#'
#' This function generates a product name based on the provided parameters.
#'
#' @param ProjectAccronym A character string representing the project acronym.
#' @param SeverityIndicator A character string representing the severity indicator.
#' @param BaseIndex A character string representing the base index.
#' @param SatCode A character string representing the satellite code.
#' @param BurntAreaDataset A character string representing the burnt area dataset.
#' @param ReferenceYear A character string representing the reference year.
#' @param RefPeriods A character string representing the reference periods.
#' @param PrimaryCRScode A character string representing the primary CRS code.
#' @param addCalcDate A logical value indicating whether to include the calculation date in the product name. Default is FALSE.
#' @param VersionNumber A character string representing the version number.
#' @return A character string representing the generated product name.
#' @examples
#' spt_product_name("PROJ", "SEV", "BASE", "SAT", "BA", "2022", "PERIODS", "CRS", addCalcDate = TRUE, "V1.0")
#' spt_product_name("PROJ", "SEV", "BASE", "SAT", "BA", "2022", "PERIODS", "CRS", "V1.0")
#' @importFrom base paste
#' @export
#' 

spt_product_name <- function(ProjectAccronym, 
                           SeverityIndicator, 
                           BaseIndex, 
                           SatCode, 
                           BurntAreaDataset, 
                           ReferenceYear, 
                           RefPeriods, 
                           PrimaryCRScode,
                           addCalcDate = FALSE, 
                           VersionNumber) {
  if (addCalcDate) {
    CalculationDate <- spt_ymd_date()

    productName <- paste(
      ProjectAccronym, "_", 
      SeverityIndicator, "_", 
      BaseIndex, "_",  
      SatCode, "_",
      spt_ba_dataset_code(BurntAreaDataset), 
      ReferenceYear, "_",
      RefPeriods, "_", 
      PrimaryCRScode, "_", ## Added CRS code to changes in names from version 0.6-20230506 doc
      CalculationDate, "_",
      VersionNumber,
      sep = "")
    
  } else {
    
    productName <- paste(
      ProjectAccronym, "_", 
      SeverityIndicator, "_", 
      BaseIndex, "_",  
      SatCode, "_",
      spt_ba_dataset_code(BurntAreaDataset), 
      ReferenceYear, "_",
      RefPeriods, "_", 
      PrimaryCRScode, "_",## Added CRS code to changes in names from version 0.6-20230506 doc
      VersionNumber,
      sep = "")
  }

  return(productName)
}


#' Create a Backup of a File
#'
#' This function creates a backup of a specified file by appending the suffix "_bkp" to the file name.
#'
#' @param targetFile A character string representing the file to be backed up.
#' @examples
#' spt_backup_file("data.csv")
#' @importFrom tools file_ext file_path_sans_ext
#' @importFrom base paste0
#' @export
#' 

spt_backup_file <- function(targetFile) {
  # Get the file name and extension
  file_ext <- tools::file_ext(targetFile)
  file_name <- tools::file_path_sans_ext(targetFile)

  # Create the backup file name with the suffix _bkp
  bkp_file_name <- paste0(file_name, "_bkp.", file_ext)

  # Copy the file to the backup file name
  file.copy(targetFile, bkp_file_name)
}


#' Convert Data Frame to Markdown Table
#'
#' This function converts a data frame into a Markdown table format.
#'
#' @param df A data frame to be converted to Markdown.
#' @return A knitr_kable object representing the data frame in Markdown table format.
#' @examples
#' spt_df_to_md(iris)
#' @importFrom knitr kable
#' @export
#' 

spt_df_to_md <- function(df) {
  knitr::kable(df,
    format = "markdown",
    col.names = colnames(df), align = "l"
  )
}


#' Export Data Frame to Markdown File
#'
#' This function exports a data frame to a Markdown file.
#'
#' @param df A data frame to be exported to Markdown.
#' @param filename A character string representing the name of the Markdown file.
#' @examples
#' spt_export_to_md(iris, "output.md")
#' @importFrom kableExtra save_kable
#' @export
#' 

spt_export_to_md <- function(df, filename) {
  # Create a pretty table
  ktable <- spt_df_to_md(df)
  kableExtra::save_kable(ktable, file = filename)
}


#' Check File Creation and Modification Dates
#'
#' This function checks the creation and modification dates of a file and returns TRUE if either of the dates
#' falls within a specified time interval from the current time.
#'
#' @param filename A character string representing the name of the file to check.
#' @param tdelta The time interval in minutes. If the file's creation or modification date is within this interval
#' from the current time, TRUE is returned. Default is 60 minutes.
#' @return A logical value indicating whether the file's creation or modification date falls within the specified time interval.
#' @examples
#' spt_file_dates("data.txt", tdelta = 60)
#' spt_file_dates("data.txt")
#' @export
#' 

spt_file_dates <- function(filename, tdelta = 60) {
  if(!file.exists(filename)) {
    return(FALSE)
    #stop(paste0("File ", filename, " does not exist."))
  }
  else {
    ctime <- file.info(filename)$ctime # file creation time
    mtime <- file.info(filename)$mtime # file modification time
    now <- Sys.time()
    if(difftime(now,ctime,units = "mins") < tdelta || 
       difftime(now,mtime,units="mins") < tdelta) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }
}


#' Create File with Current Timestamp
#'
#' This function creates a new file with the specified filename and writes the current timestamp to it.
#' If a file with the same name already exists, it will be removed before creating the new file.
#'
#' @param filename A character string representing the name of the file to be created.
#' @examples
#' spt_create_file("output.txt")
#' @export
#' 

spt_create_file <- function(filename) {
  if(file.exists(filename)){
    file.remove(filename)
  }
  fileConn <- file(filename, open = "w")
  writeLines(as.character(paste0(Sys.time(),"\n")), fileConn)
  close(fileConn)
}


#' Download and Unzip File
#'
#' This function downloads a file from the specified URL and optionally unzips it if it is a ZIP file.
#'
#' @param url A character string representing the URL of the file to be downloaded.
#' @param dest_file A character string representing the destination file path where the downloaded file will be saved.
#' @param extdir A character string representing the path to extract the contents of the ZIP file. Default is the current working directory.
#' @importFrom tools file_ext
#' @export
#' 

spt_download_unzip <- function(url, dest_file, extdir) {
  
  download.file(url, dest_file, mode = "wb")
  
  if (tools::file_ext(dest_file) == "zip") {
    unzip(dest_file, exdir = extdir, overwrite = TRUE)
  }
}


#' Replace the Coordinate Reference System (CRS Code) in a String
#'
#' This function replaces a specific CRS (Coordinate Reference System) code in a given string with a new CRS code.
#' The default CRS codes used are "32629" for the source CRS and "3763" for the target CRS.
#'
#' @param x A character string in which the CRS code will be replaced.
#' @param from A character string representing the CRS code to be replaced. Default is "32629".
#' @param to A character string representing the new CRS code to replace the original CRS code. Default is "3763".
#' @return A character string with the replaced CRS code.
#' @examples
#' spt_replace_crs_code("data_32629.csv")
#' spt_replace_crs_code("data_32629.csv", from = "32629", to = "4326")
#' @export
#' 

spt_replace_crs_code <- function(x, from = "32629", to = "3763"){
  gsub(paste0("_",from,"_"),paste0("_",to,"_"),x)
}
