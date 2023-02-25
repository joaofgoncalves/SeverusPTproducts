
## ----------------------------------------------------------------------------------- ##
## Ancillary R functions ----
## ----------------------------------------------------------------------------------- ##


numPeriodsFromToday <- function(date, ndays) {
  today <- Sys.Date()
  d <- as.Date(date)
  timeDiff <- as.numeric(difftime(today, d, units = "days"))
  daysDiff <- ceiling(timeDiff)
  return(floor(daysDiff / ndays))
}


padNumber <- function(x, width = 3){
  stringr::str_pad(x, width = width, side = "left", pad = 0)
} 


getDateString <- function() {
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


getCurrentDatetime <- function() {
  current_time <- Sys.time()
  datetime_str <- format(current_time, "%Y-%m-%d %H:%M:%S")
  return(datetime_str)
}


getProductName <- function(ProjectAccronym, SatCode, BaseIndex, SeverityIndicator,
                           BurntAreaDataset, ReferenceYear, RefPeriods, addCalcDate = FALSE) {
  if (addCalcDate) {
    CalculationDate <- getDateString()

    productName <- paste(ProjectAccronym, "_", SatCode, "_", BaseIndex, "_", SeverityIndicator, "_",
      getBurntAreaDatasetCode(BurntAreaDataset), ReferenceYear, "_",
      RefPeriods, "_", CalculationDate,
      sep = ""
    )
  } else {
    productName <- paste(ProjectAccronym, "_", SatCode, "_", BaseIndex, "_", SeverityIndicator, "_",
      getBurntAreaDatasetCode(BurntAreaDataset), ReferenceYear, "_",
      RefPeriods,
      sep = ""
    )
  }

  return(productName)
}


backupFile <- function(targetFile) {
  # Get the file name and extension
  file_ext <- tools::file_ext(targetFile)
  file_name <- tools::file_path_sans_ext(targetFile)

  # Create the backup file name with the suffix _bkp
  bkp_file_name <- paste0(file_name, "_bkp.", file_ext)

  # Copy the file to the backup file name
  file.copy(targetFile, bkp_file_name)
}


dataframeToMarkdown <- function(df) {
  knitr::kable(df,
    format = "markdown",
    col.names = colnames(df), align = "l"
  )
}


exportToMarkdown <- function(df, filename) {
  # Create a pretty table
  ktable <- knitr::kable(df,
    format = "markdown",
    col.names = colnames(df), align = "l"
  )

  kableExtra::save_kable(ktable, file = filename)
}
