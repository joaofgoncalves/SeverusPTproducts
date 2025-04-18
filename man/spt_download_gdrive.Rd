% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gee_utils.R
\name{spt_download_gdrive}
\alias{spt_download_gdrive}
\title{Download a Google Earth Engine (GEE) task result from Google Drive}
\usage{
spt_download_gdrive(geeTask, outFolder, dataFormat = "tif")
}
\arguments{
\item{geeTask}{The GEE task object or GEE status list.}

\item{outFolder}{The output folder to save the downloaded file.}

\item{dataFormat}{The format of the downloaded file (default is "tif").}
}
\value{
The path to the downloaded file if successful, NULL otherwise.
}
\description{
This function downloads a GEE task result from Google Drive to a local folder.
}
\details{
The function first checks the status of the GEE task using the \code{spt_gee_task_status}
function. If the task is in the "COMPLETED" state, it proceeds to download the result from Google
Drive to the specified \code{outFolder}. The downloaded file is named based on the task description
and the specified \code{dataFormat}.

If the input \code{geeTask} is a GEE task object of class \code{"ee.batch.Task"}, the
function calls \code{ee_drive_to_local} the task has already been completed
and calls \code{ee_drive_to_local} directly.

The function returns the path to the downloaded file if the download is successful. Otherwise,
it returns NULL.
}
\seealso{
\code{\link{spt_gee_task_status}}, \code{\link{ee_drive_to_local}}
}
