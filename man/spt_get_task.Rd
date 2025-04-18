% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/task_management.R
\name{spt_get_task}
\alias{spt_get_task}
\title{Get a single task by its identifiers}
\usage{
spt_get_task(
  taskTable = NULL,
  taskID = NULL,
  taskUID = NULL,
  geeTaskID = NULL,
  geeTaskCode = NULL,
  postProcTaskID = NULL,
  closingTaskID = NULL,
  taskTablePath
)
}
\arguments{
\item{taskTable}{A data frame representing the task table. If not provided, it will be
read from the specified taskTablePath.}

\item{taskID}{The ID of the task to retrieve.}

\item{taskUID}{The UID of the task to retrieve.}

\item{geeTaskID}{The GEE task ID of the task to retrieve.}

\item{geeTaskCode}{The GEE task code of the task to retrieve.}

\item{postProcTaskID}{The post-processing task ID of the task to retrieve.}

\item{closingTaskID}{The closing task ID of the task to retrieve.}

\item{taskTablePath}{The path to the task table file.}
}
\value{
A data frame representing the retrieved task, with an additional "task" class.
}
\description{
This function retrieves a specific task from a task table based on the provided parameters.
It can search for a task using taskID, taskUID, geeTaskID, geeTaskCode, postProcTaskID, or
closingTaskID.
}
\examples{
spt_get_task(taskTable = myTaskTable, taskID = "12345")

}
