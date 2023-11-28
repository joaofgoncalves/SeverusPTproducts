

## --------------------------------------------------------------------------- ##
## SET THE MAIN WORKING DIR ----
## --------------------------------------------------------------------------- ##


setwd("C:/MyFiles/R-dev/SeverusPTproducts")
#setwd("C:/Users/JG/Desktop/Projects/SeverusPT_Products")


## --------------------------------------------------------------------------- ##
## LOAD LIBS ----
## --------------------------------------------------------------------------- ##


source("globvars.r")
source("./R/r_utils.R")
source("./R/task_generator.R")
source("./R/task_management.R")
source("./R/gee_data.R")
source("./R/gee_indices.R")
source("./R/gee_utils.R")
source("./R/gee_calc.R")
source("./R/gee_prepare.R")
source("./R/post_proc.R")
source("./R/metadata.R")


## --------------------------------------------------------------------------- ##
## INITIALIZE EARTH ENGINE ----
## --------------------------------------------------------------------------- ##


ee_Initialize(user = "joaofgoncalves", gcs = TRUE, drive = TRUE)


## --------------------------------------------------------------------------- ##
## RUN THE TASKS / CALCULATE DATA AND METADATA ----
## --------------------------------------------------------------------------- ##

source("./scripts/products_tasks_parallel.R")

