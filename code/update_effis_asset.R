
library(sf)
library(rgee)
library(dplyr)


# Get ancillary functions
source("./code/modules/r_utils.R")

# Initialize GEE
ee_Initialize(user = "joaofgo@gmail.com", gcs = TRUE)


## ---------------------------------------------------------------------- ##


# Update historical data? (no updates/static)
updateHistData <- TRUE

# Update current year data? (updates in near-real time)
updateCurYearData <- TRUE


## ---------------------------------------------------------------------- ##
## DOWNLOAD DATA ----
## ---------------------------------------------------------------------- ##


url <- "https://maps.wild-fire.eu/effis?service=WFS&request=getfeature&typename=ms:modis.ba.poly&version=1.1.0&outputformat=SHAPEZIP"

# Download and unzip data for EFFIS (latest version online)
spt_download_unzip(url, "./temp/effis_data.zip", "./temp")


## PRE-PROCESS ----

aa <- read_sf("./temp/modis.ba.poly.shp") %>% 
        st_set_crs("EPSG:4326") # Set proper CRS / before was GCS Unknown

aapt <- aa %>% 
  filter(COUNTRY == "PT") %>% # Filter data only to Portugal
  mutate(year = substr(FIREDATE,1,4)) %>% # Add year field
  mutate(AREA_HA = as.numeric(AREA_HA)) %>% # Convert area field to numeric
  st_make_valid() %>%  # Data corrections to avoid topological errors
  st_buffer(0.0) %>% 
  mutate(area_ht = as.numeric(st_area(.)/10000)) # Add area field in hectares / remove units


## ---------------------------------------------------------------------- ##
## HISTORICAL DATA ----
## ---------------------------------------------------------------------- ##


## Update historical data from 2000 to current year minus one

if(updateHistData){
  
  aa_hist <- aapt %>% filter(year >= 2000, year < spt_current_year())
  
  sf_as_ee(
    aa_hist,
    via = "gcs_to_asset",
    assetId = "users/joaofgo/severus_pt/effis_hist",
    bucket = "my_gee_data_bucket",
    predefinedAcl = "bucketLevel",
    command_line_tool_path = NULL,
    overwrite = TRUE,
    monitoring = TRUE,
    proj = "EPSG:4326",
    evenOdd = TRUE,
    geodesic = NULL,
    quiet = FALSE
  )
  
}


## ---------------------------------------------------------------------- ##
## CURRENT YEAR DATA ----
## ---------------------------------------------------------------------- ##

## Update current year data

if(updateCurYearData){
  
  baCurrentYear <- aapt %>% filter(year == spt_current_year())
  
  sf_as_ee(
    baCurrentYear,
    via = "gcs_to_asset",
    assetId = "users/joaofgo/severus_pt/effis_current",
    bucket = "my_gee_data_bucket",
    predefinedAcl = "bucketLevel",
    command_line_tool_path = NULL,
    overwrite = TRUE,
    monitoring = TRUE,
    proj = "EPSG:4326",
    evenOdd = TRUE,
    geodesic = NULL,
    quiet = FALSE
  )
  
}



