
library(sf)
library(rgee)
library(dplyr)


# Get ancillary functions
#source("./R/r_utils.R")
source("C:/MyFiles/R-dev/SeverusPTproducts/R/r_utils.R")

#ee_Authenticate()

# Initialize GEE
ee_Initialize(user = "joaofgoncalves", gcs = TRUE)


## ---------------------------------------------------------------------- ##


# Update historical data? (no updates/static)
updateHistData <- FALSE

# Update current year data? (updates in near-real time)
updateCurYearData <- FALSE

# Update all fire polygons for year >= 2000
updateAll <- TRUE

updateRecent <- TRUE

## ---------------------------------------------------------------------- ##
## DOWNLOAD DATA ----
## ---------------------------------------------------------------------- ##

# Sys.setenv(R_DEFAULT_INTERNET_TIMEOUT = 180)
# Sys.getenv("R_DEFAULT_INTERNET_TIMEOUT")
#
# .Options$timeout <- 180

options(timeout = 600)

#url <- "https://maps.wild-fire.eu/effis?service=WFS&request=getfeature&typename=ms:modis.ba.poly&version=1.1.0&outputformat=SHAPEZIP"
url <- "https://maps.effis.emergency.copernicus.eu/effis?service=WFS&request=getfeature&typename=ms:modis.ba.poly&version=1.1.0&outputformat=SHAPEZIP"

# Download and unzip data for EFFIS (latest version online)
#spt_download_unzip(url, "./temp/effis_data.zip", "./temp")
spt_download_unzip(url, "C:/MyFiles/R-dev/SeverusPTproducts/temp/effis_data.zip",
                        "C:/MyFiles/R-dev/SeverusPTproducts/temp")



## PRE-PROCESS ----

aa <- read_sf("C:/MyFiles/R-dev/SeverusPTproducts/temp/modis.ba.poly.shp") %>%
        st_set_crs("EPSG:4326") # Set proper CRS / before was GCS Unknown

aapt <- aa %>%
  filter(COUNTRY == "PT") %>% # Filter data only to Portugal
  mutate(year = substr(FIREDATE,1,4)) %>% # Add year field
  rename(fire_date = FIREDATE) %>%
  mutate(AREA_HA = as.numeric(AREA_HA)) %>% # Convert area field to numeric
  st_make_valid() %>%  # Data corrections to avoid topological errors
  st_buffer(0.0) %>%
  mutate(area_ht = as.numeric(st_area(.)/10000)) # Add area field in hectares / remove units


aapt <- aapt %>% mutate(fire_dt = as.Date(substr(fire_date,1,10)))

aapt_last7d <- aapt %>% filter(fire_dt >= todaySubDate(7))
aapt_last15d <- aapt %>% filter(fire_dt >= todaySubDate(15))
aapt_last30d <- aapt %>% filter(fire_dt >= todaySubDate(30))


## ---------------------------------------------------------------------- ##
## ALL DATA (to use on GEE app) ----
## ---------------------------------------------------------------------- ##



if(updateAll){

  aa_all <- aapt %>% filter(year >= 2000)

  aa_all <- aa_all %>%
    select(id, area_ht, fire_date, fire_dt, year) %>%
    mutate(guid = uuid::UUIDgenerate(n = nrow(.))) %>%
    mutate(year = as.integer(year))

  sf_as_ee(
    aa_all,
    via = "gcs_to_asset",
    assetId = "users/joaofgo/severus_pt/effis_all",
    #assetId = "projects/midyear-byway-378403/assets/severus_pt/effis_all",
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

  ee_manage_asset_access(
    path_asset = "users/joaofgo/severus_pt/effis_all",
    owner = NULL,
    editor = NULL,
    viewer = c("serviceAccount:severuspt--ty-mapper-859ec08f5@intrepid-nova-195705.iam.gserviceaccount.com",
               "serviceAccount:severuspt--y-analyst-a6ae88443@intrepid-nova-195705.iam.gserviceaccount.com"),
    all_users_can_read = TRUE,
    quiet = FALSE
  )

}


## ---------------------------------------------------------------------- ##
## UPDATE LAST 7, 15 AND 30 DAYS ----
## ---------------------------------------------------------------------- ##



if(updateRecent){

  aapt_last7d <- aapt_last7d %>%
    select(id, area_ht, fire_date, fire_dt, year) %>%
    mutate(guid = uuid::UUIDgenerate(n = nrow(.))) %>%
    mutate(year = as.integer(year))

  aapt_last15d <- aapt_last15d %>%
    select(id, area_ht, fire_date, fire_dt, year) %>%
    mutate(guid = uuid::UUIDgenerate(n = nrow(.))) %>%
    mutate(year = as.integer(year))

  aapt_last30d <- aapt_last30d %>%
    select(id, area_ht, fire_date, fire_dt, year) %>%
    mutate(guid = uuid::UUIDgenerate(n = nrow(.))) %>%
    mutate(year = as.integer(year))

  sf_as_ee(
    aapt_last7d,
    via = "gcs_to_asset",
    assetId = "users/joaofgo/severus_pt/effis_last_7days",
    #assetId = "projects/midyear-byway-378403/severus_pt/effis_last_7days",
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

  sf_as_ee(
    aapt_last15d,
    via = "gcs_to_asset",
    assetId = "users/joaofgo/severus_pt/effis_last_15days",
    #assetId = "projects/midyear-byway-378403/assets/severus_pt/effis_last_15days",
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

  sf_as_ee(
    aapt_last30d,
    via = "gcs_to_asset",
    assetId = "users/joaofgo/severus_pt/effis_last_30days",
    #assetId = "projects/midyear-byway-378403/assets/severus_pt/effis_last_30days",
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

  ee_manage_asset_access(
    path_asset = "users/joaofgo/severus_pt/effis_last_7days",
    owner = NULL,
    editor = NULL,
    viewer = c("serviceAccount:severuspt--ty-mapper-859ec08f5@intrepid-nova-195705.iam.gserviceaccount.com",
               "serviceAccount:severuspt--y-analyst-a6ae88443@intrepid-nova-195705.iam.gserviceaccount.com"),
    all_users_can_read = TRUE,
    quiet = FALSE
  )

  ee_manage_asset_access(
    path_asset = "users/joaofgo/severus_pt/effis_last_15days",
    owner = NULL,
    editor = NULL,
    viewer = c("serviceAccount:severuspt--ty-mapper-859ec08f5@intrepid-nova-195705.iam.gserviceaccount.com",
               "serviceAccount:severuspt--y-analyst-a6ae88443@intrepid-nova-195705.iam.gserviceaccount.com"),
    all_users_can_read = TRUE,
    quiet = FALSE
  )

  ee_manage_asset_access(
    path_asset = "users/joaofgo/severus_pt/effis_last_30days",
    owner = NULL,
    editor = NULL,
    viewer = c("serviceAccount:severuspt--ty-mapper-859ec08f5@intrepid-nova-195705.iam.gserviceaccount.com",
               "serviceAccount:severuspt--y-analyst-a6ae88443@intrepid-nova-195705.iam.gserviceaccount.com"),
    all_users_can_read = TRUE,
    quiet = FALSE
  )


}



## ---------------------------------------------------------------------- ##
## HISTORICAL DATA ----
## ---------------------------------------------------------------------- ##


## Update historical data from 2000 to current year minus one

if(updateHistData){

  aa_hist <- aapt %>% filter(year >= 2000, year < spt_current_year())

  aa_hist <- aa_hist %>%
    select(id, area_ht, fire_date, year) %>%
    mutate(guid = uuid::UUIDgenerate(n = nrow(.))) %>%
    mutate(year = as.integer(year))

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

  baCurrentYear <- baCurrentYear %>%
    select(id, area_ht, fire_date, year) %>%
    mutate(guid = uuid::UUIDgenerate(n = nrow(.))) %>%
    mutate(year = as.integer(year))

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



