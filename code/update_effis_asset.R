
library(sf)
library(rgee)
library(dplyr)

source("./code/modules/r_utils.R")

ee_Initialize(user = "joaofgo@gmail.com", gcs = TRUE)


url <- "https://maps.wild-fire.eu/effis?service=WFS&request=getfeature&typename=ms:modis.ba.poly&version=1.1.0&outputformat=SHAPEZIP"

download_and_unzip(url, "./temp/effis_data.zip", "./temp")


aa <- read_sf("./temp/modis.ba.poly.shp")

aapt <- aa %>% 
  filter(COUNTRY == "PT") %>% 
  mutate(year = substr(FIREDATE,1,4)) %>% 
  mutate(AREA_HA = as.numeric(AREA_HA)) %>% 
  st_make_valid() %>% 
  st_buffer(0.0) %>% 
  mutate(area_ht = as.numeric(st_area(.)/10000))


dir.create(paste("./data/vector/EFFIS",getDateString(),sep="_"))

shpFile <- paste("./data/vector/EFFIS_",getDateString(),
                 "/EFFIS_",getDateString(),".shp",sep="")

write_sf(aapt, shpFile)
aapt_ <- read_sf(shpFile)


sf_as_ee(
  aapt_,
  via = "gcs_to_asset",
  assetId = "users/joaofgo/severus_pt/effis_test",
  bucket = "my_gee_data_bucket",
  predefinedAcl = "bucketLevel",
  command_line_tool_path = NULL,
  overwrite = TRUE,
  monitoring = TRUE,
  proj = "EPSG:4326",
  evenOdd = TRUE,
  geodesic = NULL,
  quiet = TRUE
)

