
library(sf)
library(rgee)
library(dplyr)

ee_Initialize(user = "joaofgo", gcs = TRUE)



download_and_unzip <- function(url, dest_file, extdir) {
  
  download.file(url, dest_file, mode = "wb")
  
  if (tools::file_ext(dest_file) == "zip") {
    unzip(dest_file, exdir = extdir, overwrite = TRUE)
  }
}


url <- "https://maps.wild-fire.eu/effis?service=WFS&request=getfeature&typename=ms:modis.ba.poly&version=1.1.0&outputformat=SHAPEZIP"

download_and_unzip(url, "./temp/effis_data.zip", "./temp")



aa <- read_sf("./temp/modis.ba.poly.shp")

aapt <- aa %>% 
  filter(COUNTRY == "PT") %>% 
  mutate(year = substr(FIREDATE,1,4))


# aapt %>% select(year) %>% st_drop_geometry()

sf_as_ee(
  aapt,
  via = "gcs_to_asset",
  assetId = "users/joaofgo/severus_pt/effis_test",
  bucket = "my_gee_data_bucket",
  predefinedAcl = "joaofgo",
  command_line_tool_path = NULL,
  overwrite = TRUE,
  monitoring = TRUE,
  proj = "EPSG:4326",
  evenOdd = TRUE,
  geodesic = NULL,
  quiet = TRUE
)

