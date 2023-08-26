

library(readxl)
library(tidyverse)
library(sf)
library(tidygeocoder)



## -------------------------------------------------------------------------------------- ##
## ANCILLARY FUNCTIONS TO PROCESS DATA ----
## -------------------------------------------------------------------------------------- ##


create_spatial_points <- function(data_frame) {
  if (!all(c("Latitude", "Longitude") %in% colnames(data_frame))) {
    stop("Input data frame must have 'Latitude' and 'Longitude' columns.")
  }

  coordinates <- data.frame(x = data_frame$Lon, y = data_frame$Lat)
  spatial_points <- st_as_sf(coordinates, coords = c("x", "y"), crs = st_crs("EPSG:4326"))

  return(spatial_points)
}


dms_to_dd_ <- function(input_string) {

  degrees <- as.numeric(sub("(-?\\d+)d.*", "\\1", input_string))
  minutes <- as.numeric(sub("-?\\d+d(-?\\d+)m.*", "\\1", input_string))
  seconds <- as.numeric(sub("-?\\d+d\\d+m(-?\\d+\\.\\d+)s.*", "\\1", input_string))

  if(degrees < 0){
    dd <- -1*((-1*degrees) + (minutes/60) + (seconds/3600))
  }else{
    dd <- degrees + (minutes/60) + (seconds/3600)
  }

  return(dd)
}


dms_to_dd <- function(x) sapply(x, dms_to_dd_)


date_diff_days <- function(date1, date2) {

  difference <- as.numeric(difftime(date2, date1, units = "days"))
  return(difference)
}

replaceNA <- function(x){
  sapply(x,
         FUN = function(x) if(is.na(x)) return("") else return(x)
  )
}

replaceCommas <- function(x){
  x<-gsub("\\,+",",",x)
  x<-gsub("^\\,","",x)
  x<-gsub("\\,$","",x)
  x<-gsub("\\,+",", ",x)
  return(x)
}


## -------------------------------------------------------------------------------------- ##
## READ ALL DATA NEEDED ----
## -------------------------------------------------------------------------------------- ##

# EFFIS DATA
effis <- st_read("./temp/modis.ba.poly.shp")

# SAMPLE POINTS COORDINATES
xls_path <- "C:/Users/JG/Desktop/severus_alterado.xlsx"
spt_coords <- read_excel(xls_path)

# GEO_CBI DATASET
geo_cbi_data <- read_excel("C:/Users/JG/Desktop/SPT_Field-data-S020-S028-v1.xlsx")


## -------------------------------------------------------------------------------------- ##
## READ AND PROCESS SPATIAL POINTS/LOCATIONS ----
## -------------------------------------------------------------------------------------- ##


spt_coords <- spt_coords %>%
  select(Name, Date, Latitude, Longitude, Northing, Easting, HRMS, VRMS, PDOP) %>%
  mutate(Lat = dms_to_dd(Latitude)) %>%
  mutate(Lon = dms_to_dd(Longitude))

# sf sptial points object to get EFFIS data
samp_pts <- create_spatial_points(spt_coords)


## ----------------------------------- ##
## Select the samples to process
## Define the row range in samp_pts

subsamp_rows_idx <- 66:nrow(samp_pts)

## Subset the data
samp_pts_selected <- samp_pts[subsamp_rows_idx, ]

spt_coords_selected <- spt_coords[subsamp_rows_idx, ]


## -------------------------------------------------------------------------------------- ##
## INTERSECT POINTS WITH EFFIS DATA TO GET IDS AND FIRE/IGNITION DATES ----
## -------------------------------------------------------------------------------------- ##


effis_pt <- effis %>%
  filter(COUNTRY == "PT") %>% # Filter data only to Portugal
  mutate(year = substr(FIREDATE,1,4)) %>% # Add year field
  filter(year >= 2021) %>%
  rename(fire_date = FIREDATE) %>%
  mutate(AREA_HA = as.numeric(AREA_HA)) %>% # Convert area field to numeric
  st_make_valid() %>%  # Data corrections to avoid topological errors
  st_buffer(0.0) %>%
  mutate(area_ht = as.numeric(st_area(.)/10000)) # Add area field in hectares / remove units


int_effis <- st_intersects(samp_pts_selected, effis_pt, sparse = TRUE)


effis_samp_fires <-
  cbind(
    spt_coords_selected %>%
      select(Name, Lat, Lon),

    effis_pt[unlist(int_effis), ] %>%
      select(id, fire_date) %>%
      mutate(fire_date = substr(fire_date, 1, 10)) %>%
      st_drop_geometry()
  )


## -------------------------------------------------------------------------------------- ##
## REVERSE GEOCODE LOCATIONS BASED ON COORDINATES ----
## -------------------------------------------------------------------------------------- ##


addrs <- reverse_geocode(spt_coords_selected,
                         lat = "Lat",
                         long = "Lon",
                         method = 'osm',
                         full_results = TRUE)


## -------------------------------------------------------------------------------------- ##
## COMBINE ALL DATASETS AND FORMAT AS THE MAIN FIELD DATA WORKSHEET ----
## -------------------------------------------------------------------------------------- ##


field_data <-
spt_coords_selected %>%
  select(-Latitude, -Longitude) %>%
  left_join(effis_samp_fires %>% select(-Lon, -Lat), by ="Name") %>%
  mutate(date = substr(Date,1,10)) %>%
  select(-Date) %>%
  mutate(dt_diff = date_diff_days(fire_date, date)) %>%
  left_join(addrs %>% select(Name, address,village,city,town,county), by ="Name") %>%
  mutate(short_address = replaceCommas(
                              paste(replaceNA(village),
                               replaceNA(city),
                               replaceNA(town),
                               replaceNA(county),sep=","))) %>%
  select(Name, date, short_address, id, fire_date, dt_diff, Lon, Lat, Easting, Northing)


# Combine the final dataset with correct column order
final_dt <- field_data %>%
  left_join(
    geo_cbi_data, by=c("Name"="sample_ID")) %>%
  mutate(GeoCBI = ifelse(is.na(GeoCBI), 0, GeoCBI))

write_excel_csv(final_dt,"c:/Users/JG/Desktop/SPT_field_data_june_july-2023.csv",
          na = "")



