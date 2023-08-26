

library(tidyverse)
library(terra)
library(readxl)
library(nlcor)

## ----------------------------------------------------------------------------- ##

create_spatial_points <- function(data_frame) {

  coordinates <- data.frame(x = data_frame$lon, y = data_frame$lat)
  spatial_points <- st_as_sf(coordinates, coords = c("x", "y"), crs = st_crs("EPSG:4326"))

  return(spatial_points)
}

get_file_name_parts <- function(x){
  unlist(strsplit(x,"_"), use.names = FALSE)[2:6]
}


cor_mod <- function(x,y,...){
  x_rm <- !is.na(x)
  out<- cor(x=x[x_rm], y=y[x_rm],...)
  return(out)
}

nlcor_mod <- function(x,y,...){
  x_rm <- !is.na(x)
  out<- nlcor(x=x[x_rm], y=y[x_rm], plt=FALSE,...)
  return(out[[1]])
}


## ----------------------------------------------------------------------------- ##


field_data <- read_excel("G:/O meu disco/SeverusPT/FIELD_SURVEYS/SURVEYS_BY_DATE/SeverusPT_FieldSurveys_All-v3.2.xlsx")

#field_data <- field_data %>% filter(as.Date(Date_EFFIS) >= as.Date("2022-01-01"))


## ----------------------------------------------------------------------------- ##

field_data_pts_sf <- create_spatial_points(field_data) %>%
  cbind(field_data) %>%
  st_transform(crs="EPSG:32629")

field_data_pts <- vect(field_data_pts_sf)


geocbi <- field_data$GeoCBI

periods <-
  c(P003 = median(1:90),
    P006 = median(91:180),
    P009 = median(181:270),
    P012 = median(271:390))

periods_df <- data.frame(periods= c(
          rep("P003", 90),
           rep("P006", 90),
           rep("P009", 90),
           rep("P012", 120)))


best_periods <- periods_df[field_data$TSF_days, ]

## ----------------------------------------------------------------------------- ##

fl <- list.files("C:/MyFiles/R-dev/SeverusPTproducts/out/gee_products/products",
                 pattern=".tif$", recursive = TRUE, full.names = TRUE)

fl <- fl[grepl("_32629_", fl)]

fn <- basename(fl)

head(fn)

## ----------------------------------------------------------------------------- ##

pb <- txtProgressBar(1, length(fl), style=3)

for(i in 1:length(fl)){

  fpath <- fl[i]
  fname <- fn[i]
  ids <- get_file_name_parts(fname)


  month_id <- as.integer(substr(ids[5],6,8))
  period_id <- substr(ids[5],5,8)

  rst <- rast(fpath)

  ext_data <- extract(rst, field_data_pts)
  ext_data_buff20m <- extract(rst, buffer(field_data_pts,20), fun="mean", na.rm=TRUE)
  ext_data_buff30m <- extract(rst, buffer(field_data_pts,30), fun="mean", na.rm=TRUE)
  ext_data_buff50m <- extract(rst, buffer(field_data_pts,50), fun="mean", na.rm=TRUE)
  ext_data_buff100m <- extract(rst, buffer(field_data_pts,100), fun="mean", na.rm=TRUE)

  ext_data[,2]         <- ext_data[,2] / 10000
  ext_data_buff20m[,2] <- ext_data_buff20m[,2] / 10000
  ext_data_buff30m[,2] <- ext_data_buff30m[,2] / 10000
  ext_data_buff50m[,2] <- ext_data_buff50m[,2] / 10000
  ext_data_buff100m[,2] <- ext_data_buff100m[,2] / 10000

  ext_data         <- cbind(ext_data, geocbi = geocbi)
  ext_data_buff20m <- cbind(ext_data_buff20m, geocbi = geocbi)
  ext_data_buff30m <- cbind(ext_data_buff30m, geocbi = geocbi)
  ext_data_buff50m <- cbind(ext_data_buff50m, geocbi = geocbi)
  ext_data_buff100m <- cbind(ext_data_buff100m, geocbi = geocbi)

  ext_data[is.na(ext_data[,2]) & (ext_data[,3]==0), 2] <- 0
  ext_data_buff20m[is.na(ext_data_buff20m[,2]) & (ext_data_buff20m[,3]==0), 2] <- 0
  ext_data_buff30m[is.na(ext_data_buff30m[,2]) & (ext_data_buff30m[,3]==0), 2] <- 0
  ext_data_buff50m[is.na(ext_data_buff50m[,2]) & (ext_data_buff50m[,3]==0), 2] <- 0
  ext_data_buff100m[is.na(ext_data_buff100m[,2]) & (ext_data_buff100m[,3]==0), 2] <- 0

  nr <- nrow(ext_data)

  meta <- data.frame(
            geo_cbi         = geocbi,
            analysis_uid    = rep(paste(ids[c(1,2,3,5)],sep="",collapse="_"),nr),
            severity_index  = rep(ids[1],nr),
            spec_index      = rep(ids[2],nr),
            sat             = rep(ids[3],nr),
            ba_data_yr      = rep(ids[4],nr),
            post_period     = rep(ids[5],nr),
            best_period     = as.integer(best_periods == period_id))

  tmp <- cbind(
    field_data %>% select(Plot_Code, lon, lat),
    meta,
    point_value = ext_data[,2],
    buff20_value = ext_data_buff20m[,2],
    buff30_value = ext_data_buff30m[,2],
    buff50_value = ext_data_buff50m[,2],
    buff100_value = ext_data_buff100m[,2]
  )

  if(i==1){
    field_sat_data <- tmp
  }else{
    field_sat_data <- rbind(field_sat_data, tmp)
  }

  setTxtProgressBar(pb, i)
}


cor_values <-
field_sat_data %>%
  #group_by(analysis_uid) %>%
  group_by(severity_index, spec_index, sat, post_period) %>%
  summarize(point_cor   = cor_mod(x = point_value,   y = geo_cbi, method = "spearman"),
            buff20_cor  = cor_mod(x = buff20_value,  y = geo_cbi, method = "spearman"),
            buff30_cor  = cor_mod(x = buff30_value,  y = geo_cbi, method = "spearman"),
            buff50_cor  = cor_mod(x = buff50_value,  y = geo_cbi, method = "spearman"),
            buff100_cor = cor_mod(x = buff100_value, y = geo_cbi, method = "spearman"),

            point_nlcor   = nlcor_mod(x = point_value,   y = geo_cbi),
            buff20_nlcor  = nlcor_mod(x = buff20_value,  y = geo_cbi),
            buff30_nlcor  = nlcor_mod(x = buff30_value,  y = geo_cbi),
            buff50_nlcor  = nlcor_mod(x = buff50_value,  y = geo_cbi),
            buff100_nlcor = nlcor_mod(x = buff100_value, y = geo_cbi)
)

View(cor_values)

write_excel_csv(field_sat_data, "./out/field_sat_data-20230811-v1.csv")
write_excel_csv(cor_values, "./out/corr_table_sat_vs_field-20230811-v2.csv")


counts_ <-
  field_sat_data %>%
  #group_by(analysis_uid) %>%
  group_by(severity_index, spec_index, sat, post_period) %>%
  summarize(n_point = sum(!is.na(point_value)),
            n_b20 = sum(!is.na(buff20_value)),
            n_b30 = sum(!is.na(buff30_value)),
            n_b50 = sum(!is.na(buff50_value)),
            n_b100 = sum(!is.na(buff100_value))
  )

write_excel_csv(counts_, "./out/num_samples_analyzed-sat-vs-field-by-period-20230811-v2.csv")




cor_values_best_period <-
  field_sat_data %>%
  filter(best_period == 1) %>%
  group_by(severity_index, spec_index, sat) %>%
  summarize(point_cor   = cor_mod(x = point_value,   y = geo_cbi, method = "spearman"),
            buff20_cor  = cor_mod(x = buff20_value,  y = geo_cbi, method = "spearman"),
            buff30_cor  = cor_mod(x = buff30_value,  y = geo_cbi, method = "spearman"),
            buff50_cor  = cor_mod(x = buff50_value,  y = geo_cbi, method = "spearman"),
            buff100_cor = cor_mod(x = buff100_value, y = geo_cbi, method = "spearman"),

            point_nlcor   = nlcor_mod(x = point_value,   y = geo_cbi),
            buff20_nlcor  = nlcor_mod(x = buff20_value,  y = geo_cbi),
            buff30_nlcor  = nlcor_mod(x = buff30_value,  y = geo_cbi),
            buff50_nlcor  = nlcor_mod(x = buff50_value,  y = geo_cbi),
            buff100_nlcor = nlcor_mod(x = buff100_value, y = geo_cbi)
  )

View(cor_values_best_period)

write_excel_csv(cor_values_best_period, "./out/corr_table_sat_vs_field_best-period-20230811-v1.csv")


counts <-
  field_sat_data %>%
  filter(best_period == 1) %>%
  group_by(severity_index, spec_index, sat) %>%
  summarize(n_point = sum(!is.na(point_value)),
            n_b20 = sum(!is.na(buff20_value)),
            n_b30 = sum(!is.na(buff30_value)),
            n_b50 = sum(!is.na(buff50_value)),
            n_b100 = sum(!is.na(buff100_value))
)

write_excel_csv(counts, "./out/num_samples_analyzed-sat-vs-field-best-period-20230811-v1.csv")





