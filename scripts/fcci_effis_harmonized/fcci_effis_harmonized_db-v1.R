
library(sf)
library(tidyverse)
library(ggplot2)

fcci_path <- "C:/MyFiles/R-dev/SeverusPT/DATA/VECTOR/FireCCI_v5_vect/FireCCI_aggregated"

fl <- list.files(fcci_path, pattern = ".shp$", full.names = TRUE)

i<-0
pb<-txtProgressBar(1,length(fl),style=3)
for(fn in fl){
  i<-i+1
  tmp <- read_sf(fn)
  if(i==1){
    fcci<-tmp
  }else{
    fcci<-rbind(fcci,tmp)
  }
  setTxtProgressBar(pb,i)
}

fcci <- fcci %>% mutate(year = year(min_dt))

## ---------------------------------------------------------------------------------------- ##


effis <- read_sf("C:/MyFiles/R-dev/SeverusPTproducts/temp/effis_pt.shp") %>%
  st_transform(crs="EPSG:32629")


yrs <- 2001:2020

yr <- 2001

fcci_yr <- fcci %>% filter(year == yr)
effis_yr <- effis %>% filter(year == yr)


effis_sel <- effis_yr[1,]

idx <- st_intersects(effis_sel, fcci_yr, sparse = TRUE)[[1]]

fcci_sel <- fcci_yr[idx,]

plot(fcci_sel[,"min_doy"])
plot(effis_sel[,1], add=TRUE)
