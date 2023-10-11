

library(sf)
library(tidyverse)
library(ggplot2)

## ---------------------------------------------------------------------------------------- ##

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

#write_sf(aapt, "C:/MyFiles/R-dev/SeverusPTproducts/temp/effis_pt.shp")


## ---------------------------------------------------------------------------------------- ##


wrong_date_counts_all <-
  aapt %>%
  st_drop_geometry() %>%
  select(year, area_ht, fire_dt) %>%
  group_by(year, .drop = FALSE) %>%
  summarise(total_nr_fires = n(), total_area = sum(area_ht))

wrong_date_counts <- aapt %>%
  st_drop_geometry() %>%
  select(year, area_ht, fire_dt) %>%
  mutate(date = ifelse(as.character(fire_dt) ==
                                  paste0(year,"-01-01"),
                                "WRONG","GOOD")) %>%
  group_by(year, date, .drop = FALSE) %>%
  summarise(nr_fires = n(), area_group = sum(area_ht)) %>%
  left_join(wrong_date_counts_all, by="year") %>%
  as.data.frame() %>%
  mutate(p_nr_fires = (nr_fires / total_nr_fires)*100) %>%
  mutate(p_area = (area_group / total_area)*100)

wrong_date_counts_long <- wrong_date_counts %>%
  pivot_longer(cols = nr_fires:p_area)

## ---------------------------------------------------------------------------------------- ##



g1 <- ggplot(wrong_date_counts_long %>% filter(name == "p_area"),
             aes(x=year, y=value, fill=date)) +
  geom_bar(stat = "identity",alpha=0.8) +
  scale_fill_manual(values=c("#DB2929","#50784C")) +
  scale_x_continuous(breaks = 2000:2021) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust = 0.5)) +
  theme(legend.position = "bottom") +
  xlab("Year") +
  ylab("% of annual burned area")

plot(g1)

g2 <- ggplot(wrong_date_counts_long %>% filter(name == "p_nr_fires"),
             aes(x=year, y=value, fill=date)) +
  geom_bar(stat = "identity",alpha=0.8) +
  scale_fill_manual(values=c( "#DB2929","#50784C")) +
  scale_x_continuous(breaks = 2000:2021) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust = 0.5)) +
  theme(legend.position = "bottom") +
  xlab("Year") +
  ylab("% of annual wildfires")

plot(g2)

g3 <- ggplot(wrong_date_counts_long %>% filter(name == "nr_fires"),
             aes(x=year, y=value, fill=date)) +
  geom_bar(stat = "identity",alpha=0.8) +
  scale_fill_manual(values=c( "#DB2929","#50784C")) +
  scale_x_continuous(breaks = 2000:2021) +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, vjust = 0.5)) +
  theme(legend.position = "bottom") +
  xlab("Year") +
  ylab("% of annual burned area") +
  xlab("Year") +
  ylab("Number of annual wildfires")

plot(g3)

library(patchwork)

m <- g1 | g2 | g3

plot(m)

ggsave("./out/EFFIS_wrong_dates.png", m, width = 13, height = 6)

## ---------------------------------------------------------------------------------------- ##





