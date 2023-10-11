

library(sf)
library(tidyverse)
library(ggplot2)

p <- "C:/MyFiles/R-dev/SeverusPT/DATA/VECTOR/ICNF_AA/AA_ICNF_2000_2021_mergedWGS84_v3.shp"

aa <- read_sf(p) %>%
  rename("fire_dt" = "data_inici",
         "year" = "Ano")

head(aa)

wrong_date_counts_all <-
  aa %>%
  st_drop_geometry() %>%
  select(year, area_ht, fire_dt) %>%
  group_by(year, .drop = FALSE) %>%
  summarise(total_nr_fires = n(), total_area = sum(area_ht))

wrong_date_counts <- aa %>%
  st_drop_geometry() %>%
  select(year, area_ht, fire_dt) %>%
  mutate(date = ifelse(is.na(fire_dt) | fire_dt=="" | is.null(fire_dt),
                       "No date","With date")) %>%
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

ggsave("./out/ICNFdb_wrong_dates.png", m, width = 13, height = 6)



