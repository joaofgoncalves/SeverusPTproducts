
library(tidyverse)
library(dplyr)
library(magrittr)
library(rgee)
library(bitops)
library(terra)
library(knitr)
library(kableExtra)
library(crayon)
library(clisymbols)

# Global variables

SPT_PROJ_ACRONYM <- "SPT"

SPT_TASK_TABLE_FILENAME <<- "SPT_MainTaskTable.csv"
SPT_TASK_TABLE_BASENAME <<- "SPT_MainTaskTable"
SPT_TASK_TABLE_FORMAT   <<- "csv"
SPT_TASK_TABLE_DIR      <<- "./DATA/TABLES"
SPT_TASK_TABLE_PATH     <<- "./DATA/TABLES/SPT_MainTaskTable.csv"

SPT_EFFIS_GEE_ASSET <<- "users/joaofgo/severus_pt/EFFIS_2000_2022_PT_v2"
SPT_EFFIS_YEAR_FIELD <<- "year"
SPT_EFFIS_DATE_FIELD <<- "FIREDATE"
SPT_EFFIS_AREA_FIELD <<- "AREA_HA"

SPT_ICNF_GEE_ASSET  <<- "users/joaofgo/severus_pt/AA_ICNF_2000_2021_PT_v2"
SPT_ICNF_YEAR_FIELD <<- "Ano"
SPT_ICNF_DATE_FIELD <<- "data_inici"
SPT_ICNF_AREA_FIELD <<- "area_ha"

SPT_GEE_TASK_PATH <<- "./out/gee_tasks"
SPT_GEE_PRODUCTS_PATH <<- "./out/gee_products"

SPT_META_TEMPLATE <<- "./data/tables/SPT_meta_template.xlsx"
SPT_META_TABLE <- readxl::read_excel(SPT_META_TEMPLATE)
SPT_META_TABLE[is.na(SPT_META_TABLE[,"Value"]), 2] <- ""
SPT_META_TABLE <<- SPT_META_TABLE

SPT_LOG_PATH <<- "./out/logs"








