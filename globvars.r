
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


## ---- PARAMETERS TO CHANGE ----

SPT_PROJ_COORD_SYSTEM_CODE <<- "EPSG:32629"

SPT_GDRIVE_FOLDER <<- "GEE"

SPT_TASK_TABLE_FILENAME <<- "SPT_MainTaskTable.csv"
SPT_TASK_TABLE_BASENAME <<- "SPT_MainTaskTable"
SPT_TASK_TABLE_FORMAT   <<- "csv"
SPT_TASK_TABLE_DIR      <<- "./R/task_table"
SPT_TASK_TABLE_PATH     <<- "./R/task_table/SPT_MainTaskTable.csv"

#SPT_EFFIS_GEE_ASSET <<- "users/joaofgo/severus_pt/EFFIS_20230313"
SPT_EFFIS_GEE_ASSET <<- "users/joaofgo/severus_pt/effis_all"
SPT_EFFIS_HIST_GEE_ASSET <<- "users/joaofgo/severus_pt/effis_hist"
SPT_EFFIS_CURR_GEE_ASSET <<- "users/joaofgo/severus_pt/effis_current"

SPT_EFFIS_YEAR_FIELD <<- "year"
SPT_EFFIS_DATE_FIELD <<- "fire_date"
SPT_EFFIS_AREA_FIELD <<- "area_ht"

SPT_ICNF_GEE_ASSET  <<- "users/joaofgo/severus_pt/AA_ICNF_2000_2021_PT_v3"
SPT_ICNF_YEAR_FIELD <<- "Ano"
SPT_ICNF_DATE_FIELD <<- "data_inici"
SPT_ICNF_AREA_FIELD <<- "area_ht"

SPT_GEE_TASK_PATH <<- "./out/gee_tasks"
SPT_GEE_PRODUCTS_PATH <<- "./out/gee_products/temp"


## ---- DO NOT CHANGE THIS PART FROM HERE ----

SPT_PROJ_ACRONYM <- "SPT"

# SPT_META_TEMPLATE <<- "./R/meta_template/SPT_meta_template.xlsx"
# SPT_META_TABLE <- readxl::read_excel(SPT_META_TEMPLATE)

SPT_META_TEMPLATE <<- "./R/meta_template/SPT_meta_template.csv"
SPT_META_TABLE <- readr::read_csv(SPT_META_TEMPLATE)

SPT_META_TABLE[is.na(SPT_META_TABLE[,"Value"]), 2] <- ""
SPT_META_TABLE <<- SPT_META_TABLE

SPT_LOG_PATH <<- "./out/logs"

SPT_VERSION <<- "v01"
SPT_FULL_VERSION_NR <<- "v0.1.2"

# List of admissible values for each parameter
SPT_VALUES <<- list(

  # Valid satellite codes
  satCode = c(
    "S2MSI", "MOD", "MYD", "MCD", "L5TM", "L7ETM",
    "L8OLI", "L8TIRS", "LTH"
  ),
  # Valid satellite codes by main mission/collection
  satCode_s2 = c("S2MSI"),
  satCode_lt = c("L5TM", "L7ETM","L8OLI", "L8TIRS", "LTH"),
  satCode_md = c("MOD", "MYD", "MCD"),

  # Processing levels
  procLevel = c("L1", "L1C", "L2", "L2A",
                "NA", "N/A", NA), # This may be undefined if MODIS is selected!
  modisProduct = c(
    "MOD09A1", "MOD13Q1", "MOD09GQ", "MOD09Q1", "MYD09A1",
    "MYD09GQ", "MYD09Q1", "MYD13Q1", "MCD43A4",
    "NA", "N/A", NA # This may be undefined if S2/Landsat are selected!
  ),
  modisProduct_terra = c("MOD09A1", "MOD13Q1", "MOD09GQ", "MOD09Q1"),
  modisProduct_aqua = c("MYD09A1","MYD09GQ", "MYD09Q1", "MYD13Q1"),
  modisProduct_comb = c("MCD43A4"),

  baseIndex = c(
    "NBR", "NDVI", "EVI", "TCTB", "TCTG", "TCTW",
    "NBRSWIR", "MIRBI", "CSI", "NBRP", "LST",
    "LAI", "GPP", "NPP", "ALB", "FVC",
    # New indices added from Duarte Velho contributed code:
    "BAI",  "DBSI", "NBR2", "GEMI", "SAVI", "MSAVI", "NDWI"
  ),

  baseIndex_spi = c( "NBR", "NDVI", "EVI", "NBRSWIR", "MIRBI", "CSI", "NBRP",
                     # New indices added from Duarte Velho contributed code:
                     "BAI",  "DBSI", "NBR2", "GEMI", "SAVI", "MSAVI", "NDWI"),

  baseIndex_tct = c( "TCTB", "TCTG", "TCTW"),

  severityIndicator = c("DELTA", "DLT", "RDELTA", "RDT", "RBR", "S95"),
  burntAreaDataset = c(
    "ICNF", "EFFIS", "MCD64",
    "MICNF", # Modified date-imputation fires from ICNF db
    "FireCCI", "VIIRS"
  ),
  referenceYear = 2000:as.integer(format(Sys.Date(), "%Y")),
  # preFireRef = c(),
  preFireType = c("moving", "m", "mov",
                  "fixed",  "f", "fix")
)
