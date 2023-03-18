

source("globvars.r")
source("./CODE/modules/r_utils.R")
source("./CODE/modules/task_generator.R")
source("./CODE/modules/task_management.R")
source("./CODE/modules/gee_data.R")
source("./CODE/modules/gee_indices.R")
source("./CODE/modules/gee_utils.R")
source("./CODE/modules/gee_calc.R")
source("./CODE/modules/post_proc.R")
source("./CODE/modules/metadata.R")

ttb <-
  spt_generate_tasks(taskTable    = NULL,
                satCode           = "S2MSI",
                baseIndex         = "NBR",
                procLevel         = "L2A",
                modisProduct      = NA,
                severityIndicator = "DELTA",
                burntAreaDataset  = "ICNF",
                referenceYear     = 2020:2021,
                preFireRef        = 3,
                preFireType       = "moving",
                postFireRef       = seq(3,12,by=3),
                minFireSize       = 5)

spt_write_tasks_table(ttb)



spt_rm_uncompleted_tasks()

ttb <-
  spt_generate_tasks(taskTable    = spt_read_tasks_table(SPT_TASK_TABLE_PATH),
                satCode           = c("LTH"),
                baseIndex         = c("NBR"),
                procLevel         = "L2",
                modisProduct      = "NA",
                severityIndicator = c("DELTA"),
                burntAreaDataset  = "EFFIS",
                referenceYear     = c(2001:2009,2011:2022),#2010,
                preFireRef        = 12,
                preFireType       = "moving",
                postFireRef       = seq(12,12,by=12),
                minFireSize       = 1)

spt_write_tasks_table(ttb)
View(spt_read_tasks_table(SPT_TASK_TABLE_PATH))



