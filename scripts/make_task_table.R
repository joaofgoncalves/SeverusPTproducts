

source("globvars.r")
source("./R/modules/r_utils.R")
source("./R/modules/task_generator.R")
source("./R/modules/task_management.R")
source("./R/modules/gee_data.R")
source("./R/modules/gee_indices.R")
source("./R/modules/gee_utils.R")
source("./R/modules/gee_calc.R")
source("./R/modules/post_proc.R")
source("./R/modules/metadata.R")


spt_rm_uncompleted_tasks(taskTablePath = SPT_TASK_TABLE_PATH)

ttb <-
  spt_generate_tasks(taskTable    = spt_read_tasks_table(SPT_TASK_TABLE_PATH),
                satCode           = c("MOD"),
                baseIndex         = c("NBR"),
                procLevel         = "L2",
                modisProduct      = "MOD13Q1",
                severityIndicator = c("DELTA","RDT","RBR"),
                burntAreaDataset  = "EFFIS",
                referenceYear     = 2001:2022,
                preFireRef        = 3,
                preFireType       = "moving",
                postFireRef       = seq(15,24,by=3),
                minFireSize       = 10)

spt_write_tasks_table(taskTable = ttb,taskTablePath = SPT_TASK_TABLE_PATH)
View(spt_read_tasks_table(SPT_TASK_TABLE_PATH))


ttb <-
  spt_generate_tasks(taskTable    = spt_read_tasks_table(SPT_TASK_TABLE_PATH),
                     satCode           = c("LTH"),
                     baseIndex         = c("NBR"),
                     procLevel         = "L2",
                     modisProduct      = NA,
                     severityIndicator = c("DELTA","RDT","RBR"),
                     burntAreaDataset  = "EFFIS",
                     referenceYear     = 2000:2022,
                     preFireRef        = 3,
                     preFireType       = "moving",
                     postFireRef       = seq(15,24,by=3),
                     minFireSize       = 10)

spt_write_tasks_table(taskTable = ttb,taskTablePath = SPT_TASK_TABLE_PATH)
View(spt_read_tasks_table(SPT_TASK_TABLE_PATH))

ttb <-
  spt_generate_tasks(taskTable    = spt_read_tasks_table(SPT_TASK_TABLE_PATH),
                     satCode           = c("L8OLI"),
                     baseIndex         = c("NBR"),
                     procLevel         = "L2",
                     modisProduct      = NA,
                     severityIndicator = c("DELTA","RDT","RBR"),
                     burntAreaDataset  = "EFFIS",
                     referenceYear     = 2014:2022,
                     preFireRef        = 3,
                     preFireType       = "moving",
                     postFireRef       = seq(15,24,by=3),
                     minFireSize       = 10)

spt_write_tasks_table(taskTable = ttb,taskTablePath = SPT_TASK_TABLE_PATH)
View(spt_read_tasks_table(SPT_TASK_TABLE_PATH))

ttb <-
  spt_generate_tasks(taskTable    = spt_read_tasks_table(SPT_TASK_TABLE_PATH),
                     satCode           = c("S2MSI"),
                     baseIndex         = c("NBR"),
                     procLevel         = "L2",
                     modisProduct      = NA,
                     severityIndicator = c("DELTA","RDT","RBR"),
                     burntAreaDataset  = "EFFIS",
                     referenceYear     = 2017:2022,
                     preFireRef        = 3,
                     preFireType       = "moving",
                     postFireRef       = seq(15,24,by=3),
                     minFireSize       = 10)

spt_write_tasks_table(taskTable = ttb,taskTablePath = SPT_TASK_TABLE_PATH)
View(spt_read_tasks_table(SPT_TASK_TABLE_PATH))


