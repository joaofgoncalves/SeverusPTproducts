

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
  generateTasks(taskTable         = NULL,
                satCode           = "S2MSI",
                baseIndex         = "NBR",
                severityIndicator = "DELTA",
                burntAreaDataset  = "ICNF",
                referenceYear     = 2020:2021,
                preFireRef        = 3,
                preFireType       = "moving",
                postFireRef       = seq(3,12,by=3),
                minFireSize       = 5)

writeTaskTable(ttb)




ttb <-
  generateTasks(taskTable         = readTaskTable(),
                satCode           = "S2MSI",
                baseIndex         = "NBR",
                severityIndicator = "DELTA",
                burntAreaDataset  = "ICNF",
                referenceYear     = 2017:2019,
                preFireRef        = 3,
                preFireType       = "moving",
                postFireRef       = seq(3,12,by=3),
                minFireSize       = 5)

writeTaskTable(ttb)

