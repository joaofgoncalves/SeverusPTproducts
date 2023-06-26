
## -------------------------------------------------------------------------------- ##
## Make product folders and move files from the original location to the final one
## -------------------------------------------------------------------------------- ##


# SPT-D-DELTA-NBR-S2-I-v01

projectCode <- "SPT"

pipelineCode <- "D"

sevInds <- c("DELTA","RDT","RBR")

specInds <- c("BAI","CSI","MIRBI","NBR",
              "NBRSWIR","NDVI","TCTG")

sats <- c("S2","L8")

baDatasets <- "E"

versionNr <- "v01"

satCodeNames <- c("L8OLI", "S2MSI")
names(satCodeNames) <- c("L8","S2")


## -------------------------------------------------------------------------------- ##
## Generate codes for product directories
## -------------------------------------------------------------------------------- ##


folderNames <- expand.grid(projectCode,
                            pipelineCode,
                            sevInds,
                            specInds,
                            sats,
                            baDatasets,
                            versionNr)

folderNames <- apply(folderNames, 1, FUN = function(x) paste(x,collapse="-"))

length(folderNames)

for(i in seq_along(folderNames)){

  dir.create(paste("./out/gee_products/_prod_folders/",
                   folderNames[i],sep=""))


}


## -------------------------------------------------------------------------------- ##
## Move files around
## -------------------------------------------------------------------------------- ##


library(fs)

fl1 <- list.files("./out/gee_products/L8/", full.names = TRUE)
fl2 <- list.files("./out/gee_products/S2/", full.names = TRUE)

fl <- c(fl1, fl2)

from_folder <- c(L8 = "./out/gee_products/L8",
                 S2 = "./out/gee_products/S2")

to_folder <- "./out/gee_products/_prod_folders"



for(i in seq_along(folderNames)){


  fn <- unlist(strsplit(folderNames[i], "-"))

  str_pattern <- paste(fn[1], fn[3], fn[4], satCodeNames[fn[5]], fn[6], sep="_")
  print(str_pattern)

  from_files <- fl[grepl(str_pattern,x = fl)]
  print(length(from_files))

  cat("\n#########################################################\n\n")

  to_files <- gsub(from_folder[fn[5]],
                   paste(to_folder,folderNames[i],sep="/"),
                   from_files)

  cat("Moving files ..... ")
  fs::file_move(from_files, to_files)
  cat("done.\n\n")
}




