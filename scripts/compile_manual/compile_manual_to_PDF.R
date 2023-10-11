
setwd("C:/MyFiles/R-dev/SeverusPTproducts")

roxygen2::roxygenise()

setwd("C:/MyFiles/R-dev")

system("R CMD Rd2pdf SeverusPTproducts")


file.copy(from = "C:/MyFiles/R-dev/SeverusPTproducts.pdf",
          to   = "C:/MyFiles/R-dev/SeverusPTproducts/SeverusPTproducts.pdf")

try(file.remove("C:/MyFiles/R-dev/SeverusPTproducts.pdf"))
