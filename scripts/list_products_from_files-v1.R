
data_dir <- "D:/DATA/SeverusPT/DATASETS/SPT_Products_v02"



fl <- list.files(data_dir, pattern=".tif$")

fn <- fl[1]

get_product_name <- function(x){
  fni <- unlist(strsplit(x, split = "_"))
  pname <- paste(fni[1], "D", fni[2], fni[3], substr(fni[4],1,2), substr(fni[5],1,1),
        substr(fni[9],1,3), sep="-")
  gsub("LT","LH",pname)
  }



cat(unique(sapply(fl, FUN = get_product_name, USE.NAMES = FALSE)),sep = "\n")

prod_name <- paste(fni[1],"D",fni[2], fni[3],substr(fni[4],1,2),substr(fni[5],1,1),
                   substr(fni[9],1,3),sep="-")
print(prod_name)
