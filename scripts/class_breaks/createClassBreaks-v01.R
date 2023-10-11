

library(terra)
library(tidyverse)


#fl <- list.files("C:/Users/JG/Desktop/SeverusPT_DATA/", pattern=".tif$", full.names = TRUE)
fl <- list.files("C:/Users/JG/Desktop/Severus_PT/SPT_DATA", 
                 pattern=".tif$", full.names = TRUE)
fl <- c(fl[grepl("_E2017_",fl)], fl[grepl("E2022.*S2MSI|S2MSI.*E2022",fl)])
fl <- fl[grepl("_32629_",fl)]


classBrks <- function(qts, max_value=1E7){
  
  out<-c()
  
  qts[length(qts)] <- max_value
  
  for(j in 1:(length(qts)-1)){
    
    if(j==1){
      out[j] <- paste("[1 ,",qts[j+1],"[",sep="")
    }else{
      out[j] <- paste("[",qts[j],", ",qts[j+1],"[",sep="")
    }
  }
  return(out)
}



dataBrks <- as.data.frame(matrix(NA,nrow = length(fl), ncol=9))
colnames(dataBrks) <- c("Severity_indicator","Spectral_index","Satellite",
                        "No_severity", "Very_low","Low","Moderate","High","Very_high")



for(i in seq_along(fl)){
  
  r <- rast(fl[i])
  v <- values(r)
  v <- v[!(is.na(v) | is.nan(v))]
 
  qts <- quantile(v[v > 0], probs=seq(0, 1, 0.2))
  
  print(basename(fl[i]))
  print(qts)
  
  dataBrks[i, 1] <- unlist(str_split(basename(fl[i]),"_"))[2]
  dataBrks[i, 2] <- unlist(str_split(basename(fl[i]),"_"))[3]
  dataBrks[i, 3] <- unlist(str_split(basename(fl[i]),"_"))[4]
  
  #dataBrks[i, 1] <- basename(fl[i])
  dataBrks[i, 4] <- "[-1E7, 0]"
  dataBrks[i, 5:9] <- classBrks(qts)
  
}

write.csv(dataBrks, "./out/data_breaks-5qts-v2.csv",row.names = FALSE)


sev_indicators <- c("Delta NBR (dNBR)",
                    "Relativized Burn Ratio (RBR)",
                    "Relative Delta NBR (RdNBR)")
k<-0

for(i in 10:12){
  
  k<-k+1
  r <- rast(fl[i])
  v <- values(r)
  v <- v[!(is.na(v) | is.nan(v))]
  v <- v[v>0]
  qts <- quantile(v[v > 0], probs=seq(0, 1, 0.2))
  
  qts_df <- data.frame(brks = c("0: No_severity", 
                                "1: Very_low",
                                "2: Low",
                                "3: Moderate",
                                "4: High",
                                "5: Very_high"),
                       vls  = c(0,qts[-1]))
  
  DF <- data.frame(sev_ind = v)
  rm(v)
  
  g <- ggplot(DF, aes(x=sev_ind)) + 
    #geom_histogram() + 
    geom_histogram(aes(y=after_stat(density)), colour=NA, fill="light grey")+
    geom_density(alpha=.2, linewidth=1) +
    geom_vline(data= qts_df, mapping=aes(xintercept = vls, color=brks),
               linewidth = 1.3, linetype="dashed") +
    #scale_color_brewer(palette = "BuRd") + 
    scale_colour_manual(values = rev(RColorBrewer::brewer.pal(6, "RdBu"))) +
    xlab(paste(sev_indicators[k],"severity indicator")) +
    ylab("Density") +
    theme_bw() + 
    theme(text = element_text(size = 18))
  
  if(i==12) {g <- g+scale_x_log10()}
  
  plot(g)
  
  ggsave(filename = paste("./out/",sev_indicators[k],
                          "_hist_fsev_classes-qts-brks-v1.png"),
         plot = g)

  cat("Done:",sev_indicators[k],"...\n\n\n")
}


