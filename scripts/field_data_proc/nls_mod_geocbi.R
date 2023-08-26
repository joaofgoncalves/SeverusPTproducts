
library(stats)
library(aomisc)
library(tidyverse)
library(ggplot2)
library(grid)
library(propagate)

stoc_nls <- function(fit_dt, nmax=100){

  #pb <- txtProgressBar(1,nmax,style=1)

  for(i in 1:nmax){

    rand_a <- runif(1, min=-10, max=10)
    rand_b <- runif(1, min=-10, max=10)
    rand_c <- runif(1, min=-20, max=20)

    nls_mod <- suppressMessages(
               suppressWarnings(
                try(nls(formula = y ~ a + b * exp(c*x),
                      data    = fit_dt,
                      start   = c(a=rand_a, b=rand_b, c=rand_c),
                      control = nls.control(maxiter = 1000)))))

    if(!inherits(nls_mod,"try-error")){
      break
    }

    #setTxtProgressBar(pb,i)
  }

  return(nls_mod)
}

get_eq_sign <- function(x){
  if(x<0){return(" - ")}
  else{return(" + ")}
}

get_eq_line <- function(mod, vname){
  s <- summary(mod)
  coeffs <- s$coefficients[,1]
  a <- s$coefficients[1,1]
  b <- s$coefficients[2,1]
  c <- s$coefficients[3,1]
  vname <- gsub("\\ +","_",vname)
  exp_out <- paste("GeoCBI = ",round(a,3),get_eq_sign(b), abs(round(b,3)), " × exp(",round(c,3)," × ",vname,")",sep="")
  return(exp_out)
}

get_var_names <- function(x){
  paste(unlist(strsplit(x,"_"))[1:2], collapse = " ")
}

r2_nls <- function(model, fit_dt){

  y_pred <- predict(model, fit_dt$x)

  # Calculate sum of squared residuals (SSR)
  ssr <- sum((fit_dt$y - y_pred)^2)
  sst <- sum((fit_dt$y - mean(fit_dt$y))^2)
  r2 <- 1 - ssr / sst
  return(r2)

}

nls_pred_line <- function(model, fit_dt){

  pred_dt <- data.frame(x= seq(min(fit_dt$x), max(fit_dt$x), length=1000))
  y_pred_ <- predict(model, newdata = pred_dt)
  pred_dt <- pred_dt %>% mutate(y = y_pred_) %>% filter(y > 0)

  return(pred_dt)
}


## ------------------------------------------------------------------------ ##

field_sat_data <- read_csv("./out/field_sat_data-20230811-v1.csv") %>%
  as.data.frame


## ------------------------------------------------------------------------ ##

#
# field_sat_data <- field_sat_data %>%
#   mutate(analysis_uid_1 = paste(severity_index, spec_index, sat, sep="_"))

field_sat_data <- field_sat_data %>%
  mutate(analysis_uid_1 = paste(severity_index, spec_index, sat, sep="_")) %>%
  filter(best_period == 1)

#uids <- field_sat_data %>% dplyr::select(analysis_uid) %>% pull %>% unique
uids <- field_sat_data %>% dplyr::select(analysis_uid_1) %>% pull %>% unique


pb <- txtProgressBar(1,length(uids),style=3)
i <- 0

r2_df <- as.data.frame(matrix(NA,nrow=length(uids),ncol=2))

colnames(r2_df) <- c("analysis_uid", "r2_nls")

active_var <- "buff20_value"

#active_var <- "buff30_value"



for(uid in uids){

  i <- i+1

  r2_df[i,1] <- uid

  #ds <- field_sat_data %>% filter(analysis_uid == uid) %>% as.data.frame
  ds <- field_sat_data %>% filter(analysis_uid_1 == uid) %>% as.data.frame

  x <- ds[,active_var]

  y <- ds[!((is.na(x)) | (is.nan(x))), "geo_cbi"]

  x <- x[!is.na(x)]

  fit_dt <- data.frame(x = x, y = y) %>% arrange(y)

  ## ------------------------------------------------------------------------ ##

  nls_mod <- stoc_nls(fit_dt, nmax=1000)

  ## ------------------------------------------------------------------------ ##

  if(inherits(nls_mod, "try-error")){
    next
  }

  s <- summary(nls_mod)
  print(s)

  r2 <- r2_nls(nls_mod, fit_dt)
  print(r2)

  eq_line <- get_eq_line(nls_mod, vname = get_var_names(uid))
  eq_line <- paste("R² =",round(r2,2),"\n", eq_line)
  equation_grob <- textGrob(eq_line, x = 0.95, y = 0.1, hjust = 1, vjust = 0)

  pred_dt <- nls_pred_line(nls_mod, fit_dt) %>% arrange(x)

  pred_dt_ <- data.frame(x= seq(min(fit_dt$x), max(fit_dt$x), length=300))

  pred_conf <- predictNLS(nls_mod, newdata = data.frame(x = pred_dt_),
                          interval = "confidence", alpha=0.05, nsim=10000)


  pred_conf_df <- as.data.frame(cbind(pred_dt_,pred_conf$summary))

  g <- ggplot() +
    geom_point(aes(x,y), fit_dt, size = 3, color = "black", alpha = 0.4) +
    geom_line(aes(x,y), pred_dt, color = "#C91C1C", linewidth = 1.2) +

    geom_line(aes(x, `Sim.2.5%`), pred_conf_df, color = "#C91C1C", linewidth = 0.5, linetype = "dashed") +
    geom_line(aes(x, `Sim.97.5%`), pred_conf_df, color = "#C91C1C", linewidth = 0.5, linetype = "dashed") +

    xlab(get_var_names(uid)) +
    ylab("GeoCBI") +
    annotation_custom(equation_grob) +
    theme_bw() +
    theme(text = element_text(size = 17))


  # ggsave(paste("./out/geo_cbi_plots_all/buffer_30m/GeoCBI_vs_",uid,"_buff30m.png",sep=""), g,
  #        width = 9, height = 7.5)

  # ggsave(paste("./out/geo_cbi_plots_all/buffer_20m/GeoCBI_vs_",uid,"_buff20m.png",sep=""), g,
  #        width = 9, height = 7.5)

  ggsave(paste("./out/geo_cbi_plots_best_period_conf/buffer_20m/GeoCBI_vs_",uid,
               "_best_period_buff20m_conf.png",sep=""), g,
         width = 9, height = 7.5)

  # ggsave(paste("./out/geo_cbi_plots_best_period_conf/buffer_30m/GeoCBI_vs_",uid,"_best_period_buff30m.png",sep=""), g,
  #        width = 9, height = 7.5)

  r2_df[i,2] <- r2

  setTxtProgressBar(pb, i)

}

## ------------------------------------------------------------------------ ##

#write_csv(r2_df, "./out/GeoCBI_vs_SatData-r2_df_all_periods_buffer30m-v1.csv")
#write_csv(r2_df, "./out/GeoCBI_vs_SatData-r2_df_all_periods_buffer20m-v1.csv")

#write_csv(r2_df, "./out/GeoCBI_vs_SatData-r2_df_best_period_buffer20m-v1.csv")
#write_csv(r2_df, "./out/GeoCBI_vs_SatData-r2_df_best_period_buffer30m-v1.csv")

