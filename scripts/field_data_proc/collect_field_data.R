


library(readxl)
library(tidyverse)


## ---------------------------------------------------------------------------------- ##
## ANCILLARY FUNCTIONS
## ---------------------------------------------------------------------------------- ##

clean_string <- function(input_string) {
  cleaned_string <- gsub("/|\\.|\\%|\\-", "", input_string) %>%
    gsub("\\ +", "_", .) %>%
    gsub("^_+|_+$", "", .) %>%
    gsub("\\(|\\)", "", .) %>%
    str_to_title(.)
  return(cleaned_string)
}

convert_zero_to_na <- function(value) {
  if (value == 0) {
    return(NA)
  } else {
    return(value)
  }
}

convert_string <- function(input_string) {
  numeric_value <- as.numeric(input_string)
  if (!is.na(numeric_value)) {
    formatted_value <- round(numeric_value, digits = 1)
    return(formatted_value)
  } else {
    return(NA)
  }
}


## ---------------------------------------------------------------------------------- ##
## DATA FILES TO COLLECT DATA FROM
## ---------------------------------------------------------------------------------- ##


files_to_proc <- list(

  # A list sub-object containing two parts:
  #
  # path -> the full path to the data
  # sheets -> the range of sheets to use

  S020 = list(
    path = "G:/O meu disco/SeverusPT/FIELD_SURVEYS/SURVEYS_BY_DATE/S020_2023_07_11/SeverusPT_GeoCBI_Survey-S020-v2-mod.xlsx",
    sheets = 1:4
  ),

  S021 = list(
    path = "G:/O meu disco/SeverusPT/FIELD_SURVEYS/SURVEYS_BY_DATE/S021_2023_07_11/SeverusPT_GeoCBI_Survey-S021-v2.xlsx",
    sheets = 1:2
  ),

  S022 = list(
    path = "G:/O meu disco/SeverusPT/FIELD_SURVEYS/SURVEYS_BY_DATE/S022_2023_07_13-14/SeverusPT_GeoCBI_Survey-S022-v1.xlsx",
    sheets = 1
  ),

  S023 = list(
    path = "G:/O meu disco/SeverusPT/FIELD_SURVEYS/SURVEYS_BY_DATE/S023_2023_07_13-14/SeverusPT_GeoCBI_Survey-S023-v1.xlsx",
    sheets = 1:11
  ),

  S024 = list(
    path = "G:/O meu disco/SeverusPT/FIELD_SURVEYS/SURVEYS_BY_DATE/S024_2023_07_27/SeverusPT_GeoCBI_Survey-S024-v1.xlsx",
    sheets = 1:5
  ),

  S025 = list(
    path = "G:/O meu disco/SeverusPT/FIELD_SURVEYS/SURVEYS_BY_DATE/S025_2023_07_27/SeverusPT_GeoCBI_Survey-S025-v1.xlsx",
    sheets = 1:4
  ),

  S026 = list(
    path = "G:/O meu disco/SeverusPT/FIELD_SURVEYS/SURVEYS_BY_DATE/S026_2023_07_28/SeverusPT_GeoCBI_Survey-S026-v1.xlsx",
    sheets = 1:2
  ),

  S027 = list(
    path = "G:/O meu disco/SeverusPT/FIELD_SURVEYS/SURVEYS_BY_DATE/S027_2023_07_28/SeverusPT_GeoCBI_Survey-S027-v1.xlsx",
    sheets = 1:3
  ),

  S028 = list(
    path = "G:/O meu disco/SeverusPT/FIELD_SURVEYS/SURVEYS_BY_DATE/S028_2023_07_28/SeverusPT_GeoCBI_Survey-S028-v1.xlsx",
    sheets = 1:3
  )
)



## ---------------------------------------------------------------------------------- ##
## LOCATIONS IN THE EXCEL SHEET THAT WILL BE USED TO GATHER DATA BY STRATUM
## ---------------------------------------------------------------------------------- ##

GeoCBI_val_row <- 75
GeoCBI_val_col <- 4

A_names_row <- 19:22
A_names_col <- 1

A_scores_row <- 19:22
A_scores_col <- 9

A_average_row <- 23
A_average_col <- 7
## --------------------------------
B_fcov_row <- 26
B_fcov_col <- 7

B_names_row <- 29:31
B_names_col <- 1

B_scores_row <- 29:31
B_scores_col <- 9

B_average_row <- 32
B_average_col <- 7
## --------------------------------
C_fcov_row <- 35
C_fcov_col <- 7

C_names_row <- 38:40
C_names_col <- 1

C_scores_row <- 38:40
C_scores_col <- 9

C_average_row <- 41
C_average_col <- 7
## --------------------------------
D_fcov_row <- 44
D_fcov_col <- 7

D_names_row <- 47:51
D_names_col <- 1

D_scores_row <- 47:51
D_scores_col <- 9

D_average_row <- 52
D_average_col <- 7
## --------------------------------
E_fcov_row <- 55
E_fcov_col <- 7

E_names_row <- 58:62
E_names_col <- 1

E_scores_row <- 58:62
E_scores_col <- 9

E_average_row <- 63
E_average_col <- 7


## ---------------------------------------------------------------------------------- ##


for(j in 1:length(files_to_proc)){

  # Get the file path
  xlsfile <- files_to_proc[[j]][["path"]]

  # Get the sheet names
  xls_sheet_names <- excel_sheets(xlsfile)

  # Subset the main list to the sheets that will actually be used
  xls_sheets_to_use <- xls_sheet_names[files_to_proc[[j]][["sheets"]]]


  i <- 0
  for(xls_sheet in xls_sheets_to_use){

    i <- i+1

    # Read the active excel file
    xlstab <- try(suppressMessages(read_excel(xlsfile, sheet = xls_sheet)))

    if(inherits(xlstab, "try-error")){
      xls_idx <- files_to_proc[[j]][["sheets"]][i]
      xlstab <- suppressMessages(read_excel(xlsfile, sheet = xls_idx))
    }


    ## ---------------------------------------------------------------------------------- ##
    ## GeoCBI MAIN VALUE
    ## ---------------------------------------------------------------------------------- ##

    GeoCBI_value <-
      xlstab[GeoCBI_val_row, GeoCBI_val_col] %>%
      pull() %>%
      as.numeric()


    ## ---------------------------------------------------------------------------------- ##
    ## STRATUM A
    ## SUBSTRATES
    ## ---------------------------------------------------------------------------------- ##

    # A1, A2, A3, A4, A_avg
    A_names_seq <-
      c(
        xlstab[A_names_row, A_names_col] %>%
          pull() %>%
          clean_string(.) %>%
          paste("A",1:length(.),"_",.,sep=""),
        "A_avg"
      )

    A_values_seq <-
      c(
        xlstab[A_scores_row, A_scores_col] %>%
          pull() %>%
          as.numeric(),

        xlstab[A_average_row, A_average_col] %>%
          pull() %>%
          convert_string %>%
          convert_zero_to_na
      )


    names(A_values_seq) <- A_names_seq


    ## ---------------------------------------------------------------------------------- ##
    ## STRATUM B
    ## HERBS, LOW SHRUBS AND TREES (less than 1 meter)
    ## ---------------------------------------------------------------------------------- ##

    # B_FCOV, B1, B2, B3, B_avg
    B_names_seq <-
      c(
        "B_Fcov",
        xlstab[B_names_row, B_names_col] %>%
          pull() %>%
          clean_string(.) %>%
          paste("B",1:length(.),"_",.,sep=""),
        "B_avg"
      )

    B_values_seq <-
      c(

        xlstab[B_fcov_row, B_fcov_col] %>%
          pull() %>%
          as.numeric(),

        xlstab[B_scores_row, B_scores_col] %>%
          pull() %>%
          as.numeric(),

        xlstab[B_average_row, B_average_col] %>%
          pull() %>%
          convert_string %>%
          convert_zero_to_na
      )

    names(B_values_seq) <- B_names_seq


    ## ---------------------------------------------------------------------------------- ##
    ## STRATUM C
    ## TALL SHRUBS AND TREES (1 to 5 meters)
    ## ---------------------------------------------------------------------------------- ##

    # C_FCOV, C1, C2, C3, C_avg
    C_names_seq <-
      c(
        "C_Fcov",
        xlstab[C_names_row, C_names_col] %>%
          pull() %>%
          clean_string(.) %>%
          paste("C",1:length(.),"_",.,sep=""),
        "C_avg"
      )

    C_values_seq <-
      c(

        xlstab[C_fcov_row, C_fcov_col] %>%
          pull() %>%
          as.numeric(),

        xlstab[C_scores_row, C_scores_col] %>%
          pull() %>%
          as.numeric(),

        xlstab[C_average_row, C_average_col] %>%
          pull() %>%
          convert_string %>%
          convert_zero_to_na
      )

    names(C_values_seq) <- C_names_seq


    ## ---------------------------------------------------------------------------------- ##
    ## STRATUM D
    ## INTERMEDIATE TREES (5 to 20 meters)
    ## ---------------------------------------------------------------------------------- ##

    # D_FCOV, D1, D2, D3, D4, D5, D_avg
    D_names_seq <-
      c(
        "D_Fcov",
        xlstab[D_names_row, D_names_col] %>%
          pull() %>%
          clean_string(.) %>%
          paste("D",1:length(.),"_",.,sep=""),
        "D_avg"
      )

    D_values_seq <-
      c(

        xlstab[D_fcov_row, D_fcov_col] %>%
          pull() %>%
          as.numeric(),

        xlstab[D_scores_row, D_scores_col] %>%
          pull() %>%
          as.numeric(),

        xlstab[D_average_row, D_average_col] %>%
          pull() %>%
          convert_string %>%
          convert_zero_to_na
      )

    names(D_values_seq) <- D_names_seq


    ## ---------------------------------------------------------------------------------- ##
    ## STRATUM E
    ## TALL TREES (> 20 meters)
    ## ---------------------------------------------------------------------------------- ##

    # E_FCOV, E1, E2, E3, E4, E5, E_avg
    E_names_seq <-
      c(
        "E_Fcov",
        xlstab[E_names_row, E_names_col] %>%
          pull() %>%
          clean_string(.) %>%
          paste("E",1:length(.),"_",.,sep=""),
        "E_avg"
      )

    E_values_seq <-
      c(

        xlstab[E_fcov_row, E_fcov_col] %>%
          pull() %>%
          as.numeric(),

        xlstab[E_scores_row, E_scores_col] %>%
          pull() %>%
          as.numeric(),

        xlstab[E_average_row, E_average_col] %>%
          pull() %>%
          convert_string %>%
          convert_zero_to_na
      )

    names(E_values_seq) <- E_names_seq


    ## ---------------------------------------------------------------------------------- ##

    tmp <- data.frame( sample_ID = xls_sheet,

                       params = c("GeoCBI",
                                  names(A_values_seq),
                                  names(B_values_seq),
                                  names(C_values_seq),
                                  names(D_values_seq),
                                  names(E_values_seq)),

                       values = c(GeoCBI_value,
                                  A_values_seq,
                                  B_values_seq,
                                  C_values_seq,
                                  D_values_seq,
                                  E_values_seq))


    rownames(tmp) <- NULL

    if(i==1){
      dt <- tmp
    }else{
      dt <- rbind(dt, tmp)
    }

    cat("Finished processing:", xls_sheet, "......\n")

  }

  if(j==1){
    dt_all <- dt
  }else{
    dt_all <- rbind(dt_all, dt)
  }

  cat("\n|| FINISHED PROCESSING XLS SHEET:", basename(xlsfile), "||\n\n\n")

}




dtw <- pivot_wider(dt_all, id_cols = "sample_ID", names_from = "params", values_from = "values")

View(dtw)


write.csv(dtw, "C:/Users/JG/Desktop/SPT_Field-data-S020-S028-v1.csv",row.names = FALSE, na = "")



