#########################################
## Functions to download and extract data ##
#########################################

library(lubridate)
library(pdftools)
library(readr)
library(data.table)
library(readxl)
#load package for reduced major axis regression
library(smatr)
library(robustbase)

#################################
## download and load functions ##
#################################

# requires a metadata file with 2 columns: 'file' and 'url'
download_files <- function(x) {
    for (i in 1:nrow(x)) { 
        cat("\n\n===== Processing file: ", x[i,file], "\n")
        if (!file.exists(sprintf("input/raw/%s", x[i,file]))) {
            cat("\n Downloading ", x[i,file])
            download.file(x[i,url],
                          destfile = sprintf("input/raw/%s", x[i,file]),
                          mode = "wb")
        } else cat(x[i,file], "already downloaded \n")
    }
}

################
## extract files ##
################

# requires download, pdf and xls metadata files
extract_files <- function(download_meta, pdf_meta, xls_meta){
    for (i in 1:nrow(download_meta)) {
        if (download_meta[i,file_type == "excel"]) {
            extract_xls_files(xls_meta[file == download_meta[i,file],])
        }
        if (download_meta[i,file_type == "pdf"]) {
            
            extract_pdf_files(pdf_meta[file == download_meta[i,file],])
        }
    }
}

# takes (x) a data.table of metadata for excel files with a particular schema
extract_xls_files <- function(x) {
    cat("\n\n===== Extracting file: ", x[,file], "\n")
    if (!file.exists(sprintf("input/%s", gsub("\\.xlsx?$", "\\.rdata", x[,file])))) {
        df <- read_excel(
            path = sprintf("input/raw/%s", x[,file]),
            sheet = x[,sheet],
            range = x[,range],
            col_names = c(x[,head1],
                          x[,head2],
                          if (is.na(x[,head3])){NULL} else (x[,head3]),
                          if (is.na(x[,head4])){NULL} else (x[,head4]),
                          if (is.na(x[,head5])){NULL} else (x[,head5])),
            col_types = c(x[,type1],
                          x[,type2],
                          if (is.na(x[,type3])){NULL} else (x[,type3]),
                          if (is.na(x[,type4])){NULL} else (x[,type4]),
                          if (is.na(x[,type5])){NULL} else (x[,type5])))
        filename <- gsub("\\.xlsx?$", "", x[,file])
        assign(paste(filename), value = df)
        save(list = filename,
            file = sprintf("input/%s", gsub("\\.xlsx?$", "\\.rdata", x[,file])))
        rm(df, filename)
        cat("\n\nSuccess!! file: ", x[,file], " extracted \n")
    } else cat(gsub("\\.xlsx?$", "\\.rdata", x[,file]), "already extracted \n")
}
    
extract_pdf_files <- function(x) {
    cat("\n\n===== Extracting file: ", x[1,file], "\n")
    if (!file.exists(sprintf("input/%s", gsub("\\.pdf?$", "\\.rdata", x[1,file])))) {
        future <- pdf_text(sprintf("input/raw/%s", x[1,file]))
        page_nos <- as.numeric(min(x[,page]):max(x[,page]))
        datalist <- list()
        #data format is 'ragged fwf' fixed with with a variable-with last column
        for (i  in page_nos) {
            dat <- future[[i]]
                if (i != 57) {
                # clean long lines with hyphen
                dat <- gsub("-\n +", "", dat)
                # clean long lines with no hyphen
                dat <- gsub(" {0}\n +", " ", dat)
                }
            writeLines(dat, sprintf("%s.txt", x[page == i, table]))
            dat <- read_fwf(
                file = sprintf("%s.txt", x[page == i, table]),
                skip = x[page == i, skip_rows],
                fwf_widths(widths = c(x[page == i, col1],
                                      x[page == i, col2],
                                      x[page == i, col3],
                                      x[page == i, col4],
                                      x[page == i, col5]), 
                            col_names = c("rank", 
                                         "probability", 
                                         "label", 
                                         "soc_code",
                                        "occupation")),
                n_max = as.numeric(x[page == i, num_rows]))
            dat$page <- i
            datalist[[i]] <- dat
            unlink(sprintf("%s.txt", x[page == i, table]))
            dat <- NULL
        }
        df <- data.table::rbindlist(datalist)
        # remove page numbers
        df[, occupation := gsub(" \\d{2}", "", occupation)]
        filename <- gsub("\\.pdf?$", "_data", x[1,file])
        assign(paste(filename), df)
        save(list = filename,
            file = sprintf("input/%s", gsub("\\.pdf", "\\.rdata", x[1,file])))
        rm(df,filename)
        cat("\n\nSuccess!! file: ", x[1,file], " extracted \n")
    } else cat(gsub("\\.pdf$", "\\.rdata", x[1,file]), "already extracted \n")
}

read_data_files <- function(filetype, directory) {
    file_list <- list.files(path = directory,
                            pattern = sprintf("*\\.%s", filetype),
                            full.names = TRUE)
    for (i in 1:length(file_list)) {
        cat("\n\nLoading file:", file_list[i])
        load(file_list[i], .GlobalEnv)
        cat("\n===== Done\n")
    }
}
