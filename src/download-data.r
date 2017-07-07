# Download and read files

library(lubridate)
library(pdftools)
library(readr)
library(data.table)
library(readxl)
source("src/helpers.r")

#read metadata files 
metadata_download <- fread("input/metadata/metadata_download.csv")
metadata_pdf <- fread("input/metadata/metadata_pdf.csv")
metadata_xls <- fread("input/metadata/metadata_xls.csv")

rdata_files <- c("anzsco_isco.rdata", "future_jobs.rdata", "isco_soc.rdata")

# download files if they don't exist
if (!file.exists("input/raw/*.xls | input/raw/*.pdf")) {
    download_files(metadata_download)
}

# read in data if they haven't been
if (!(grep(".*\\.rdata$",list.files("input/")))) {
    read_files(metadata_download, metadata_pdf, metadata_xls)
}

list.files("input/")[grepl(".*\\.rdata$",list.files("input/"))]

# page numbers need to be removed
#correlation between SOC code and census data?
