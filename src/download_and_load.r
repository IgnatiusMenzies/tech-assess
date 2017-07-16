#############################
## Download and read files ##
#############################

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

# download files if they don't exist
download_files(metadata_download)

# import data
import_files(metadata_download, metadata_pdf, metadata_xls)
