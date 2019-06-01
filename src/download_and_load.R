#############################
## Download and read files ##
#############################

source("src/helpers.R")

#read metadata files 
metadata_download <- fread("input/metadata/metadata_download.csv")
metadata_pdf <- fread("input/metadata/metadata_pdf.csv")
metadata_xls <- fread("input/metadata/metadata_xls.csv")

# download files if they don't exist
download_files(metadata_download)

# extract data to RDS
extract_files(metadata_download, metadata_pdf, metadata_xls)

# read datafiles
read_data_files(filetype = "rdata", directory = "input")
