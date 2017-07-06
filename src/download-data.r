##############################
##Download and extract data ##
##############################

library(lubridate)
library(pdftools)
library(readr)
library(data.table)
library(readxl)
metadata_download <- fread("input/metadata_download.csv")
metadata_pdf <- fread("input/metadata_pdf.csv")
metadata_xls <- fread("input/metadata_xls.csv")

####################
## download files ##
####################

# requires a metadata file with 2 columns: 'file' and 'url'
download_files <- function(x) {
    for (i in 1:nrow(x)) { 
        cat("\n\n===== Processing file: ", x[i,file], "\n")
        if (!file.exists(sprintf("input/%s", x[i,file]))) {
            cat("\n Downloading ", x[i,file])
            download.file(x[i,url],
                  destfile = sprintf("input/%s", x[i,file]),
                  mode = "wb")
        } else cat("\ninput", x[i,file], " already downloaded \n")
    }
}

download_files(metadata_download)

if (!file.exists("input/future_jobs.rdata")) {

future <- pdf_text("input/future-employment.pdf")
page_nos <- as.numeric(57:72)
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
    writeLines(dat, sprintf("%s.txt", metadata[page == i, table]))
    dat <- read_fwf(
        file = sprintf("%s.txt", metadata[page == i, table]),
        skip = metadata[page == i, skip_rows],
        fwf_widths(widths = c(metadata[page == i, col1],
                     metadata[page == i, col2],
                     metadata[page == i, col3],
                     metadata[page == i, col4],
                     metadata[page == i, col5]), col_names = c("rank", "probability", "label", "SOC_code", "occupation")),
        n_max = as.numeric(metadata[page == i, num_rows]))
    dat$page <- i
    datalist[[i]] <- dat
    unlink(sprintf("%s.txt", metadata[page == i, table]))
    dat <- NULL
}

df <- data.table::rbindlist(datalist)
save(df, file = "input/future_jobs.rdata")
}
# page numbers need to be removed
#correlation between SOC code and census data?