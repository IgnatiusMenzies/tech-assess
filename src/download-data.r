##############################
##Download and extract data ##
##############################

library(lubridate)
library(pdftools)
library(readr)
library(data.table)
metadata <- fread("input/metadata_future.csv")

# urls
pdfUrl <- "http://www.oxfordmartin.ox.ac.uk/downloads/academic/The_Future_of_Employment.pdf"

# download input files
if (!file.exists("input/future-employment.pdf")) {
download.file(pdfUrl, destfile = "input/future-employment.pdf", mode = "wb")
}

future <- pdf_text("input/future-employment.pdf")
page_nos <- c(57:72)
datalist <- list()

#data format is 'ragged fwf' fixed with with a variable-with last column
for (i  in page_nos) {
    dat <- future[[i]]
    # clean long lines with hyphen
    dat <- gsub("-\r\n +", "", dat)
    # clean long lines with no hyphen
    dat <- gsub("\r\n +", " ", dat)
    writeLines(dat, sprintf("%s.txt", metadata[page == i, table]))
    dat <- read_fwf(
        file = sprintf("%s.txt", metadata[page == i, table]),
        skip = metadata[page == i, skip_rows],
        fwf_widths(c(metadata[page == i, col1],
                     metadata[page == i, col2],
                     metadata[page == i, col3],
                     metadata[page == i, col4],
                     metadata[page == i, col5])),
        n_max = metadata[page == i, num_rows])
    dat$i <- i
    datalist[[i]] <- dat
}

df <- data.table::rbindlist(datalist)
names(df) <- c("rank", "probability", "label", "SOC_code", "occupation", "page")
#correlation between SOC code and census data?