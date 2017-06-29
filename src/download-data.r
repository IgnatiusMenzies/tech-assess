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

#data format is 'ragged fwf' fixed with with a variable-with last column
tab1 <- future[[57]]
writeLines(tab1, "tab1.txt")

names_tab1 <- c("rank", "probability", "label", "SOC_code", "occupation")
widths_tab1 <- c(7, 13, 6, 9, NA)
rows_tab1 <- 39
skip_rows_tab1 <- 8

tab2 <- future[[58]]
tab2 <- gsub("Edu-\r\n                                   cation", "Education", tab2)
writeLines(tab2, "tab2.txt")
widths_tab2 <- c(6, 14, 6, 9, NA)
skip_rows_tab2 <- 2
rows_tab1 <- 48

tab3 <- future[[59]]
tab3 <- gsub("In-\r\n                                   spectors", "Inspectors", tab3)
writeLines(tab3, "tab3.txt")
widths_tab3 <- c(6, 14, 6, 9, NA)
skip_rows_tab2 <- 2
rows_tab1 <- 47

tab4 <- future[[60]]
writeLines(tab4, "tab4.txt")

tab4 <- future[[60]]
#hyphen
tab3 <- gsub("-\r\n +", "", tab3)
#no hyphen
tab3 <- gsub("\r\n +", " ", tab3)
writeLines(tab4, "tab4.txt")
table3 <- read_fwf(
  file = "tab3.txt",
  skip = 2,
  fwf_widths(widths_tab3),
  colnames(names_tab1),
  n_max = rows_tab1)