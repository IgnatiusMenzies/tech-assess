##############################
##Download and extract data ##
##############################

library(lubridate)
library(pdftools)
library(readr)
library(data.table)

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
widths_tab2 <- c(6, 14, 6, 9, NA)
skip_rows_tab2 <- 2
rows_tab1 <- 49

read_fwf(
  file = "tab1.txt",
  skip = 8,
  fwf_widths(widths),
  colnames(names_tab1),
  n_max = rows_tab1)