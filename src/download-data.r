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
widths <- c(7, 13, 6, 9, NA)
rows_tab1 <- 39

read_fwf(
  file = "tab1.txt",
  skip = 8,
  fwf_widths(widths),
  colnames(names_tab1),
  n_max = rows_tab1)