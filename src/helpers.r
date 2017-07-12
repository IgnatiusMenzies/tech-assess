#########################################
## Functions to download and read data ##
#########################################

####################
## download files ##
####################

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
## read files ##
################

# requires download, pdf and xls metadata files
read_files <- function(download_meta, pdf_meta, xls_meta){
    for (i in 1:nrow(download_meta)) {
        if (download_meta[i,file_type == "excel"]) {
            read_xls_files(xls_meta[file == download_meta[i,file],])
        }
        if (download_meta[i,file_type == "pdf"]) {
            
            read_pdf_files(pdf_meta[file == download_meta[i,file],])
        }
    }
}

# takes (x) a data.table of metadata for excel files with a particular schema
read_xls_files <- function(x) {
    cat("\n\n===== Reading file: ", x[,file], "\n")
    if (!file.exists(sprintf("input/%s", gsub("\\.xlsx?$", "\\.RDS", x[,file])))) {
        df <- read_excel(
            path = sprintf("input/raw/%s", x[,file]),
            sheet = x[,sheet],
            range = x[,range],
            col_names = c(x[,head1],
                          x[,head2],
                          x[,head3],
                          x[,head4],
                          x[,head5])
            )
        saveRDS(
            assign(gsub("\\.xlsx?$", "_data", x[,file]), df),
            file = sprintf("input/%s", gsub("\\.xlsx?$", "\\.RDS", x[,file])))
    } else cat(gsub("\\.xlsx?$", "\\.RDS", x[,file]), "already exists \n")
}
    
read_pdf_files <- function(x) {
    cat("\n\n===== Reading file: ", x[1,file], "\n")
    if (!file.exists(sprintf("input/%s", gsub("\\.pdf?$", "\\.RDS", x[1,file])))) {
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
                                         "SOC_code", 
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
        saveRDS(
            assign(gsub("\\.pdf?$", "_data", x[1,file]), df),
            file = sprintf("input/%s", gsub("\\.pdf", "\\.RDS", x[1,file])))
    } else cat(gsub("\\.pdf?$", "\\.RDS", x[1,file]), "already exists \n")
}