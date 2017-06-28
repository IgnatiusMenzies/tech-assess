######################
## Helper functions ##
######################

# function for grooming pdf table data
groom.tables <- function(tab) {
    if (tab == "tab2")
        tab2 <- tab2 <- gsub("Edu-\r\n                                   cation", "Education", tab2)
}