######################
## Helper functions ##
######################

# function for grooming pdf table data
groom.tables <- function(table) {
    if (table == "tab2") {
        tab2 <- gsub("Edu-\r\n                                   cation", "Education", tab2)
    }
    if (table == "tab3") {
        tab3 <- gsub("In-\r\n                                   spectors", "Inspectors", tab3)
        tab3 <- gsub("Machine\r\n                                   and", "Machine and", tab3)
    }
    if (table == "tab5") {
        tab5 <- gsub("-\r\n                                   ", "", tab5)
        tab5 <- gsub("In-\r\n                                   structors", "Instructors", tab5)
    }
}

# no changes for 1,4