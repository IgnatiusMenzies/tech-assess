##########################
## load and join tables ##
##########################

library(data.table)
library(dbplyr)
library(dplyr)
source('src/helpers.r')

# read datafiles
read_data_files(filetype = "rdata", directory = "input")

anzsco_isco <- as.data.table(anzsco_isco)
future_employment_data <- as.data.table(future_employment_data)
isco_soc <- as.data.table(isco_soc)

#tidy anzco 
# higher-lever categories are interspersed throughout data. 
#it could be useful in future to get the titles and row numbers for these
# occupation categories are stored with a number. this number forms the first digit of the 6 digit anzsco code.

# store names and numbers in a sparate table
anzsco_level <- anzsco_isco[is.na(isco_code) & !(is.na(anzsco_code)), .(anzsco_code, anzsco_title)]
#rename and set types for merge downstream
setnames(anzsco_level, 
         old=c("anzsco_code", "anzsco_title"), 
         new = c("anzsco_category_num", "anzsco_category"))
anzsco_level <- anzsco_level[, anzsco_category_num := as.numeric(anzsco_category_num)]

# exclude un-tidy rows
anzsco_isco <- anzsco_isco[!is.na(isco_code)]

# first assign anzsco_category_num according to first digit of anzsco_code
# then join anzsco_level and anzsco_isco 
for (i in as.numeric(min(anzsco_level[,anzsco_category_num])):as.numeric(max(anzsco_level[,anzsco_category_num]))) {
    anzsco_isco[grepl(sprintf("^%s.*$", i), anzsco_code), anzsco_category_num := i]
}
                           
anzsco_isco <- merge(anzsco_isco, anzsco_level, by = "anzsco_category_num", all.x = TRUE)
names(anzsco_isco)
names(isco_soc)
summary(anzsco_isco[,isco_code])
summary(isco_soc[,isco_code])


df <- merge(anzsco_isco, isco_soc, by = "isco_code", all = TRUE, allow.cartesian = TRUE)

df[is.na(anzsco_code)]

