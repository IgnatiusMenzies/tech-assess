#################
## tidy tables ##
#################

source("src/download_and_load.R")

# coerce to data.table
anzsco_isco <- as.data.table(anzsco_isco)
future_employment_data <- as.data.table(future_employment_data)
isco_soc <- as.data.table(isco_soc)
occupation_2006 <- as.data.table(occupation_2006)
occupation_2013 <- as.data.table(occupation_2013)
occupation_2018 <- as.data.table(occupation_2018)

# tidy crosswork table

# higher-level categories are interspersed throughout data. 
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
crosswalk <- merge(anzsco_isco, isco_soc, by = "isco_code", all = TRUE, allow.cartesian = TRUE)
crosswalk[,anzsco_code := as.numeric(anzsco_code)]

## census data
census <- merge(occupation_2006, occupation_2013, by = "anzsco_code", all = TRUE, allow.cartesian = TRUE)
census <- merge(census, occupation_2018, by = "anzsco_code", all = TRUE, allow.cartesian = TRUE)

census[is.na(anzsco_code)]

census <- census[!is.na(anzsco_code)]
census[is.na(count_2013)]

census[, absolute_difference_06_13 := (count_2013 - count_2006)]
census[, absolute_difference_13_18 := (count_2018 - count_2013)]
 
census[,`:=` (total_2006 = sum(count_2006, na.rm = TRUE),
              total_2013 = sum(count_2013, na.rm = TRUE),
              total_2018 = sum(count_2018, na.rm = TRUE))]

census[, `:=` (census_prop_2006 = count_2006/total_2006*100,
               census_prop_2013 = count_2013/total_2013*100,
               census_prop_2018 = count_2018/total_2018*100)]

census[, perc_difference_06_13 := (census_prop_2013 - census_prop_2006)]
census[, perc_difference_13_18 := (census_prop_2018 - census_prop_2013)]