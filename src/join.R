#################
## Join tables ##
################

## Join crosswalk with future employment data

tmp <- merge(future_employment_data, crosswalk, by = "soc_code", all.x = TRUE)
head(tmp)
tail(tmp)
tmp[is.na(anzsco_code)]

## join  with census data
df <- merge(tmp, census, by = "anzsco_code", all.x = TRUE)

## tidy the join
df <- df[!is.na(anzsco_code)]
