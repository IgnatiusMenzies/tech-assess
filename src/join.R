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

## make decisions with where there are partial matches or overlaps - take the average probability?
# for example below, there are three occupations classified as "Apiarists" with pretty wildly varying probabilities
df[anzsco_title == "Apiarist",]
plot(log(df$probability+1), log(df$count_2013))
cor.test( ~ census_diff + probability, 
          data=df,
          method = "spearman",
          continuity = FALSE,
          conf.level = 0.95)
lm(log(count_2013) ~ log(pro2bability+1), df)

#transform data for the non-normal distribution. 
#average