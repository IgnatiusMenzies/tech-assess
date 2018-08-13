# analysis.R
plot(df$probability, df$census_diff) + abline(lm(df$probability ~ df$census_diff))

cor.test(df$probability,
         df$census_diff,
         method = "spearman",
         exact = FALSE)

#transform data for the non-normal distribution. 
#average

# four different comparison methods
# hist for delta
hist(df$census_diff)
# Paraphyletic? 
# Bootstrap 10000times, look at average slope and intercept

## make decisions with where there are partial matches or overlaps - take the average probability?
# for example below, there are three occupations classified as "Apiarists" with pretty wildly varying probabilities
df[anzsco_title == "Apiarist",]

length(unique(df[,soc_title]))
length(unique(df[,anzsco_title]))
sample(df)

df_list <- vector("list", length(unique(df[,anzsco_title])))
for (i in 1:length(unique(df[,anzsco_title]))) {
    df[sample(nrow(df),1), ]
    df_list[[i]] <- df[anzsco_title == unique(df[,anzsco_title][i]),]
    df_list[[i]][sample(nrow(df_list[[i]]),1), ]
}
