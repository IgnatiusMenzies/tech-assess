#################
## Join tables ##
################

## Join crosswalk with future employment data
thin_census <- census[,.(anzsco_code,prop_difference)]

tmp <- merge(future_employment_data, crosswalk, by = "soc_code", all.x = TRUE)
head(tmp)
tail(tmp)
tmp <- tmp[!is.na(anzsco_code)]

job_prob <- list()
for (i in 1:length(unique(tmp$anzsco_code))) {
    df <- tmp[anzsco_code == (unique(tmp$anzsco_code)[i]),]
    job_prob[[i]] <- df
    df <- NULL
}

B <- 10000  # No.of bootstrap samples
boot.slopes <- rep(NA,B)
for (i in 1:B){
    sample_prob <- list()
    for (x in 1:length(job_prob)) {
        prob <- sample(job_prob[[x]]$probability, 1)
        code <- job_prob[[x]]$anzsco_code[1]
        
        df <- data.table()
        df$probability <- prob
        df$anzsco_code <- code
        sample_prob[[x]] <- df
        df <- NULL
    }
    sample_prob <- rbindlist(sample_prob)
    
    ## join  with census data
    census_prob <- merge(sample_prob, thin_census, by = "anzsco_code", all.x = TRUE)

    PD <- census_prob$prop_difference
    P <- census_prob$probability
    this.fit <- line.cis(PD,P, alpha = 0.05, method = "SMA")
    boot.slopes[i] <- this.fit[2,1]
    print(i)
}

mean(boot.slopes)
sd(boot.slopes)
summary(boot.slopes)
