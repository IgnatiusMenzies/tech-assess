#################
## Join tables ##
################

## Join crosswalk with future employment data
thin_census <- census[!is.na(count_2006),.(anzsco_code,perc_difference)]

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

B <- 2000  # No.of bootstrap samples
boot_slopes <- list()
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

    spearman_test <- cor.test( ~ perc_difference + probability,
              data = census_prob,
              method = "spearman",
              continuity = FALSE,
              conf.level = 0.95)
    
    df <- data.table()
    df$p_value <- spearman_test[3]
    df$rho <- spearman_test[4]
    boot_slopes[[i]] <- df
    print(i)
}
boot_slopes <- rbindlist(boot_slopes)

mean(boot_slopes$rho)
sd(boot_slopes$rho)
summary(boot_slopes)

plot(census_prob$probability, census_prob$perc_difference)

# major axis regression
# POOL()
# LI <- log(Island)
# LM <- log(Mainland)
# this.fit <- line.cis(LI,LM, alpha = 0.05,         method = "SMA")
# boot.slopes[i] <- this.fit[2,1]}