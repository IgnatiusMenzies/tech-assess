#################
## Join tables ##
################

source("src/tidy.R")

## Join crosswalk with future employment data
thin_census <- census[,.(anzsco_code,perc_difference_06_13, perc_difference_13_18)]

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
boot_slopes_06_13 <- list()
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

    model <- lmrob(perc_difference_06_13 ~ probability,
              data = census_prob)
    
    df <- data.table()
    df$p_value <- summary(model)$coefficients[2,4]
    df$adj_r_square <- summary(model)$adj.r.squared
    df$slope <- model$coefficient["probability"]
    df$intercept <- model$coefficient["(Intercept)"]
    
    boot_slopes_06_13[[i]] <- df
    print(i)
}
boot_slopes_06_13 <- rbindlist(boot_slopes_06_13)

boot_slopes_13_18 <- list()
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
    
    model <- lmrob(perc_difference_13_18 ~ probability,
                data = census_prob)
    
    df <- data.table()
    df$p_value <- summary(model)$coefficients[2,4]
    df$adj_r_square <- summary(model)$adj.r.squared
    df$slope <- model$coefficient["probability"]
    df$intercept <- model$coefficient["(Intercept)"]
    
    boot_slopes_13_18[[i]] <- df
    print(i)
}
boot_slopes_13_18 <- rbindlist(boot_slopes_13_18)
