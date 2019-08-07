compute_statistics_for_woody_and_nonwoody_comparison <- function(wdDF, nwdDF) {
    ### use Welch's t-test for unequal sample sizes and unequal variance
    
    ### need to calculate the weighted mean
    require(weights)
    
    ### photosynthesis
    myDF1 <- subset(wdDF, Variable == "CO2 assimilation rate")
    myDF2 <- subset(nwdDF, Variable == "CO2 assimilation rate")
    
    wtd.t.test(myDF1$log_interaction, myDF2$log_interaction, 
               weight=myDF1$v_variance, weighty=myDF2$v_variance)
    
    ### photosynthesis method 2
    myDF1 <- subset(intDF2, variable == "CO2_assimilation_rate")
    myDF2 <- subset(intDF3, variable == "CO2_assimilation_rate")
    
    myDF1$sd = myDF1$se_pct * 1.96 #sqrt(myDF1$ne)
    myDF2$sd = myDF2$se_pct * 1.96 #sqrt(myDF2$ne)
    
    tDF1 <- rnorm(n = myDF1$ne, mean=myDF1$int_pct, sd=myDF1$sd)
    tDF2 <- rnorm(n = myDF2$ne, mean=myDF2$int_pct, sd=myDF2$sd)
    
    t.test(tDF1, tDF2)
    
}