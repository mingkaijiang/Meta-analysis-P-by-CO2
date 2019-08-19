metafor_p_statistics_gas_exchange_100_eCO2 <- function(reDF, sumDF2) {
    
    ### create directory
    if(!dir.exists("output/statistics_p_gas_exchange_100_eCO2")) {
        dir.create("output/statistics_p_gas_exchange_100_eCO2", showWarnings = FALSE)
    }
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="CO2 assimilation rate")
    
    ### random-effect model
    # res <- rma(log_P_eCO2, variance_p_eCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_eCO2, variance_p_eCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_eCO2, variance_p_eCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="CO2_assimilation_rate",
                                                                   trt="eCO2") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stomatal conductance")
    
    ### random-effect model
    # res <- rma(log_P_eCO2, variance_p_eCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_eCO2, variance_p_eCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_eCO2, variance_p_eCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="stomatal_conductance",
                                                                   trt="eCO2") 
    return(sumDF2)
    
    
}