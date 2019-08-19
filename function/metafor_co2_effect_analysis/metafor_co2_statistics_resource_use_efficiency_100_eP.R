metafor_co2_statistics_resource_use_efficiency_100_eP <- function(reDF, sumDF) {
    
    ### create directory
    if(!dir.exists("output/statistics_co2_resource_use_efficiency_100_eP")) {
        dir.create("output/statistics_co2_resource_use_efficiency_100_eP", showWarnings = FALSE)
    }
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="WUE")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="WUE",
                                                                    trt="eP") 

    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="NUE")
    
    ### use 1/n to get the variance
    tDF$variance_co2_eP <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="NUE",
                                                                    trt="eP") 

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="PUE")
    
    ### use 1/n to get the variance
    tDF$variance_co2_eP <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="PUE",
                                                                    trt="eP") 

    return(sumDF)
    
}