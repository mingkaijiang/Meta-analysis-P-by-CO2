metafor_co2_statistics_nutrient_uptake_100_aP <- function(reDF, sumDF) {
    
    ### create directory
    if(!dir.exists("output/statistics_co2_nutrient_uptake_100_aP")) {
        dir.create("output/statistics_co2_nutrient_uptake_100_aP", showWarnings = FALSE)
    }
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Plant N uptake")
    tDF <- subset(tDF, Unit %in%c("mg N mg-1 of nodule", "mg g-1 root", "mg N g-1 root"))
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="N_uptake",
                                                                    trt="aP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Plant P uptake")
    tDF <- subset(tDF, Unit %in%c("ug m-1 root", "mg g-1 root", "mg P g-1 root"))
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="P_uptake",
                                                                    trt="aP") 
    
    return(sumDF)
    
    
}