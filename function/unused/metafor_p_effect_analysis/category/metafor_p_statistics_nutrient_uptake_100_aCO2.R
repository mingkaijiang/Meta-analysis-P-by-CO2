metafor_p_statistics_nutrient_uptake_100_aCO2 <- function(reDF, sumDF2) {
    
    ### create directory
    if(!dir.exists("output/statistics_p_nutrient_uptake_100_aCO2")) {
        dir.create("output/statistics_p_nutrient_uptake_100_aCO2", showWarnings = FALSE)
    }
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Plant N uptake")
    tDF <- subset(tDF, Unit %in%c("mg N mg-1 of nodule", "mg g-1 root", "mg N g-1 root"))
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="N_uptake",
                                                                   trt="aCO2")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Plant P uptake")
    tDF <- subset(tDF, Unit %in%c("ug m-1 root", "mg g-1 root", "mg P g-1 root"))
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="P_uptake",
                                                                   trt="aCO2")
    
    return(sumDF2)
    
}