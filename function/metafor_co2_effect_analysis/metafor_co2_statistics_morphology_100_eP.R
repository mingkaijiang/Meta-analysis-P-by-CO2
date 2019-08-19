metafor_co2_statistics_morphology_100_eP <- function(reDF, sumDF) {
    
    ### create directory
    if(!dir.exists("output/statistics_co2_morphology_100_eP")) {
        dir.create("output/statistics_co2_morphology_100_eP", showWarnings = FALSE)
    }
    
    ### change LAI to leaf area and combine it with Total leaf area
    reDF[reDF$Variable=="LAI","Variable"] <- "Leaf area"
    reDF$Variable[reDF$Variable=="Total leaf area"] <- "Leaf area"
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf area")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="leaf_area",
                                                                    trt="eP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="LMA")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="LMA",
                                                                    trt="eP") 

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="SLA")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="SLA",
                                                                    trt="eP") 

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total root length")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="Root_length",
                                                                    trt="eP") 
    

    return(sumDF)
    
}