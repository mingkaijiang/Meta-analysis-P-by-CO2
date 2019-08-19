metafor_statistics_nutrient_ratio_100 <- function(reDF, intDF) {
    
    ### all nutrient ratio sample variance equals 1/n
    
    ### create directory
    if(!dir.exists("output/statistics_nutrient_ratio_100")) {
        dir.create("output/statistics_nutrient_ratio_100", showWarnings = FALSE)
    }
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf NP ratio")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="leaf_NP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stem NP ratio")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="stem_NP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root NP ratio")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="root_NP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total NP ratio")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="total_NP") 
    

    return(intDF)
    
}