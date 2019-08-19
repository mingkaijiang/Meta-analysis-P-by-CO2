metafor_p_statistics_aCO2_advanced <- function(reDF, sumDF2) {
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable%in%c("Aboveground biomass"))
    
    tDF <- subset(tDF, variance_p_aCO2 >= 0.01)
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                  var.name="aboveground_biomass",
                                                                  trt="aCO2") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf biomass")
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="leaf_biomass",
                                                                   trt="aCO2") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stem biomass")
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="stem_biomass",
                                                                   trt="aCO2") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    #tDF <- subset(reDF, Variable=="Root biomass")
    tDF <- subset(reDF, Variable%in%c("Root biomass", "Belowground biomass"))
    tDF <- subset(tDF, variance_p_aCO2 >= 0.01)
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="root_biomass",
                                                                   trt="aCO2") 

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total plant biomass")
    
    ### random-effect model
    tDF <- subset(tDF, variance_p_aCO2 >= 0.01)
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="total_biomass",
                                                                   trt="aCO2") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf N content")
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="leaf_N_content",
                                                                   trt="aCO2") 

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf P content")
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    

    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="leaf_P_content",
                                                                   trt="aCO2") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stem N content")
    
    ### use 1/n to get the variance
    tDF$variance_p_aCO2 <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="stem_N_content",
                                                                   trt="aCO2") 

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stem P content")
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="stem_P_content",
                                                                   trt="aCO2") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root N content")
    
    ### random-effect model
    #res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="root_N_content",
                                                                   trt="aCO2") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root P content")
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="root_P_content",
                                                                   trt="aCO2") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total plant N content")
    
    ### use 1/n to get the variance
    tDF$variance_p_aCO2 <- 1/tDF$Sample.Size
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="total_N_content",
                                                                   trt="aCO2") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total plant P content")
    
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="total_P_content",
                                                                   trt="aCO2") 

    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf N concentration")
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="leaf_N_concentration",
                                                                   trt="aCO2") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf P concentration")
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="leaf_P_concentration",
                                                                   trt="aCO2") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root P concentration")
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="root_P_concentration",
                                                                   trt="aCO2") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stem N concentration")
    
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="stem_N_concentration",
                                                                   trt="aCO2") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stem P concentration")
    
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="stem_P_concentration",
                                                                   trt="aCO2") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root N concentration")
    
    
    ### random-effect model
    #res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="root_N_concentration",
                                                                   trt="aCO2") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total plant N concentration")
    
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="total_N_concentration",
                                                                   trt="aCO2") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total plant P concentration")
    
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="total_P_concentration",
                                                                   trt="aCO2") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="CO2 assimilation rate")
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="CO2_assimilation_rate",
                                                                   trt="aCO2") 
    
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stomatal conductance")
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="stomatal_conductance",
                                                                   trt="aCO2") 
    
    
    ### change LAI to leaf area and combine it with Total leaf area
    reDF[reDF$Variable=="LAI","Variable"] <- "Leaf area"
    reDF$Variable[reDF$Variable=="Total leaf area"] <- "Leaf area"
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf area")
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="leaf_area",
                                                                   trt="aCO2") 
    
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="LMA")
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="LMA",
                                                                   trt="aCO2")
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="SLA")
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="SLA",
                                                                   trt="aCO2")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total root length")
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="Root_length",
                                                                   trt="aCO2")
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf NP ratio")
    
    ### use 1/n to get the variance
    tDF$variance_p_aCO2 <- 1/tDF$Sample.Size
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="leaf_NP",
                                                                   trt="aCO2")
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stem NP ratio")
    
    ### use 1/n to get the variance
    tDF$variance_p_aCO2 <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="stem_NP",
                                                                   trt="aCO2")
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root NP ratio")
    
    ### use 1/n to get the variance
    tDF$variance_p_aCO2 <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="root_NP",
                                                                   trt="aCO2")
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total NP ratio")
    
    ### use 1/n to get the variance
    tDF$variance_p_aCO2 <- 1/tDF$Sample.Size
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="total_NP",
                                                                   trt="aCO2")
    
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
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="WUE")
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="WUE",
                                                                   trt="aCO2")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="NUE")
    
    ### use 1/n to get the variance
    tDF$variance_p_aCO2 <- 1/tDF$Sample.Size
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="NUE",
                                                                   trt="aCO2")
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="PUE")
    
    ### use 1/n to get the variance
    tDF$variance_p_aCO2 <- 1/tDF$Sample.Size
    
    ### random-effect model
    # res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_P_aCO2, variance_p_aCO2, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_P_aCO2, variance_p_aCO2, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF2 <- assign_P_effect_model_stats_and_forest_plot_advanced(tDF, sumDF2, res, 
                                                                   var.name="PUE",
                                                                   trt="aCO2")
    
    

    return(sumDF2)
}
