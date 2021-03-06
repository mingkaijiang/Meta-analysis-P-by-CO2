make_step3_metafor_statistics_nonwoody_plants <- function(inDF, intDF) {
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf biomass")
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~ 1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_advanced(tDF, intDF, res, var.name="leaf_biomass") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem biomass")
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~ 1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_advanced(tDF, intDF, res, var.name="stem_biomass") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Root biomass")
    tDF <- subset(tDF, v_variance >= 0.01)
    
    #    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~ 1 | random_factor, data = tDF)

    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_advanced(tDF, intDF, res, var.name="root_biomass") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total plant biomass")
    tDF <- subset(tDF, v_variance >= 0.01)
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~ 1 | random_factor, data = tDF)

    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_advanced(tDF, intDF, res, var.name="total_biomass") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Aboveground biomass")
    tDF <- subset(tDF, v_variance >= 0.01)
    
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~ 1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_advanced(tDF, intDF, res, var.name="aboveground_biomass") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable%in%c("Root biomass", "Belowground biomass"))
    
    ### random-effect model
    tDF <- subset(tDF, v_variance >= 0.001)
    #tDF <- subset(tDF, v_variance <= 2)
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~ 1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_advanced(tDF, intDF, res, var.name="belowground_biomass") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf N content")
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, digits=5, 
    #           control=list(stepadj=0.05))


    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~ 1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_advanced(tDF, intDF, res, var.name="leaf_N_content") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf P content")
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.5))

    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~ 1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_advanced(tDF, intDF, res, var.name="leaf_P_content") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf N concentration")
    
    #    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~ 1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_advanced(tDF, intDF, res, var.name="leaf_N_concentration") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf P concentration")
    
    ### random-effect model
    tDF <- tDF[tDF$v_variance > 0, ]
    
    #res <- rma(log_interaction, v_variance, data = tDF,control=list(stepadj=0.5))
    

    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~ 1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_advanced(tDF, intDF, res, var.name="leaf_P_concentration") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Root P concentration")
    
    ### random-effect model
    tDF <- tDF[tDF$v_variance > 0, ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~ 1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_advanced(tDF, intDF, res, var.name="root_P_concentration") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Root N concentration")
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))

    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~ 1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_advanced(tDF, intDF, res, var.name="root_N_concentration") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="CO2 assimilation rate")
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~ 1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF,
                  control=list(optimizer="optim", optmethod="Nelder-Mead"))
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_advanced(tDF, intDF, res, var.name="CO2_assimilation_rate") 
    
    ### change LAI to leaf area and combine it with Total leaf area
    inDF[inDF$Variable=="LAI","Variable"] <- "Leaf area"
    inDF$Variable[inDF$Variable=="Total leaf area"] <- "Leaf area"
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf area")
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~ 1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_advanced(tDF, intDF, res, var.name="leaf_area") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="LMA")
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF,control=list(stepadj=0.5))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~ 1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_advanced(tDF, intDF, res, var.name="LMA") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total root length")
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~ 1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_advanced(tDF, intDF, res, var.name="Root_length") 
    
    return(intDF)

}
