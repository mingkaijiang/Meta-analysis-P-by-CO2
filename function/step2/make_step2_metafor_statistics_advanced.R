make_step2_metafor_statistics_advanced <- function(inDF, intDF) {
  
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Aboveground biomass")
    tDF <- subset(tDF, v_variance >= 0.001)
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="aboveground_biomass") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="aboveground_biomass")
    
    #res2 <- rma.uni(log_interaction, v_variance, data=tDF)
    #test <- leave1out(res2)
    #test2 <- permutest(res2)

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf biomass")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="leaf_biomass") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="leaf_biomass")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem biomass")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="stem_biomass") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="stem_biomass")
    
    
    ####################### subset the dataframe for the right variable ##############################
    #tDF <- subset(inDF, Variable=="Root biomass")
    tDF <- subset(inDF, Variable%in%c("Root biomass", "Belowground biomass"))
    
    tDF <- subset(tDF, v_variance >= 0.001)
    #tDF <- subset(tDF, v_variance <= 2)
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~ 1 | random_factor, data = tDF)

    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    
    #print(res)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="root_biomass") 
    
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="root_biomass")
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total plant biomass")
    tDF <- subset(tDF, v_variance >= 0.001)
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="total_biomass") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="total_biomass")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf N content")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, digits=5, 
    #           control=list(stepadj=0.05))

    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="leaf_N_content") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="leaf_N_content")

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf P content")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.5))

    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="leaf_P_content") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="leaf_P_content")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem N content")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="stem_N_content") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="stem_N_content")
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem P content")
    
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
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="stem_P_content") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="stem_P_content")
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Root N content")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))

    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="root_N_content") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="root_N_content")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Root P content")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.5))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="root_P_content") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="root_P_content")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total plant N content")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="total_N_content") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="total_N_content")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total plant P content")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.5))

    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="total_P_content") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="total_P_content")
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf N concentration")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="leaf_N_concentration")
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="leaf_N_concentration")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf P concentration")
    
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
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="leaf_P_concentration") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="leaf_P_concentration")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Root P concentration")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="root_P_concentration") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="root_P_concentration")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem N concentration")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)

    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="stem_N_concentration") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="stem_N_concentration")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem P concentration")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="stem_P_concentration") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="stem_P_concentration")
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Root N concentration")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="root_N_concentration") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="root_N_concentration")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total plant N concentration")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="total_N_concentration") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="total_N_concentration")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total plant P concentration")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="total_P_concentration") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="total_P_concentration")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="CO2 assimilation rate")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance,  
    #              random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="CO2_assimilation_rate") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="CO2_assimilation_rate")
    
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stomatal conductance")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05)) 
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="stomatal_conductance") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="stomatal_conductance")
    
    
    ### change LAI to leaf area and combine it with Total leaf area
    inDF[inDF$Variable=="LAI","Variable"] <- "Leaf area"
    inDF$Variable[inDF$Variable=="Total leaf area"] <- "Leaf area"
    
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf area")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="leaf_area") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="leaf_area")
    
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="LMA")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF,control=list(stepadj=0.5))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="LMA") 
    
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="LMA")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="SLA")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="SLA") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="SLA")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total root length")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="Root_length") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="Root_length")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf NP ratio")
    
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
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="leaf_NP")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem NP ratio")
    
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
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="stem_NP")
    
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Root NP ratio")
    
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
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="root_NP")
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total NP ratio")
    
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
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="total_NP")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Plant N uptake")
    tDF <- subset(tDF, Unit %in%c("mg N mg-1 of nodule", "mg g-1 root", "mg N g-1 root"))
    tDF$v_variance <- 1/tDF$Sample.Size
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="N_uptake") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="N_uptake")
    
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Plant P uptake")
    tDF <- subset(tDF, Unit %in%c("ug P root-1", "mg P g-1 root", "ug P mg root-1"))
    tDF$v_variance <- 1/tDF$Sample.Size
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="P_uptake") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="P_uptake")
    
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="WUE")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="WUE") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="WUE")
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="NUE")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="NUE") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="NUE")
    
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="PUE")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### multivariate linear (mixed-effects) model with study as a random variable, and LP/HP ratio as moderator
    res <- rma.mv(log_interaction, v_variance, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    intDF <- assign_model_stats_and_forest_plot_advanced(tDF, intDF, res, var.name="PUE") 
    
    ### funnel plot of the multivariate model
    make_step2_interaction_funnel_plot(res, var.name="PUE")
    
    
    return(intDF)

}
