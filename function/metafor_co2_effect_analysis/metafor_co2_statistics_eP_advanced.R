metafor_co2_statistics_eP_advanced <- function(reDF, sumDF) {
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable%in%c("Aboveground biomass"))
    
    tDF <- subset(tDF, variance_co2_eP >= 0.01)
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF, control=list(maxiter=1000,
    #                                                                 stepadj=0.1))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                     var.name="aboveground_biomass",
                                                                     trt="eP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf biomass")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="leaf_biomass",
                                                                    trt="eP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stem biomass")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="stem_biomass",
                                                                    trt="eP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    #tDF <- subset(reDF, Variable=="Root biomass")
    tDF <- subset(reDF, Variable%in%c("Root biomass"))#, "Belowground biomass"))
    tDF <- subset(tDF, variance_co2_eP >= 0.01)
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="root_biomass",
                                                                    trt="eP") 
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total plant biomass")
    tDF <- subset(tDF, variance_co2_eP >= 0.01)
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="total_biomass",
                                                                    trt="eP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf N content")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF, digits=2, control=list(maxiter=1000,
    #                                                                     stepadj=0.1))

    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="leaf_N_content",
                                                                    trt="eP") 
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf P content")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)

    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="leaf_P_content",
                                                                    trt="eP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stem N content")

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
                                                                    var.name="stem_N_content",
                                                                    trt="eP") 
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stem P content")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="stem_P_content",
                                                                    trt="eP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root N content")

    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="root_N_content",
                                                                    trt="eP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root P content")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF, control=list(stepadj=0.5))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="root_P_content",
                                                                    trt="eP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total plant N content")
    
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
                                                                    var.name="total_N_content",
                                                                    trt="eP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total plant P content")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="total_P_content",
                                                                    trt="eP") 

    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf N concentration")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="leaf_N_concentration",
                                                                    trt="eP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf P concentration")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="leaf_P_concentration",
                                                                    trt="eP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root P concentration")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="root_P_concentration",
                                                                    trt="eP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stem N concentration")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="stem_N_concentration",
                                                                    trt="eP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stem P concentration")
    
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))     ### paired
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="stem_P_concentration",
                                                                    trt="eP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root N concentration")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="root_N_concentration",
                                                                    trt="eP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total plant N concentration")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="total_N_concentration",
                                                                    trt="eP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total plant P concentration")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="total_P_concentration",
                                                                    trt="eP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="CO2 assimilation rate")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="CO2_assimilation_rate",
                                                                    trt="eP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stomatal conductance")
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="stomatal_conductance",
                                                                    trt="eP") 
    
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
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf NP ratio")
    
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
                                                                    var.name="leaf_NP",
                                                                    trt="eP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stem NP ratio")
    
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
                                                                    var.name="stem_NP",
                                                                    trt="eP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root NP ratio")
    
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
                                                                    var.name="root_NP",
                                                                    trt="eP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total NP ratio")
    
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
                                                                    var.name="total_NP",
                                                                    trt="eP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Plant N uptake")
    tDF <- subset(tDF, Unit %in%c("mg N mg-1 of nodule", "mg g-1 root", "mg N g-1 root"))
    tDF$variance_co2_eP <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="N_uptake",
                                                                    trt="eP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Plant P uptake")
    tDF <- subset(tDF, Unit %in%c("ug P root-1", "mg P g-1 root", "ug P mg root-1"))
    tDF$variance_co2_eP <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_eP, variance_co2_eP, mods = ~Trt_LP_HP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="P_uptake",
                                                                    trt="eP") 
    
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
