make_step5_metafor_co2_statistics_aP_basic <- function(inDF, sumDF) {
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable%in%c("Aboveground biomass"))

    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    #print(res)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                   var.name="aboveground_biomass",
                                                                   trt="aP") 
    
    #l <- length(tDF$Literature)
    #ns <- length(unique(tDF$Literature))
    #
    #### forest plot
    #pdf(paste0("output/statistics_interaction/aboveground_test_aCO2.pdf"),
    #    height=16, width=9)
    #forest(res, slab = tDF$Literature,
    #       xlim = c(-14, 4), 
    #       ylim = c(-3.5, l+3.5),
    #       at = c(-1, 0, 1, 2), #atransf = exp,
    #       ilab = cbind(as.character(tDF$Vegetation_type),
    #                    as.character(tDF$Species),
    #                    as.character(tDF$Mycorrhizae_2), 
    #                    round(tDF$Trt_eC_by_aC,1), 
    #                    round(tDF$Trt_eP_by_aP,1),
    #                    as.character(tDF$Experiment_duration)), 
    #       ilab.xpos = c(-10, -8, -6.5, -5, -4, -2.5), cex = 0.6)
    #text(c(-10, -8, -6.5, -5, -4, -2.5, 0), l+3, c("Vegetation", 
    #                                               "Species",
    #                                               "Mycorrhizal",
    #                                               expression(paste(eCO[2], "/", aCO[2])),
    #                                               "ePaP", "Experiment", "Range"),
    #     cex=0.7)
    #text(c(-10, -8, -6.5, -5, -4, -2.5), l+2,
    #     c("type","", "association", "", "", "duration"), cex=0.7)
    #text(-14, l+3, "Author & Year", pos = 4, cex=0.7)
    #text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    #text(-13.5, -3.5, paste0("ne = ", l), cex = 0.6)
    #text(-13.5, -2.5, paste0("ns = ", ns), cex = 0.6)
    #dev.off()
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf biomass")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                     var.name="leaf_biomass",
                                                                     trt="aP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem biomass")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                     var.name="stem_biomass",
                                                                     trt="aP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    #tDF <- subset(inDF, Variable=="Root biomass")
    tDF <- subset(inDF, Variable%in%c("Root biomass", "Belowground biomass"))
    tDF <- subset(tDF, variance_co2_aP >= 0.001)
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                     var.name="root_biomass",
                                                                     trt="aP") 

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total plant biomass")
    tDF <- subset(tDF, variance_co2_aP >= 0.01)
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                     var.name="total_biomass",
                                                                     trt="aP") 
    
    #print(res)
    #
    #l <- length(tDF$Literature)
    #ns <- length(unique(tDF$Literature))
    #
    #### forest plot
    #pdf(paste0("output/statistics_interaction/biomass_total_test_aCO2.pdf"),
    #    height=16, width=9)
    #forest(res, slab = tDF$Literature,
    #       xlim = c(-14, 4), 
    #       ylim = c(-3.5, l+3.5),
    #       at = c(-1, 0, 1, 2), #atransf = exp,
    #       ilab = cbind(as.character(tDF$Vegetation_type),
    #                    as.character(tDF$Species),
    #                    as.character(tDF$Mycorrhizae_2), 
    #                    round(tDF$Trt_eC_by_aC,1), 
    #                    round(tDF$Trt_eP_by_aP,1),
    #                    as.character(tDF$Experiment_duration)), 
    #       ilab.xpos = c(-10, -8, -6.5, -5, -4, -2.5), cex = 0.6)
    #text(c(-10, -8, -6.5, -5, -4, -2.5, 0), l+3, c("Vegetation", 
    #                                               "Species",
    #                                               "Mycorrhizal",
    #                                               expression(paste(eCO[2], "/", aCO[2])),
    #                                               "ePaP", "Experiment", "Range"),
    #     cex=0.7)
    #text(c(-10, -8, -6.5, -5, -4, -2.5), l+2,
    #     c("type","", "association", "", "", "duration"), cex=0.7)
    #text(-14, l+3, "Author & Year", pos = 4, cex=0.7)
    #text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    #text(-13.5, -3.5, paste0("ne = ", l), cex = 0.6)
    #text(-13.5, -2.5, paste0("ns = ", ns), cex = 0.6)
    #dev.off()
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf N content")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF, digits=2, control=list(maxiter=1000,
    #                                                                     stepadj=0.1))

    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                     var.name="leaf_N_content",
                                                                     trt="aP") 
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf P content")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)

    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                     var.name="leaf_P_content",
                                                                     trt="aP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem N content")

    ### use 1/n to get the variance
    tDF$variance_co2_aP <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                     var.name="stem_N_content",
                                                                     trt="aP") 
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem P content")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                     var.name="stem_P_content",
                                                                     trt="aP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Root N content")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                     var.name="root_N_content",
                                                                     trt="aP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Root P content")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF, control=list(stepadj=0.5))
   
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                     var.name="root_P_content",
                                                                     trt="aP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total plant N content")

    ### use 1/n to get the variance
    tDF$variance_co2_aP <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                     var.name="total_N_content",
                                                                     trt="aP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total plant P content")
    
    ### random-effect model
    tDF <- tDF[tDF$variance_co2_aP > 0, ]
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                     var.name="total_P_content",
                                                                     trt="aP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf N concentration")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="leaf_N_concentration",
                                                                    trt="aP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf P concentration")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="leaf_P_concentration",
                                                                    trt="aP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Root P concentration")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="root_P_concentration",
                                                                    trt="aP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem N concentration")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="stem_N_concentration",
                                                                    trt="aP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem P concentration")
    
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="stem_P_concentration",
                                                                    trt="aP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Root N concentration")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="root_N_concentration",
                                                                    trt="aP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total plant N concentration")
    
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="total_N_concentration",
                                                                    trt="aP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total plant P concentration")
    
    ### use 1/n to get the variance
    tDF$variance_co2_aP <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="total_P_concentration",
                                                                    trt="aP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="CO2 assimilation rate")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="CO2_assimilation_rate",
                                                                    trt="aP") 
    
    #print(res)
    #
    #l <- length(tDF$Literature)
    #ns <- length(unique(tDF$Literature))
    #
    #### forest plot
    #pdf(paste0("output/statistics_interaction/photo_test_aCO2.pdf"),
    #    height=16, width=9)
    #forest(res, slab = tDF$Literature,
    #       xlim = c(-14, 4), 
    #       ylim = c(-3.5, l+3.5),
    #       at = c(-1, 0, 1, 2), #atransf = exp,
    #       ilab = cbind(as.character(tDF$Vegetation_type),
    #                    as.character(tDF$Species),
    #                    as.character(tDF$Mycorrhizae_2), 
    #                    round(tDF$Trt_eC_by_aC,1), 
    #                    round(tDF$Trt_eP_by_aP,1),
    #                    as.character(tDF$Experiment_duration)), 
    #       ilab.xpos = c(-10, -8, -6.5, -5, -4, -2.5), cex = 0.6)
    #text(c(-10, -8, -6.5, -5, -4, -2.5, 0), l+3, c("Vegetation", 
    #                                               "Species",
    #                                               "Mycorrhizal",
    #                                               expression(paste(eCO[2], "/", aCO[2])),
    #                                               "ePaP", "Experiment", "Range"),
    #     cex=0.7)
    #text(c(-10, -8, -6.5, -5, -4, -2.5), l+2,
    #     c("type","", "association", "", "", "duration"), cex=0.7)
    #text(-14, l+3, "Author & Year", pos = 4, cex=0.7)
    #text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    #text(-13.5, -3.5, paste0("ne = ", l), cex = 0.6)
    #text(-13.5, -2.5, paste0("ns = ", ns), cex = 0.6)
    #dev.off()
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stomatal conductance")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="stomatal_conductance",
                                                                    trt="aP") 
    
    
    ### change LAI to leaf area and combine it with Total leaf area
    inDF[inDF$Variable=="LAI","Variable"] <- "Leaf area"
    inDF$Variable[inDF$Variable=="Total leaf area"] <- "Leaf area"
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf area")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="leaf_area",
                                                                    trt="aP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="LMA")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="LMA",
                                                                    trt="aP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="SLA")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="SLA",
                                                                    trt="aP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total root length")
    tDF <- tDF[tDF$variance_co2_aP > 0.0001, ]
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="Root_length",
                                                                    trt="aP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Leaf NP ratio")
    
    ### use 1/n to get the variance
    tDF$variance_co2_aP <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="leaf_NP",
                                                                    trt="aP") 
    
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Stem NP ratio")
    
    ### use 1/n to get the variance
    tDF$variance_co2_aP <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="stem_NP",
                                                                    trt="aP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Root NP ratio")
    
    ### use 1/n to get the variance
    tDF$variance_co2_aP <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="root_NP",
                                                                    trt="aP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Total NP ratio")
    
    ### use 1/n to get the variance
    tDF$variance_co2_aP <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="total_NP",
                                                                    trt="aP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Plant N uptake")
    tDF <- subset(tDF, Unit %in%c("mg N mg-1 of nodule", "mg g-1 root", "mg N g-1 root"))
    tDF$variance_co2_aP <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="N_uptake",
                                                                    trt="aP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="Plant P uptake")
    tDF <- subset(tDF, Unit %in%c("ug P root-1", "mg P g-1 root", "ug P mg root-1"))
    tDF$variance_co2_aP <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="P_uptake",
                                                                    trt="aP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="WUE")
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="WUE",
                                                                    trt="aP") 
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="NUE")
    
    ### use 1/n to get the variance
    tDF$variance_co2_aP <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="NUE",
                                                                    trt="aP") 
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(inDF, Variable=="PUE")
    
    
    ### use 1/n to get the variance
    tDF$variance_co2_aP <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_co2_aP, variance_co2_aP, random = ~1 | random_factor, data = tDF)
    
    res <- rma.mv(log_co2_aP, variance_co2_aP, 
                  random = ~1 | random_factor, data = tDF)
    
    ### assign values and make forest plot
    sumDF <- assign_CO2_effect_model_stats_and_forest_plot_advanced(tDF, sumDF, res, 
                                                                    var.name="PUE",
                                                                    trt="aP") 

    return(sumDF)
    
}
