metafor_co2_statistics_biomass_100_aP <- function(reDF, sumDF) {
    
    ### create directory
    if(!dir.exists("output/statistics_co2_biomass_100_aP")) {
        dir.create("output/statistics_co2_biomass_100_aP", showWarnings = FALSE)
    }
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable%in%c("Aboveground biomass"))
    
    tDF <- subset(tDF, variance_co2_aP >= 0.01)
    
    ### random-effect model
    res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    #confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="aboveground_biomass"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="aboveground_biomass"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="aboveground_biomass"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="aboveground_biomass"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="aboveground_biomass"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="aboveground_biomass"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="aboveground_biomass"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_co2_biomass_100_aP/aboveground_biomass_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-12, 6), 
           ylim = c(-3.5, l+3.5),
           at = log(c(0.082085, 0.3678794, 1, 2.718282, 12.18249)), #atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-8, -6.5, -4.5, -3), cex = 0.6)
    text(c(-8, -6.5, -4.5, -3, 0), l+3, c("Vegetation", 
                                          expression(paste(eCO[2], "/", aCO[2])),
                                          "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-8, -6.5, -4.5, -3), l+2,
         c("type","", "", "duration"), cex=0.7)
    text(-12, l+3, "Author & Year", pos = 4, cex=0.7)
    text(6, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    text(-11.5, -3.0, paste0("ne = ", l), cex = 0.6)
    text(-11.5, -2.0, paste0("ns = ", ns), cex = 0.6)
    dev.off()
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf biomass")
    
    ### random-effect model
    res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="leaf_biomass"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="leaf_biomass"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="leaf_biomass"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="leaf_biomass"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="leaf_biomass"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="leaf_biomass"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="leaf_biomass"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_co2_biomass_100_aP/leaf_biomass_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-12, 6), 
           ylim = c(-3.5, l+3.5),
           at = log(c(0.082085, 0.3678794, 1, 2.718282, 12.18249)), #atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-8, -6.5, -4.5, -3), cex = 0.6)
    text(c(-8, -6.5, -4.5, -3, 0), l+3, c("Vegetation", 
                                       expression(paste(eCO[2], "/", aCO[2])),
                                       "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-8, -6.5, -4.5, -3), l+2,
         c("type","", "", "duration"), cex=0.7)
    text(-12, l+3, "Author & Year", pos = 4, cex=0.7)
    text(6, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    text(-11.5, -3.0, paste0("ne = ", l), cex = 0.6)
    text(-11.5, -2.0, paste0("ns = ", ns), cex = 0.6)
    dev.off()
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stem biomass")
    
    ### random-effect model
    res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### confidence interval
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="stem_biomass"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="stem_biomass"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="stem_biomass"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="stem_biomass"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="stem_biomass"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="stem_biomass"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="stem_biomass"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_co2_biomass_100_aP/stem_biomass_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-12, 4), 
           ylim = c(-3.5, l+3.5),
           at = log(c(0.3678794, 1, 2.718282, 7.389056)), #atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-8, -6.5, -4.5, -3), cex = 0.6)
    text(c(-8, -6.5, -4.5, -3, 0), l+3, c("Vegetation", 
                                          expression(paste(eCO[2], "/", aCO[2])),
                                          "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-8, -6.5, -4.5, -3), l+2,
         c("type","", "", "duration"), cex=0.7)
    text(-12, l+3, "Author & Year", pos = 4, cex=0.7)
    text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    text(-11.5, -3.0, paste0("ne = ", l), cex = 0.6)
    text(-11.5, -2.0, paste0("ns = ", ns), cex = 0.6)
    dev.off()
    
    
    ####################### subset the dataframe for the right variable ##############################
    #tDF <- subset(reDF, Variable=="Root biomass")
    tDF <- subset(reDF, Variable%in%c("Root biomass", "Belowground biomass"))
    tDF <- subset(tDF, variance_co2_aP >= 0.01)
    
    ### random-effect model
    res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="root_biomass"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="root_biomass"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="root_biomass"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="root_biomass"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="root_biomass"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="root_biomass"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="root_biomass"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_co2_biomass_100_aP/root_biomass_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-12, 4), 
           ylim = c(-3.5, l+3.5),
           at = log(c(0.1353353, 0.3678794, 1, 2.718282, 7.389056)), #atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-8, -6.5, -4.5, -3), cex = 0.6)
    text(c(-8, -6.5, -4.5, -3, 0), l+3, c("Vegetation", 
                                          expression(paste(eCO[2], "/", aCO[2])),
                                          "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-8, -6.5, -4.5, -3), l+2,
         c("type","", "", "duration"), cex=0.7)
    text(-12, l+3, "Author & Year", pos = 4, cex=0.7)
    text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    text(-11.5, -3.0, paste0("ne = ", l), cex = 0.6)
    text(-11.5, -2.0, paste0("ns = ", ns), cex = 0.6)
    dev.off()
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total plant biomass")
    tDF <- subset(tDF, variance_co2_aP >= 0.01)
    
    ### random-effect model
    res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    #confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="total_biomass"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="total_biomass"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="total_biomass"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="total_biomass"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="total_biomass"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="total_biomass"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="total_biomass"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_co2_biomass_100_aP/total_biomass_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-12, 4), 
           ylim = c(-3.5, l+3.5),
           at = log(c(0.1353353, 0.3678794, 1, 2.718282, 7.389056)), #atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-8, -6.5, -4.5, -3), cex = 0.6)
    text(c(-8, -6.5, -4.5, -3, 0), l+3, c("Vegetation", 
                                          expression(paste(eCO[2], "/", aCO[2])),
                                          "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-8, -6.5, -4.5, -3), l+2,
         c("type","", "", "duration"), cex=0.7)
    text(-12, l+3, "Author & Year", pos = 4, cex=0.7)
    text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    text(-11.5, -3.0, paste0("ne = ", l), cex = 0.6)
    text(-11.5, -2.0, paste0("ns = ", ns), cex = 0.6)
    dev.off()
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf N content")
    
    ### random-effect model
    res <- rma(log_co2_aP, variance_co2_aP, data = tDF, digits=2, control=list(maxiter=1000,
                                                                         stepadj=0.1))

    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    #confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="leaf_N_content"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="leaf_N_content"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="leaf_N_content"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="leaf_N_content"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="leaf_N_content"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="leaf_N_content"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="leaf_N_content"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_co2_biomass_100_aP/leaf_N_content_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-12, 4), 
           ylim = c(-3.5, l+3.5),
           at = log(c(0.1353353, 0.3678794, 1, 2.718282, 7.389056)), #atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-8, -6.5, -4.5, -3), cex = 0.6)
    text(c(-8, -6.5, -4.5, -3, 0), l+3, c("Vegetation", 
                                          expression(paste(eCO[2], "/", aCO[2])),
                                          "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-8, -6.5, -4.5, -3), l+2,
         c("type","", "", "duration"), cex=0.7)
    text(-12, l+3, "Author & Year", pos = 4, cex=0.7)
    text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    text(-11.5, -3.0, paste0("ne = ", l), cex = 0.6)
    text(-11.5, -2.0, paste0("ns = ", ns), cex = 0.6)
    dev.off()
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf P content")
    
    ### random-effect model
    res <- rma(log_co2_aP, variance_co2_aP, data = tDF)#, control=list(stepadj=0.5))

    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    #confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="leaf_P_content"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="leaf_P_content"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="leaf_P_content"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="leaf_P_content"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="leaf_P_content"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="leaf_P_content"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="leaf_P_content"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_co2_biomass_100_aP/leaf_P_content_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-12, 4), 
           ylim = c(-3.5, l+3.5),
           at = log(c(0.1353353, 0.3678794, 1, 2.718282, 7.389056)), #atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-8, -6.5, -4.5, -3), cex = 0.6)
    text(c(-8, -6.5, -4.5, -3, 0), l+3, c("Vegetation", 
                                          expression(paste(eCO[2], "/", aCO[2])),
                                          "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-8, -6.5, -4.5, -3), l+2,
         c("type","", "", "duration"), cex=0.7)
    text(-12, l+3, "Author & Year", pos = 4, cex=0.7)
    text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    text(-11.5, -3.0, paste0("ne = ", l), cex = 0.6)
    text(-11.5, -2.0, paste0("ns = ", ns), cex = 0.6)
    dev.off()
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stem N content")
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    ### use 1/n to get the variance
    tDF$variance_co2_aP <- 1/tDF$Sample.Size
    
    ### random-effect model
    res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    sumDF$CO2_effect[sumDF$variable=="stem_N_content"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="stem_N_content"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="stem_N_content"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="stem_N_content"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="stem_N_content"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="stem_N_content"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="stem_N_content"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_co2_biomass_100_aP/stem_N_content_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-12, 4), 
           ylim = c(-3.5, l+3.5),
           at = log(c(0.1353353, 0.3678794, 1, 2.718282, 7.389056)), #atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-8, -6.5, -4.5, -3), cex = 0.6)
    text(c(-8, -6.5, -4.5, -3, 0), l+3, c("Vegetation", 
                                          expression(paste(eCO[2], "/", aCO[2])),
                                          "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-8, -6.5, -4.5, -3), l+2,
         c("type","", "", "duration"), cex=0.7)
    text(-12, l+3, "Author & Year", pos = 4, cex=0.7)
    text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    text(-11.5, -3.0, paste0("ne = ", l), cex = 0.6)
    text(-11.5, -2.0, paste0("ns = ", ns), cex = 0.6)
    dev.off()
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Stem P content")
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    ### random-effect model
    res <- rma(log_co2_aP, variance_co2_aP, data = tDF)#, control=list(stepadj=0.05))
    
    sumDF$CO2_effect[sumDF$variable=="stem_P_content"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="stem_P_content"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="stem_P_content"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="stem_P_content"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="stem_P_content"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="stem_P_content"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="stem_P_content"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_co2_biomass_100_aP/stem_P_content_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-12, 4), 
           ylim = c(-3.5, l+3.5),
           at = log(c(0.1353353, 0.3678794, 1, 2.718282, 7.389056)), #atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-8, -6.5, -4.5, -3), cex = 0.6)
    text(c(-8, -6.5, -4.5, -3, 0), l+3, c("Vegetation", 
                                          expression(paste(eCO[2], "/", aCO[2])),
                                          "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-8, -6.5, -4.5, -3), l+2,
         c("type","", "", "duration"), cex=0.7)
    text(-12, l+3, "Author & Year", pos = 4, cex=0.7)
    text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    text(-11.5, -3.0, paste0("ne = ", l), cex = 0.6)
    text(-11.5, -2.0, paste0("ns = ", ns), cex = 0.6)
    dev.off()
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root N content")
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    ### random-effect model
    res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    sumDF$CO2_effect[sumDF$variable=="root_N_content"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="root_N_content"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="root_N_content"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="root_N_content"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="root_N_content"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="root_N_content"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="root_N_content"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_co2_biomass_100_aP/root_N_content_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-12, 4), 
           ylim = c(-3.5, l+3.5),
           at = log(c(0.1353353, 0.3678794, 1, 2.718282, 7.389056)), #atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-8, -6.5, -4.5, -3), cex = 0.6)
    text(c(-8, -6.5, -4.5, -3, 0), l+3, c("Vegetation", 
                                          expression(paste(eCO[2], "/", aCO[2])),
                                          "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-8, -6.5, -4.5, -3), l+2,
         c("type","", "", "duration"), cex=0.7)
    text(-12, l+3, "Author & Year", pos = 4, cex=0.7)
    text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    text(-11.5, -3.0, paste0("ne = ", l), cex = 0.6)
    text(-11.5, -2.0, paste0("ns = ", ns), cex = 0.6)
    dev.off()
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root P content")
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    ### random-effect model
    res <- rma(log_co2_aP, variance_co2_aP, data = tDF, control=list(stepadj=0.5))
    
    sumDF$CO2_effect[sumDF$variable=="root_P_content"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="root_P_content"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="root_P_content"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="root_P_content"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="root_P_content"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="root_P_content"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="root_P_content"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_co2_biomass_100_aP/root_P_content_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-12, 4), 
           ylim = c(-3.5, l+3.5),
           at = log(c(0.1353353, 0.3678794, 1, 2.718282, 7.389056)), #atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-8, -6.5, -4.5, -3), cex = 0.6)
    text(c(-8, -6.5, -4.5, -3, 0), l+3, c("Vegetation", 
                                          expression(paste(eCO[2], "/", aCO[2])),
                                          "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-8, -6.5, -4.5, -3), l+2,
         c("type","", "", "duration"), cex=0.7)
    text(-12, l+3, "Author & Year", pos = 4, cex=0.7)
    text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    text(-11.5, -3.0, paste0("ne = ", l), cex = 0.6)
    text(-11.5, -2.0, paste0("ns = ", ns), cex = 0.6)
    dev.off()
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total plant N content")
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    

    ### use 1/n to get the variance
    tDF$variance_co2_aP <- 1/tDF$Sample.Size
    
    ### random-effect model
    res <- rma(log_co2_aP, variance_co2_aP, data = tDF)
    
    sumDF$CO2_effect[sumDF$variable=="total_N_content"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="total_N_content"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="total_N_content"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="total_N_content"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="total_N_content"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="total_N_content"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="total_N_content"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_co2_biomass_100_aP/total_N_content_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-12, 4), 
           ylim = c(-3.5, l+3.5),
           at = log(c(0.1353353, 0.3678794, 1, 2.718282, 7.389056)), #atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-8, -6.5, -4.5, -3), cex = 0.6)
    text(c(-8, -6.5, -4.5, -3, 0), l+3, c("Vegetation", 
                                          expression(paste(eCO[2], "/", aCO[2])),
                                          "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-8, -6.5, -4.5, -3), l+2,
         c("type","", "", "duration"), cex=0.7)
    text(-12, l+3, "Author & Year", pos = 4, cex=0.7)
    text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    text(-11.5, -3.0, paste0("ne = ", l), cex = 0.6)
    text(-11.5, -2.0, paste0("ns = ", ns), cex = 0.6)
    dev.off()
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Total plant P content")
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    ### random-effect model
    tDF <- tDF[tDF$variance_co2_aP > 0, ]
    
    res <- rma(log_co2_aP, variance_co2_aP, data = tDF)#, control=list(stepadj=0.5))
    
    sumDF$CO2_effect[sumDF$variable=="total_P_content"&sumDF$P_treatment=="aP"] <- res$b
    sumDF$se[sumDF$variable=="total_P_content"&sumDF$P_treatment=="aP"] <- res$se
    sumDF$p_value[sumDF$variable=="total_P_content"&sumDF$P_treatment=="aP"] <- res$pval
    sumDF$ns[sumDF$variable=="total_P_content"&sumDF$P_treatment=="aP"] <- ns
    sumDF$ne[sumDF$variable=="total_P_content"&sumDF$P_treatment=="aP"] <- l
    sumDF$ci_lb[sumDF$variable=="total_P_content"&sumDF$P_treatment=="aP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="total_P_content"&sumDF$P_treatment=="aP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_co2_biomass_100_aP/total_P_content_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-12, 6), 
           ylim = c(-3.5, l+3.5),
           at = log(c(0.1353353, 0.3678794, 1, 2.718282, 7.389056)), #atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-9, -7, -5.5, -4), cex = 0.6)
    text(c(-9, -7, -5.5, -4, 0), l+3, c("Vegetation", 
                                          expression(paste(eCO[2], "/", aCO[2])),
                                          "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-9, -7, -5.5, -4), l+2,
         c("type","", "", "duration"), cex=0.7)
    text(-12, l+3, "Author & Year", pos = 4, cex=0.7)
    text(6, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    text(-11.5, -3.0, paste0("ne = ", l), cex = 0.6)
    text(-11.5, -2.0, paste0("ns = ", ns), cex = 0.6)
    dev.off()

    return(sumDF)
    
}
