metafor_p_statistics_biomass_100_aCO2 <- function(reDF, sumDF2) {
    
    ### create directory
    if(!dir.exists("output/statistics_p_biomass_100_aCO2")) {
        dir.create("output/statistics_p_biomass_100_aCO2", showWarnings = FALSE)
    }
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable%in%c("Aboveground biomass"))
    
    tDF <- subset(tDF, variance_p_aCO2 >= 0.01)
    
    
    ### random-effect model
    res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    #confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF2$P_effect[sumDF2$variable=="aboveground_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$b
    sumDF2$se[sumDF2$variable=="aboveground_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$se
    sumDF2$p_value[sumDF2$variable=="aboveground_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$pval
    sumDF2$ns[sumDF2$variable=="aboveground_biomass"&sumDF2$CO2_treatment=="aCO2"] <- ns
    sumDF2$ne[sumDF2$variable=="aboveground_biomass"&sumDF2$CO2_treatment=="aCO2"] <- l
    sumDF2$ci_lb[sumDF2$variable=="aboveground_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.lb
    sumDF2$ci_ub[sumDF2$variable=="aboveground_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_p_biomass_100_aCO2/aboveground_biomass_response_ratio_random_effect_model.pdf",
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
    res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF2$P_effect[sumDF2$variable=="leaf_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$b
    sumDF2$se[sumDF2$variable=="leaf_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$se
    sumDF2$p_value[sumDF2$variable=="leaf_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$pval
    sumDF2$ns[sumDF2$variable=="leaf_biomass"&sumDF2$CO2_treatment=="aCO2"] <- ns
    sumDF2$ne[sumDF2$variable=="leaf_biomass"&sumDF2$CO2_treatment=="aCO2"] <- l
    sumDF2$ci_lb[sumDF2$variable=="leaf_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.lb
    sumDF2$ci_ub[sumDF2$variable=="leaf_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_p_biomass_100_aCO2/leaf_biomass_response_ratio_random_effect_model.pdf",
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
    res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### confidence interval
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF2$P_effect[sumDF2$variable=="stem_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$b
    sumDF2$se[sumDF2$variable=="stem_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$se
    sumDF2$p_value[sumDF2$variable=="stem_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$pval
    sumDF2$ns[sumDF2$variable=="stem_biomass"&sumDF2$CO2_treatment=="aCO2"] <- ns
    sumDF2$ne[sumDF2$variable=="stem_biomass"&sumDF2$CO2_treatment=="aCO2"] <- l
    sumDF2$ci_lb[sumDF2$variable=="stem_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.lb
    sumDF2$ci_ub[sumDF2$variable=="stem_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_p_biomass_100_aCO2/stem_biomass_response_ratio_random_effect_model.pdf",
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
    tDF <- subset(tDF, variance_p_aCO2 >= 0.01)
    ### random-effect model
    res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF2$P_effect[sumDF2$variable=="root_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$b
    sumDF2$se[sumDF2$variable=="root_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$se
    sumDF2$p_value[sumDF2$variable=="root_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$pval
    sumDF2$ns[sumDF2$variable=="root_biomass"&sumDF2$CO2_treatment=="aCO2"] <- ns
    sumDF2$ne[sumDF2$variable=="root_biomass"&sumDF2$CO2_treatment=="aCO2"] <- l
    sumDF2$ci_lb[sumDF2$variable=="root_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.lb
    sumDF2$ci_ub[sumDF2$variable=="root_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_p_biomass_100_aCO2/root_biomass_response_ratio_random_effect_model.pdf",
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
    
    ### random-effect model
    tDF <- subset(tDF, variance_p_aCO2 >= 0.01)
    res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    #confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF2$P_effect[sumDF2$variable=="total_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$b
    sumDF2$se[sumDF2$variable=="total_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$se
    sumDF2$p_value[sumDF2$variable=="total_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$pval
    sumDF2$ns[sumDF2$variable=="total_biomass"&sumDF2$CO2_treatment=="aCO2"] <- ns
    sumDF2$ne[sumDF2$variable=="total_biomass"&sumDF2$CO2_treatment=="aCO2"] <- l
    sumDF2$ci_lb[sumDF2$variable=="total_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.lb
    sumDF2$ci_ub[sumDF2$variable=="total_biomass"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_p_biomass_100_aCO2/total_biomass_response_ratio_random_effect_model.pdf",
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
    res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)

    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    #confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF2$P_effect[sumDF2$variable=="leaf_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$b
    sumDF2$se[sumDF2$variable=="leaf_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$se
    sumDF2$p_value[sumDF2$variable=="leaf_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$pval
    sumDF2$ns[sumDF2$variable=="leaf_N_content"&sumDF2$CO2_treatment=="aCO2"] <- ns
    sumDF2$ne[sumDF2$variable=="leaf_N_content"&sumDF2$CO2_treatment=="aCO2"] <- l
    sumDF2$ci_lb[sumDF2$variable=="leaf_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.lb
    sumDF2$ci_ub[sumDF2$variable=="leaf_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_p_biomass_100_aCO2/leaf_N_content_response_ratio_random_effect_model.pdf",
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
    res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)#, control=list(stepadj=0.5))

    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    #confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF2$P_effect[sumDF2$variable=="leaf_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$b
    sumDF2$se[sumDF2$variable=="leaf_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$se
    sumDF2$p_value[sumDF2$variable=="leaf_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$pval
    sumDF2$ns[sumDF2$variable=="leaf_P_content"&sumDF2$CO2_treatment=="aCO2"] <- ns
    sumDF2$ne[sumDF2$variable=="leaf_P_content"&sumDF2$CO2_treatment=="aCO2"] <- l
    sumDF2$ci_lb[sumDF2$variable=="leaf_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.lb
    sumDF2$ci_ub[sumDF2$variable=="leaf_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_p_biomass_100_aCO2/leaf_P_content_response_ratio_random_effect_model.pdf",
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
    tDF$variance_p_aCO2 <- 1/tDF$Sample.Size
    
    ### random-effect model
    res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF, control=list(stepadj=0.05))
    
    sumDF2$P_effect[sumDF2$variable=="stem_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$b
    sumDF2$se[sumDF2$variable=="stem_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$se
    sumDF2$p_value[sumDF2$variable=="stem_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$pval
    sumDF2$ns[sumDF2$variable=="stem_N_content"&sumDF2$CO2_treatment=="aCO2"] <- ns
    sumDF2$ne[sumDF2$variable=="stem_N_content"&sumDF2$CO2_treatment=="aCO2"] <- l
    sumDF2$ci_lb[sumDF2$variable=="stem_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.lb
    sumDF2$ci_ub[sumDF2$variable=="stem_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_p_biomass_100_aCO2/stem_N_content_response_ratio_random_effect_model.pdf",
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
    res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)#, control=list(stepadj=0.05))
    
    sumDF2$P_effect[sumDF2$variable=="stem_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$b
    sumDF2$se[sumDF2$variable=="stem_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$se
    sumDF2$p_value[sumDF2$variable=="stem_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$pval
    sumDF2$ns[sumDF2$variable=="stem_P_content"&sumDF2$CO2_treatment=="aCO2"] <- ns
    sumDF2$ne[sumDF2$variable=="stem_P_content"&sumDF2$CO2_treatment=="aCO2"] <- l
    sumDF2$ci_lb[sumDF2$variable=="stem_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.lb
    sumDF2$ci_ub[sumDF2$variable=="stem_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_p_biomass_100_aCO2/stem_P_content_response_ratio_random_effect_model.pdf",
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
    
    ### use 1/n to get the variance
    tDF$variance_p_aCO2 <- 1/tDF$Sample.Size
    
    ### random-effect model
    res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF, control=list(stepadj=0.05))
    
    sumDF2$P_effect[sumDF2$variable=="root_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$b
    sumDF2$se[sumDF2$variable=="root_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$se
    sumDF2$p_value[sumDF2$variable=="root_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$pval
    sumDF2$ns[sumDF2$variable=="root_N_content"&sumDF2$CO2_treatment=="aCO2"] <- ns
    sumDF2$ne[sumDF2$variable=="root_N_content"&sumDF2$CO2_treatment=="aCO2"] <- l
    sumDF2$ci_lb[sumDF2$variable=="root_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.lb
    sumDF2$ci_ub[sumDF2$variable=="root_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_p_biomass_100_aCO2/root_N_content_response_ratio_random_effect_model.pdf",
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
    res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)# control=list(stepadj=0.5))
    
    sumDF2$P_effect[sumDF2$variable=="root_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$b
    sumDF2$se[sumDF2$variable=="root_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$se
    sumDF2$p_value[sumDF2$variable=="root_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$pval
    sumDF2$ns[sumDF2$variable=="root_P_content"&sumDF2$CO2_treatment=="aCO2"] <- ns
    sumDF2$ne[sumDF2$variable=="root_P_content"&sumDF2$CO2_treatment=="aCO2"] <- l
    sumDF2$ci_lb[sumDF2$variable=="root_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.lb
    sumDF2$ci_ub[sumDF2$variable=="root_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_p_biomass_100_aCO2/root_P_content_response_ratio_random_effect_model.pdf",
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
    tDF$variance_p_aCO2 <- 1/tDF$Sample.Size
    
    ### random-effect model
    res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF, control=list(stepadj=0.05))
    
    sumDF2$P_effect[sumDF2$variable=="total_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$b
    sumDF2$se[sumDF2$variable=="total_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$se
    sumDF2$p_value[sumDF2$variable=="total_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$pval
    sumDF2$ns[sumDF2$variable=="total_N_content"&sumDF2$CO2_treatment=="aCO2"] <- ns
    sumDF2$ne[sumDF2$variable=="total_N_content"&sumDF2$CO2_treatment=="aCO2"] <- l
    sumDF2$ci_lb[sumDF2$variable=="total_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.lb
    sumDF2$ci_ub[sumDF2$variable=="total_N_content"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_p_biomass_100_aCO2/total_N_content_response_ratio_random_effect_model.pdf",
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
    tDF <- tDF[tDF$variance_p_aCO2 > 0, ]
    
    res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)#, control=list(stepadj=0.5))
    
    sumDF2$P_effect[sumDF2$variable=="total_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$b
    sumDF2$se[sumDF2$variable=="total_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$se
    sumDF2$p_value[sumDF2$variable=="total_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$pval
    sumDF2$ns[sumDF2$variable=="total_P_content"&sumDF2$CO2_treatment=="aCO2"] <- ns
    sumDF2$ne[sumDF2$variable=="total_P_content"&sumDF2$CO2_treatment=="aCO2"] <- l
    sumDF2$ci_lb[sumDF2$variable=="total_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.lb
    sumDF2$ci_ub[sumDF2$variable=="total_P_content"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_p_biomass_100_aCO2/total_P_content_response_ratio_random_effect_model.pdf",
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


    return(sumDF2)
}
