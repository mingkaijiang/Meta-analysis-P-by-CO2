metafor_p_statistics_morphology_100_aCO2 <- function(reDF, sumDF2) {
    
    ### create directory
    if(!dir.exists("output/statistics_p_morphology_100_aCO2")) {
        dir.create("output/statistics_p_morphology_100_aCO2", showWarnings = FALSE)
    }
    
    ### change LAI to leaf area and combine it with Total leaf area
    reDF[reDF$Variable=="LAI","Variable"] <- "Leaf area"
    reDF$Variable[reDF$Variable=="Total leaf area"] <- "Leaf area"
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf area")
    
    ### random-effect model
    res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF2$P_effect[sumDF2$variable=="leaf_area"&sumDF2$CO2_treatment=="aCO2"] <- res$b
    sumDF2$se[sumDF2$variable=="leaf_area"&sumDF2$CO2_treatment=="aCO2"] <- res$se
    sumDF2$p_value[sumDF2$variable=="leaf_area"&sumDF2$CO2_treatment=="aCO2"] <- res$pval
    sumDF2$ns[sumDF2$variable=="leaf_area"&sumDF2$CO2_treatment=="aCO2"] <- ns
    sumDF2$ne[sumDF2$variable=="leaf_area"&sumDF2$CO2_treatment=="aCO2"] <- l
    sumDF2$ci_lb[sumDF2$variable=="leaf_area"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.lb
    sumDF2$ci_ub[sumDF2$variable=="leaf_area"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_p_morphology_100_aCO2/leaf_area_morphology_response_ratio_random_effect_model.pdf",
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
    tDF <- subset(reDF, Variable=="LMA")
    
    ### random-effect model
    res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### confidence interval
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF2$P_effect[sumDF2$variable=="LMA"&sumDF2$CO2_treatment=="aCO2"] <- res$b
    sumDF2$se[sumDF2$variable=="LMA"&sumDF2$CO2_treatment=="aCO2"] <- res$se
    sumDF2$p_value[sumDF2$variable=="LMA"&sumDF2$CO2_treatment=="aCO2"] <- res$pval
    sumDF2$ns[sumDF2$variable=="LMA"&sumDF2$CO2_treatment=="aCO2"] <- ns
    sumDF2$ne[sumDF2$variable=="LMA"&sumDF2$CO2_treatment=="aCO2"] <- l
    sumDF2$ci_lb[sumDF2$variable=="LMA"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.lb
    sumDF2$ci_ub[sumDF2$variable=="LMA"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_p_morphology_100_aCO2/LMA_morphology_response_ratio_random_effect_model.pdf",
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
    tDF <- subset(reDF, Variable=="SLA")
    
    ### random-effect model
    res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF2$P_effect[sumDF2$variable=="SLA"&sumDF2$CO2_treatment=="aCO2"] <- res$b
    sumDF2$se[sumDF2$variable=="SLA"&sumDF2$CO2_treatment=="aCO2"] <- res$se
    sumDF2$p_value[sumDF2$variable=="SLA"&sumDF2$CO2_treatment=="aCO2"] <- res$pval
    sumDF2$ns[sumDF2$variable=="SLA"&sumDF2$CO2_treatment=="aCO2"] <- ns
    sumDF2$ne[sumDF2$variable=="SLA"&sumDF2$CO2_treatment=="aCO2"] <- l
    sumDF2$ci_lb[sumDF2$variable=="SLA"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.lb
    sumDF2$ci_ub[sumDF2$variable=="SLA"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_p_morphology_100_aCO2/SLA_morphology_response_ratio_random_effect_model.pdf",
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
    tDF <- subset(reDF, Variable=="Total root length")
    
    ### random-effect model
    res <- rma(log_P_aCO2, variance_p_aCO2, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF2$P_effect[sumDF2$variable=="Root_length"&sumDF2$CO2_treatment=="aCO2"] <- res$b
    sumDF2$se[sumDF2$variable=="Root_length"&sumDF2$CO2_treatment=="aCO2"] <- res$se
    sumDF2$p_value[sumDF2$variable=="Root_length"&sumDF2$CO2_treatment=="aCO2"] <- res$pval
    sumDF2$ns[sumDF2$variable=="Root_length"&sumDF2$CO2_treatment=="aCO2"] <- ns
    sumDF2$ne[sumDF2$variable=="Root_length"&sumDF2$CO2_treatment=="aCO2"] <- l
    sumDF2$ci_lb[sumDF2$variable=="Root_length"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.lb
    sumDF2$ci_ub[sumDF2$variable=="Root_length"&sumDF2$CO2_treatment=="aCO2"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_p_morphology_100_aCO2/root_length_morphology_response_ratio_random_effect_model.pdf",
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
    
    return(sumDF2)
    
    
}