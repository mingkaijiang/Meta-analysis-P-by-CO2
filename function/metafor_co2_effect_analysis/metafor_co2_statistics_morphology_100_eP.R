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
    res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="leaf_area"&sumDF$P_treatment=="eP"] <- res$b
    sumDF$se[sumDF$variable=="leaf_area"&sumDF$P_treatment=="eP"] <- res$se
    sumDF$p_value[sumDF$variable=="leaf_area"&sumDF$P_treatment=="eP"] <- res$pval
    sumDF$ns[sumDF$variable=="leaf_area"&sumDF$P_treatment=="eP"] <- ns
    sumDF$ne[sumDF$variable=="leaf_area"&sumDF$P_treatment=="eP"] <- l
    sumDF$ci_lb[sumDF$variable=="leaf_area"&sumDF$P_treatment=="eP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="leaf_area"&sumDF$P_treatment=="eP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_co2_morphology_100_eP/leaf_area_morphology_response_ratio_random_effect_model.pdf",
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
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    ### confidence interval
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="LMA"&sumDF$P_treatment=="eP"] <- res$b
    sumDF$se[sumDF$variable=="LMA"&sumDF$P_treatment=="eP"] <- res$se
    sumDF$p_value[sumDF$variable=="LMA"&sumDF$P_treatment=="eP"] <- res$pval
    sumDF$ns[sumDF$variable=="LMA"&sumDF$P_treatment=="eP"] <- ns
    sumDF$ne[sumDF$variable=="LMA"&sumDF$P_treatment=="eP"] <- l
    sumDF$ci_lb[sumDF$variable=="LMA"&sumDF$P_treatment=="eP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="LMA"&sumDF$P_treatment=="eP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_co2_morphology_100_eP/LMA_morphology_response_ratio_random_effect_model.pdf",
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
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="SLA"&sumDF$P_treatment=="eP"] <- res$b
    sumDF$se[sumDF$variable=="SLA"&sumDF$P_treatment=="eP"] <- res$se
    sumDF$p_value[sumDF$variable=="SLA"&sumDF$P_treatment=="eP"] <- res$pval
    sumDF$ns[sumDF$variable=="SLA"&sumDF$P_treatment=="eP"] <- ns
    sumDF$ne[sumDF$variable=="SLA"&sumDF$P_treatment=="eP"] <- l
    sumDF$ci_lb[sumDF$variable=="SLA"&sumDF$P_treatment=="eP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="SLA"&sumDF$P_treatment=="eP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_co2_morphology_100_eP/SLA_morphology_response_ratio_random_effect_model.pdf",
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
    #res <- rma(log_co2_eP, variance_co2_eP, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_co2_eP, variance_co2_eP, random = ~1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    sumDF$CO2_effect[sumDF$variable=="Root_length"&sumDF$P_treatment=="eP"] <- res$b
    sumDF$se[sumDF$variable=="Root_length"&sumDF$P_treatment=="eP"] <- res$se
    sumDF$p_value[sumDF$variable=="Root_length"&sumDF$P_treatment=="eP"] <- res$pval
    sumDF$ns[sumDF$variable=="Root_length"&sumDF$P_treatment=="eP"] <- ns
    sumDF$ne[sumDF$variable=="Root_length"&sumDF$P_treatment=="eP"] <- l
    sumDF$ci_lb[sumDF$variable=="Root_length"&sumDF$P_treatment=="eP"] <- res$ci.lb
    sumDF$ci_ub[sumDF$variable=="Root_length"&sumDF$P_treatment=="eP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_co2_morphology_100_eP/root_length_morphology_response_ratio_random_effect_model.pdf",
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
    

    return(sumDF)
    
}