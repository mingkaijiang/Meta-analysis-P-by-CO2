metafor_statistics_nutrient_uptake_100 <- function(reDF, intDF) {
    
    ### create directory
    if(!dir.exists("output/statistics_nutrient_uptake_100")) {
        dir.create("output/statistics_nutrient_uptake_100", showWarnings = FALSE)
    }
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Plant N uptake")
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="N_uptake"] <- res$b
    intDF$se[intDF$variable=="N_uptake"] <- res$se
    intDF$p_value[intDF$variable=="N_uptake"] <- res$pval
    intDF$ns[intDF$variable=="N_uptake"] <- ns
    intDF$ne[intDF$variable=="N_uptake"] <- l
    
    ### forest plot
    pdf("output/statistics_nutrient_uptake_100/plant_N_uptake_nutrient_uptake_response_ratio_random_effect_model.pdf",
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
    tDF <- subset(reDF, Variable=="Plant P uptake")
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF,control=list(stepadj=0.5))
    
    ### confidence interval
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="P_uptake"] <- res$b
    intDF$se[intDF$variable=="P_uptake"] <- res$se
    intDF$p_value[intDF$variable=="P_uptake"] <- res$pval
    intDF$ns[intDF$variable=="P_uptake"] <- ns
    intDF$ne[intDF$variable=="P_uptake"] <- l
    
    ### forest plot
    pdf("output/statistics_nutrient_uptake_100/plant_P_uptake_nutrient_uptake_response_ratio_random_effect_model.pdf",
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
    
    
    return(intDF)
    
}