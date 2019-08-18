metafor_statistics_gas_exchange_100 <- function(reDF, intDF) {
    
    ### create directory
    if(!dir.exists("output/statistics_gas_exchange_100")) {
        dir.create("output/statistics_gas_exchange_100", showWarnings = FALSE)
    }
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="CO2 assimilation rate")
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    #print(res)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="CO2_assimilation_rate"] <- res$b
    intDF$se[intDF$variable=="CO2_assimilation_rate"] <- res$se
    intDF$p_value[intDF$variable=="CO2_assimilation_rate"] <- res$pval
    intDF$ns[intDF$variable=="CO2_assimilation_rate"] <- ns
    intDF$ne[intDF$variable=="CO2_assimilation_rate"] <- l
    intDF$ci_lb[intDF$variable=="CO2_assimilation_rate"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="CO2_assimilation_rate"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_gas_exchange_100/co2_assimilation_rate_gas_exchange_response_ratio_random_effect_model.pdf",
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
    tDF <- subset(reDF, Variable=="Stomatal conductance")
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF)
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05)) ## low P high P

    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="stomatal_conductance"] <- res$b
    intDF$se[intDF$variable=="stomatal_conductance"] <- res$se
    intDF$p_value[intDF$variable=="stomatal_conductance"] <- res$pval
    intDF$ns[intDF$variable=="stomatal_conductance"] <- ns
    intDF$ne[intDF$variable=="stomatal_conductance"] <- l
    intDF$ci_lb[intDF$variable=="stomatal_conductance"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="stomatal_conductance"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_gas_exchange_100/stomatal_conductance_gas_exchange_response_ratio_random_effect_model.pdf",
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
    
    return(intDF)
    
}