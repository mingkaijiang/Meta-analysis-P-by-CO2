metafor_statistics_gas_exchange <- function(reDF) {
    
    ### create directory
    if(!dir.exists("output/statistics_gas_exchange")) {
        dir.create("output/statistics_gas_exchange", showWarnings = FALSE)
    }
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="CO2 assimilation rate")
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    
    ### forest plot
    pdf("output/statistics_gas_exchange/co2_assimilation_rate_gas_exchange_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-12, 4), 
           at = log(c(0.3678794, 1, 2.718282, 7.389056)), #atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-8, -6.5, -4.5, -3), cex = 0.6)
    text(c(-8, -6.5, -4.5, -3, 0), l+2.5, c("Vegetation", 
                                            expression(paste(eCO[2], "/", aCO[2])),
                                            "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-8, -6.5, -4.5, -3), l+1.5,
         c("type","", "", "duration"), cex=0.7)
    text(-12, l+2.5, "Author & Year", pos = 4, cex=0.7)
    text(4, l+2.5, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    dev.off()
    
    ### mixed effect model
    res1 <- rma(log_interaction, v_variance, mods = cbind(Trt_eC_by_aC), 
                data = tDF)
    
    ### make predictions
    preds <- predict(res1, newmods = c(1.25, 1.5, 2.0))
    
    ### 2nd mixed model
    res2 <- rma(log_interaction, v_variance, mods = cbind(Trt_eC_by_aC, Trt_eP_by_aP), 
                data = tDF)
    
    ### forest plot
    pdf("output/statistics_gas_exchange/co2_assimilation_rate_gas_exchange_response_ratio_mixed_effect_model.pdf",
        height=12, width=9)
    forest(res1, slab = tDF$Literature,
           xlim = c(-12, 4), 
           ylim = c(-3.5, l+3.5),
           at = log(c(0.3678794, 1, 2.718282, 7.389056)), #atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-8, -6.5, -4.5, -3), cex = 0.6)
    addpoly(preds$pred, sei = preds$se, #atransf = exp,
            mlab = c(expression(paste(eCO[2], "/", aCO[2], "=1.25")),
                     expression(paste(eCO[2], "/", aCO[2], "=1.5")),
                     expression(paste(eCO[2], "/", aCO[2], "=2.0"))),
            cex=0.6)
    text(c(-8, -6.5, -4.5, -3), l+3, c("Vegetation", 
                                       expression(paste(eCO[2], "/", aCO[2])),
                                       "ePaP", "Experiment"),
         cex=0.7)
    text(c(-8, -6.5, -4.5, -3), l+2,
         c("type","", "", "duration"), cex=0.7)
    text(-12, l+3, "Author & Year", pos = 4, cex=0.7)
    text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    dev.off()

    
    ## categorical factor
    res3 <- rma(log_interaction, v_variance,
                mods = ~ factor(Vegetation_type) + Trt_eC_by_aC + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    
    ### check for type I error
    
    
    ## check for influential studies
    inf <- influence(res)
    #plot(inf, plotdfb = TRUE)
    
    pdf("output/statistics_gas_exchange/co2_assimilation_rate_gas_exchange_response_ratio_mixed_effect_model_categorical.pdf",
        height=12, width=9)
    forest(res3, slab = tDF$Literature,
           xlim = c(-12, 4), 
           at = log(c(0.3678794, 1, 2.718282, 7.389056)), #atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-8, -6.5, -4.5, -3), cex = 0.6)
    text(c(-8, -6.5, -4.5, -3, 0), l+2.5, c("Vegetation", 
                                            expression(paste(eCO[2], "/", aCO[2])),
                                            "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-8, -6.5, -4.5, -3), l+1.5,
         c("type","", "", "duration"), cex=0.7)
    text(-12, l+2.5, "Author & Year", pos = 4, cex=0.7)
    text(4, l+2.5, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    dev.off()
    
    ### check across models model performance
    pdf("output/statistics_gas_exchange/co2_assimilation_rate_gas_exchange_funnel_plot_intermodel_comparison.pdf")
    par(mfrow=c(2,2))
    funnel(res, main="Random effect model")
    funnel(res1, main="Mixed-effect model [CO2]")
    funnel(res2, main="Mixed-effect model [CO2 + P]")
    funnel(res3, main="Mixed-effect model [CO2 + P + Veg]")
    dev.off()
    
    
    pdf("output/statistics_gas_exchange/co2_assimilation_rate_gas_exchange_qq_plot_intermodel_comparison.pdf")
    par(mfrow=c(2,2))
    qqnorm(res, main="Random effect model")
    qqnorm(res1, main="Mixed-effect model [CO2]")
    qqnorm(res2, main="Mixed-effect model [CO2 + P]")
    qqnorm(res3, main="Mixed-effect model [CO2 + P + Veg]")  
    dev.off()
    
 
    
}