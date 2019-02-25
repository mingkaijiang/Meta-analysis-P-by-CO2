metafor_statistics_concentration <- function(reDF) {
    
    ### create directory
    if(!dir.exists("output/statistics_concentration")) {
        dir.create("output/statistics_concentration", showWarnings = FALSE)
    }
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf N concentration")
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    
    ### forest plot
    pdf("output/statistics_concentration/leaf_N_concentration_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-16, 6), 
           at = log(c(0.05, 0.25, 1, 4)), atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-9.5, -8, -6, -4.5), cex = 0.6)
    text(c(-9.5, -8, -6, -4.5), l+2.5, c("Vegetation", 
                                       expression(paste(eCO[2], "/", aCO[2])),
                                       "ePaP", "Experiment"),
         cex=0.7)
    text(c(-9.5, -8, -6, -4.5), l+1.5,
         c("type","", "", "duration"), cex=0.7)
    text(-16, l+2.5, "Author & Year", pos = 4, cex=0.7)
    text(6, l+2.5, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    dev.off()
    
    ### mixed effect model
    res1 <- rma(log_interaction, v_variance, mods = cbind(Trt_eC_by_aC), 
                data = tDF)
    
    ### make predictions
    preds <- predict(res1, newmods = c(1.5, 2, 2.5))
    
    ### 2nd mixed model
    res2 <- rma(log_interaction, v_variance, mods = cbind(Trt_eC_by_aC, Trt_eP_by_aP), 
                data = tDF)
    
    ### forest plot
    pdf("output/statistics_concentration/leaf_N_concentration_response_ratio_mixed_effect_model.pdf",
        height=12, width=9)
    forest(res1, slab = tDF$Literature,
           xlim = c(-16, 6),
           ylim = c(-3.5, l+3.5),
           at = log(c(0.05, 0.25, 1, 4)), atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-9.5, -8, -6, -4.5), cex = 0.6)
    addpoly(preds$pred, sei = preds$se, atransf = exp,
            mlab = c(expression(paste(eCO[2], "/", aCO[2], "=1.5")),
                     expression(paste(eCO[2], "/", aCO[2], "=2.0")),
                     expression(paste(eCO[2], "/", aCO[2], "=2.5"))),
            cex=0.6)
    text(c(-9.5, -8, -6, -4.5), l+3, c("Vegetation", 
                                         expression(paste(eCO[2], "/", aCO[2])),
                                         "ePaP", "Experiment"),
         cex=0.7)
    text(c(-9.5, -8, -6, -4.5), l+2,
         c("type","", "", "duration"), cex=0.7)
    text(-16, l+3, "Author & Year", pos = 4, cex=0.7)
    text(6, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
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
    
    pdf("output/statistics_concentration/leaf_N_concentration_response_ratio_mixed_effect_model_categorical.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-16, 6), 
           at = log(c(0.05, 0.25, 1, 4)), atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-9.5, -8, -6, -4.5), cex = 0.6)
    text(c(-9.5, -8, -6, -4.5), l+2.5, c("Vegetation", 
                                         expression(paste(eCO[2], "/", aCO[2])),
                                         "ePaP", "Experiment"),
         cex=0.7)
    text(c(-9.5, -8, -6, -4.5), l+1.5,
         c("type","", "", "duration"), cex=0.7)
    text(-16, l+2.5, "Author & Year", pos = 4, cex=0.7)
    text(6, l+2.5, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    dev.off()
    
    ### check across models model performance
    pdf("output/statistics_concentration/leaf_N_concentration_funnel_plot_intermodel_comparison.pdf")
    par(mfrow=c(2,2))
    funnel(res, main="Random effect model")
    funnel(res1, main="Mixed-effect model [CO2]")
    funnel(res2, main="Mixed-effect model [CO2 + P]")
    funnel(res3, main="Mixed-effect model [CO2 + P + Veg]")
    dev.off()
    
    
    pdf("output/statistics_concentration/leaf_N_concentration_qq_plot_intermodel_comparison.pdf")
    par(mfrow=c(2,2))
    qqnorm(res, main="Random effect model")
    qqnorm(res1, main="Mixed-effect model [CO2]")
    qqnorm(res2, main="Mixed-effect model [CO2 + P]")
    qqnorm(res3, main="Mixed-effect model [CO2 + P + Veg]")  
    dev.off()
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf P concentration")
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF,control=list(stepadj=0.5))
    
    ### confidence interval
    
    ### length of the data frame
    l <- length(tDF$Literature)
    
    ### forest plot
    pdf("output/statistics_concentration/leaf_P_concentration_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-16, 6), 
           at = log(c(0.05, 0.25, 1, 4)), atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-9.5, -8, -6, -4.5), cex = 0.6)
    text(c(-9.5, -8, -6, -4.5), l+2.5, c("Vegetation", 
                                         expression(paste(eCO[2], "/", aCO[2])),
                                         "ePaP", "Experiment"),
         cex=0.7)
    text(c(-9.5, -8, -6, -4.5), l+1.5,
         c("type","", "", "duration"), cex=0.7)
    text(-16, l+2.5, "Author & Year", pos = 4, cex=0.7)
    text(6, l+2.5, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    dev.off()
    
    ### mixed effect model
    res1 <- rma(log_interaction, v_variance, mods = cbind(Trt_eC_by_aC), 
                data = tDF)
    
    ### 2nd mixed model
    res2 <- rma(log_interaction, v_variance, mods = cbind(Trt_eC_by_aC, Trt_eP_by_aP), 
                data = tDF)
    
    ## categorical factor
    res3 <- rma(log_interaction, v_variance,
                mods = ~ factor(Vegetation_type) + Trt_eC_by_aC + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    
    ### check for type I error
    

    ## check for influential studies
    #inf <- influence(res)
    #plot(inf, plotdfb = TRUE)
    
    ### check across models model performance
    pdf("output/statistics_concentration/leaf_P_concentration_funnel_plot_intermodel_comparison.pdf")
    par(mfrow=c(2,2))
    funnel(res, main="Random effect model")
    funnel(res1, main="Mixed-effect model [CO2]")
    funnel(res2, main="Mixed-effect model [CO2 + P]")
    funnel(res3, main="Mixed-effect model [CO2 + P + Veg]")
    dev.off()
    
    
    pdf("output/statistics_concentration/leaf_P_concentration_qq_plot_intermodel_comparison.pdf")
    par(mfrow=c(2,2))
    qqnorm(res, main="Random effect model")
    qqnorm(res1, main="Mixed-effect model [CO2]")
    qqnorm(res2, main="Mixed-effect model [CO2 + P]")
    qqnorm(res3, main="Mixed-effect model [CO2 + P + Veg]")  
    dev.off()
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Root P concentration")
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF)

    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    
    ### forest plot
    pdf("output/statistics_concentration/root_P_concentration_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-16, 6), 
           at = log(c(0.05, 0.25, 1, 4)), atransf = exp,
           ilab = cbind(tDF$Vegetation_type,
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-9.5, -8, -6, -4.5), cex = 0.6)
    text(c(-9.5, -8, -6, -4.5), l+2.5, c("Vegetation", 
                                         expression(paste(eCO[2], "/", aCO[2])),
                                         "ePaP", "Experiment"),
         cex=0.7)
    text(c(-9.5, -8, -6, -4.5), l+1.5,
         c("type","", "", "duration"), cex=0.7)
    text(-16, l+2.5, "Author & Year", pos = 4, cex=0.7)
    text(6, l+2.5, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    dev.off()
    
    ### mixed effect model
    res1 <- rma(log_interaction, v_variance, mods = cbind(Trt_eC_by_aC), 
                data = tDF)
    
    #res1 <- rma(log_interaction, v_variance, mods = cbind(Trt_eP_by_aP), 
    #            data = tDF)
    
    ### 2nd mixed model
    res2 <- rma(log_interaction, v_variance, mods = cbind(Trt_eC_by_aC, Trt_eP_by_aP), 
                data = tDF)
    
    ## categorical factor
    res3 <- rma(log_interaction, v_variance,
                mods = ~ factor(Vegetation_type) + Trt_eC_by_aC + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    
    ### check for type I error
    
    
    ## check for influential studies
    inf <- influence(res)
    #plot(inf, plotdfb = TRUE)
    
    
    ### check across models model performance
    pdf("output/statistics_concentration/root_P_concentration_funnel_plot_intermodel_comparison.pdf")
    par(mfrow=c(2,2))
    funnel(res, main="Random effect model")
    funnel(res1, main="Mixed-effect model [CO2]")
    funnel(res2, main="Mixed-effect model [CO2 + P]")
    funnel(res3, main="Mixed-effect model [CO2 + P + Veg]")
    dev.off()
    
    
    pdf("output/statistics_concentration/root_P_concentration_qq_plot_intermodel_comparison.pdf")
    par(mfrow=c(2,2))
    qqnorm(res, main="Random effect model")
    qqnorm(res1, main="Mixed-effect model [CO2]")
    qqnorm(res2, main="Mixed-effect model [CO2 + P]")
    qqnorm(res3, main="Mixed-effect model [CO2 + P + Veg]")  
    dev.off()
    

    
}