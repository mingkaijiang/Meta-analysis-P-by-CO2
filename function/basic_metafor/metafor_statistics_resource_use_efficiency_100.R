metafor_statistics_resource_use_efficiency_100 <- function(reDF, intDF) {
    
    ### create directory
    if(!dir.exists("output/statistics_resource_use_efficiency_100")) {
        dir.create("output/statistics_resource_use_efficiency_100", showWarnings = FALSE)
    }
    

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="WUE")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### random-effect model
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="WUE"] <- res$b
    intDF$se[intDF$variable=="WUE"] <- res$se
    intDF$p_value[intDF$variable=="WUE"] <- res$pval
    intDF$ns[intDF$variable=="WUE"] <- ns
    intDF$ne[intDF$variable=="WUE"] <- l
    intDF$ci_lb[intDF$variable=="WUE"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="WUE"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_resource_use_efficiency_100/WUE_resource_use_efficiency_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-14, 4), 
           ylim = c(-3.5, l+3.5),
           at = c(-1, 0, 1, 2), #atransf = exp,
           ilab = cbind(as.character(tDF$Vegetation_type),
                        as.character(tDF$Species),
                        as.character(tDF$Mycorrhizae_2), 
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-10, -8, -6.5, -5, -4, -2.5), cex = 0.6)
    text(c(-10, -8, -6.5, -5, -4, -2.5, 0), l+3, c("Vegetation", 
                                                   "Species",
                                                   "Mycorrhizal",
                                                   expression(paste(eCO[2], "/", aCO[2])),
                                                   "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-10, -8, -6.5, -5, -4, -2.5), l+2,
         c("type","", "association", "", "", "duration"), cex=0.7)
    text(-14, l+3, "Author & Year", pos = 4, cex=0.7)
    text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    text(-13.5, -3.0, paste0("ne = ", l), cex = 0.6)
    text(-13.5, -2.0, paste0("ns = ", ns), cex = 0.6)
    dev.off()
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="NUE")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    intDF$interaction[intDF$variable=="NUE"] <- res$b
    intDF$se[intDF$variable=="NUE"] <- res$se
    intDF$p_value[intDF$variable=="NUE"] <- res$pval
    intDF$ns[intDF$variable=="NUE"] <- ns
    intDF$ne[intDF$variable=="NUE"] <- l
    intDF$ci_lb[intDF$variable=="NUE"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="NUE"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_biomass_100/NUE_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-14, 4), 
           ylim = c(-3.5, l+3.5),
           at = c(-1, 0, 1, 2), #atransf = exp,
           ilab = cbind(as.character(tDF$Vegetation_type),
                        as.character(tDF$Species),
                        as.character(tDF$Mycorrhizae_2), 
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-10, -8, -6.5, -5, -4, -2.5), cex = 0.6)
    text(c(-10, -8, -6.5, -5, -4, -2.5, 0), l+3, c("Vegetation", 
                                                   "Species",
                                                   "Mycorrhizal",
                                                   expression(paste(eCO[2], "/", aCO[2])),
                                                   "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-10, -8, -6.5, -5, -4, -2.5), l+2,
         c("type","", "association", "", "", "duration"), cex=0.7)
    text(-14, l+3, "Author & Year", pos = 4, cex=0.7)
    text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    text(-13.5, -3.0, paste0("ne = ", l), cex = 0.6)
    text(-13.5, -2.0, paste0("ns = ", ns), cex = 0.6)
    dev.off()

    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="PUE")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    intDF$interaction[intDF$variable=="PUE"] <- res$b
    intDF$se[intDF$variable=="PUE"] <- res$se
    intDF$p_value[intDF$variable=="PUE"] <- res$pval
    intDF$ns[intDF$variable=="PUE"] <- ns
    intDF$ne[intDF$variable=="PUE"] <- l
    intDF$ci_lb[intDF$variable=="PUE"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="PUE"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_biomass_100/PUE_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-14, 4), 
           ylim = c(-3.5, l+3.5),
           at = c(-1, 0, 1, 2), #atransf = exp,
           ilab = cbind(as.character(tDF$Vegetation_type),
                        as.character(tDF$Species),
                        as.character(tDF$Mycorrhizae_2), 
                        round(tDF$Trt_eC_by_aC,1), 
                        round(tDF$Trt_eP_by_aP,1),
                        as.character(tDF$Experiment_duration)), 
           ilab.xpos = c(-10, -8, -6.5, -5, -4, -2.5), cex = 0.6)
    text(c(-10, -8, -6.5, -5, -4, -2.5, 0), l+3, c("Vegetation", 
                                                   "Species",
                                                   "Mycorrhizal",
                                                   expression(paste(eCO[2], "/", aCO[2])),
                                                   "ePaP", "Experiment", "Range"),
         cex=0.7)
    text(c(-10, -8, -6.5, -5, -4, -2.5), l+2,
         c("type","", "association", "", "", "duration"), cex=0.7)
    text(-14, l+3, "Author & Year", pos = 4, cex=0.7)
    text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    text(-13.5, -3.0, paste0("ne = ", l), cex = 0.6)
    text(-13.5, -2.0, paste0("ns = ", ns), cex = 0.6)
    dev.off()
    
    return(intDF)
    

    
}