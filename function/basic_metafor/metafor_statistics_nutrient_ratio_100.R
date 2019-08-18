metafor_statistics_nutrient_ratio_100 <- function(reDF, intDF) {
    
    ### all nutrient ratio sample variance equals 1/n
    
    ### create directory
    if(!dir.exists("output/statistics_nutrient_ratio_100")) {
        dir.create("output/statistics_nutrient_ratio_100", showWarnings = FALSE)
    }
    
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Leaf NP ratio")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="leaf_NP"] <- res$b
    intDF$se[intDF$variable=="leaf_NP"] <- res$se
    intDF$p_value[intDF$variable=="leaf_NP"] <- res$pval
    intDF$ns[intDF$variable=="leaf_NP"] <- ns
    intDF$ne[intDF$variable=="leaf_NP"] <- l
    intDF$ci_lb[intDF$variable=="leaf_NP"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="leaf_NP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_nutrient_ratio_100/leaf_NP_response_ratio_random_effect_model.pdf",
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
    tDF <- subset(reDF, Variable=="Stem NP ratio")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="stem_NP"] <- res$b
    intDF$se[intDF$variable=="stem_NP"] <- res$se
    intDF$p_value[intDF$variable=="stem_NP"] <- res$pval
    intDF$ns[intDF$variable=="stem_NP"] <- ns
    intDF$ne[intDF$variable=="stem_NP"] <- l
    intDF$ci_lb[intDF$variable=="stem_NP"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="stem_NP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_nutrient_ratio_100/stem_NP_response_ratio_random_effect_model.pdf",
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
    tDF <- subset(reDF, Variable=="Root NP ratio")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    #res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="root_NP"] <- res$b
    intDF$se[intDF$variable=="root_NP"] <- res$se
    intDF$p_value[intDF$variable=="root_NP"] <- res$pval
    intDF$ns[intDF$variable=="root_NP"] <- ns
    intDF$ne[intDF$variable=="root_NP"] <- l
    intDF$ci_lb[intDF$variable=="root_NP"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="root_NP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_nutrient_ratio_100/root_NP_response_ratio_random_effect_model.pdf",
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
    tDF <- subset(reDF, Variable=="Total NP ratio")
    
    tDF <- tDF[order(tDF$Vegetation_type, tDF$Mycorrhizae_2,
                     tDF$Species, tDF$Literature, tDF$Trt_eC_by_aC,
                     tDF$Trt_eP_by_aP), ]
    
    ### use 1/n to get the variance
    tDF$v_variance <- 1/tDF$Sample.Size
    
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
    
    intDF$interaction[intDF$variable=="total_NP"] <- res$b
    intDF$se[intDF$variable=="total_NP"] <- res$se
    intDF$p_value[intDF$variable=="total_NP"] <- res$pval
    intDF$ns[intDF$variable=="total_NP"] <- ns
    intDF$ne[intDF$variable=="total_NP"] <- l
    intDF$ci_lb[intDF$variable=="total_NP"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="total_NP"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_nutrient_ratio_100/total_NP_response_ratio_random_effect_model.pdf",
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