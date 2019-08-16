metafor_statistics_biomass_100 <- function(reDF, intDF) {
    
    ### create directory
    if(!dir.exists("output/statistics_biomass_100")) {
        dir.create("output/statistics_biomass_100", showWarnings = FALSE)
    }
    
    ####################### subset the dataframe for the right variable ##############################
    tDF <- subset(reDF, Variable=="Aboveground biomass")
    tDF <- subset(tDF, v_variance >= 0.01)

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
    
    intDF$interaction[intDF$variable=="aboveground_biomass"] <- res$b
    intDF$se[intDF$variable=="aboveground_biomass"] <- res$se
    intDF$p_value[intDF$variable=="aboveground_biomass"] <- res$pval
    intDF$ns[intDF$variable=="aboveground_biomass"] <- ns
    intDF$ne[intDF$variable=="aboveground_biomass"] <- l
    intDF$ci_lb[intDF$variable=="aboveground_biomass"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="aboveground_biomass"] <- res$ci.ub
    
    
    ### forest plot
    pdf("output/statistics_biomass_100/aboveground_biomass_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-12, 4), 
           ylim = c(-3.5, l+3.5),
           at = c(-1, 0, 1, 2), #atransf = exp,
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
    tDF <- subset(reDF, Variable=="Leaf biomass")
    
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="leaf_biomass"] <- res$b
    intDF$se[intDF$variable=="leaf_biomass"] <- res$se
    intDF$p_value[intDF$variable=="leaf_biomass"] <- res$pval
    intDF$ns[intDF$variable=="leaf_biomass"] <- ns
    intDF$ne[intDF$variable=="leaf_biomass"] <- l
    intDF$ci_lb[intDF$variable=="leaf_biomass"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="leaf_biomass"] <- res$ci.ub

    
    ### forest plot
    pdf("output/statistics_biomass_100/leaf_biomass_response_ratio_random_effect_model.pdf",
        height=12, width=9)
    forest(res, slab = tDF$Literature,
           xlim = c(-12, 4), 
           ylim = c(-3.5, l+3.5),
           at = c(-1, 0, 1, 2), #atransf = exp,
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
    tDF <- subset(reDF, Variable=="Stem biomass")
    
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### confidence interval
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="stem_biomass"] <- res$b
    intDF$se[intDF$variable=="stem_biomass"] <- res$se
    intDF$p_value[intDF$variable=="stem_biomass"] <- res$pval
    intDF$ns[intDF$variable=="stem_biomass"] <- ns
    intDF$ne[intDF$variable=="stem_biomass"] <- l
    intDF$ci_lb[intDF$variable=="stem_biomass"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="stem_biomass"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_biomass_100/stem_biomass_response_ratio_random_effect_model.pdf",
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
    tDF <- subset(tDF, v_variance >= 0.001)
    tDF <- subset(tDF, v_variance <= 2)
    
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)

    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="root_biomass"] <- res$b
    intDF$se[intDF$variable=="root_biomass"] <- res$se
    intDF$p_value[intDF$variable=="root_biomass"] <- res$pval
    intDF$ns[intDF$variable=="root_biomass"] <- ns
    intDF$ne[intDF$variable=="root_biomass"] <- l
    intDF$ci_lb[intDF$variable=="root_biomass"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="root_biomass"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_biomass_100/root_biomass_response_ratio_random_effect_model.pdf",
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
    tDF <- subset(tDF, v_variance >= 0.01)
    
    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    #confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="total_biomass"] <- res$b
    intDF$se[intDF$variable=="total_biomass"] <- res$se
    intDF$p_value[intDF$variable=="total_biomass"] <- res$pval
    intDF$ns[intDF$variable=="total_biomass"] <- ns
    intDF$ne[intDF$variable=="total_biomass"] <- l
    intDF$ci_lb[intDF$variable=="total_biomass"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="total_biomass"] <- res$ci.ub
    
    
    ### forest plot
    pdf("output/statistics_biomass_100/total_biomass_response_ratio_random_effect_model.pdf",
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
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, digits=5, 
    #           control=list(stepadj=0.05))

    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="leaf_N_content"] <- res$b
    intDF$se[intDF$variable=="leaf_N_content"] <- res$se
    intDF$p_value[intDF$variable=="leaf_N_content"] <- res$pval
    intDF$ns[intDF$variable=="leaf_N_content"] <- ns
    intDF$ne[intDF$variable=="leaf_N_content"] <- l
    intDF$ci_lb[intDF$variable=="leaf_N_content"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="leaf_N_content"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_biomass_100/leaf_N_content_response_ratio_random_effect_model.pdf",
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
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.5))

    # res <- rma(log_interaction, v_variance, data = tDF)
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    ### confidence interval
    ### The amount of heterogeneity in the true log relative risks is estimated to be tau^2
    confint(res)
    
    ### length of the data frame
    l <- length(tDF$Literature)
    ns <- length(unique(tDF$Literature))
    
    intDF$interaction[intDF$variable=="leaf_P_content"] <- res$b
    intDF$se[intDF$variable=="leaf_P_content"] <- res$se
    intDF$p_value[intDF$variable=="leaf_P_content"] <- res$pval
    intDF$ns[intDF$variable=="leaf_P_content"] <- ns
    intDF$ne[intDF$variable=="leaf_P_content"] <- l
    intDF$ci_lb[intDF$variable=="leaf_P_content"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="leaf_P_content"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_biomass_100/leaf_P_content_response_ratio_random_effect_model.pdf",
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
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    intDF$interaction[intDF$variable=="stem_N_content"] <- res$b
    intDF$se[intDF$variable=="stem_N_content"] <- res$se
    intDF$p_value[intDF$variable=="stem_N_content"] <- res$pval
    intDF$ns[intDF$variable=="stem_N_content"] <- ns
    intDF$ne[intDF$variable=="stem_N_content"] <- l
    intDF$ci_lb[intDF$variable=="stem_N_content"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="stem_N_content"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_biomass_100/stem_N_content_response_ratio_random_effect_model.pdf",
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
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    intDF$interaction[intDF$variable=="stem_P_content"] <- res$b
    intDF$se[intDF$variable=="stem_P_content"] <- res$se
    intDF$p_value[intDF$variable=="stem_P_content"] <- res$pval
    intDF$ns[intDF$variable=="stem_P_content"] <- ns
    intDF$ne[intDF$variable=="stem_P_content"] <- l
    intDF$ci_lb[intDF$variable=="stem_P_content"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="stem_P_content"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_biomass_100/stem_P_content_response_ratio_random_effect_model.pdf",
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
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))

    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    intDF$interaction[intDF$variable=="root_N_content"] <- res$b
    intDF$se[intDF$variable=="root_N_content"] <- res$se
    intDF$p_value[intDF$variable=="root_N_content"] <- res$pval
    intDF$ns[intDF$variable=="root_N_content"] <- ns
    intDF$ne[intDF$variable=="root_N_content"] <- l
    intDF$ci_lb[intDF$variable=="root_N_content"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="root_N_content"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_biomass_100/root_N_content_response_ratio_random_effect_model.pdf",
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
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.5))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    intDF$interaction[intDF$variable=="root_P_content"] <- res$b
    intDF$se[intDF$variable=="root_P_content"] <- res$se
    intDF$p_value[intDF$variable=="root_P_content"] <- res$pval
    intDF$ns[intDF$variable=="root_P_content"] <- ns
    intDF$ne[intDF$variable=="root_P_content"] <- l
    intDF$ci_lb[intDF$variable=="root_P_content"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="root_P_content"] <- res$ci.ub
    
    
    ### forest plot
    pdf("output/statistics_biomass_100/root_P_content_response_ratio_random_effect_model.pdf",
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
    tDF$v_variance <- 1/tDF$Sample.Size
    
    ### random-effect model
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.05))
    
    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    intDF$interaction[intDF$variable=="total_N_content"] <- res$b
    intDF$se[intDF$variable=="total_N_content"] <- res$se
    intDF$p_value[intDF$variable=="total_N_content"] <- res$pval
    intDF$ns[intDF$variable=="total_N_content"] <- ns
    intDF$ne[intDF$variable=="total_N_content"] <- l
    intDF$ci_lb[intDF$variable=="total_N_content"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="total_N_content"] <- res$ci.ub
    
    
    ### forest plot
    pdf("output/statistics_biomass_100/total_N_content_response_ratio_random_effect_model.pdf",
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
    #res <- rma(log_interaction, v_variance, data = tDF, control=list(stepadj=0.5))

    ### multivariable linear (mixed-effects) model with study as a random variable
    res <- rma.mv(log_interaction, v_variance, random = ~1 | random_factor, data = tDF)
    
    
    intDF$interaction[intDF$variable=="total_P_content"] <- res$b
    intDF$se[intDF$variable=="total_P_content"] <- res$se
    intDF$p_value[intDF$variable=="total_P_content"] <- res$pval
    intDF$ns[intDF$variable=="total_P_content"] <- ns
    intDF$ne[intDF$variable=="total_P_content"] <- l
    intDF$ci_lb[intDF$variable=="total_P_content"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="total_P_content"] <- res$ci.ub
    
    ### forest plot
    pdf("output/statistics_biomass_100/total_P_content_response_ratio_random_effect_model.pdf",
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

    return(intDF)

}
