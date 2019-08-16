test_plant_mycorrhizal_effect <- function(reDF100) {
    
    inDF <- subset(reDF100, Mycorrhizae!="NA")
    
    ### create directory
    if(!dir.exists("output/mycorrhizae_effect")) {
        dir.create("output/mycorrhizae_effect", showWarnings = FALSE)
    }
    
    
    #### ag biomass
    subDF <- subset(inDF, Variable == "Aboveground biomass")
    subDF <- subset(subDF, v_variance >= 0.01)
    
    subDF <- subDF[order(subDF$Vegetation_type, subDF$Mycorrhizae_2), ]
    
    #lDF <- subset(lDF, Vegetation_type == "Woody")
    res1 <- rma(log_interaction, v_variance, 
                mods = ~factor(Mycorrhizae_2), data = subDF)
    
    res2 <- rma(log_interaction, v_variance, 
                mods = ~factor(Vegetation_type), data = subDF)
    
    print(res1)
    print(res2)
    
    res.aP <- rma(log_co2_aP, variance_co2_aP, mods = ~factor(Mycorrhizae_2), 
                  data = subDF)
    print(res.aP)
    
    res.eP <- rma(log_co2_eP, variance_co2_eP, mods = ~factor(Mycorrhizae_2), 
                  data = subDF)
    print(res.eP)
    
    l <- length(subDF$Literature)
    
    
    pdf("output/mycorrhizae_effect/aboveground_biomass_response_ratio_categories_vegetation.pdf",
        height=16, width=9)
    forest(res1, slab = subDF$Literature,
           xlim = c(-12, 4), 
           ylim = c(-3.5, l+3.5),
           at = c(-1, 0, 1, 2), #atransf = exp,
           ilab = cbind(as.character(subDF$Vegetation_type),
                        as.character(subDF$Mycorrhizae_2), 
                        as.character(subDF$Species),
                        as.character(subDF$Experiment_duration)), 
           ilab.xpos = c(-8, -6.5, -4.5, -3), cex = 0.6)
    text(c(-8, -6.5, -4.5, -3, 0), l+3, c("Vegetation", 
                                          "Mycorrhizae",
                                          "Species", "Experiment", "Range"),
         cex=0.7)
    text(c(-8, -6.5, -4.5, -3), l+2,
         c("type","", "", "duration"), cex=0.7)
    text(-12, l+3, "Author & Year", pos = 4, cex=0.7)
    text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    dev.off()
    
    
    #### Belowground biomass
    subDF <- subset(inDF, Variable %in%c("Belowground biomass", "Root biomass"))
    subDF <- subset(subDF, v_variance >= 0.01)
    subDF <- subset(subDF, v_variance <= 2)
    
    subDF <- subDF[order(subDF$Vegetation_type, subDF$Mycorrhizae_2), ]
    
    res1 <- rma(log_interaction, v_variance, 
                mods = ~factor(Mycorrhizae_2), data = subDF)
    
    res2 <- rma(log_interaction, v_variance, 
                mods = ~factor(Vegetation_type), data = subDF)
    
    print(res1)
    print(res2)
    
    res.aP <- rma(log_co2_aP, variance_co2_aP, mods = ~factor(Mycorrhizae_2), 
                  data = subDF)
    print(res.aP)
    
    res.eP <- rma(log_co2_eP, variance_co2_eP, mods = ~factor(Mycorrhizae_2), 
                  data = subDF)
    print(res.eP)
    
    l <- length(subDF$Literature)
    
    
    pdf("output/mycorrhizae_effect/belowground_biomass_response_ratio_categories_vegetation.pdf",
        height=16, width=9)
    forest(res1, slab = subDF$Literature,
           xlim = c(-12, 4), 
           ylim = c(-3.5, l+3.5),
           at = c(-1, 0, 1, 2), #atransf = exp,
           ilab = cbind(as.character(subDF$Vegetation_type),
                        as.character(subDF$Mycorrhizae_2), 
                        as.character(subDF$Species),
                        as.character(subDF$Experiment_duration)), 
           ilab.xpos = c(-8, -6.5, -4.5, -3), cex = 0.6)
    text(c(-8, -6.5, -4.5, -3, 0), l+3, c("Vegetation", 
                                          "Mycorrhizae",
                                          "Species", "Experiment", "Range"),
         cex=0.7)
    text(c(-8, -6.5, -4.5, -3), l+2,
         c("type","", "", "duration"), cex=0.7)
    text(-12, l+3, "Author & Year", pos = 4, cex=0.7)
    text(4, l+3, "Relative Response [95% CI]", pos = 2, cex = 0.7)
    dev.off()
    
    
 
    
    
    ### plotting - leaf P concentration and photosynthesis
    var.list <- rep(c("Leaf P concentration", 
                  "CO2 assimilation rate"), each = 2)
    
    m.list <- rep(c("ECM", "AM"), by = 2)
    
    ### generate a output dataframe
    intDF <- data.frame(var.list, m.list, NA, NA, NA, NA)
    
    colnames(intDF) <- c("variable", "mycorrhizae", "interaction", "ci_lb", 
                         "ci_ub", "pval")
    
    sumDF1 <- sumDF2 <- intDF
    sumDF1$P_trt <- "aP"
    sumDF2$P_trt <- "eP"
    sumDF <- rbind(sumDF1, sumDF2)
    colnames(sumDF) <- c("variable", "mycorrhizae", "effect", "ci_lb", "ci_ub", "pval", "P_trt")
    
    
    ### leaf P, ECM
    testDF <- subset(lpDF, Mycorrhizae == "ECM")
    res <- rma(log_interaction, v_variance, data = testDF)
    res.aP <- rma(log_co2_aP, variance_co2_aP, data = testDF)
    res.eP <- rma(log_co2_eP, variance_co2_eP, data = testDF)
    
    
    intDF$interaction[intDF$variable=="Leaf P concentration" & intDF$mycorrhizae == "ECM"] <- res$b
    intDF$pval[intDF$variable=="Leaf P concentration" & intDF$mycorrhizae == "ECM"] <- res$pval
    intDF$ci_lb[intDF$variable=="Leaf P concentration" & intDF$mycorrhizae == "ECM"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="Leaf P concentration" & intDF$mycorrhizae == "ECM"] <- res$ci.ub
    
    sumDF$effect[sumDF$variable=="Leaf P concentration" & sumDF$P_trt == "aP" & sumDF$mycorrhizae == "ECM"] <- res.aP$b
    sumDF$pval[sumDF$variable=="Leaf P concentration" & sumDF$P_trt == "aP" & sumDF$mycorrhizae == "ECM"] <- res.aP$pval
    sumDF$ci_lb[sumDF$variable=="Leaf P concentration" & sumDF$P_trt == "aP" & sumDF$mycorrhizae == "ECM"] <- res.aP$ci.lb
    sumDF$ci_ub[sumDF$variable=="Leaf P concentration" & sumDF$P_trt == "aP" & sumDF$mycorrhizae == "ECM"] <- res.aP$ci.ub
    
    sumDF$effect[sumDF$variable=="Leaf P concentration" & sumDF$P_trt == "eP" & sumDF$mycorrhizae == "ECM"] <- res.eP$b
    sumDF$pval[sumDF$variable=="Leaf P concentration" & sumDF$P_trt == "eP" & sumDF$mycorrhizae == "ECM"] <- res.eP$pval
    sumDF$ci_lb[sumDF$variable=="Leaf P concentration" & sumDF$P_trt == "eP" & sumDF$mycorrhizae == "ECM"] <- res.eP$ci.lb
    sumDF$ci_ub[sumDF$variable=="Leaf P concentration" & sumDF$P_trt == "eP" & sumDF$mycorrhizae == "ECM"] <- res.eP$ci.ub
    
    ### leaf P, AM
    testDF <- subset(lpDF, Mycorrhizae == "AM")
    res <- rma(log_interaction, v_variance, data = testDF)
    res.aP <- rma(log_co2_aP, variance_co2_aP, data = testDF)
    res.eP <- rma(log_co2_eP, variance_co2_eP, data = testDF)
    
    intDF$interaction[intDF$variable=="Leaf P concentration" & intDF$mycorrhizae == "AM"] <- res$b
    intDF$pval[intDF$variable=="Leaf P concentration" & intDF$mycorrhizae == "AM"] <- res$pval
    intDF$ci_lb[intDF$variable=="Leaf P concentration" & intDF$mycorrhizae == "AM"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="Leaf P concentration" & intDF$mycorrhizae == "AM"] <- res$ci.ub
    
    sumDF$effect[sumDF$variable=="Leaf P concentration" & sumDF$P_trt == "aP" & sumDF$mycorrhizae == "AM"] <- res.aP$b
    sumDF$pval[sumDF$variable=="Leaf P concentration" & sumDF$P_trt == "aP" & sumDF$mycorrhizae == "AM"] <- res.aP$pval
    sumDF$ci_lb[sumDF$variable=="Leaf P concentration" & sumDF$P_trt == "aP" & sumDF$mycorrhizae == "AM"] <- res.aP$ci.lb
    sumDF$ci_ub[sumDF$variable=="Leaf P concentration" & sumDF$P_trt == "aP" & sumDF$mycorrhizae == "AM"] <- res.aP$ci.ub
    
    sumDF$effect[sumDF$variable=="Leaf P concentration" & sumDF$P_trt == "eP" & sumDF$mycorrhizae == "AM"] <- res.eP$b
    sumDF$pval[sumDF$variable=="Leaf P concentration" & sumDF$P_trt == "eP" & sumDF$mycorrhizae == "AM"] <- res.eP$pval
    sumDF$ci_lb[sumDF$variable=="Leaf P concentration" & sumDF$P_trt == "eP" & sumDF$mycorrhizae == "AM"] <- res.eP$ci.lb
    sumDF$ci_ub[sumDF$variable=="Leaf P concentration" & sumDF$P_trt == "eP" & sumDF$mycorrhizae == "AM"] <- res.eP$ci.ub
    
    
    ### photosynthesis, ECM
    testDF <- subset(aDF, Mycorrhizae == "ECM")
    res <- rma(log_interaction, v_variance, data = testDF)
    res.aP <- rma(log_co2_aP, variance_co2_aP, data = testDF)
    res.eP <- rma(log_co2_eP, variance_co2_eP, data = testDF)
    
    
    intDF$interaction[intDF$variable=="CO2 assimilation rate" & intDF$mycorrhizae == "ECM"] <- res$b
    intDF$pval[intDF$variable=="CO2 assimilation rate" & intDF$mycorrhizae == "ECM"] <- res$pval
    intDF$ci_lb[intDF$variable=="CO2 assimilation rate" & intDF$mycorrhizae == "ECM"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="CO2 assimilation rate" & intDF$mycorrhizae == "ECM"] <- res$ci.ub
    
    sumDF$effect[sumDF$variable=="CO2 assimilation rate" & sumDF$P_trt == "aP" & sumDF$mycorrhizae == "ECM"] <- res.aP$b
    sumDF$pval[sumDF$variable=="CO2 assimilation rate" & sumDF$P_trt == "aP" & sumDF$mycorrhizae == "ECM"] <- res.aP$pval
    sumDF$ci_lb[sumDF$variable=="CO2 assimilation rate" & sumDF$P_trt == "aP" & sumDF$mycorrhizae == "ECM"] <- res.aP$ci.lb
    sumDF$ci_ub[sumDF$variable=="CO2 assimilation rate" & sumDF$P_trt == "aP" & sumDF$mycorrhizae == "ECM"] <- res.aP$ci.ub
    
    sumDF$effect[sumDF$variable=="CO2 assimilation rate" & sumDF$P_trt == "eP" & sumDF$mycorrhizae == "ECM"] <- res.eP$b
    sumDF$pval[sumDF$variable=="CO2 assimilation rate" & sumDF$P_trt == "eP" & sumDF$mycorrhizae == "ECM"] <- res.eP$pval
    sumDF$ci_lb[sumDF$variable=="CO2 assimilation rate" & sumDF$P_trt == "eP" & sumDF$mycorrhizae == "ECM"] <- res.eP$ci.lb
    sumDF$ci_ub[sumDF$variable=="CO2 assimilation rate" & sumDF$P_trt == "eP" & sumDF$mycorrhizae == "ECM"] <- res.eP$ci.ub
    
    
    ### photosynthesis, AM
    testDF <- subset(aDF, Mycorrhizae == "AM")
    res <- rma(log_interaction, v_variance, data = testDF)
    res.aP <- rma(log_co2_aP, variance_co2_aP, data = testDF)
    res.eP <- rma(log_co2_eP, variance_co2_eP, data = testDF)
    
    intDF$interaction[intDF$variable=="CO2 assimilation rate" & intDF$mycorrhizae == "AM"] <- res$b
    intDF$pval[intDF$variable=="CO2 assimilation rate" & intDF$mycorrhizae == "AM"] <- res$pval
    intDF$ci_lb[intDF$variable=="CO2 assimilation rate" & intDF$mycorrhizae == "AM"] <- res$ci.lb
    intDF$ci_ub[intDF$variable=="CO2 assimilation rate" & intDF$mycorrhizae == "AM"] <- res$ci.ub
    
    sumDF$effect[sumDF$variable=="CO2 assimilation rate" & sumDF$P_trt == "aP" & sumDF$mycorrhizae == "AM"] <- res.aP$b
    sumDF$pval[sumDF$variable=="CO2 assimilation rate" & sumDF$P_trt == "aP" & sumDF$mycorrhizae == "AM"] <- res.aP$pval
    sumDF$ci_lb[sumDF$variable=="CO2 assimilation rate" & sumDF$P_trt == "aP" & sumDF$mycorrhizae == "AM"] <- res.aP$ci.lb
    sumDF$ci_ub[sumDF$variable=="CO2 assimilation rate" & sumDF$P_trt == "aP" & sumDF$mycorrhizae == "AM"] <- res.aP$ci.ub
    
    sumDF$effect[sumDF$variable=="CO2 assimilation rate" & sumDF$P_trt == "eP" & sumDF$mycorrhizae == "AM"] <- res.eP$b
    sumDF$pval[sumDF$variable=="CO2 assimilation rate" & sumDF$P_trt == "eP" & sumDF$mycorrhizae == "AM"] <- res.eP$pval
    sumDF$ci_lb[sumDF$variable=="CO2 assimilation rate" & sumDF$P_trt == "eP" & sumDF$mycorrhizae == "AM"] <- res.eP$ci.lb
    sumDF$ci_ub[sumDF$variable=="CO2 assimilation rate" & sumDF$P_trt == "eP" & sumDF$mycorrhizae == "AM"] <- res.eP$ci.ub
    
    ## plot effect of LP on eCO2 response 
    # leaf P
    p1a <- ggplot(plotDF)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=Variable, xmin=neg, xmax=pos, color=factor(Mycorrhizae))) + 
        geom_point(aes(y=id, x=interaction, fill=Mycorrhizae), 
                   size=8, shape=21)+
        labs(x="", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 18, face = "bold"),
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-75, 55))+
        scale_y_continuous(breaks=c(1:4),
                           labels=c("RL", "LMA", "LA", "A"))+
        scale_color_manual(name=paste("Vegetation group"),
                           limits=c("woody", "nonwoody"),
                           values=c("black", "grey"),
                           labels=c("Woody", "Nonwoody"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("Vegetation group"),
                          limits=c("woody", "nonwoody"),
                          values=c("black", "grey"),
                          labels=c("Woody", "Nonwoody"))+        
        ggtitle("a")+
        guides(fill = guide_legend(title.position = "top"))
    
}