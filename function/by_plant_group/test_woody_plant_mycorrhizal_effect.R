test_woody_plant_mycorrhizal_effect <- function(wdDF) {
    
    
    
    ### prepare subset
    bDF <- subset(wdDF, Category == "Biomass")
    lDF <- subset(bDF, Variable == "Leaf biomass")
    rDF <- subset(bDF, Variable == "Root biomass")
    tDF <- subset(bDF, Variable == "Total plant biomass")
    
    
    ### total biomas
    am.list <- c("Cecropia insignis", "Cecropia longipes", "Cecropia peltata",
                 "Ficus insipida", "Guazuma ulmifolia", "Ochroma pyramidale",
                 "Trema micrantha", "Trichospermum mexicanum", "Citrus aurantium",
                 "Gossypoim hirsutum")
    
    ecm.list <- c("Pinus densiflora", "Populus deltoides", "Eucalyptus grandis")
    

    for (i in am.list) {
        tDF$Mycorrhizae[tDF$Species == i] <- "AM"
    }
    
    for (i in ecm.list) {
        tDF$Mycorrhizae[tDF$Species == i] <- "ECM"
    }
    
    tDF <- tDF[complete.cases(tDF$Species),]
    
    res <- rma(log_interaction, v_variance, 
               mods = ~factor(Mycorrhizae), data = tDF)
    
    print(res)
    
    res.aP <- rma(log_co2_aP, variance_co2_aP, mods = ~factor(Mycorrhizae), 
                    data = tDF)
    print(res.aP)
    
    res.eP <- rma(log_co2_eP, variance_co2_eP, mods = ~factor(Mycorrhizae), 
                  data = tDF)
    print(res.eP)
    
    
    #### leaf biomass
    am.list <- c("Citrus aurantium",
                 "Gossypoim hirsutum")
    
    ecm.list <- c("Populus deltoides", "Pinus ponderosa", "Pinus radiata",
                  "Pinus caribaea")
    
    
    for (i in am.list) {
        lDF$Mycorrhizae[lDF$Species == i] <- "AM"
    }
    
    for (i in ecm.list) {
        lDF$Mycorrhizae[lDF$Species == i] <- "ECM"
    }
    
    lDF <- lDF[complete.cases(lDF$Species),]
    
    res <- rma(log_interaction, v_variance, 
               mods = ~factor(Mycorrhizae), data = lDF)
    
    print(res)
    
    res.aP <- rma(log_co2_aP, variance_co2_aP, mods = ~factor(Mycorrhizae), 
                  data = lDF)
    print(res.aP)
    
    res.eP <- rma(log_co2_eP, variance_co2_eP, mods = ~factor(Mycorrhizae), 
                  data = lDF)
    print(res.eP)
    
    ### root biomass
    am.list <- c("Citrus aurantium",
                 "Gossypoim hirsutum")
    
    ecm.list <- c("Populus deltoides", "Pinus ponderosa")
    
    
    for (i in am.list) {
        rDF$Mycorrhizae[rDF$Species == i] <- "AM"
    }
    
    for (i in ecm.list) {
        rDF$Mycorrhizae[rDF$Species == i] <- "ECM"
    }
    
    rDF <- rDF[complete.cases(rDF$Species),]
    
    res <- rma(log_interaction, v_variance, 
               mods = ~factor(Mycorrhizae), data = rDF)
    
    print(res)
    
    res.aP <- rma(log_co2_aP, variance_co2_aP, mods = ~factor(Mycorrhizae), 
                  data = rDF)
    print(res.aP)
    
    res.eP <- rma(log_co2_eP, variance_co2_eP, mods = ~factor(Mycorrhizae), 
                  data = rDF)
    print(res.eP)
    
    ### leaf P concentration
    lpDF <- subset(wdDF, Variable == "Leaf P concentration")
    
    
    am.list <- c("Cecropia insignis", "Cecropia longipes", "Cecropia peltata",
                 "Ficus insipida", "Guazuma ulmifolia", "Ochroma pyramidale",
                 "Trema micrantha", "Trichospermum mexicanum", "Citrus aurantium",
                 "Gossypoim hirsutum")
    
    ecm.list <- c("Pinus densiflora", "Populus deltoides")
    
    
    for (i in am.list) {
        lpDF$Mycorrhizae[lpDF$Species == i] <- "AM"
    }
    
    for (i in ecm.list) {
        lpDF$Mycorrhizae[lpDF$Species == i] <- "ECM"
    }
    
    lpDF <- lpDF[complete.cases(lpDF$Species),]
    
    res <- rma(log_interaction, v_variance, 
               mods = ~factor(Mycorrhizae), data = lpDF)
    
    print(res)
    ### significant mycorrhizal difference on leaf P concentration
    
    
    res.aP <- rma(log_co2_aP, variance_co2_aP, mods = ~factor(Mycorrhizae), 
                  data = lpDF, control=list(stepadj=0.5))
    print(res.aP)
    
    res.eP <- rma(log_co2_eP, variance_co2_eP, mods = ~factor(Mycorrhizae), 
                  data = lpDF)
    print(res.eP)
    
    
    ### leaf N concentration
    lnDF <- subset(wdDF, Variable == "Leaf N concentration")
    
    
    am.list <- c("Cecropia insignis", "Cecropia longipes", "Cecropia peltata",
                 "Ficus insipida", "Guazuma ulmifolia", "Ochroma pyramidale",
                 "Trema micrantha", "Trichospermum mexicanum", "Citrus aurantium")
    
    ecm.list <- c("Pinus densiflora")
    
    
    for (i in am.list) {
        lnDF$Mycorrhizae[lnDF$Species == i] <- "AM"
    }
    
    for (i in ecm.list) {
        lnDF$Mycorrhizae[lnDF$Species == i] <- "ECM"
    }
    
    lnDF <- lnDF[complete.cases(lnDF$Species),]
    
    res <- rma(log_interaction, v_variance, 
               mods = ~factor(Mycorrhizae), data = lnDF)
    
    print(res)
    
    res.aP <- rma(log_co2_aP, variance_co2_aP, mods = ~factor(Mycorrhizae), 
                  data = lnDF)
    print(res.aP)
    
    res.eP <- rma(log_co2_eP, variance_co2_eP, mods = ~factor(Mycorrhizae), 
                  data = lnDF)
    print(res.eP)
    
    ### photosynthesis
    aDF <- subset(wdDF, Variable == "CO2 assimilation rate")
    
    
    am.list <- c("Cecropia insignis", "Cecropia longipes", "Cecropia peltata",
                 "Ochroma pyramidale",
                 "Trema micrantha", "Trichospermum mexicanum", "Citrus aurantium")
    
    ecm.list <- c("Pinus densiflora", "Populus deltoides", "Eucalyptus grandis",
                  "Pinus radiata")
    
    
    for (i in am.list) {
        aDF$Mycorrhizae[aDF$Species == i] <- "AM"
    }
    
    for (i in ecm.list) {
        aDF$Mycorrhizae[aDF$Species == i] <- "ECM"
    }
    
    aDF <- aDF[complete.cases(aDF$Species),]
    
    res <- rma(log_interaction, v_variance, 
               mods = ~factor(Mycorrhizae), data = aDF)
    
    print(res)
    
    res.aP <- rma(log_co2_aP, variance_co2_aP, mods = ~factor(Mycorrhizae), 
                  data = aDF)
    print(res.aP)
    
    res.eP <- rma(log_co2_eP, variance_co2_eP, mods = ~factor(Mycorrhizae), 
                  data = aDF)
    print(res.eP)
    
    
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