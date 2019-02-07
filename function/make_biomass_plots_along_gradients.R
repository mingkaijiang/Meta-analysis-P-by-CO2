make_biomass_plots_along_gradients <- function(inDF) {
    
    if(!dir.exists("output/gradient_biomass")) {
        dir.create("output/gradient_biomass", showWarnings = FALSE)
    }
    ### Subset biomass category
    bioDF <- subset(inDF, Category == "Biomass")
    
    ### Make a further subset of the dataframe
    bioDF1 <- bioDF[bioDF$Variable %in% c("Total plant biomass", "Leaf biomass", "Stem biomass",
                                          "Root biomass", "Aboveground biomass", "Belowground biomass"),]
    
    ### Aboveground biomass 
    subDF1 <- subset(bioDF1, Variable=="Aboveground biomass")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)

    
    ### plot  - elevated CO2 effect at aP and eP
    p2 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p3 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p4 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    
    ### plot  - elevated P effect at aC and eC, along CO2 gradient
    p5 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated P effect at aC and eC
    p6 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p7 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p8 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
              guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    ### summary histgram of treatments
    pdf("output/gradient_biomass/summary_aboveground_biomass_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### Belowground biomass 
    subDF1 <- subset(bioDF1, Variable=="Belowground biomass")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated CO2 effect at aP and eP
    p2 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p3 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p4 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    
    ### plot  - elevated P effect at aC and eC, along CO2 gradient
    p5 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated P effect at aC and eC
    p6 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p7 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p8 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    ### summary histgram of treatments
    pdf("output/gradient_biomass/summary_belowground_biomass_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### Leaf biomass 
    subDF1 <- subset(bioDF1, Variable=="Leaf biomass")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated CO2 effect at aP and eP
    p2 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p3 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p4 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    
    ### plot  - elevated P effect at aC and eC, along CO2 gradient
    p5 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated P effect at aC and eC
    p6 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p7 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p8 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    ### summary histgram of treatments
    pdf("output/gradient_biomass/summary_leaf_biomass_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### stem biomass 
    subDF1 <- subset(bioDF1, Variable=="Stem biomass")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated CO2 effect at aP and eP
    p2 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p3 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p4 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    
    ### plot  - elevated P effect at aC and eC, along CO2 gradient
    p5 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated P effect at aC and eC
    p6 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p7 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p8 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    ### summary histgram of treatments
    pdf("output/gradient_biomass/summary_stem_biomass_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### Root biomass 
    subDF1 <- subset(bioDF1, Variable=="Root biomass")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated CO2 effect at aP and eP
    p2 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p3 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p4 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    
    ### plot  - elevated P effect at aC and eC, along CO2 gradient
    p5 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated P effect at aC and eC
    p6 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p7 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p8 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    ### summary histgram of treatments
    pdf("output/gradient_biomass/summary_root_biomass_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### Total biomass 
    subDF1 <- subset(bioDF1, Variable=="Total plant biomass")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated CO2 effect at aP and eP
    p2 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p3 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p4 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    
    ### plot  - elevated P effect at aC and eC, along CO2 gradient
    p5 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated P effect at aC and eC
    p6 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p7 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p8 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    ### summary histgram of treatments
    pdf("output/gradient_biomass/summary_total_biomass_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### Make a further subset of the dataframe
    bioDF1 <- bioDF[bioDF$Variable %in% c("Total plant N content", "Leaf N content", "Stem N content",
                                          "Root N content", "Aboveground N content"),]
    
    ### Aboveground biomass 
    subDF1 <- subset(bioDF1, Variable=="Aboveground N content")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated CO2 effect at aP and eP
    p2 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p3 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p4 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    
    ### plot  - elevated P effect at aC and eC, along CO2 gradient
    p5 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated P effect at aC and eC
    p6 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p7 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p8 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    ### summary histgram of treatments
    pdf("output/gradient_biomass/summary_aboveground_N_content_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    

    ### Leaf biomass 
    subDF1 <- subset(bioDF1, Variable=="Leaf N content")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated CO2 effect at aP and eP
    p2 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p3 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p4 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    
    ### plot  - elevated P effect at aC and eC, along CO2 gradient
    p5 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated P effect at aC and eC
    p6 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p7 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p8 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    ### summary histgram of treatments
    pdf("output/gradient_biomass/summary_leaf_N_content_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### stem biomass 
    subDF1 <- subset(bioDF1, Variable=="Stem N content")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated CO2 effect at aP and eP
    p2 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p3 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p4 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    
    ### plot  - elevated P effect at aC and eC, along CO2 gradient
    p5 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated P effect at aC and eC
    p6 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p7 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p8 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    ### summary histgram of treatments
    pdf("output/gradient_biomass/summary_stem_N_content_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### Root biomass 
    subDF1 <- subset(bioDF1, Variable=="Root N content")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated CO2 effect at aP and eP
    p2 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p3 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p4 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    
    ### plot  - elevated P effect at aC and eC, along CO2 gradient
    p5 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated P effect at aC and eC
    p6 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p7 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p8 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    ### summary histgram of treatments
    pdf("output/gradient_biomass/summary_root_N_content_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### Total biomass 
    subDF1 <- subset(bioDF1, Variable=="Total plant N content")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated CO2 effect at aP and eP
    p2 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p3 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p4 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    
    ### plot  - elevated P effect at aC and eC, along CO2 gradient
    p5 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated P effect at aC and eC
    p6 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p7 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p8 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    ### summary histgram of treatments
    pdf("output/gradient_biomass/summary_total_N_content_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### Make a further subset of the dataframe
    bioDF1 <- bioDF[bioDF$Variable %in% c("Total plant P content", "Leaf P content", "Stem P content",
                                          "Root P content", "Aboveground P content"),]
    
    ### Aboveground biomass 
    subDF1 <- subset(bioDF1, Variable=="Aboveground P content")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated CO2 effect at aP and eP
    p2 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p3 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p4 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    
    ### plot  - elevated P effect at aC and eC, along CO2 gradient
    p5 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated P effect at aC and eC
    p6 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p7 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p8 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    ### summary histgram of treatments
    pdf("output/gradient_biomass/summary_aboveground_P_content_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    
    ### Leaf biomass 
    subDF1 <- subset(bioDF1, Variable=="Leaf P content")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated CO2 effect at aP and eP
    p2 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p3 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p4 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    
    ### plot  - elevated P effect at aC and eC, along CO2 gradient
    p5 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated P effect at aC and eC
    p6 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p7 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p8 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    ### summary histgram of treatments
    pdf("output/gradient_biomass/summary_leaf_P_content_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### stem biomass 
    subDF1 <- subset(bioDF1, Variable=="Stem P content")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated CO2 effect at aP and eP
    p2 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p3 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p4 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    
    ### plot  - elevated P effect at aC and eC, along CO2 gradient
    p5 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated P effect at aC and eC
    p6 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p7 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p8 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    ### summary histgram of treatments
    pdf("output/gradient_biomass/summary_stem_P_content_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### Root biomass 
    subDF1 <- subset(bioDF1, Variable=="Root P content")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated CO2 effect at aP and eP
    p2 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p3 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p4 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    
    ### plot  - elevated P effect at aC and eC, along CO2 gradient
    p5 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated P effect at aC and eC
    p6 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p7 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p8 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    ### summary histgram of treatments
    pdf("output/gradient_biomass/summary_root_P_content_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### Total biomass 
    subDF1 <- subset(bioDF1, Variable=="Total plant P content")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated CO2 effect at aP and eP
    p2 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab("Phosphorus Addition Ratio") + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p3 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p4 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    
    ### plot  - elevated P effect at aC and eC, along CO2 gradient
    p5 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, aCeP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Phoshorus Effect RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    
    ### plot  - elevated P effect at aC and eC
    p6 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCaP_over_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP),
                    se=T, method="gam", col="red")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab(expression(paste(CO[2], " Effect RR"))) +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - additive interaction effect 
    p7 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Additive RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)
    
    ### plot  - multiplicative interaction effect 
    p8 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Multiplicative RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))
    
    ### summary histgram of treatments
    pdf("output/gradient_biomass/summary_total_P_content_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
}