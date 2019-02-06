make_concentration_plots_along_gradients <- function(inDF) {
    
    if(!dir.exists("output/gradient_concentration")) {
        dir.create("output/gradient_concentration", showWarnings = FALSE)
    }
    
    ### Subset biomass category
    bioDF <- subset(inDF, Category == "Concentration")
    
    ### Make a further subset of the dataframe
    bioDF1 <- bioDF[bioDF$Variable %in% c("Total plant N concentration", "Leaf N concentration", "Stem N concentration",
                                          "Root N concentration", "Aboveground N concentration"),]
    
    ### Aboveground biomass 
    subDF1 <- subset(bioDF1, Variable=="Aboveground N concentration")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP/100),
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
                   aes(Trt_eP_by_aP, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP/100),
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
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP/100),
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
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100),
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
                   aes(Trt_eC_by_aC, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP/100),
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
                   aes(Trt_eC_by_aC, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP/100),
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
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP/100),
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
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100),
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
    pdf("output/gradient_concentration/summary_aboveground_N_concentration_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    

    ### Leaf biomass 
    subDF1 <- subset(bioDF1, Variable=="Leaf N concentration")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP/100),
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
                   aes(Trt_eP_by_aP, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP/100),
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
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP/100),
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
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100),
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
                   aes(Trt_eC_by_aC, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP/100),
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
                   aes(Trt_eC_by_aC, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP/100),
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
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP/100),
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
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100),
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
    pdf("output/gradient_concentration/summary_leaf_N_concentration_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### stem biomass 
    subDF1 <- subset(bioDF1, Variable=="Stem N concentration")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP/100),
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
                   aes(Trt_eP_by_aP, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP/100),
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
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP/100),
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
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100),
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
                   aes(Trt_eC_by_aC, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP/100),
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
                   aes(Trt_eC_by_aC, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP/100),
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
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP/100),
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
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100),
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
    pdf("output/gradient_concentration/summary_stem_N_concentration_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### Root biomass 
    subDF1 <- subset(bioDF1, Variable=="Root N concentration")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP/100),
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
                   aes(Trt_eP_by_aP, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP/100),
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
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP/100),
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
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100),
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
                   aes(Trt_eC_by_aC, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP/100),
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
                   aes(Trt_eC_by_aC, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP/100),
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
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP/100),
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
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100),
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
    pdf("output/gradient_concentration/summary_root_N_concentration_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### Total biomass 
    subDF1 <- subset(bioDF1, Variable=="Total plant N concentration")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP/100),
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
                   aes(Trt_eP_by_aP, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP/100),
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
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP/100),
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
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100),
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
                   aes(Trt_eC_by_aC, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP/100),
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
                   aes(Trt_eC_by_aC, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP/100),
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
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP/100),
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
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100),
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
    pdf("output/gradient_concentration/summary_total_N_concentration_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### Make a further subset of the dataframe
    bioDF1 <- bioDF[bioDF$Variable %in% c("Total plant P concentration", "Leaf P concentration", "Stem P concentration",
                                          "Root P concentration", "Aboveground P concentration"),]
    
    ### Aboveground biomass 
    subDF1 <- subset(bioDF1, Variable=="Aboveground P concentration")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP/100),
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
                   aes(Trt_eP_by_aP, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP/100),
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
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP/100),
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
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100),
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
                   aes(Trt_eC_by_aC, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP/100),
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
                   aes(Trt_eC_by_aC, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP/100),
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
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP/100),
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
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100),
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
    pdf("output/gradient_concentration/summary_aboveground_P_concentration_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    
    ### Leaf biomass 
    subDF1 <- subset(bioDF1, Variable=="Leaf P concentration")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP/100),
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
                   aes(Trt_eP_by_aP, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP/100),
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
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP/100),
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
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100),
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
                   aes(Trt_eC_by_aC, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP/100),
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
                   aes(Trt_eC_by_aC, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP/100),
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
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP/100),
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
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100),
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
    pdf("output/gradient_concentration/summary_leaf_P_concentration_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### stem biomass 
    subDF1 <- subset(bioDF1, Variable=="Stem P concentration")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP/100),
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
                   aes(Trt_eP_by_aP, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP/100),
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
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP/100),
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
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100),
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
                   aes(Trt_eC_by_aC, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP/100),
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
                   aes(Trt_eC_by_aC, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP/100),
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
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP/100),
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
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100),
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
    pdf("output/gradient_concentration/summary_stem_P_concentration_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### Root biomass 
    subDF1 <- subset(bioDF1, Variable=="Root P concentration")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP/100),
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
                   aes(Trt_eP_by_aP, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP/100),
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
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP/100),
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
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100),
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
                   aes(Trt_eC_by_aC, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP/100),
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
                   aes(Trt_eC_by_aC, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP/100),
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
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP/100),
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
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100),
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
    pdf("output/gradient_concentration/summary_root_P_concentration_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    ### Total biomass 
    subDF1 <- subset(bioDF1, Variable=="Total plant P concentration")
    
    ### plot  - elevated P effect at aC and eC
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_eCaP/100),
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
                   aes(Trt_eP_by_aP, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, eCeP_over_aCeP/100),
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
                   aes(Trt_eP_by_aP, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_additive_aCaP/100),
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
                   aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, Interaction_multiplicative_aCaP/100),
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
                   aes(Trt_eC_by_aC, aCeP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_eCaP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, aCeP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_eCaP/100),
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
                   aes(Trt_eC_by_aC, eCaP_over_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, eCeP_over_aCeP/100, col=Vegetation_type),
                   shape=17, size=2) +
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCaP_over_aCaP/100),
                    se=T, method="gam", col="black")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, eCeP_over_aCeP/100),
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
                   aes(Trt_eC_by_aC, Interaction_additive_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_additive_aCaP/100),
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
                   aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100, col=Vegetation_type),
                   shape=19, size=2) +
        geom_abline(intercept=0,slope=1, linetype="dashed")+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, Interaction_multiplicative_aCaP/100),
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
    pdf("output/gradient_concentration/summary_total_P_concentration_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    
}