make_nutrient_uptake_plots_along_gradients <- function(inDF) {
    
    if(!dir.exists("output/gradient_nutrient_uptake")) {
        dir.create("output/gradient_nutrient_uptake", showWarnings = FALSE)
    }
    
    ### Subset biomass category
    bioDF <- subset(inDF, Category == "Nutrient Uptake")
    
    ########## Make a further subset of the dataframe
    bioDF1 <- bioDF[bioDF$Variable %in% c("Plant N uptake", "Plant P uptake"),]
    
    ### Plant N uptake
    subDF1 <- subset(bioDF1, Variable=="Plant N uptake")
    
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
    pdf("output/gradient_nutrient_uptake/summary_N_uptake_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    

    ### Plant P uptake
    subDF1 <- subset(bioDF1, Variable=="Plant P uptake")
    
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
    pdf("output/gradient_nutrient_uptake/summary_P_uptake_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p5, p2, p6, p3, p7, p4, p8,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5))
    dev.off()
    
    
    
    
}