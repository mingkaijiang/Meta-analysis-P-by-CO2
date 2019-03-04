make_subplots_along_gradients <- function(inDF) {
    
    if(!dir.exists("output/subplot_gradient")) {
        dir.create("output/subplot_gradient", showWarnings = FALSE)
    }
    
    ### Make a further subset of the dataframe
    bioDF1 <- inDF[inDF$Variable %in% c("Leaf biomass", "Root biomass", "Leaf N content",
                                        "Leaf P concentration", "Plant P uptake"),]
    
    ### Leaf biomass
    subDF1 <- subset(bioDF1, Variable=="Leaf biomass")
    
    ### Root biomass
    subDF2 <- subset(bioDF1, Variable=="Root biomass")
    
    ### Leaf N biomass
    subDF3 <- subset(bioDF1, Variable=="Leaf N content")
    
    ### Leaf P concentration
    subDF4 <- subset(bioDF1, Variable=="Leaf P concentration")
    
    ### Plant P uptake
    subDF5 <- subset(bioDF1, Variable=="Plant P uptake")
    
    
    ### check the slope of gam models
    ### to be filled
    
    
    
    ### plot  - leaf biomass, P gradient
    p1 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF1, mapping=aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Leaf biomass RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.4, 0.9),
              legend.background = element_rect(fill="gray90", size=.5, linetype="dotted"))+
        guides(col=guide_legend(nrow=2,bycol=TRUE, title="Vegetation type"))+
        xlim(1,10)
    
    ### plot  - leaf biomass, CO2 gradient
    p2 <- ggplot() +  
        geom_point(data=subDF1, 
                   aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF1, mapping=aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Leaf biomass RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
              guides(col=guide_legend(nrow=2,bycol=TRUE))+
        xlim(1,2.5)
    
    ### plot  - root biomass, P gradient
    p3 <- ggplot() +  
        geom_point(data=subDF2, 
                   aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF2, mapping=aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Root biomass RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))+
        xlim(1,10)
    
    
    ### plot  - root biomass, CO2 gradient
    p4 <- ggplot() +  
        geom_point(data=subDF2, 
                   aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF2, mapping=aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Leaf biomass RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))+
        xlim(1,2.5)
    
    
    ### plot  - leaf N content, P gradient
    p5 <- ggplot() +  
        geom_point(data=subDF3, 
                   aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF3, mapping=aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Leaf N content RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))+
        xlim(1,10)
    
    
    ### plot  - leaf N content, CO2 gradient
    p6 <- ggplot() +  
        geom_point(data=subDF3, 
                   aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF3, mapping=aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Leaf N content RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))+
        xlim(1,2.5)
    
    
    ### plot  - leaf biomass, P gradient
    p7 <- ggplot() +  
        geom_point(data=subDF4, 
                   aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF4, mapping=aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus treatment ratio (eP/aP)") + 
        ylab("Leaf P concentration RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))+
        xlim(1,10)

    
    ### plot  - leaf biomass, CO2 gradient
    p8 <- ggplot() +  
        geom_point(data=subDF4, 
                   aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF4, mapping=aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " treatment ratio (", eCO[2], "/", aCO[2], ")"))) + 
        ylab("Leaf P concentration RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="none")+
        guides(col=guide_legend(nrow=2,bycol=TRUE))+
        xlim(1,2.5)
    
    
    ### plot  - leaf biomass, P gradient
    p9 <- ggplot() +  
        geom_point(data=subDF5, 
                   aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF5, mapping=aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Plant P uptake RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))+
        xlim(1,10)
    
    
    ### plot  - leaf biomass, CO2 gradient
    p10 <- ggplot() +  
        geom_point(data=subDF5, 
                   aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF5, mapping=aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Plant P uptake RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))+
        xlim(1,3)
    
    ### summary histgram of treatments
    pdf("output/subplot_gradient/summary_gradient_subplot_overall_plot.pdf", width=8, height=16)
    
    plot_grid(p1, p2, p3, p4, p5, p6, p9, p10, p7, p8,
              labels="AUTO", ncol=2, align="h", axis = "l", rel_widths=c(1.2, 1),
              label_x = 0.9, label_y=0.95)
    dev.off()
    

    
}
