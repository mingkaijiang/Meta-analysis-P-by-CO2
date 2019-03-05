make_subplots_along_gradients_100 <- function(inDF) {
    
    inDF <- subset(inDF, Trt_eP_by_aP <= 10)
    
    if(!dir.exists("output/subplot_gradient")) {
        dir.create("output/subplot_gradient", showWarnings = FALSE)
    }
    
    ### Make a further subset of the dataframe
    bioDF1 <- inDF[inDF$Variable %in% c("Leaf biomass", "Stem biomass",
                                        "Root biomass", "Total plant biomass",
                                        "Leaf P concentration", "Stem N concentration",
                                        "Root P concentration", "CO2 assimilation rate",
                                        "Plant N uptake", "Plant P uptake"),]
    
    ### Leaf biomass
    subDF1 <- subset(bioDF1, Variable=="Leaf biomass")
    
    ### stem biomass
    subDF2 <- subset(bioDF1, Variable=="Stem biomass")
    
    ### Root biomass
    subDF3 <- subset(bioDF1, Variable=="Root biomass")
    
    ### tpta; biomass
    subDF4 <- subset(bioDF1, Variable=="Total plant biomass")
    
    ### Leaf P concentration
    subDF5 <- subset(bioDF1, Variable=="Leaf P concentration")
    
    ### stem N concentration
    subDF6 <- subset(bioDF1, Variable=="Stem N concentration")
    
    ### root P concentration
    subDF7 <- subset(bioDF1, Variable=="Root P concentration")
    
    ### co2 assimilation
    subDF8 <- subset(bioDF1, Variable=="CO2 assimilation rate")
    
    ### Plant N uptake
    subDF9 <- subset(bioDF1, Variable=="Plant N uptake")
    
    ### Plant P uptake
    subDF10 <- subset(bioDF1, Variable=="Plant P uptake")
    
    
    ### check the slope of linear models
    ### leaf biomass
    m1 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eP_by_aP, data=subDF1)
    s1 <- summary(m1)
    coef1.slope <- coef(s1)[[2]]
    coef1.pval <- coef(s1)[[8]]
    
    m1.2 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eC_by_aC, data=subDF1)
    s1.2 <- summary(m1.2)
    coef1.2.slope <- coef(s1.2)[[2]]
    coef1.2.pval <- coef(s1.2)[[8]]
    
    ## stem biomass
    m2 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eP_by_aP, data=subDF2)
    s2 <- summary(m2)
    coef2.slope <- coef(s2)[[2]]
    coef2.pval <- coef(s2)[[8]]
    
    m2.2 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eC_by_aC, data=subDF2)
    s2.2 <- summary(m2.2)
    coef2.2.slope <- coef(s2.2)[[2]]
    coef2.2.pval <- coef(s2.2)[[8]]
    
    ## root biomass
    m3 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eP_by_aP, data=subDF3)
    s3 <- summary(m3)
    coef3.slope <- coef(s3)[[2]]
    coef3.pval <- coef(s3)[[8]]
    
    m3.2 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eC_by_aC, data=subDF3)
    s3.2 <- summary(m3.2)
    coef3.2.slope <- coef(s3.2)[[2]]
    coef3.2.pval <- coef(s3.2)[[8]]
    
    
    ## total biomass
    m4 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eP_by_aP, data=subDF4)
    s4 <- summary(m4)
    coef4.slope <- coef(s4)[[2]]
    coef4.pval <- coef(s4)[[8]]
    
    m4.2 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eC_by_aC, data=subDF4)
    s4.2 <- summary(m4.2)
    coef4.2.slope <- coef(s4.2)[[2]]
    coef4.2.pval <- coef(s4.2)[[8]]
    
    ## leaf P concentration
    m5 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eP_by_aP, data=subDF5)
    s5 <- summary(m5)
    coef5.slope <- coef(s5)[[2]]
    coef5.pval <- coef(s5)[[8]]
    
    m5.2 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eC_by_aC, data=subDF5)
    s5.2 <- summary(m5.2)
    coef5.2.slope <- coef(s5.2)[[2]]
    coef5.2.pval <- coef(s5.2)[[8]]
    
    ## stem N concentration
    m6 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eP_by_aP, data=subDF6)
    s6 <- summary(m6)
    coef6.slope <- coef(s6)[[2]]
    coef6.pval <- coef(s6)[[8]]
    
    m6.2 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eC_by_aC, data=subDF6)
    s6.2 <- summary(m6.2)
    coef6.2.slope <- coef(s6.2)[[2]]
    coef6.2.pval <- coef(s6.2)[[8]]
    
    ## root P concentration
    m7 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eP_by_aP, data=subDF7)
    s7 <- summary(m7)
    coef7.slope <- coef(s7)[[2]]
    coef7.pval <- coef(s7)[[8]]
    
    m7.2 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eC_by_aC, data=subDF7)
    s7.2 <- summary(m7.2)
    coef7.2.slope <- coef(s7.2)[[2]]
    coef7.2.pval <- coef(s7.2)[[8]]
    
    ## CO2 assimilation
    m8 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eP_by_aP, data=subDF8)
    s8 <- summary(m8)
    coef8.slope <- coef(s8)[[2]]
    coef8.pval <- coef(s8)[[8]]
    
    m8.2 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eC_by_aC, data=subDF8)
    s8.2 <- summary(m8.2)
    coef8.2.slope <- coef(s8.2)[[2]]
    coef8.2.pval <- coef(s8.2)[[8]]
    
    ## N uptake
    m9 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eP_by_aP, data=subDF9)
    s9 <- summary(m9)
    coef9.slope <- coef(s9)[[2]]
    coef9.pval <- coef(s9)[[8]]
    
    m9.2 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eC_by_aC, data=subDF9)
    s9.2 <- summary(m9.2)
    coef9.2.slope <- coef(s9.2)[[2]]
    coef9.2.pval <- coef(s9.2)[[8]]
    
    ## P uptake
    m10 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eP_by_aP, data=subDF10)
    s10 <- summary(m10)
    coef10.slope <- coef(s10)[[2]]
    coef10.pval <- coef(s10)[[8]]
    
    m10.2 <- lm(log(Interaction_multiplicative_aCaP)~Trt_eC_by_aC, data=subDF10)
    s10.2 <- summary(m10.2)
    coef10.2.slope <- coef(s10.2)[[2]]
    coef10.2.pval <- coef(s10.2)[[8]]
    
    
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
        xlim(1,10)+
        annotate("text", x=6.3, y=0.5, label=paste0("s = ", round(coef1.slope, 2)), size=5)+
        annotate("text", x=6.3, y=0.38, label=paste0("p < ", 0.1), size=5)
    
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
        xlim(1,2.5) +
        annotate("text", x=2.3, y=0.6, label=paste0("s = ", round(coef1.2.slope, 2)), size=5)+
        annotate("text", x=2.3, y=0.4, label=paste0("p < ", 0.01), size=5)
        
    ### plot  - stem biomass, P gradient
    p3 <- ggplot() +  
        geom_point(data=subDF2, 
                   aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF2, mapping=aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Stem biomass RR") +
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
    
    
    ### plot  - stem biomass, CO2 gradient
    p4 <- ggplot() +  
        geom_point(data=subDF2, 
                   aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF2, mapping=aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Stem biomass RR") +
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
    p5 <- ggplot() +  
        geom_point(data=subDF3, 
                   aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF3, mapping=aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP)),
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
    
    
    ### plot  - leaf N content, CO2 gradient
    p6 <- ggplot() +  
        geom_point(data=subDF3, 
                   aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF3, mapping=aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Root biomass RR") +
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
    
    
    ### plot  - total biomass, P gradient
    p7 <- ggplot() +  
        geom_point(data=subDF4, 
                   aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF4, mapping=aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus treatment ratio (eP/aP)") + 
        ylab("Total biomass RR") +
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

    
    ### plot  - total biomass, CO2 gradient
    p8 <- ggplot() +  
        geom_point(data=subDF4, 
                   aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF4, mapping=aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " treatment ratio (", eCO[2], "/", aCO[2], ")"))) + 
        ylab("Total biomass RR") +
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
    
    
    ### plot  - leaf P concentration, P gradient
    p9 <- ggplot() +  
        geom_point(data=subDF5, 
                   aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF5, mapping=aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Leaf P conc. RR") +
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
        ylab("Leaf P conc. RR") +
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
    
    ### plot  - stem N concentration, P gradient
    p11 <- ggplot() +  
        geom_point(data=subDF6, 
                   aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF6, mapping=aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Stem N conc. RR") +
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
    
    
    ### plot  - stem N conc, CO2 gradient
    p12 <- ggplot() +  
        geom_point(data=subDF6, 
                   aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF6, mapping=aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Stem N conc. RR") +
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
    
    ### plot  - root P concentration, P gradient
    p13 <- ggplot() +  
        geom_point(data=subDF7, 
                   aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF7, mapping=aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("Root P conc. RR") +
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
    
    
    ### plot  - root P conc, CO2 gradient
    p14 <- ggplot() +  
        geom_point(data=subDF7, 
                   aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF7, mapping=aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("Root P conc. RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))+
        xlim(1,2.5)
    
    ### plot  - co2 assimilation , P gradient
    p15 <- ggplot() +  
        geom_point(data=subDF8, 
                   aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF8, mapping=aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("CO2 assimilation RR") +
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
    
    
    ### plot  - co2 assimilation, CO2 gradient
    p16 <- ggplot() +  
        geom_point(data=subDF8, 
                   aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF8, mapping=aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("CO2 assimilation RR") +
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
    
    ### plot  - n uptake , P gradient
    p17 <- ggplot() +  
        geom_point(data=subDF9, 
                   aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF9, mapping=aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("N uptake RR") +
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
        xlim(1,10) +
        annotate("text", x=6.3, y=0.5, label=paste0("s = ", round(coef9.slope, 2)), size=5)+
        annotate("text", x=6.3, y=0.4, label=paste0("p < ", 0.05), size=5)
    
    
    ### plot  - n uptake, CO2 gradient
    p18 <- ggplot() +  
        geom_point(data=subDF9, 
                   aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF9, mapping=aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("N uptake RR") +
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
    
    ### plot  - p uptake , P gradient
    p19 <- ggplot() +  
        geom_point(data=subDF10, 
                   aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF10, mapping=aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("P uptake RR") +
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
    
    
    ### plot  - p uptake, CO2 gradient
    p20 <- ggplot() +  
        geom_point(data=subDF10, 
                   aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF10, mapping=aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("P uptake RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))+
        xlim(1,2.5)
    
    ### summary histgram of treatments
    pdf("output/subplot_gradient/summary_gradient_subplot_100_biomass_plot.pdf", width=8, height=14)
    
    plot_grid(p1, p2, p3, p4, p5, p6, p7, p8, 
              labels="AUTO", ncol=2, align="h", axis = "l", rel_widths=c(1.2, 1),
              label_x = 0.9, label_y=0.95)
    dev.off()
    
    
    ### summary histgram of treatments
    pdf("output/subplot_gradient/summary_gradient_subplot_100_concentration_plot.pdf", width=8, height=12)
    
    plot_grid(p9, p10, p11, p12, p13, p14,
              labels="AUTO", ncol=2, align="h", axis = "l", rel_widths=c(1.2, 1),
              label_x = 0.9, label_y=0.95)
    dev.off()

    ### summary histgram of treatments
    pdf("output/subplot_gradient/summary_gradient_subplot_100_other_plot.pdf", width=8, height=16)
    
    plot_grid(p15, p16, p17, p18, p19, p20,
              labels="AUTO", ncol=2, align="h", axis = "l", rel_widths=c(1.2, 1),
              label_x = 0.9, label_y=0.95)
    dev.off()
    
    
    ### plot  - leaf biomass, P gradient
    p21 <- ggplot() +  
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
        xlim(1,10)+
        annotate("text", x=6.3, y=0.5, label=paste0("s = ", round(coef1.slope, 2)), size=5)+
        annotate("text", x=6.3, y=0.38, label=paste0("p < ", 0.1), size=5)+
        ylim(-0.5, 1.5)
    
    ### plot  - leaf biomass, CO2 gradient
    p22 <- ggplot() +  
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
        xlim(1,2.5) +
        annotate("text", x=2.3, y=0.6, label=paste0("s = ", round(coef1.2.slope, 2)), size=5)+
        annotate("text", x=2.3, y=0.4, label=paste0("p < ", 0.01), size=5)+
        ylim(-0.5, 1.5)
    
    ### plot  - n uptake , P gradient
    p23 <- ggplot() +  
        geom_point(data=subDF9, 
                   aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF9, mapping=aes(Trt_eP_by_aP, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab("Phosphorus Addition Ratio") + 
        ylab("N uptake RR") +
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
        xlim(1,10) +
        annotate("text", x=6.3, y=0.45, label=paste0("s = ", round(coef9.slope, 2)), size=5)+
        annotate("text", x=6.3, y=0.4, label=paste0("p < ", 0.05), size=5)+
        ylim(-0.5, 0.5)
    
    
    ### plot  - n uptake, CO2 gradient
    p24 <- ggplot() +  
        geom_point(data=subDF9, 
                   aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP), col=Vegetation_type),
                   shape=19, size=2) +
        geom_hline(yintercept=0, linetype=2)+
        geom_smooth(data=subDF9, mapping=aes(Trt_eC_by_aC, log(Interaction_multiplicative_aCaP)),
                    se=T, method="gam", col="black")+
        xlab(expression(paste(CO[2], " Enrichment Ratio"))) + 
        ylab("N uptake RR") +
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=8),
              legend.title=element_text(size=10),
              panel.grid.major=element_blank(),
              legend.position="none",
              legend.text.align=0)+
        guides(col=guide_legend(nrow=2,bycol=TRUE))+
        xlim(1,2.5)+
        ylim(-0.5, 0.5)
    
    ### summary histgram of treatments
    pdf("output/subplot_gradient/summary_gradient_subplot_100_significant_plot.pdf", width=8, height=8)
    
    plot_grid(p21, p22, p23, p24,
              labels="AUTO", ncol=2, align="h", axis = "l", rel_widths=c(1.2, 1),
              label_x = 0.9, label_y=0.95)
    dev.off()
 }
