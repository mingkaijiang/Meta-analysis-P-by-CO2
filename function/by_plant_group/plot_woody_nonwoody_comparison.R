plot_woody_nonwoody_comparison <- function(intDF2, intDF3, sumDF2, sumDF3) {
    
    ### assign PFT
    intDF2$PFT <- "woody"
    intDF3$PFT <- "nonwoody"
    intDF4 <- rbind(intDF2, intDF3)
    
    sumDF2$PFT <- "woody"
    sumDF3$PFT <- "nonwoody"
    sumDF4 <- rbind(sumDF2, sumDF3)
    
    ### get %
    intDF4$interaction <- intDF4$interaction * 100
    intDF4$se <- intDF4$se * 100
    intDF4$pos <- intDF4$interaction + (intDF4$se * 1.96)
    intDF4$neg <- intDF4$interaction - (intDF4$se * 1.96)
    
    sumDF4$CO2_effect <- sumDF4$CO2_effect * 100
    sumDF4$se <- sumDF4$se * 100
    
    sumDF4$pos <- sumDF4$CO2_effect + (sumDF4$se * 1.96)
    sumDF4$neg <- sumDF4$CO2_effect - (sumDF4$se * 1.96)
    
    
    ### arrange plot order
    intDF4$id <- c(1.8, 0, 0, 4.8, 2.8, 3.8, 0, 0, 5.8, 7.8, 6.8, 8.8, 0.8, 9.8, 0, 10.8, 11.8,
                   2.2, 0, 0, 5.2, 3.2, 4.2, 0, 0, 6.2, 8.2, 7.2, 9.2, 1.2, 10.2, 0, 11.2, 12.2)
    intDF4 <- intDF4[intDF4$id > 0, ]
    
    
    plotDF1 <- subset(intDF4, variable%in%c("CO2_assimilation_rate", "leaf_area", "LMA", 
                                            "Root_length"))
    plotDF1$id <- c(0.8, 1.8, 2.8, 3.8, 1.2, 2.2, 3.2, 4.2)
    
    plotDF2 <- subset(intDF4, variable%in%c("leaf_biomass", "total_biomass", 
                                            "aboveground_biomass", "belowground_biomass"))
    
    plotDF3 <- subset(intDF4, variable%in%c("leaf_N_concentration", "leaf_P_concentration", 
                                            "root_N_concentration", "root_P_concentration"))
    

    ## select a subset of sumDF4
    plotDF4 <- subset(sumDF4, PFT=="woody" & variable%in%c("CO2_assimilation_rate", "leaf_area", "LMA",
                                                          "Root_length"))
    
    plotDF5 <- subset(sumDF4, PFT=="nonwoody" & variable%in%c("CO2_assimilation_rate", "leaf_area", "LMA",
                                                           "Root_length"))
    
    plotDF6 <- subset(sumDF4, PFT=="woody" & variable%in%c("leaf_biomass", "total_biomass", 
                                                           "aboveground_biomass", "belowground_biomass"))
    
    plotDF7 <- subset(sumDF4, PFT=="nonwoody" & variable%in%c("leaf_biomass", "total_biomass", 
                                                           "aboveground_biomass", "belowground_biomass"))
    
    plotDF8 <- subset(sumDF4, PFT=="woody" & variable%in%c("leaf_N_concentration", "leaf_P_concentration", 
                                                           "root_N_concentration", "root_P_concentration"))
    
    plotDF9 <- subset(sumDF4, PFT=="nonwoody" & variable%in%c("leaf_N_concentration", "leaf_P_concentration", 
                                                           "root_N_concentration", "root_P_concentration"))
    
    plotDF4$id <- plotDF5$id <- plotDF6$id <- plotDF7$id <- plotDF8$id <- plotDF9$id <- c(0.9, 1.1, 1.9, 2.1, 2.9, 3.1, 3.9, 4.1)
    
    
    
    ### plot effect of LP on eCO2 response
    p1a <- ggplot(plotDF1, aes(id, interaction))+ 
        geom_hline(yintercept = 0.0)+
        #geom_bar(stat = "identity", aes(fill=PFT), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(PFT)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=plotDF1, mapping=aes(x=id, y=interaction, fill=PFT), 
                   size=6, shape=21, color="black")+
        labs(y="", x="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_y_continuous(limits=c(-30, 30))+
        scale_x_continuous(breaks=c(1:4),
                           labels=c("A", "LA", "LMA", "RL"))+
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
    
    
    
    p1b <- ggplot(plotDF2, aes(id, interaction))+ 
        geom_hline(yintercept = 0.0)+
        #geom_bar(stat = "identity", aes(fill=PFT), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(PFT)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=plotDF2, mapping=aes(x=id, y=interaction, fill=PFT), 
                   size=6, shape=21, color="black")+
        labs(y=expression(paste("Effect of LP on ", eCO[2], " response (%)")), x="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_y_continuous(limits=c(-75, 50))+
        scale_x_continuous(breaks=c(2:5),
                           labels=c("Leaf","Aboveground", "Belowground", "Total"))+
        scale_color_manual(name=paste("Vegetation group"),
                           limits=c("woody", "nonwoody"),
                           values=c("black", "grey"),
                           labels=c("Woody", "Nonwoody"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("Vegetation group"),
                          limits=c("woody", "nonwoody"),
                          values=c("black", "grey"),
                          labels=c("Woody", "Nonwoody"))+
        ggtitle("b")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    p1c <- ggplot(plotDF3, aes(id, interaction))+ 
        geom_hline(yintercept = 0.0)+
        #geom_bar(stat = "identity", aes(fill=PFT), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(PFT)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=plotDF3, mapping=aes(x=id, y=interaction, fill=PFT), 
                   size=6, shape=21, color="black")+
        labs(y="", x="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "bottom",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_y_continuous(limits=c(-50, 50))+
        scale_x_continuous(breaks=c(6:9),
                           labels=c("Leaf N", "Leaf P", "Root N", "Root P"))+
        scale_color_manual(name=paste("Vegetation group"),
                           limits=c("woody", "nonwoody"),
                           values=c("black", "grey"),
                           labels=c("Woody", "Nonwoody"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("Vegetation group"),
                          limits=c("woody", "nonwoody"),
                          values=c("black", "grey"),
                          labels=c("Woody", "Nonwoody"))+
        ggtitle("c")+
        guides(fill = guide_legend(title.position = "top"))
    
    

    
    pdf("output/metafor_summary_plot/Figure7_woody_nonwoody_comparison_LP_effect_on_eCO2_response.pdf", width=8, height=16)
    plot_grid(p1a, p1b, p1c,
              #rel_widths=c(0.5, 1, 1, 0.9),
              rel_heights=c(1,1,1.4),
              labels=c(""), ncol=1, align="v", axis = "l")    
    dev.off()
    
    
    
    
    ### plot effect of LP on eCO2 response
    p2a <- ggplot(plotDF4, aes(id, CO2_effect))+ 
        geom_hline(yintercept = 0.0)+
        #geom_bar(stat = "identity", aes(fill=PFT), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(P_treatment)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=plotDF4, mapping=aes(x=id, y=CO2_effect, fill=P_treatment), 
                   size=6, shape=21, color="black")+
        labs(y="", x="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_y_continuous(limits=c(-50, 100))+
        scale_x_continuous(breaks=c(1:4),
                           labels=c("A", "LA", "LMA", "RL"))+
        scale_color_manual(name=paste("P treatment"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"),)+        
        ggtitle("a")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    
    p2b <- ggplot(plotDF5, aes(id, CO2_effect))+ 
        geom_hline(yintercept = 0.0)+
        #geom_bar(stat = "identity", aes(fill=PFT), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(P_treatment)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=plotDF5, mapping=aes(x=id, y=CO2_effect, fill=P_treatment), 
                   size=6, shape=21, color="black")+
        labs(y="", x="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_y_continuous(limits=c(-50, 100))+
        scale_x_continuous(breaks=c(1:4),
                           labels=c("A", "LA", "LMA", "RL"))+
        scale_color_manual(name=paste("P treatment"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"),)+        
        ggtitle("b")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    ### plot effect of LP on eCO2 response
    p2c <- ggplot(plotDF6, aes(id, CO2_effect))+ 
        geom_hline(yintercept = 0.0)+
        #geom_bar(stat = "identity", aes(fill=PFT), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(P_treatment)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=plotDF6, mapping=aes(x=id, y=CO2_effect, fill=P_treatment), 
                   size=6, shape=21, color="black")+
        labs(y=expression(paste("Effect of ", eCO[2], " (%)")), x="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_y_continuous(limits=c(-25, 100))+
        scale_x_continuous(breaks=c(1:4),
                           labels=c("Leaf", "Aboveground", "Belowground", "Total"))+
        scale_color_manual(name=paste("P treatment"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"),)+        
        ggtitle("c")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    
    p2d <- ggplot(plotDF7, aes(id, CO2_effect))+ 
        geom_hline(yintercept = 0.0)+
        #geom_bar(stat = "identity", aes(fill=PFT), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(P_treatment)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=plotDF7, mapping=aes(x=id, y=CO2_effect, fill=P_treatment), 
                   size=6, shape=21, color="black")+
        labs(y="", x="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_y_continuous(limits=c(-25, 100))+
        scale_x_continuous(breaks=c(1:4),
                           labels=c("Leaf", "Aboveground", "Belowground", "Total"))+
        scale_color_manual(name=paste("P treatment"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"),)+        
        ggtitle("d")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    ### plot effect of LP on eCO2 response
    p2e <- ggplot(plotDF8, aes(id, CO2_effect))+ 
        geom_hline(yintercept = 0.0)+
        #geom_bar(stat = "identity", aes(fill=PFT), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(P_treatment)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=plotDF8, mapping=aes(x=id, y=CO2_effect, fill=P_treatment), 
                   size=6, shape=21, color="black")+
        labs(y="", x="Woody plants")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "bottom",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_y_continuous(limits=c(-100, 50))+
        scale_x_continuous(breaks=c(1:4),
                           labels=c("Leaf N", "Root N", "Leaf P", "Root P"))+
        scale_color_manual(name=paste("P treatment"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"),)+        
        ggtitle("e")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    
    p2f <- ggplot(plotDF9, aes(id, CO2_effect))+ 
        geom_hline(yintercept = 0.0)+
        #geom_bar(stat = "identity", aes(fill=PFT), position="dodge") +
        geom_errorbar(aes(ymax=pos, ymin=neg, color=factor(P_treatment)), 
                      position = position_dodge(0.9), width=0.2, size=0.4) +
        geom_point(data=plotDF9, mapping=aes(x=id, y=CO2_effect, fill=P_treatment), 
                   size=6, shape=21, color="black")+
        labs(y="", x="Nonwoody plants")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "bottom",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_y_continuous(limits=c(-100, 50))+
        scale_x_continuous(breaks=c(1:4),
                           labels=c("Leaf N", "Root N", "Leaf P", "Root P"))+
        scale_color_manual(name=paste("P treatment"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"),)+        
        ggtitle("f")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    pdf("output/metafor_summary_plot/Figure6_woody_nonwoody_comparison_eCO2_effect.pdf", width=12, height=16)
    plot_grid(p2a, p2b, 
              p2c, p2d,
              p2e, p2f,
              rel_widths=c(1, 0.9),
              rel_heights=c(1,1,1.4),
              labels=c(""), ncol=2, align="h", axis = "l")    
    dev.off()
    
}