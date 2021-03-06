make_step3_woody_nonwoody_comparison_chart <- function(intDF.wd, intDF.nwd, sumDF.wd, sumDF.nwd) {
    
    ### assign PFT
    intDF.wd$PFT <- "woody"
    intDF.nwd$PFT <- "nonwoody"
    intDF4 <- rbind(intDF.wd, intDF.nwd)
    
    sumDF.wd$PFT <- "woody"
    sumDF.nwd$PFT <- "nonwoody"
    sumDF4 <- rbind(sumDF.wd, sumDF.nwd)
    
    
    intDF4$pos <- intDF4$ci_ub_pct
    intDF4$neg <- intDF4$ci_lb_pct
    intDF4$interaction <- intDF4$int_pct
    
    
    
    sumDF4$pos <- sumDF4$ci_ub_pct
    sumDF4$neg <- sumDF4$ci_lb_pct
    sumDF4$CO2_effect <- sumDF4$CO2_effect_pct
    
    
    write.csv(sumDF4, "output/step3/co2_effect_woody_nonwoody.csv", row.names=F)
    write.csv(intDF4, "output/step3/lp_effect_on_co2_response_woody_nonwoody.csv", row.names=F)
    
    ### arrange plot order
    intDF4$id <- c(1.8, 0, 0, 4.8, 2.8, 3.8, 0, 0, 5.8, 7.8, 6.8, 8.8, 0.8, 9.8, 0, 10.8, 11.8,
                   2.2, 0, 0, 5.2, 3.2, 4.2, 0, 0, 6.2, 8.2, 7.2, 9.2, 1.2, 10.2, 0, 11.2, 12.2)
    intDF4 <- intDF4[intDF4$id > 0, ]
    
    
    plotDF1 <- subset(intDF4, variable%in%c("CO2_assimilation_rate", "leaf_area",  
                                            "Root_length"))
    plotDF1$id <- c(2.8, 1.8, 0.8, 3.2, 2.2, 1.2)
    
    plotDF2 <- subset(intDF4, variable%in%c("aboveground_biomass", 
                                            "belowground_biomass"))
    
    plotDF2$id <- c(2.8, 1.8, 3.2, 2.2)
    
    
    plotDF3 <- subset(intDF4, variable%in%c("leaf_N_concentration", "leaf_P_concentration", 
                                            "root_N_concentration", "root_P_concentration"))
    
    plotDF3$id <- c(3.8, 2.8, 1.8, 0.8, 4.2, 3.2, 2.2, 1.2)
    
    

    ## select a subset of sumDF4
    plotDF4 <- subset(sumDF4, PFT=="woody" & variable%in%c("CO2_assimilation_rate", "leaf_area", 
                                                          "Root_length"))
    
    plotDF5 <- subset(sumDF4, PFT=="nonwoody" & variable%in%c("CO2_assimilation_rate", "leaf_area", 
                                                           "Root_length"))
    
    plotDF6 <- subset(sumDF4, PFT=="woody" & variable%in%c("aboveground_biomass", 
                                                           "belowground_biomass"))
    
    plotDF7 <- subset(sumDF4, PFT=="nonwoody" & variable%in%c("aboveground_biomass", 
                                                              "belowground_biomass"))
    
    plotDF8 <- subset(sumDF4, PFT=="woody" & variable%in%c("leaf_N_concentration", "leaf_P_concentration", 
                                                           "root_N_concentration", "root_P_concentration"))
    
    plotDF9 <- subset(sumDF4, PFT=="nonwoody" & variable%in%c("leaf_N_concentration", "leaf_P_concentration", 
                                                           "root_N_concentration", "root_P_concentration"))
    
    plotDF8$id <- plotDF9$id <- c(3.9, 4.1, 2.9, 3.1, 1.9, 2.1, 0.9, 1.1)
    
    plotDF4$id <- plotDF5$id <- c(2.9, 3.1, 1.9, 2.1, 0.9, 1.1)
    
    
    plotDF6$id <- plotDF7$id <- c(2.9, 3.1, 1.9, 2.1)

    ### plot effect of LP on eCO2 response
    p1a <- ggplot(plotDF1)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=neg, xmax=pos, color=factor(PFT))) + 
        geom_point(aes(y=id, x=interaction, fill=PFT), 
                   size=8, shape=21)+
        labs(x="", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
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
        scale_x_continuous(limits=c(-50, 80))+
        scale_y_continuous(breaks=c(1:3),
                           labels=c("RL", "LA", "A"))+
        scale_color_manual(name=paste("Vegetation group"),
                           limits=c("woody", "nonwoody"),
                           values=c("black", "grey"),
                           labels=c("Woody", "Nonwoody"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("Vegetation group"),
                          limits=c("woody", "nonwoody"),
                          values=c("black", "grey"),
                          labels=c("Woody", "Nonwoody"))+        
        ggtitle("(a)")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    
    p1b <- ggplot(plotDF2)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=neg, xmax=pos, color=factor(PFT))) + 
        geom_point(aes(y=id, x=interaction, fill=PFT), 
                   size=8, shape=21)+
        labs(x="", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
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
        scale_x_continuous(limits=c(-50, 80))+
        scale_y_continuous(breaks=c(2:3),
                           labels=c("    BG", "AG"))+
        scale_color_manual(name=paste("Vegetation group"),
                           limits=c("woody", "nonwoody"),
                           values=c("black", "grey"),
                           labels=c("Woody", "Nonwoody"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("Vegetation group"),
                          limits=c("woody", "nonwoody"),
                          values=c("black", "grey"),
                          labels=c("Woody", "Nonwoody"))+        
        ggtitle("(b)")+
        guides(fill = guide_legend(title.position = "top"))
    
    #plot(p1b)
    
 
    
    
    p1c <- ggplot(plotDF3)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=neg, xmax=pos, color=factor(PFT))) + 
        geom_point(aes(y=id, x=interaction, fill=PFT), 
                   size=8, shape=21)+
        labs(x=expression("LP effect on " * eCO[2] * " response (%)"), y="")+
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
              legend.position = "bottom",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-50, 80))+
        scale_y_continuous(breaks=c(1:4),
                           labels=c("Root P", "Leaf P", "Root N", "Leaf N"))+
        scale_color_manual(name=paste("Vegetation group"),
                           limits=c("woody", "nonwoody"),
                           values=c("black", "grey"),
                           labels=c("Woody", "Nonwoody"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("Vegetation group"),
                          limits=c("woody", "nonwoody"),
                          values=c("black", "grey"),
                          labels=c("Woody", "Nonwoody"))+        
        ggtitle("(c)")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    #plot(p1c)
    
    #pdf("output/step3/Figure_S10_woody_nonwoody_comparison_LP_effect_on_eCO2_response.pdf", 
    #    width=10, height=10)
    #plot_grid(p1a, p1b, p1c,
    #          #rel_widths=c(0.5, 1, 1, 0.9),
    #          rel_heights=c(0.4,0.3,1.0),
    #          labels=c(""), ncol=1, align="v", axis = "l")    
    #dev.off()
    
    
    
    
    ### plot effect of LP on eCO2 response
    p2a <- ggplot(plotDF4, aes(id, CO2_effect))+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=neg, xmax=pos, color=factor(P_treatment))) + 
        geom_point(data=plotDF4, mapping=aes(y=id, x=CO2_effect, fill=P_treatment), 
                   size=4, shape=21, color="black")+
        labs(y="", x="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=18),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 16, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-65, 150))+
        scale_y_continuous(breaks=c(1:3),
                           labels=c("        RL", "LA", "A"))+
        scale_color_manual(name=paste("P treatment"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"),)+        
        ggtitle("(a)")+
        guides(fill = guide_legend(title.position = "top"))
    
    #plot(p2a)
    
    
    p2b <- ggplot(plotDF5, aes(id, CO2_effect))+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=neg, xmax=pos, color=factor(P_treatment))) + 
        geom_point(data=plotDF5, mapping=aes(y=id, x=CO2_effect, fill=P_treatment), 
                   size=4, shape=21, color="black")+
        labs(y="", x="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 16, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-65, 65))+
        scale_y_continuous(breaks=c(1:3),
                           labels=c("RL", "LA", "A"))+
        scale_color_manual(name=paste("P treatment"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"),)+        
        ggtitle("(b)")+
        guides(fill = guide_legend(title.position = "top"))
    
    #plot(p2b)
    
    
    ### plot effect of LP on eCO2 response
    p2c <- ggplot(plotDF6, aes(id, CO2_effect))+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=neg, xmax=pos, color=factor(P_treatment))) + 
        geom_point(data=plotDF6, mapping=aes(y=id, x=CO2_effect, fill=P_treatment), 
                   size=4, shape=21, color="black")+
        labs(y="", x="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=18),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 16, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-65, 150))+
        scale_y_continuous(breaks=c(2:3),
                           labels=c("      BG", "AG"))+
        scale_color_manual(name=paste("P treatment"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"),)+        
        ggtitle("(c)")+
        guides(fill = guide_legend(title.position = "top"))
    
    #plot(p2c)
    
    
    
    p2d <- ggplot(plotDF7, aes(id, CO2_effect))+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=neg, xmax=pos, color=factor(P_treatment))) + 
        geom_point(data=plotDF7, mapping=aes(y=id, x=CO2_effect, fill=P_treatment), 
                   size=4, shape=21, color="black")+
        labs(y="", x="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 16, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-65, 65))+
        scale_y_continuous(breaks=c(2:3),
                           labels=c("BG", "AG"))+
        scale_color_manual(name=paste("P treatment"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"),)+        
        ggtitle("(d)")+
        guides(fill = guide_legend(title.position = "top"))
    
    #plot(p2d)
    
    
    ### plot effect of LP on eCO2 response
    p2e <- ggplot(plotDF8, aes(id, CO2_effect))+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=neg, xmax=pos, color=factor(P_treatment))) + 
        geom_point(data=plotDF8, mapping=aes(y=id, x=CO2_effect, fill=P_treatment), 
                   size=4, shape=21, color="black")+
        labs(y="", x=expression("Woody " * eCO[2] * " response (%)"))+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=16), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_text(size=18),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position = "bottom",
              plot.title = element_text(size = 16, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-65, 150))+
        scale_y_continuous(breaks=c(1:4),
                           labels=c("Root P", "Leaf P", "Root N", "Leaf N"))+
        scale_color_manual(name=paste("P treatment"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"),)+        
        ggtitle("(e)")+
        guides(fill = guide_legend(title.position = "top"))
    
    #plot(p2e)
    
    
    
    
    
    p2f <- ggplot(plotDF9, aes(id, CO2_effect))+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=neg, xmax=pos, color=factor(P_treatment))) + 
        geom_point(data=plotDF9, mapping=aes(y=id, x=CO2_effect, fill=P_treatment), 
                   size=4, shape=21, color="black")+
        labs(y="", x=expression("Non-woody " * eCO[2] * " response (%)"))+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=16), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=14),
              legend.title=element_text(size=16),
              panel.grid.major=element_blank(),
              legend.position = "bottom",
              plot.title = element_text(size = 16, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-65, 65))+
        scale_y_continuous(breaks=c(1:4),
                           labels=c("Root P", "Leaf P", "Root N", "Leaf N"))+
        scale_color_manual(name=paste("P treatment"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"),)+        
        ggtitle("(f)")+
        guides(fill = guide_legend(title.position = "top"))
    
    #pdf("output/step3/Figure_S11_woody_nonwoody_comparison_eCO2_effect.pdf", 
    #    width=8, height=8)
    #plot_grid(p2a, p2b, 
    #          p2c, p2d,
    #          p2e, p2f,
    #          rel_widths=c(1,0.8),
    #          rel_heights=c(0.4,0.3,0.8),
    #          labels=c(""), ncol=2, align="h", axis = "l")    
    #dev.off()
    
    
    
    ### a different way of plotting main text figure
    ### just show results that are statistically significant
    plotDF10 <- subset(intDF4, variable=="aboveground_biomass")
    plotDF13 <- subset(intDF4, variable=="leaf_N_concentration")
    
    plotDF11 <- subset(sumDF4, P_treatment == "eP" & variable=="aboveground_biomass")
    plotDF12 <- subset(sumDF4, P_treatment == "aP" & variable=="aboveground_biomass")
    
    plotDF14 <- subset(sumDF4, P_treatment == "eP" & variable=="leaf_N_concentration")
    plotDF15 <- subset(sumDF4, P_treatment == "aP" & variable=="leaf_N_concentration")
    
    
    ### plot effect of LP on eCO2 response
    p1a <- ggplot(plotDF10)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=PFT, xmin=neg, xmax=pos, color=factor(PFT)), height=0.2) + 
        geom_point(aes(y=PFT, x=interaction, fill=PFT), 
                   size=8, shape=21)+
        labs(x="", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 18, face = "bold"),
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-40, 20))+
        scale_y_discrete(breaks=c("woody", "nonwoody"),
                           labels=c("WD", "NWD"))+
        scale_color_manual(name=paste("Vegetation group"),
                           limits=c("woody", "nonwoody"),
                           values=c("black", "grey"),
                           labels=c("Woody", "Nonwoody"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("Vegetation group"),
                          limits=c("woody", "nonwoody"),
                          values=c("black", "grey"),
                          labels=c("Woody", "Nonwoody"))+        
        ggtitle("(c)")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    p1d <- ggplot(plotDF13)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=PFT, xmin=neg, xmax=pos, color=factor(PFT)), height=0.2) + 
        geom_point(aes(y=PFT, x=interaction, fill=PFT), 
                   size=8, shape=21)+
        labs(x=expression("LP effect on " * eCO[2] * " response (%)"), 
             y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=16), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 18, face = "bold"),
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-40, 20))+
        scale_y_discrete(breaks=c("woody", "nonwoody"),
                         labels=c("WD", "NWD"))+
        scale_color_manual(name=paste("Vegetation group"),
                           limits=c("woody", "nonwoody"),
                           values=c("black", "grey"),
                           labels=c("Woody", "Nonwoody"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("Vegetation group"),
                          limits=c("woody", "nonwoody"),
                          values=c("black", "grey"),
                          labels=c("Woody", "Nonwoody"))+        
        ggtitle("(f)")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    p1b <- ggplot(plotDF11)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=PFT, xmin=neg, xmax=pos, color=factor(PFT)), height=0.2) + 
        geom_point(aes(y=PFT, x=CO2_effect, fill=PFT), 
                   size=8, shape=21)+
        labs(x="", y="AG")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 18, face = "bold"),
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-50, 120))+
        scale_y_discrete(breaks=c("woody", "nonwoody"),
                         labels=c("WD", "NWD"))+
        scale_color_manual(name=paste("Vegetation group"),
                           limits=c("woody", "nonwoody"),
                           values=c("black", "grey"),
                           labels=c("Woody", "Nonwoody"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("Vegetation group"),
                          limits=c("woody", "nonwoody"),
                          values=c("black", "grey"),
                          labels=c("Woody", "Nonwoody"))+        
        ggtitle("(a)")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    p1e <- ggplot(plotDF14)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=PFT, xmin=neg, xmax=pos, color=factor(PFT)), height=0.2) + 
        geom_point(aes(y=PFT, x=CO2_effect, fill=PFT), 
                   size=8, shape=21)+
        labs(x=expression(eCO[2] * " response under HP (%)"), y="Leaf N")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=16), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 18, face = "bold"),
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-50, 120))+
        scale_y_discrete(breaks=c("woody", "nonwoody"),
                         labels=c("WD", "NWD"))+
        scale_color_manual(name=paste("Vegetation group"),
                           limits=c("woody", "nonwoody"),
                           values=c("black", "grey"),
                           labels=c("Woody", "Nonwoody"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("Vegetation group"),
                          limits=c("woody", "nonwoody"),
                          values=c("black", "grey"),
                          labels=c("Woody", "Nonwoody"))+        
        ggtitle("(d)")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    p1c <- ggplot(plotDF12)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=PFT, xmin=neg, xmax=pos, color=factor(PFT)), height=0.2) + 
        geom_point(aes(y=PFT, x=CO2_effect, fill=PFT), 
                   size=8, shape=21)+
        labs(x="", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 18, face = "bold"),
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-50, 120))+
        scale_y_discrete(breaks=c("woody", "nonwoody"),
                         labels=c("WD", "NWD"))+
        scale_color_manual(name=paste("Vegetation group"),
                           limits=c("woody", "nonwoody"),
                           values=c("black", "grey"),
                           labels=c("Woody", "Nonwoody"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("Vegetation group"),
                          limits=c("woody", "nonwoody"),
                          values=c("black", "grey"),
                          labels=c("Woody", "Nonwoody"))+        
        ggtitle("(b)")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    p1f <- ggplot(plotDF15)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=PFT, xmin=neg, xmax=pos, color=factor(PFT)), height=0.2) + 
        geom_point(aes(y=PFT, x=CO2_effect, fill=PFT), 
                   size=8, shape=21)+
        labs(x=expression(eCO[2] * " response under LP (%)"), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=16), 
              axis.text.x = element_text(size=14),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 18, face = "bold"),
              legend.position = "none",
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-50, 120))+
        scale_y_discrete(breaks=c("woody", "nonwoody"),
                         labels=c("WD", "NWD"))+
        scale_color_manual(name=paste("Vegetation group"),
                           limits=c("woody", "nonwoody"),
                           values=c("black", "grey"),
                           labels=c("Woody", "Nonwoody"),
                           guide = FALSE)+
        scale_fill_manual(name=paste("Vegetation group"),
                          limits=c("woody", "nonwoody"),
                          values=c("black", "grey"),
                          labels=c("Woody", "Nonwoody"))+        
        ggtitle("(e)")+
        guides(fill = guide_legend(title.position = "top"))
    
    

    pdf("output/step3/Figure_8_woody_nonwoody_comparison.pdf", 
        width=12, height=6)
    plot_grid(p1b, p1c, p1a, 
              p1e, p1f, p1d, 
              rel_widths=c(1.2,1,1.1),
              rel_heights=c(1,1.1),
              labels=c(""), ncol=3, align="h", axis = "l")    
    dev.off()
    
    
}
