### This script plot bar chart for all significant response ratio means and confidence interval

make_graphic_abstract_chart <- function(sumDF, sumDF2, intDF) {
    
    ###this script include nutrient ratio
    ### prepare df
    sumDF$Category <- c(rep("Biomass", 26), rep("Concentration", 16), rep("Gas exchange", 4),
                         rep("Morphology", 8), rep("Nutrient uptake", 4), 
                         rep("Resource use efficiency", 6), rep("Nutrient ratio", 8))
    sumDF$Pos <- sumDF$ci_ub_pct
    sumDF$Neg <- sumDF$ci_lb_pct

    
    sumDF2$Category <- c(rep("Biomass", 26), rep("Concentration", 16), rep("Gas exchange", 4),
                       rep("Morphology", 8), rep("Nutrient uptake", 4), 
                       rep("Resource use efficiency", 6), rep("Nutrient ratio", 8))
    sumDF2$Pos <- sumDF2$ci_ub_pct
    sumDF2$Neg <- sumDF2$ci_lb_pct
    
    
    intDF$Category <- c(rep("Biomass", 13), rep("Concentration", 8), rep("Gas exchange", 2),
                         rep("Morphology", 4), rep("Nutrient uptake", 2), 
                         rep("Resource use efficiency", 3), rep("Nutrient ratio", 4))
    intDF$Pos <- intDF$ci_ub_pct
    intDF$Neg <- intDF$ci_lb_pct
    
    ###
    sumDF$CO2_effect <- sumDF$CO2_effect_pct
    sumDF2$P_effect <- sumDF2$P_effect_pct
    intDF$interaction <- intDF$int_pct
    
    ### assign color
    sumDF$sig <- ifelse(sumDF$Pos < 0, "neg", ifelse(sumDF$Neg > 0, "pos", "neutral"))
    sumDF2$sig <- ifelse(sumDF2$Pos < 0, "neg", ifelse(sumDF2$Neg > 0, "pos", "neutral"))
    intDF$sig <- ifelse(intDF$Pos < 0, "neg", ifelse(intDF$Neg > 0, "pos", "neutral"))
    
    ### remove nas
    sumDF <- sumDF[complete.cases(sumDF$CO2_effect),]
    sumDF2 <- sumDF2[complete.cases(sumDF2$P_effect),]
    intDF <- intDF[complete.cases(intDF$interaction),]
    
    sumDF$id <- as.character(sumDF$id)
    sumDF2$id <- as.character(sumDF2$id)
    intDF$id <- as.character(intDF$id)
    
    
    ### create subset plotting groups
    ## biomass
    plotDF1 <- subset(intDF, variable %in% c("CO2_assimilation_rate",
                                             "aboveground_biomass", "root_biomass",
                                             "total_biomass",
                                             "leaf_area"))
    plotDF1a <- subset(sumDF2, variable %in% c("CO2_assimilation_rate",
                                               "aboveground_biomass", "root_biomass",
                                               "total_biomass",
                                               "leaf_area"))
    plotDF1b <- subset(sumDF, variable %in% c("CO2_assimilation_rate",
                                              "aboveground_biomass", "root_biomass",
                                              "total_biomass",
                                              "leaf_area"))
    plotDF1$id <- c(7.5, 5.5, 3.5, 9.5, 1.5)
    plotDF1a$id <- plotDF1b$id <- c(7.2, 7.8, 
                                    5.2, 5.8, 
                                    3.2, 3.8, 
                                    9.2, 9.8,
                                    1.2, 1.8)
    
    
    
    ### prepare labels
    y.lab1 <- c("leaf_area"="LA",
                "total_biomass"="Total",
                "root_biomass"="BG",
                "aboveground_biomass"="AG",
                "CO2_assimilation_rate"="A")
    
    y2.lab1 <- c(substitute(paste(n[s], "=", val, sep=""), list(val=plotDF1$ns[5])),
                 substitute(paste(n[e], "=", val, sep=""), list(val=plotDF1$ne[5])),
                 substitute(paste(n[s], "=", val, sep=""), list(val=plotDF1$ns[3])),
                 substitute(paste(n[e], "=", val, sep=""), list(val=plotDF1$ne[3])),
                 substitute(paste(n[s], "=", val, sep=""), list(val=plotDF1$ns[2])),
                 substitute(paste(n[e], "=", val, sep=""), list(val=plotDF1$ne[2])),
                 substitute(paste(n[s], "=", val, sep=""), list(val=plotDF1$ns[1])),
                 substitute(paste(n[e], "=", val, sep=""), list(val=plotDF1$ne[1])),
                 substitute(paste(n[s], "=", val, sep=""), list(val=plotDF1$ns[4])),
                 expression(paste(n[e], "=59", sep="")))
    

    
    ### plotting

    p1a <- ggplot(plotDF1a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect_pct, fill=CO2_treatment), 
                   size=8, shape=21)+
        labs(x="LP response (%)", y="")+
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
        scale_x_continuous(limits=c(-100, 75))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 9.5),
                         labels=y.lab1)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("aCO2", "eCO2"),
                           values=c("grey", "black"),
                           labels=c(expression(aCO[2]), expression(eCO[2])), 
                           guide=F)+
        scale_fill_manual(name=expression(paste(CO[2], " treatment")),
                          limits=c("aCO2", "eCO2"),
                          values=c("grey", "black"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        ggtitle("(a)")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    p1b <- ggplot(plotDF1b)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect_pct, fill=P_treatment), 
                   size=8, shape=23)+
        labs(x=expression(paste(eCO[2], " response (%)")), y="")+
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
        scale_x_continuous(limits=c(-50, 125))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 9.5),
                           labels=y.lab1)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"))+
        ggtitle("(b)")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    p1c <- ggplot(plotDF1)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos), height=0.5) + 
        geom_point(aes(y=id, x=interaction, fill=sig), shape=22,
                   size=8)+
        labs(x=expression(paste("LP effect on ", eCO[2], " response (%)")), y="")+
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
        scale_x_continuous(limits=c(-50, 25))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 9.5),
                           labels=c("","","","",""),
                           sec.axis = sec_axis(~., name = "", 
                                               breaks=c(1.3, 1.7, 3.3, 3.7, 5.3, 5.7, 7.3, 7.7, 9.3, 9.7),
                                               labels = y2.lab1))+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("black", "grey", "white"),
                          labels=c("Positive", "Negative", "Neutral"))+
        scale_shape_manual(name=expression(paste("LP x ", eCO[2])),
                          values=c("pos"=22, "neg"=23, "neutral"=24),
                          labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("(c)")+
        guides(fill = guide_legend(title.position = "top"))
    

    
    pdf("output/step2/Graphic_Abstract_1.pdf", width=14, height=6)
    plot_grid(p1a, p1b, p1c,
              rel_widths=c(1.0, 0.9, 1.0),
              labels=c(""), ncol=3, align="h", axis = "l")
    dev.off()
    
    
    
    p1b <- ggplot(plotDF1b)+ 
        geom_vline(xintercept = 0.0, lty=2)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect_pct, fill=P_treatment), 
                   size=8, shape=23)+
        labs(x=expression(paste(eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "bottom",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-10, 60))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 9.5),
                           labels=c("Leaf area",
                                    "Total biomass",
                                    "BG biomass",
                                    "AG biomass",
                                    expression(CO[2]*" assim. rate")))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"))+
        ggtitle("(a)")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    p1c <- ggplot(plotDF1)+ 
        geom_vline(xintercept = 0.0, lty=2)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos), height=0.5) + 
        geom_point(aes(y=id, x=interaction, fill=sig), shape=22,
                   size=8)+
        labs(x=expression(paste("LP effect on ", eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "bottom",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-25, 5))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 9.5),
                           labels=c("","","","",""),
                           sec.axis = sec_axis(~., name = "", 
                                               breaks=c(1.2, 1.7, 3.2, 3.7, 5.2, 5.7, 7.2, 7.7, 9.2, 9.7),
                                               labels = y2.lab1))+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("black", "grey", "white"),
                          labels=c("Positive", "Negative", "Neutral"))+
        scale_shape_manual(name=expression(paste("LP x ", eCO[2])),
                           values=c("pos"=22, "neg"=23, "neutral"=24),
                           labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("(b)")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    
    pdf("output/step2/Graphic_Abstract_2.pdf", width=8, height=6)
    plot_grid(p1b, p1c,
              rel_widths=c(1.0, 0.8),
              labels=c(""), ncol=2, align="h", axis = "l")
    dev.off()
    
    
    
    p1c <- ggplot(plotDF1)+ 
        geom_vline(xintercept = 0.0, lty=2)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos), height=0.5) + 
        geom_point(aes(y=id, x=interaction, fill=sig), shape=22,
                   size=8)+
        labs(x=expression(paste("LP effect on ", eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=12), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=12),
              legend.text=element_text(size=12),
              legend.title=element_text(size=12),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-25, 5))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 9.5),
                           labels=c("Leaf area",
                                    "Total biomass",
                                    "BG biomass",
                                    "AG biomass",
                                    expression(CO[2] * " assim. rate")))+#,
                           #sec.axis = sec_axis(~., name = "", 
                           #                    breaks=c(1.2, 1.7, 3.2, 3.7, 5.2, 5.7, 7.2, 7.7, 9.2, 9.7),
                           #                    labels = y2.lab1))+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("black", "grey", "white"),
                          labels=c("Positive", "Negative", "Neutral"))+
        scale_shape_manual(name=expression(paste("LP x ", eCO[2])),
                           values=c("pos"=22, "neg"=23, "neutral"=24),
                           labels=c("Positive", "Negative", "Neutral"))+
        guides(fill = guide_legend(title.position = "top"))
    
    
    
    pdf("output/step2/Graphic_Abstract_3.pdf", width=4, height=4)
    plot_grid(p1c,
              labels=c(""), ncol=1, align="h", axis = "l")
    dev.off()
    
    
}
