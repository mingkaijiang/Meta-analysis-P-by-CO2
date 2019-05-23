### This script plot bar chart for all significant response ratio means and confidence interval

make_split_interaction_effect_chart_2 <- function(sumDF, sumDF2, intDF) {
    
    ###this script include nutrient ratio
    
    ### This function processes the dataframe with some basic summaries
    if(!dir.exists("output/metafor_summary_plot")) {
        dir.create("output/metafor_summary_plot", showWarnings = FALSE)
    }
    
    ### prepare df
    sumDF$Category <- c(rep("Biomass", 24), rep("Concentration", 16), rep("Gas exchange", 4),
                         rep("Morphology", 8), rep("Nutrient uptake", 4), 
                         rep("Resource use efficiency", 6))
    sumDF$Pos <- sumDF$CO2_effect + (sumDF$se *1.96)
    sumDF$Neg <- sumDF$CO2_effect - (sumDF$se *1.96)
    
    sumDF2$Category <- c(rep("Biomass", 24), rep("Concentration", 16), rep("Gas exchange", 4),
                       rep("Morphology", 8), rep("Nutrient uptake", 4), 
                       rep("Resource use efficiency", 6))
    sumDF2$Pos <- sumDF2$P_effect + (sumDF2$se *1.96)
    sumDF2$Neg <- sumDF2$P_effect - (sumDF2$se *1.96)
    
    intDF$Category <- c(rep("Biomass", 12), rep("Concentration", 8), rep("Gas exchange", 2),
                         rep("Morphology", 4), rep("Nutrient uptake", 2), 
                         rep("Resource use efficiency", 3))
    intDF$Pos <- intDF$interaction + (intDF$se *1.96)
    intDF$Neg <- intDF$interaction - (intDF$se *1.96)
    
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
    
    ### subset
    plotDF1 <- subset(intDF, variable %in% c("leaf_biomass", "stem_biomass", "root_biomass", "total_biomass"))
    plotDF1a <- subset(sumDF2, variable %in% c("leaf_biomass", "stem_biomass", "root_biomass", "total_biomass"))
    plotDF1b <- subset(sumDF, variable %in% c("leaf_biomass", "stem_biomass", "root_biomass", "total_biomass"))
    
    plotDF2 <- subset(intDF, variable %in% c("leaf_N_content", "leaf_P_content", "stem_P_content", "root_P_content", "total_P_content"))
    plotDF2a <- subset(sumDF2, variable %in% c("leaf_N_content", "leaf_P_content", "stem_P_content", "root_P_content", "total_P_content"))
    plotDF2b <- subset(sumDF, variable %in% c("leaf_N_content", "leaf_P_content", "stem_P_content", "root_P_content", "total_P_content"))
    
    plotDF3 <- subset(intDF, Category == "Concentration")
    plotDF3a <- subset(sumDF2, Category == "Concentration")
    plotDF3b <- subset(sumDF, Category == "Concentration")
    
    plotDF4 <- subset(intDF, Category == "Gas exchange")
    plotDF4a <- subset(sumDF2, Category == "Gas exchange")
    plotDF4b <- subset(sumDF, Category == "Gas exchange")
    
    plotDF5 <- subset(intDF, Category == "Nutrient uptake")
    plotDF5a <- subset(sumDF2, Category == "Nutrient uptake")
    plotDF5b <- subset(sumDF, Category == "Nutrient uptake")
    
    plotDF6 <- subset(intDF, variable %in% c("leaf_area", "LMA", "Root_length"))
    plotDF6a <- subset(sumDF2, variable %in% c("leaf_area", "LMA", "Root_length"))
    plotDF6b <- subset(sumDF, variable %in% c("leaf_area", "LMA", "Root_length"))
    
    plotDF7 <- subset(intDF, variable == "WUE")
    plotDF7a <- subset(sumDF2, variable == "WUE")
    plotDF7b <- subset(sumDF, variable == "WUE")
    
    ### combine gas exchange with WUE
    plotDF4 <- rbind(plotDF4, plotDF7)
    plotDF4a <- rbind(plotDF4a, plotDF7a)
    plotDF4b <- rbind(plotDF4b, plotDF7b)
    
    
    ## reassigning id numbers
    plotDF4$id[plotDF4$id=="57.5"] <- "45.5"
    plotDF4$id <- as.numeric(plotDF4$id)

    plotDF4a$id[plotDF4a$id=="57"] <- 45
    plotDF4a$id[plotDF4a$id=="58"] <- 46
    
    plotDF4b$id[plotDF4b$id=="57"] <- 45
    plotDF4b$id[plotDF4b$id=="58"] <- 46
    
    plotDF6$id <- seq(45.5, 49.5, by=2)
    plotDF6a$id <- plotDF6b$id <- c(45.2, 45.8, 47.2, 47.8, 49.2, 49.8)
    
    
    plotDF1a$id <- plotDF1b$id <- c(1.2, 1.8, 3.2, 3.8, 5.2, 5.8, 7.2, 7.8)
    

    plotDF2$id <- seq(9.5, 17.5, by=2)
    plotDF2a$id <- plotDF2b$id <- c(9.2, 9.8, 11.2, 11.8, 13.2, 13.8, 15.2, 15.8,
                                    17.2, 17.8)
    
    plotDF3$id <- seq(25.5, 35.5, by=2)
    plotDF3a$id <- plotDF3b$id <- c(25.2, 25.8, 27.2, 27.8, 29.2, 29.8, 31.2, 31.8,
                                    33.2, 33.8, 35.2, 35.8)
    
    plotDF4a$id <- plotDF4b$id <- c(41.2, 41.8, 43.2, 43.8, 45.2, 45.8)
    
    plotDF5a$id <- plotDF5b$id <- c(53.2, 53.8, 55.2, 55.8)
    
    plotDF4$id <- as.numeric(plotDF4$id)
    plotDF5$id <- as.numeric(plotDF5$id)
    plotDF1$id <- as.numeric(plotDF1$id)
    
    ### prepare labels
    y.lab1 <- c("leaf_biomass"="    Leaf",
                "stem_biomass"="    Stem",
                "root_biomass"="    Root",
                "total_biomass"="   Total")
    
    y2.lab1 <- c(bquote(n[e]==.(plotDF1$ne[1])),
                 bquote(n[e]==.(plotDF1$ne[2])),
                 bquote(n[e]==.(plotDF1$ne[3])),
                 bquote(n[e]==.(plotDF1$ne[4])))
    
    y.lab2 <- c("leaf_N_content"="Leaf N",
                "leaf_P_content"="Leaf P",
                "stem_P_content"="Stem P",
                "root_P_content"="Root P",
                "total_P_content"="Total P")
    
    y2.lab2 <- c(bquote(n[e]==.(plotDF2$ne[1])),
                 bquote(n[e]==.(plotDF2$ne[2])),
                 bquote(n[e]==.(plotDF2$ne[3])),
                 bquote(n[e]==.(plotDF2$ne[4])),
                 bquote(n[e]==.(plotDF2$ne[5])))
    
    y.lab3 <- c("leaf_N_concentration"="Leaf N",
                "stem_N_concentration"="Stem N",
                "total_N_concentration"="Total N",
                "leaf_P_concentration"="Leaf P",
                "stem_P_concentration"="Stem P",
                "root_P_concentration"="Root P")
    
    y2.lab3 <- c(bquote(n[e]==.(plotDF3$ne[1])),
                 bquote(n[e]==.(plotDF3$ne[2])),
                 bquote(n[e]==.(plotDF3$ne[3])),
                 bquote(n[e]==.(plotDF3$ne[4])),
                 bquote(n[e]==.(plotDF3$ne[5])),
                 bquote(n[e]==.(plotDF3$ne[6])))
    
    y.lab4 <- c("CO2_assimilation_rate"="A",
                "stomatal_conductance"=expression(g[s]),
                "WUE"="iWUE")
    
    y2.lab4 <- c(bquote(n[e]==.(plotDF4$ne[1])),
                 bquote(n[e]==.(plotDF4$ne[2])),
                 bquote(n[e]==.(plotDF4$ne[3])))
    
    y.lab6 <- c("leaf_area"="LA",
                "LMA"="LMA",
                "Root_length"="RL")
    
    y2.lab6 <- c(#bquote(n[s]==.(plotDF4$ns[1])),
                 bquote(n[e]==.(plotDF6$ne[1])),
                 #bquote(n[s]==.(plotDF4$ns[2])),
                 bquote(n[e]==.(plotDF6$ne[2])),
                 #bquote(n[s]==.(plotDF4$ns[3])),
                 bquote(n[e]==.(plotDF6$ne[3])))
    
    y.lab5 <- c("N_uptake"="Nupt",
                "P_uptake"="Pupt")
    
    y2.lab5 <- c(#bquote(n[s]==.(plotDF5$ns[1])),
                 bquote(n[e]==.(plotDF5$ne[1])),
                 #bquote(n[s]==.(plotDF5$ns[2])),
                 bquote(n[e]==.(plotDF5$ne[2])))
    
    
    ### plotting
    p1a <- ggplot(plotDF1a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect*100, fill=CO2_treatment), 
                   size=4, shape=21)+
        labs(x="LP treatment response (%)", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = "none",
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-250, 50))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
                         labels=y.lab1)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("aCO2", "eCO2"),
                           values=c("blue3", "red2"),
                           labels=c(expression(aCO[2]), expression(eCO[2])), 
                           guide=F)+
        scale_fill_manual(name=expression(paste(CO[2], " treatment")),
                          limits=c("aCO2", "eCO2"),
                          values=c("blue3", "red2"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        ggtitle("Biomass")
    
    
    p1b <- ggplot(plotDF1b)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect*100, fill=P_treatment), 
                   size=4, shape=21)+
        labs(x=expression(paste(eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = "none",
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-50, 70))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
                           labels=y.lab1)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("blue3", "red2"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("blue3", "red2"),
                          labels=c("HP", "LP"))
    

    p1c <- ggplot(plotDF1)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=sig), height=0.5) + 
        geom_point(aes(y=id, x=interaction*100, fill=sig), 
                   size=4, shape=21)+
        labs(x="Interaction response (%)", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = "none",
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-40, 60))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
                           labels=c("","","",""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(1.5,7.5, by=2),
                                               labels = y2.lab1))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("pos", "neg", "neutral"),
                           values=c("#0072B2", "#D55E00", "#999999"),
                           labels=c("Positive", "Negative", "Neutral"),
                           guide = FALSE)+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("#0072B2", "#D55E00", "#999999"),
                          labels=c("Positive", "Negative", "Neutral"))
    
    
    p2a <- ggplot(plotDF2a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect*100, fill=CO2_treatment), 
                   size=4, shape=21)+
        labs(x="LP treatment response (%)", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = "none",
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-250, 50))+
        scale_y_continuous(breaks=c(9.5, 11.5, 13.5, 15.5, 17.5),
                           labels=y.lab2)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("aCO2", "eCO2"),
                           values=c("blue3", "red2"),
                           labels=c(expression(aCO[2]), expression(eCO[2])), 
                           guide=F)+
        scale_fill_manual(name=expression(paste(CO[2], " treatment")),
                          limits=c("aCO2", "eCO2"),
                          values=c("blue3", "red2"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        ggtitle("Nutrient content")
    
    
    p2b <- ggplot(plotDF2b)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect*100, fill=P_treatment), 
                   size=4, shape=21)+
        labs(x=expression(paste(eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = "none",
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-50, 70))+
        scale_y_continuous(breaks=c(9.5, 11.5, 13.5, 15.5, 17.5),
                           labels=y.lab2)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("blue3", "red2"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("blue3", "red2"),
                          labels=c("HP", "LP"))
    
    
    p2c <- ggplot(plotDF2)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=sig), height=0.5) + 
        geom_point(aes(y=id, x=interaction*100, fill=sig), 
                   size=4, shape=21)+
        labs(x="Interaction response (%)", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = "none",
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-40, 60))+
        scale_y_continuous(breaks=c(9.5, 11.5, 13.5, 15.5, 17.5),
                           labels=c("","","","", ""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(9.5,17.5, by=2),
                                               labels = y2.lab2))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("pos", "neg", "neutral"),
                           values=c("#0072B2", "#D55E00", "#999999"),
                           labels=c("Positive", "Negative", "Neutral"),
                           guide = FALSE)+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("#0072B2", "#D55E00", "#999999"),
                          labels=c("Positive", "Negative", "Neutral"))
    
    
    p3a <- ggplot(plotDF3a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect*100, fill=CO2_treatment), 
                   size=4, shape=21)+
        labs(x="LP treatment response (%)", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.1, 0.5),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-250, 50))+
        scale_y_continuous(breaks=c(25.5, 27.5, 29.5, 31.5, 
                                    33.5, 35.5),
                           labels=y.lab3)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("aCO2", "eCO2"),
                           values=c("blue3", "red2"),
                           labels=c(expression(aCO[2]), expression(eCO[2])), 
                           guide=F)+
        scale_fill_manual(name=expression(paste(CO[2], " treatment")),
                          limits=c("aCO2", "eCO2"),
                          values=c("blue3", "red2"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        ggtitle("Nutrient concentration")
    
    
    p3b <- ggplot(plotDF3b)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect*100, fill=P_treatment), 
                   size=4, shape=21)+
        labs(x=expression(paste(eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.7, 0.5),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-50, 70))+
        scale_y_continuous(breaks=c(25.5, 27.5, 29.5, 31.5, 
                                    33.5, 35.5),
                           labels=y.lab3)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("blue3", "red2"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("blue3", "red2"),
                          labels=c("HP", "LP"))
    
    
    p3c <- ggplot(plotDF3)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=sig), height=0.5) + 
        geom_point(aes(y=id, x=interaction*100, fill=sig), 
                   size=4, shape=21)+
        labs(x="Interaction response (%)", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.65, 0.6),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-40, 60))+
        scale_y_continuous(breaks=c(25.5, 27.5, 29.5, 31.5, 
                                    33.5, 35.5),
                           labels=c("","","","","",""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(25.5, 35.5, by=2),
                                               labels = y2.lab3))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("pos", "neg", "neutral"),
                           values=c("#0072B2", "#D55E00", "#999999"),
                           labels=c("Positive", "Negative", "Neutral"),
                           guide = FALSE)+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("#0072B2", "#D55E00", "#999999"),
                          labels=c("Positive", "Negative", "Neutral"))
    
    
    pdf("output/metafor_summary_plot/Figure1_biomass_concentration_responses_all_results.pdf", width=14, height=8)
    plot_grid(p1a, p1b, p1c,
              p2a, p2b, p2c,
              p3a, p3b, p3c,
              rel_heights=c(0.2, 0.25, 0.3),
              rel_widths=c(1.0, 1.0, 1.0),
              labels="auto", ncol=3, align="h", axis = "l")
    dev.off()
    
    
    

    
    ### plotting
    p4a <- ggplot(plotDF4a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect*100, fill=CO2_treatment), 
                   size=4, shape=21)+
        labs(x="LP treatment response (%)", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = "none",
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-250, 50))+
        scale_y_continuous(breaks=c(41.5, 43.5, 45.5),
                           labels=y.lab4)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("aCO2", "eCO2"),
                           values=c("blue3", "red2"),
                           labels=c(expression(aCO[2]), expression(eCO[2])), 
                           guide=F)+
        scale_fill_manual(name=expression(paste(CO[2], " treatment")),
                          limits=c("aCO2", "eCO2"),
                          values=c("blue3", "red2"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        ggtitle("Gas exchange")
    
    
    
    p4b <- ggplot(plotDF4b)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect*100, fill=P_treatment), 
                   size=4, shape=21)+
        labs(x=expression(paste(eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position = "none")+
        scale_x_continuous(limits=c(-50, 100))+
        scale_y_continuous(breaks=c(41.5, 43.5, 45.5),
                           labels=y.lab4)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("blue3", "red2"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("blue3", "red2"),
                          labels=c("HP", "LP"))+
        ggtitle("")
    
    
    p4c <- ggplot(plotDF4)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=sig), height=0.5) + 
        geom_point(aes(y=id, x=interaction*100, fill=sig), 
                   size=4, shape=21)+
        labs(x="Interaction response (%)", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position = "none")+
        scale_x_continuous(limits=c(-40, 60))+
        scale_y_continuous(breaks=c(41.5, 43.5, 45.5),
                           labels=c("","",""),
                           sec.axis = sec_axis(~., name = "", breaks=c(41.5, 43.5, 45.5),
                                               labels = y2.lab4))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("pos", "neg", "neutral"),
                           values=c("#0072B2", "#D55E00", "#999999"),
                           labels=c("Positive", "Negative", "Neutral"),
                           guide = FALSE)+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("#0072B2", "#D55E00", "#999999"),
                          labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("")
    

    p6a <- ggplot(plotDF6a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect*100, fill=CO2_treatment), 
                   size=4, shape=21)+
        labs(x="LP treatment response (%)", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.08, 0.8),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-250, 50))+
        scale_y_continuous(breaks=c(45.5, 47.5, 49.5),
                           labels=y.lab6)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("aCO2", "eCO2"),
                           values=c("blue3", "red2"),
                           labels=c(expression(aCO[2]), expression(eCO[2])), 
                           guide=F)+
        scale_fill_manual(name=expression(paste(CO[2], " treatment")),
                          limits=c("aCO2", "eCO2"),
                          values=c("blue3", "red2"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        ggtitle("Morphology")
    
    
    
    p6b <- ggplot(plotDF6b)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect*100, fill=P_treatment), 
                   size=4, shape=21)+
        labs(x=expression(paste(eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.7, 0.72),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-50, 100))+
        scale_y_continuous(breaks=c(45.5, 47.5, 49.5),
                           labels=y.lab6)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("blue3", "red2"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("blue3", "red2"),
                          labels=c("HP", "LP"))+
        ggtitle("")
    
    
    p6c <- ggplot(plotDF6)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=sig), height=0.5) + 
        geom_point(aes(y=id, x=interaction*100, fill=sig), 
                   size=4, shape=21)+
        labs(x="Interaction response (%)", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.61, 0.92),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-40, 60))+
        scale_y_continuous(breaks=c(45.5, 47.5, 49.5),
                           labels=c("","",""),
                           sec.axis = sec_axis(~., name = "", breaks=c(45.5, 47.5, 49.5),
                                               labels = y2.lab6))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("pos", "neg", "neutral"),
                           values=c("#0072B2", "#D55E00", "#999999"),
                           labels=c("Positive", "Negative", "Neutral"),
                           guide = FALSE)+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("#0072B2", "#D55E00", "#999999"),
                          labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("")
    
    
    p5a <- ggplot(plotDF5a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect*100, fill=CO2_treatment), 
                   size=4, shape=21)+
        labs(x="LP treatment response (%)", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position = "none")+
        scale_x_continuous(limits=c(-250, 50))+
        scale_y_continuous(breaks=c(53.5, 55.5),
                           labels=y.lab5)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("aCO2", "eCO2"),
                           values=c("blue3", "red2"),
                           labels=c(expression(aCO[2]), expression(eCO[2])), 
                           guide=F)+
        scale_fill_manual(name=expression(paste(CO[2], " treatment")),
                          limits=c("aCO2", "eCO2"),
                          values=c("blue3", "red2"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        ggtitle("Nutrient uptake")
    
    
    
    p5b <- ggplot(plotDF5b)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect*100, fill=P_treatment), 
                   size=4, shape=21)+
        labs(x=expression(paste(eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position = "none")+
        scale_x_continuous(limits=c(-50, 100))+
        scale_y_continuous(breaks=c(53.5, 55.5),
                           labels=y.lab5)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("blue3", "red2"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("blue3", "red2"),
                          labels=c("HP", "LP"))+
        ggtitle("")
    
    
    p5c <- ggplot(plotDF5)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=sig), height=0.5) + 
        geom_point(aes(y=id, x=interaction*100, fill=sig), 
                   size=4, shape=21)+
        labs(x="Interaction response (%)", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position = "none")+
        scale_x_continuous(limits=c(-40, 60))+
        scale_y_continuous(breaks=c(53.5, 55.5),
                           labels=c("",""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(53.5, 55.5, by=2),
                                               labels = y2.lab5))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("pos", "neg", "neutral"),
                           values=c("#0072B2", "#D55E00", "#999999"),
                           labels=c("Positive", "Negative", "Neutral"),
                           guide = FALSE)+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("#0072B2", "#D55E00", "#999999"),
                          labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("")
    
    pdf("output/metafor_summary_plot/Figure2_other_responses_all_results.pdf", width=14, height=6)
    plot_grid(p4a, p4b, p4c,
              p5a, p5b, p5c,
              p6a, p6b, p6c,
              labels="auto", ncol=3, align="h", axis = "l",
              rel_heights=c(0.4, 0.3, 0.4),
              rel_widths=c(1.0, 1.0, 0.9))
    dev.off()
    
    
    
    
    
    
}