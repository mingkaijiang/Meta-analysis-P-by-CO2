### This script plot bar chart for all significant response ratio means and confidence interval

make_split_interaction_effect_chart_3 <- function(sumDF, sumDF2, intDF) {
    
    ###this script include nutrient ratio
    
    ### This function processes the dataframe with some basic summaries
    if(!dir.exists("output/metafor_summary_plot")) {
        dir.create("output/metafor_summary_plot", showWarnings = FALSE)
    }
    
    ### prepare df
    sumDF$Category <- c(rep("Biomass", 24), rep("Concentration", 16), rep("Gas exchange", 4),
                         rep("Morphology", 8), rep("Nutrient uptake", 4), 
                         rep("Resource use efficiency", 6), rep("Nutrient ratio", 8))
    sumDF$Pos <- sumDF$CO2_effect + (sumDF$se *1.96)
    sumDF$Neg <- sumDF$CO2_effect - (sumDF$se *1.96)
    
    sumDF2$Category <- c(rep("Biomass", 24), rep("Concentration", 16), rep("Gas exchange", 4),
                       rep("Morphology", 8), rep("Nutrient uptake", 4), 
                       rep("Resource use efficiency", 6), rep("Nutrient ratio", 8))
    sumDF2$Pos <- sumDF2$P_effect + (sumDF2$se *1.96)
    sumDF2$Neg <- sumDF2$P_effect - (sumDF2$se *1.96)
    
    intDF$Category <- c(rep("Biomass", 12), rep("Concentration", 8), rep("Gas exchange", 2),
                         rep("Morphology", 4), rep("Nutrient uptake", 2), 
                         rep("Resource use efficiency", 3), rep("Nutrient ratio", 4))
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
    
    
    ### create subset plotting groups
    ## leaf
    plotDF1 <- subset(intDF, variable %in% c("leaf_biomass", "leaf_N_concentration", "leaf_P_concentration",
                                             "leaf_N_content", "leaf_P_content", "leaf_NP"))
    plotDF1a <- subset(sumDF2, variable %in% c("leaf_biomass", "leaf_N_concentration", "leaf_P_concentration",
                                               "leaf_N_content", "leaf_P_content", "leaf_NP"))
    plotDF1b <- subset(sumDF, variable %in% c("leaf_biomass", "leaf_N_concentration", "leaf_P_concentration",
                                              "leaf_N_content", "leaf_P_content", "leaf_NP"))
    plotDF1$id <- seq(1.5, 11.5, by=2)
    plotDF1a$id <- plotDF1b$id <- c(1.2, 1.8, 3.2, 3.8, 5.2, 5.8, 
                                    7.2, 7.8, 9.2, 9.8, 11.2, 11.8)
    
    ## stem
    plotDF2 <- subset(intDF, variable %in% c("stem_biomass", "stem_N_concentration", "stem_P_concentration",
                                             "stem_N_content", "stem_P_content", "stem_NP"))
    plotDF2a <- subset(sumDF2, variable %in% c("stem_biomass", "stem_N_concentration", "stem_P_concentration",
                                               "stem_N_content", "stem_P_content", "stem_NP"))
    plotDF2b <- subset(sumDF, variable %in% c("stem_biomass", "stem_N_concentration", "stem_P_concentration",
                                              "stem_N_content", "stem_P_content", "stem_NP"))
    plotDF2$id <- seq(1.5, 11.5, by=2)
    plotDF2a$id <- plotDF2b$id <- c(1.2, 1.8, 3.2, 3.8, 5.2, 5.8, 
                                    7.2, 7.8, 9.2, 9.8, 11.2, 11.8)
    
    ## root
    plotDF3 <- subset(intDF, variable %in% c("root_biomass", "root_N_concentration", "root_P_concentration",
                                             "root_N_content", "root_P_content", "root_NP"))
    plotDF3a <- subset(sumDF2, variable %in% c("root_biomass", "root_N_concentration", "root_P_concentration",
                                               "root_N_content", "root_P_content", "root_NP"))
    plotDF3b <- subset(sumDF, variable %in% c("root_biomass", "root_N_concentration", "root_P_concentration",
                                              "root_N_content", "root_P_content", "root_NP"))
    plotDF3$id <- seq(1.5, 11.5, by=2)
    plotDF3a$id <- plotDF3b$id <- c(1.2, 1.8, 3.2, 3.8, 5.2, 5.8, 
                                    7.2, 7.8, 9.2, 9.8, 11.2, 11.8)
    
    ## total
    plotDF4 <- subset(intDF, variable %in% c("total_biomass", "total_N_concentration", "total_P_concentration",
                                             "total_N_content", "total_P_content", "total_NP"))
    plotDF4a <- subset(sumDF2, variable %in% c("total_biomass", "total_N_concentration", "total_P_concentration",
                                               "total_N_content", "total_P_content", "total_NP"))
    plotDF4b <- subset(sumDF, variable %in% c("total_biomass", "total_N_concentration", "total_P_concentration",
                                              "total_N_content", "total_P_content", "total_NP"))
    plotDF4$id <- seq(1.5, 11.5, by=2)
    plotDF4a$id <- plotDF4b$id <- c(1.2, 1.8, 3.2, 3.8, 5.2, 5.8, 
                                    7.2, 7.8, 9.2, 9.8, 11.2, 11.8)
    
    ## gas exchange and WUE combined together
    plotDF5 <- subset(intDF, Category == "Gas exchange")
    plotDF5a <- subset(sumDF2, Category == "Gas exchange")
    plotDF5b <- subset(sumDF, Category == "Gas exchange")
    
    plotDF6 <- subset(intDF, variable == "WUE")
    plotDF6a <- subset(sumDF2, variable == "WUE")
    plotDF6b <- subset(sumDF, variable == "WUE")
    
    plotDF5 <- rbind(plotDF5, plotDF6)
    plotDF5a <- rbind(plotDF5a, plotDF6a)
    plotDF5b <- rbind(plotDF5b, plotDF6b)
    
    plotDF5$id <- seq(1.5, 5.5, by=2)
    plotDF5a$id <- plotDF5b$id <- c(1.2, 1.8, 3.2, 3.8, 5.2, 5.8)
    
    ## nutrient uptake and nutrient use efficiency
    plotDF6 <- subset(intDF, variable %in% c("N_uptake", "P_uptake", "NUE", "PUE"))
    plotDF6a <- subset(sumDF2, variable %in% c("N_uptake", "P_uptake", "NUE", "PUE"))
    plotDF6b <- subset(sumDF, variable %in% c("N_uptake", "P_uptake", "NUE", "PUE"))
    
    plotDF6$id <- seq(1.5, 7.5, by=2)
    plotDF6a$id <- plotDF6b$id <- c(1.2, 1.8, 3.2, 3.8, 5.2, 5.8, 7.2, 7.8)
    
    ## morphology
    plotDF7 <- subset(intDF, variable %in% c("leaf_area", "LMA", "Root_length"))
    plotDF7a <- subset(sumDF2, variable %in% c("leaf_area", "LMA", "Root_length"))
    plotDF7b <- subset(sumDF, variable %in% c("leaf_area", "LMA", "Root_length"))
    
    plotDF7$id <- seq(1.5, 5.5, by=2)
    plotDF7a$id <- plotDF7b$id <- c(1.2, 1.8, 3.2, 3.8, 5.2, 5.8)
    
    

    
    ### prepare labels
    y.lab1 <- c("leaf_biomass"="Biomass",
                "leaf_N_content"="N content",
                "leaf_P_content"="P content",
                "leaf_N_concentration"="N conc.",
                "leaf_P_concentration"="P conc.",
                "leaf_P"="NP ratio")
    
    y2.lab1 <- c(bquote(n[e]==.(plotDF1$ne[1])),
                 bquote(n[e]==.(plotDF1$ne[2])),
                 bquote(n[e]==.(plotDF1$ne[3])),
                 bquote(n[e]==.(plotDF1$ne[4])),
                 bquote(n[e]==.(plotDF1$ne[5])),
                 bquote(n[e]==.(plotDF1$ne[6])))
    
    y.lab2 <- c("stem_biomass"="Biomass",
                "stem_N_content"="N content",
                "stem_P_content"="P content",
                "stem_N_concentration"="N conc.",
                "stem_P_concentration"="P conc.",
                "stem_P"="NP ratio")
    
    y2.lab2 <- c(bquote(n[e]==.(plotDF2$ne[1])),
                 bquote(n[e]==.(plotDF2$ne[2])),
                 bquote(n[e]==.(plotDF2$ne[3])),
                 bquote(n[e]==.(plotDF2$ne[4])),
                 bquote(n[e]==.(plotDF2$ne[5])),
                 bquote(n[e]==.(plotDF2$ne[6])))
    
    y.lab3 <- c("root_biomass"="Biomass",
                "root_N_content"="N content",
                "root_P_content"="P content",
                "root_N_concentration"="N conc.",
                "root_P_concentration"="P conc.",
                "root_P"="NP ratio")
    
    y2.lab3 <- c(bquote(n[e]==.(plotDF3$ne[1])),
                 bquote(n[e]==.(plotDF3$ne[2])),
                 bquote(n[e]==.(plotDF3$ne[3])),
                 bquote(n[e]==.(plotDF3$ne[4])),
                 bquote(n[e]==.(plotDF3$ne[5])),
                 bquote(n[e]==.(plotDF3$ne[6])))
    
    y.lab4 <- c("total_biomass"="Biomass",
                "total_N_content"="N content",
                "total_P_content"="P content",
                "total_N_concentration"="N conc.",
                "total_P_concentration"="P conc.",
                "total_P"="NP ratio")
    
    y2.lab4 <- c(bquote(n[e]==.(plotDF4$ne[1])),
                 bquote(n[e]==.(plotDF4$ne[2])),
                 bquote(n[e]==.(plotDF4$ne[3])),
                 bquote(n[e]==.(plotDF4$ne[4])),
                 bquote(n[e]==.(plotDF4$ne[5])),
                 bquote(n[e]==.(plotDF4$ne[6])))
    
    
    y.lab5 <- c("CO2_assimilation_rate"="A",
                "stomatal_conductance"=expression(g[s]),
                "WUE"="iWUE")
    
    y2.lab5 <- c(bquote(n[e]==.(plotDF5$ne[1])),
                 bquote(n[e]==.(plotDF5$ne[2])),
                 bquote(n[e]==.(plotDF5$ne[3])))
    
    
    y.lab6 <- c("N_uptake"="Nupt",
                "P_uptake"="Pupt",
                "NUE" = "NUE",
                "PUE" = "PUE")
    
    y2.lab6 <- c(bquote(n[e]==.(plotDF6$ne[1])),
                 bquote(n[e]==.(plotDF6$ne[2])),
                 bquote(n[e]==.(plotDF6$ne[3])),
                 bquote(n[e]==.(plotDF6$ne[4])))
    
    
    y.lab7 <- c("leaf_area"="LA",
                "LMA"="LMA",
                "Root_length"="RL")
    
    y2.lab7 <- c(bquote(n[e]==.(plotDF6$ne[1])),
                 bquote(n[e]==.(plotDF6$ne[2])),
                 bquote(n[e]==.(plotDF6$ne[3])))
    
    
    
    ### plotting
    p1a <- ggplot(plotDF1a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect*100, fill=CO2_treatment), 
                   size=4, shape=21)+
        labs(x="LP treatment response (%)", y="Leaf")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14, angle=90),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = "none",
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-225, 200))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5),
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
        ggtitle("a")
    
    
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
        scale_x_continuous(limits=c(-75, 100))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5),
                           labels=y.lab1)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("blue3", "red2"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("blue3", "red2"),
                          labels=c("HP", "LP"))+
        ggtitle("b")
    

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
        scale_x_continuous(limits=c(-100, 100))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5),
                           labels=c("","","","","",""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(1.5,11.5, by=2),
                                               labels = y2.lab1))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("pos", "neg", "neutral"),
                           values=c("#0072B2", "#D55E00", "#999999"),
                           labels=c("Positive", "Negative", "Neutral"),
                           guide = FALSE)+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("#0072B2", "#D55E00", "#999999"),
                          labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("c")
    
    
    p2a <- ggplot(plotDF2a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect*100, fill=CO2_treatment), 
                   size=4, shape=21)+
        labs(x="LP treatment response (%)", y="Stem")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14, angle=90),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = "none",
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-225, 200))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5),
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
        ggtitle("d")
    
    
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
        scale_x_continuous(limits=c(-75, 100))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5),
                           labels=y.lab2)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("blue3", "red2"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("blue3", "red2"),
                          labels=c("HP", "LP"))+
        ggtitle("e")
    
    
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
        scale_x_continuous(limits=c(-100, 100))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5),
                           labels=c("","","","", "",""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(1.5, 11.5, by=2),
                                               labels = y2.lab2))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("pos", "neg", "neutral"),
                           values=c("#0072B2", "#D55E00", "#999999"),
                           labels=c("Positive", "Negative", "Neutral"),
                           guide = FALSE)+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("#0072B2", "#D55E00", "#999999"),
                          labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("f")
    
    
    p3a <- ggplot(plotDF3a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect*100, fill=CO2_treatment), 
                   size=4, shape=21)+
        labs(x="LP treatment response (%)", y="Root")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14, angle=90),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = "none",
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-225, 200))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5),
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
        ggtitle("g")
    
    
    p3b <- ggplot(plotDF3b)+ 
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
        scale_x_continuous(limits=c(-75, 100))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5),
                           labels=y.lab3)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("blue3", "red2"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("blue3", "red2"),
                          labels=c("HP", "LP"))+
        ggtitle("h")
    
    
    p3c <- ggplot(plotDF3)+ 
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
        scale_x_continuous(limits=c(-100, 100))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5),
                           labels=c("","","","","",""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(1.5, 11.5, by=2),
                                               labels = y2.lab3))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("pos", "neg", "neutral"),
                           values=c("#0072B2", "#D55E00", "#999999"),
                           labels=c("Positive", "Negative", "Neutral"),
                           guide = FALSE)+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("#0072B2", "#D55E00", "#999999"),
                          labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("i")
    
    
    p4a <- ggplot(plotDF4a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect*100, fill=CO2_treatment), 
                   size=4, shape=21)+
        labs(x="LP treatment response (%)", y="Total")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14, angle=90),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.6, 0.4),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-225, 200))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5),
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
        ggtitle("j")
    
    
    p4b <- ggplot(plotDF4b)+ 
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
              legend.position = c(0.72, 0.4),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-75, 100))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5),
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
        ggtitle("k")
    
    p4c <- ggplot(plotDF4)+ 
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
              legend.position = c(0.65, 0.5),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-100, 100))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 9.5, 11.5),
                           labels=c("","","","","",""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(1.5, 11.5, by=2),
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
        ggtitle("l")
    
    
    pdf("output/metafor_summary_plot/Figure1_biomass_concentration_responses_all_results.pdf", width=16, height=18)
    plot_grid(p1a, p1b, p1c,
              p2a, p2b, p2c,
              p3a, p3b, p3c,
              p4a, p4b, p4c,
              #rel_heights=c(0.2, 0.25, 0.3),
              rel_widths=c(1.0, 0.9, 1.0),
              labels=c(""), ncol=3, align="h", axis = "l")
    dev.off()
    
    
    

    
    ### plotting
    p5a <- ggplot(plotDF5a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect*100, fill=CO2_treatment), 
                   size=4, shape=21)+
        labs(x="LP treatment response (%)", y="Gas exchange")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14, angle = 90),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = "none",
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-225, 200))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5),
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
        ggtitle("a")
    
    
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
              legend.justification = c(0, 1), 
              legend.position = "none",
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-75, 100))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5),
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
        ggtitle("b")
    
    
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
              legend.justification = c(0, 1), 
              legend.position = "none",
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-100, 100))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5),
                           labels=c("","",""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(1.5, 5.5, by=2),
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
        ggtitle("c")
    
    
    p6a <- ggplot(plotDF6a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect*100, fill=CO2_treatment), 
                   size=4, shape=21)+
        labs(x="LP treatment response (%)", y="Nutrient uptake & use")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14, angle = 90),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = "none",
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-225, 200))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
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
        ggtitle("d")
    
    
    p6b <- ggplot(plotDF6b)+ 
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
        scale_x_continuous(limits=c(-75, 100))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
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
        ggtitle("e")
    
    
    p6c <- ggplot(plotDF6)+ 
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
        scale_x_continuous(limits=c(-100, 100))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
                           labels=c("","","",""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(1.5, 7.5, by=2),
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
        ggtitle("f")
    
    
    p7a <- ggplot(plotDF7a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg*100, xmax=Pos*100, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect*100, fill=CO2_treatment), 
                   size=4, shape=21)+
        labs(x="LP treatment response (%)", y="Morphology")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14, angle = 90),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.64, 0.4),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-225, 200))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5),
                           labels=y.lab7)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("aCO2", "eCO2"),
                           values=c("blue3", "red2"),
                           labels=c(expression(aCO[2]), expression(eCO[2])), 
                           guide=F)+
        scale_fill_manual(name=expression(paste(CO[2], " treatment")),
                          limits=c("aCO2", "eCO2"),
                          values=c("blue3", "red2"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        ggtitle("g")
    
    
    p7b <- ggplot(plotDF7b)+ 
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
              legend.position = c(0.7, 0.4),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-75, 100))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5),
                           labels=y.lab7)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("blue3", "red2"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("blue3", "red2"),
                          labels=c("HP", "LP"))+
        ggtitle("h")
    
    p7c <- ggplot(plotDF7)+ 
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
              legend.position = c(0.65, 0.5),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-100, 100))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5),
                           labels=c("","",""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(1.5, 5.5, by=2),
                                               labels = y2.lab7))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("pos", "neg", "neutral"),
                           values=c("#0072B2", "#D55E00", "#999999"),
                           labels=c("Positive", "Negative", "Neutral"),
                           guide = FALSE)+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("#0072B2", "#D55E00", "#999999"),
                          labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("i")
    
    pdf("output/metafor_summary_plot/Figure2_other_responses_all_results.pdf", width=14, height=12)
    plot_grid(p5a, p5b, p5c,
              p6a, p6b, p6c,
              p7a, p7b, p7c,
              labels="", ncol=3, align="h", axis = "l",
              #rel_heights=c(0.4, 0.5, 0.4),
              rel_widths=c(1.0, 0.9, 1.0))
    dev.off()
    
    
    
    
    
    
}