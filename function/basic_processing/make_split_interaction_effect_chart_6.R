### This script plot bar chart for all significant response ratio means and confidence interval

make_split_interaction_effect_chart_6 <- function(sumDF, sumDF2, intDF) {
    
    ###this script include nutrient ratio
    
    ### This function processes the dataframe with some basic summaries
    if(!dir.exists("output/metafor_summary_plot")) {
        dir.create("output/metafor_summary_plot", showWarnings = FALSE)
    }
    
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
    #sumDF2$Pos <- exp(sumDF2$P_effect + (sumDF2$se *1.96)) - 1
    #sumDF2$Neg <- exp(sumDF2$P_effect - (sumDF2$se *1.96)) - 1
    #sumDF2$Pos <- sumDF2$P_effect + (sumDF2$se * sqrt(sumDF2$ne))
    #sumDF2$Neg <- sumDF2$P_effect - (sumDF2$se * sqrt(sumDF2$ne))
    
    
    intDF$Category <- c(rep("Biomass", 13), rep("Concentration", 8), rep("Gas exchange", 2),
                         rep("Morphology", 4), rep("Nutrient uptake", 2), 
                         rep("Resource use efficiency", 3), rep("Nutrient ratio", 4))
    intDF$Pos <- intDF$ci_ub_pct
    intDF$Neg <- intDF$ci_lb_pct
    #intDF$Pos <- exp(intDF$interaction + (intDF$se *1.96)) - 1
    #intDF$Neg <- exp(intDF$interaction - (intDF$se *1.96)) - 1
    #intDF$Pos <- intDF$interaction + (intDF$se * sqrt(intDF$ne))
    #intDF$Neg <- intDF$interaction - (intDF$se * sqrt(intDF$ne))
    
    
    ### back transform the log-transformed response ratios for plotting
    #sumDF$CO2_effect <- exp(sumDF$CO2_effect)-1
    #sumDF2$P_effect <- exp(sumDF2$P_effect)-1
    #intDF$interaction <- exp(intDF$interaction)-1
    
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
    plotDF1 <- subset(intDF, variable %in% c("aboveground_biomass", "root_biomass",
                                             "total_biomass"))
    plotDF1a <- subset(sumDF2, variable %in% c("aboveground_biomass", "root_biomass",
                                               "total_biomass"))
    plotDF1b <- subset(sumDF, variable %in% c("aboveground_biomass", "root_biomass",
                                              "total_biomass"))
    plotDF1$id <- seq(5.5, 1.5, by=-2)
    plotDF1a$id <- plotDF1b$id <- c(5.2, 5.8, 3.2, 3.8, 
                                    1.2, 1.8)
    
    ## P concentration
    plotDF2 <- subset(intDF, variable %in% c("leaf_P_concentration", "stem_P_concentration", "root_P_concentration",
                                             "total_P_concentration"))
    plotDF2a <- subset(sumDF2, variable %in% c("leaf_P_concentration", "stem_P_concentration", "root_P_concentration",
                                               "total_P_concentration"))
    plotDF2b <- subset(sumDF, variable %in% c("leaf_P_concentration", "stem_P_concentration", "root_P_concentration",
                                              "total_P_concentration"))
    plotDF2$id <- seq(7.5, 1.5, by=-2)
    plotDF2a$id <- plotDF2b$id <- c(7.2, 7.8, 5.2, 5.8, 3.2, 3.8, 
                                    1.2, 1.8)
    
    ## N concentration
    plotDF3 <- subset(intDF, variable %in% c("leaf_N_concentration", "stem_N_concentration", "root_N_concentration",
                                             "total_N_concentration"))
    plotDF3a <- subset(sumDF2, variable %in% c("leaf_N_concentration", "stem_N_concentration", "root_N_concentration",
                                               "total_N_concentration"))
    plotDF3b <- subset(sumDF, variable %in% c("leaf_N_concentration", "stem_N_concentration", "root_N_concentration",
                                              "total_N_concentration"))
    plotDF3$id <- seq(7.5, 1.5, by=-2)
    plotDF3a$id <- plotDF3b$id <- c(7.2, 7.8, 5.2, 5.8, 3.2, 3.8, 
                                    1.2, 1.8)
    
    ## NP ratio
    plotDF4 <- subset(intDF, variable %in% c("leaf_NP", "stem_NP", "root_NP", "total_NP"))
    plotDF4a <- subset(sumDF2, variable %in% c("leaf_NP", "stem_NP", "root_NP", "total_NP"))
    plotDF4b <- subset(sumDF, variable %in% c("leaf_NP", "stem_NP", "root_NP", "total_NP"))
    plotDF4$id <- seq(7.5, 1.5, by=-2)
    plotDF4a$id <- plotDF4b$id <- c(7.2, 7.8, 5.2, 5.8, 3.2, 3.8, 
                                    1.2, 1.8)
    
    ## P content
    plotDF5 <- subset(intDF, variable %in% c("leaf_P_content", "stem_P_content", "root_P_content", "total_P_content"))
    plotDF5a <- subset(sumDF2, variable %in% c("leaf_P_content", "stem_P_content", "root_P_content", "total_P_content"))
    plotDF5b <- subset(sumDF, variable %in% c("leaf_P_content", "stem_P_content", "root_P_content", "total_P_content"))
    plotDF5$id <- seq(7.5, 1.5, by=-2)
    plotDF5a$id <- plotDF5b$id <- c(7.2, 7.8, 5.2, 5.8, 3.2, 3.8, 
                                    1.2, 1.8)
    
    ## N content
    plotDF6 <- subset(intDF, variable %in% c("leaf_N_content", "stem_N_content", "root_N_content", "total_N_content"))
    plotDF6a <- subset(sumDF2, variable %in% c("leaf_N_content", "stem_N_content", "root_N_content", "total_N_content"))
    plotDF6b <- subset(sumDF, variable %in% c("leaf_N_content", "stem_N_content", "root_N_content", "total_N_content"))
    plotDF6$id <- seq(7.5, 1.5, by=-2)
    plotDF6a$id <- plotDF6b$id <- c(7.2, 7.8, 5.2, 5.8, 3.2, 3.8, 
                                    1.2, 1.8)
    
    ## gas exchange and WUE combined together
    plotDF7 <- subset(intDF, variable %in% c("CO2_assimilation_rate", "stomatal_conductance", "WUE"))
    plotDF7a <- subset(sumDF2, variable %in% c("CO2_assimilation_rate", "stomatal_conductance", "WUE"))
    plotDF7b <- subset(sumDF, variable %in% c("CO2_assimilation_rate", "stomatal_conductance", "WUE"))
    
    plotDF7$id <- seq(5.5, 1.5, by=-2)
    plotDF7a$id <- plotDF7b$id <- c(5.2, 5.8, 3.2, 3.8, 
                                    1.2, 1.8)
    
    
    ## nutrient uptake and nutrient use efficiency
    plotDF8 <- subset(intDF, variable %in% c("N_uptake", "P_uptake"))
    plotDF8a <- subset(sumDF2, variable %in% c("N_uptake", "P_uptake"))
    plotDF8b <- subset(sumDF, variable %in% c("N_uptake", "P_uptake"))
    
    plotDF8$id <- seq(3.5, 1.5, by=-2)
    plotDF8a$id <- plotDF8b$id <- c(3.2, 3.8, 
                                    1.2, 1.8)
    
    ## nutrient use efficiency
    plotDF9 <- subset(intDF, variable %in% c("NUE", "PUE"))
    plotDF9a <- subset(sumDF2, variable %in% c("NUE", "PUE"))
    plotDF9b <- subset(sumDF, variable %in% c("NUE", "PUE"))
    
    plotDF9$id <- seq(3.5, 1.5, by=-2)
    plotDF9a$id <- plotDF9b$id <- c(3.2, 3.8, 
                                    1.2, 1.8)
    
    
    ## morphology
    plotDF10 <- subset(intDF, variable%in% c("leaf_area", "LMA", "Root_length"))
    plotDF10a <- subset(sumDF2, variable%in% c("leaf_area", "LMA", "Root_length"))
    plotDF10b <- subset(sumDF, variable%in% c("leaf_area", "LMA", "Root_length"))
    
    plotDF10$id <- seq(5.5, 1.5, by=-2)
    plotDF10a$id <- plotDF10b$id <- c(5.2, 5.8, 3.2, 3.8, 
                                    1.2, 1.8)
    
    ### prepare labels
    y.lab1 <- c("total_biomass"="Total",
                "root_biomass"="BG",
                "aboveground_biomass"="AG")
    
    y2.lab1 <- c(bquote(n[e]==.(plotDF1$ne[3])),
                 bquote(n[e]==.(plotDF1$ne[2])),
                 bquote(n[e]==.(plotDF1$ne[1])))
    
    y.lab2 <- c("total_P_concentration"="Total",
                "root_P_concentration"="Root",
                "stem_P_concentration"="Stem",
                "leaf_P_concentration"="Leaf")
    
    y2.lab2 <- c(bquote(n[e]==.(plotDF2$ne[4])),
                 bquote(n[e]==.(plotDF2$ne[3])),
                 bquote(n[e]==.(plotDF2$ne[2])),
                 bquote(n[e]==.(plotDF2$ne[1])))
    
    y.lab3 <- c("total_N_concentration"="Total",
                "root_N_concentration"="Root",
                "stem_N_concentration"="Stem",
                "leaf_N_concentration"="Leaf")
    
    y2.lab3 <- c(bquote(n[e]==.(plotDF3$ne[4])),
                 bquote(n[e]==.(plotDF3$ne[3])),
                 bquote(n[e]==.(plotDF3$ne[2])),
                 bquote(n[e]==.(plotDF3$ne[1])))
    
    y.lab4 <- c("total_NP"="Total",
                "root_NP"="Root",
                "stem_NP"="Stem",
                "leaf_NP"="Leaf")
    
    y2.lab4 <- c(bquote(n[e]==.(plotDF4$ne[4])),
                 bquote(n[e]==.(plotDF4$ne[3])),
                 bquote(n[e]==.(plotDF4$ne[2])),
                 bquote(n[e]==.(plotDF4$ne[1])))
    
    y.lab5 <- c("total_P_content"="Total",
                "root_P_content"="Root",
                "stem_P_content"="Stem",
                "leaf_P_content"="Leaf")
    
    y2.lab5 <- c(bquote(n[e]==.(plotDF5$ne[4])),
                 bquote(n[e]==.(plotDF5$ne[3])),
                 bquote(n[e]==.(plotDF5$ne[2])),
                 bquote(n[e]==.(plotDF5$ne[1])))
    
    y.lab6 <- c("total_N_content"="Total",
                "root_N_content"="Root",
                "stem_N_content"="Stem",
                "leaf_N_content"="Leaf")
    
    y2.lab6 <- c(bquote(n[e]==.(plotDF6$ne[4])),
                 bquote(n[e]==.(plotDF6$ne[3])),
                 bquote(n[e]==.(plotDF6$ne[2])),
                 bquote(n[e]==.(plotDF6$ne[1])))
    
    
    y.lab7 <- c("WUE"="iWUE",
                "stomatal_conductance"=expression(g[s]),
                "CO2_assimilation_rate"="A")
    
    y2.lab7 <- c(bquote(n[e]==.(plotDF7$ne[3])),
                 bquote(n[e]==.(plotDF7$ne[2])),
                 bquote(n[e]==.(plotDF7$ne[1])))
    
    
    y.lab8 <- c("P_uptake"=expression(P[upt]),
                "N_uptake"=expression(N[upt]))
    
    y2.lab8 <- c(bquote(n[e]==.(plotDF8$ne[2])),
                 bquote(n[e]==.(plotDF8$ne[1])))
    
    
    y.lab9 <- c("PUE"="PUE",
                "NUE"="NUE")
    
    y2.lab9 <- c(bquote(n[e]==.(plotDF9$ne[2])),
                 bquote(n[e]==.(plotDF9$ne[1])))
    
    
    y.lab10 <- c("Root_length" = "RL",
                 "LMA" = "LMA", 
                 "leaf_area"="LA")
                
    
    y2.lab10 <- c(bquote(n[e]==.(plotDF10$ne[3])),
                  bquote(n[e]==.(plotDF10$ne[2])),
                  bquote(n[e]==.(plotDF10$ne[1])))
    

    
    ### plotting
    p1a <- ggplot(plotDF1a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect_pct, fill=CO2_treatment), 
                   size=8, shape=21)+
        labs(x="LP treatment response (%)", y="Biomass")+
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
        scale_x_continuous(limits=c(-100, 50))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5),
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
        ggtitle("a")+
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
              legend.position = "bottom",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-25, 65))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5),
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
        ggtitle("b")+
        guides(fill = guide_legend(title.position = "top"))
    

    p1c <- ggplot(plotDF1)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos), height=0.5) + 
        geom_point(aes(y=id, x=interaction, fill=sig), shape=22,
                   size=8)+
        labs(x=expression(paste("effect of LP on ", eCO[2], " response (%)")), y="")+
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
        scale_x_continuous(limits=c(-50, 25))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5),
                           labels=c("","",""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(1.5,5.5, by=2),
                                               labels = y2.lab1))+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("black", "grey", "white"),
                          labels=c("Positive", "Negative", "Neutral"))+
        scale_shape_manual(name=expression(paste("LP x ", eCO[2])),
                          values=c("pos"=22, "neg"=23, "neutral"=24),
                          labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("c")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    pdf("output/metafor_summary_plot/Figure2_biomass_responses.pdf", width=14, height=6)
    plot_grid(p1a, p1b, p1c,
              rel_widths=c(1.0, 0.9, 1.0),
              labels=c(""), ncol=3, align="h", axis = "l")
    dev.off()
    
    p2a <- ggplot(plotDF2a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect_pct, fill=CO2_treatment), 
                   size=8, shape=21)+
        labs(x="LP treatment response (%)", y="P concentration")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20, angle=90),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-100, 75))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
                           labels=y.lab2)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("aCO2", "eCO2"),
                           values=c("grey", "black"),
                           labels=c(expression(aCO[2]), expression(eCO[2])), 
                           guide=F)+
        scale_fill_manual(name=expression(paste(CO[2], " treatment")),
                          limits=c("aCO2", "eCO2"),
                          values=c("grey", "black"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        ggtitle("a")
    
    
    p2b <- ggplot(plotDF2b)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect_pct, fill=P_treatment), 
                   size=8, shape=23)+
        labs(x=expression(paste(eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_blank(),
              axis.title.y=element_blank(),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-50, 50))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
                           labels=y.lab2)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"))+
        ggtitle("b")
    

    p2c <- ggplot(plotDF2)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos), height=0.5) + 
        geom_point(aes(y=id, x=interaction, fill=sig), 
                   size=8, shape=22)+
        labs(x=expression(paste("effect of LP on ", eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-40, 40))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
                           labels=c("","","",""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(1.5, 7.5, by=2),
                                               labels = y2.lab2))+
        scale_shape_manual(name=expression(paste("LP x ", eCO[2])),
                           values=c(22, 23, 24),
                           labels=c("Positive", "Negative", "Neutral"))+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("black", "grey", "white"),
                          labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("c")
    
    
    p3a <- ggplot(plotDF3a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect_pct, fill=CO2_treatment), 
                   size=8, shape=21)+
        labs(x="LP treatment response (%)", y="N concentration")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20, angle=90),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              plot.title = element_text(size = 18, face = "bold"),
              legend.position = "bottom", #c(0.05, 0.4),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-100, 75))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
                           labels=y.lab3)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("aCO2", "eCO2"),
                           values=c("grey", "black"),
                           labels=c(expression(aCO[2]), expression(eCO[2])), 
                           guide=F)+
        scale_fill_manual(name=expression(paste(CO[2], " treatment")),
                          limits=c("aCO2", "eCO2"),
                          values=c("grey", "black"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        ggtitle("d")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    p3b <- ggplot(plotDF3b)+ 
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
              legend.position = "bottom", #c(0.68, 0.4),
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-50, 50))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
                           labels=y.lab3)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"))+
        ggtitle("e")+
        guides(fill = guide_legend(title.position = "top"))
    

    p3c <- ggplot(plotDF3)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos), height=0.5) + 
        geom_point(aes(y=id, x=interaction, fill=sig), 
                   size=8, shape=22)+
        labs(x=expression(paste("effect of LP on ", eCO[2], " response (%)")), y="")+
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
        scale_x_continuous(limits=c(-40, 40))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
                           labels=c("","","",""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(1.5, 7.5, by=2),
                                               labels = y2.lab3))+
        scale_shape_manual(name=expression(paste("LP x ", eCO[2])),
                           breaks=c("pos", "neg", "neutral"),
                           values=c(22, 23, 24),
                           labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("f")+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("black", "grey", "white"),
                          labels=c("Positive", "Negative", "Neutral"))+
        guides(fill = guide_legend(title.position = "top"))
    
    pdf("output/metafor_summary_plot/Figure3_concentration_responses.pdf", width=14, height=10)
    plot_grid(p2a, p2b, p2c,
              p3a, p3b, p3c,
              rel_heights=c(1.0, 1.4),
              rel_widths=c(1.0, 0.9, 1.0),
              labels=c(""), ncol=3, align="h", axis = "l")
    dev.off()
    
    
    ### plotting
    p1a <- ggplot(plotDF7a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect_pct, fill=CO2_treatment), 
                   size=8, shape=21)+
        labs(x="LP treatment response (%)", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20, angle = 90),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "bottom", #c(0.65, 0.35),
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-100, 25))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5),
                           labels=y.lab7)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("aCO2", "eCO2"),
                           values=c("grey", "black"),
                           labels=c(expression(aCO[2]), expression(eCO[2])), 
                           guide=F)+
        scale_fill_manual(name=expression(paste(CO[2], " treatment")),
                          limits=c("aCO2", "eCO2"),
                          values=c("grey", "black"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        ggtitle("a")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    p1b <- ggplot(plotDF7b)+ 
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
              legend.position = "bottom", #c(0.05, 0.35),
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-50, 150))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5),
                           labels=y.lab7)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"))+
        ggtitle("b")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    p1c <- ggplot(plotDF7)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos), height=0.5) + 
        geom_point(aes(y=id, x=interaction, fill=sig), 
                   size=8, shape=22)+
        labs(x=expression(paste("effect of LP on ", eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=18),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "bottom", #c(0.05, 0.4),
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-50, 80))+
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
                          values=c("black", "grey", "white"),
                          labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("c")+
        guides(fill = guide_legend(title.position = "top"))
    
    pdf("output/metafor_summary_plot/Figure1_gas_exchange_responses.pdf", width=16, height=6)
    plot_grid(p1a, p1b, p1c,
              rel_widths=c(1.0, 0.9, 1.0),
              labels=c(""), ncol=3, align="h", axis = "l")
    dev.off()
    
    
    ### plotting
    p1a <- ggplot(plotDF10a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect_pct, fill=CO2_treatment), 
                   size=8, shape=21)+
        labs(x="LP treatment response (%)", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20, angle = 90),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "bottom", #c(0.65, 0.35),
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-100, 75))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5),
                           labels=y.lab10)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("aCO2", "eCO2"),
                           values=c("grey", "black"),
                           labels=c(expression(aCO[2]), expression(eCO[2])), 
                           guide=F)+
        scale_fill_manual(name=expression(paste(CO[2], " treatment")),
                          limits=c("aCO2", "eCO2"),
                          values=c("grey", "black"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        ggtitle("a")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    p1b <- ggplot(plotDF10b)+ 
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
              legend.position = "bottom", #c(0.05, 0.35),
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-25, 50))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5),
                           labels=y.lab10)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"))+
        ggtitle("b")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    p1c <- ggplot(plotDF10)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos), height=0.5) + 
        geom_point(aes(y=id, x=interaction, fill=sig), 
                   size=8, shape=22)+
        labs(x=expression(paste("effect of LP on ", eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "bottom", #c(0.05, 0.4),
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-30, 30))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5),
                           labels=c("","",""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(1.5, 5.5, by=2),
                                               labels = y2.lab10))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("pos", "neg", "neutral"),
                           values=c("#0072B2", "#D55E00", "#999999"),
                           labels=c("Positive", "Negative", "Neutral"),
                           guide = FALSE)+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("black", "grey", "white"),
                          labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("c")+
        guides(fill = guide_legend(title.position = "top"))
    
    pdf("output/metafor_summary_plot/Figure4_morphology_responses.pdf", width=16, height=6)
    plot_grid(p1a, p1b, p1c,
              rel_widths=c(1.0, 0.9, 1.0),
              labels=c(""), ncol=3, align="h", axis = "l")
    dev.off()
    
    
    ### plotting
    p1a <- ggplot(plotDF8a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect_pct, fill=CO2_treatment), 
                   size=8, shape=21)+
        labs(x="LP treatment response (%)", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=20),
              axis.title.y=element_text(size=20, angle = 90),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "bottom", #c(0.65, 0.35),
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-100, 30))+
        scale_y_continuous(breaks=c(1.5, 3.5),
                           labels=y.lab8)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("aCO2", "eCO2"),
                           values=c("grey", "black"),
                           labels=c(expression(aCO[2]), expression(eCO[2])), 
                           guide=F)+
        scale_fill_manual(name=expression(paste(CO[2], " treatment")),
                          limits=c("aCO2", "eCO2"),
                          values=c("grey", "black"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        ggtitle("a")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    p1b <- ggplot(plotDF8b)+ 
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
              legend.position = "bottom", #c(0.65, 0.35),
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-35, 25))+
        scale_y_continuous(breaks=c(1.5, 3.5),
                           labels=y.lab8)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"))+
        ggtitle("b")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    p1c <- ggplot(plotDF8)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos), height=0.5) + 
        geom_point(aes(y=id, x=interaction, fill=sig), 
                   size=8, shape=22)+
        labs(x=expression(paste("effect of LP on ", eCO[2], " response (%)")), y="")+
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
        scale_x_continuous(limits=c(-25, 25))+
        scale_y_continuous(breaks=c(1.5, 3.5),
                           labels=c("",""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(1.5, 3.5, by=2),
                                               labels = y2.lab8))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("pos", "neg", "neutral"),
                           values=c("#0072B2", "#D55E00", "#999999"),
                           labels=c("Positive", "Negative", "Neutral"),
                           guide = FALSE)+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("black", "grey", "white"),
                          labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("c")+
        guides(fill = guide_legend(title.position = "top"))
    
    pdf("output/metafor_summary_plot/Figure5_nutrient_uptake_responses.pdf", width=16, height=6)
    plot_grid(p1a, p1b, p1c,
              rel_widths=c(1.0, 0.9, 1.0),
              labels=c(""), ncol=3, align="h", axis = "l")
    dev.off()
    
    
    #### plotting
    ### plotting
    p1a <- ggplot(plotDF5a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect_pct, fill=CO2_treatment), 
                   size=4, shape=21)+
        labs(x="LP treatment response (%)", y="P content")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=18, angle=90),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-100, 650))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
                           labels=y.lab5)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("aCO2", "eCO2"),
                           values=c("grey", "black"),
                           labels=c(expression(aCO[2]), expression(eCO[2])), 
                           guide=F)+
        scale_fill_manual(name=expression(paste(CO[2], " treatment")),
                          limits=c("aCO2", "eCO2"),
                          values=c("grey", "black"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        ggtitle("a")
    
    
    p1b <- ggplot(plotDF5b)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect_pct, fill=P_treatment), 
                   size=4, shape=23)+
        labs(x=expression(paste(eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
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
        scale_x_continuous(limits=c(-75, 125))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
                           labels=y.lab5)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"))+
        ggtitle("b")
    
    p1c <- ggplot(plotDF5)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos), height=0.5) + 
        geom_point(aes(y=id, x=interaction, fill=sig), 
                   size=4, shape=22)+
        labs(x=expression(paste("effect of LP on ", eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-100, 110))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
                           labels=c("","","",""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(1.5, 7.5, by=2),
                                               labels = y2.lab5))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("pos", "neg", "neutral"),
                           values=c("#0072B2", "#D55E00", "#999999"),
                           labels=c("Positive", "Negative", "Neutral"),
                           guide = FALSE)+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("black", "grey", "white"),
                          labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("c")
    
    
    p2a <- ggplot(plotDF6a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect_pct, fill=CO2_treatment), 
                   size=4, shape=21)+
        labs(x="LP treatment response (%)", y="N content")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=18, angle=90),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-100, 650))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
                           labels=y.lab6)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("aCO2", "eCO2"),
                           values=c("grey", "black"),
                           labels=c(expression(aCO[2]), expression(eCO[2])), 
                           guide=F)+
        scale_fill_manual(name=expression(paste(CO[2], " treatment")),
                          limits=c("aCO2", "eCO2"),
                          values=c("grey", "black"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        ggtitle("d")
    
    
    p2b <- ggplot(plotDF6b)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect_pct, fill=P_treatment), 
                   size=4, shape=23)+
        labs(x=expression(paste(eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
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
        scale_x_continuous(limits=c(-75, 125))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
                           labels=y.lab6)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"))+
        ggtitle("e")
    
    p2c <- ggplot(plotDF6)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos), height=0.5) + 
        geom_point(aes(y=id, x=interaction, fill=sig), 
                   size=4, shape=22)+
        labs(x=expression(paste("effect of LP on ", eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-100, 110))+
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
                          values=c("black", "grey", "white"),
                          labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("f")
    
    
    ### plotting
    p3a <- ggplot(plotDF9a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect_pct, fill=CO2_treatment), 
                   size=4, shape=21)+
        labs(x="LP treatment response (%)", y="Nutrient use efficiency")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=18, angle = 90),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-100, 650))+
        scale_y_continuous(breaks=c(1.5, 3.5),
                           labels=y.lab9)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("aCO2", "eCO2"),
                           values=c("grey", "black"),
                           labels=c(expression(aCO[2]), expression(eCO[2])), 
                           guide=F)+
        scale_fill_manual(name=expression(paste(CO[2], " treatment")),
                          limits=c("aCO2", "eCO2"),
                          values=c("grey", "black"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        ggtitle("g")
    
    
    p3b <- ggplot(plotDF9b)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect_pct, fill=P_treatment), 
                   size=4, shape=23)+
        labs(x=expression(paste(eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
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
        scale_x_continuous(limits=c(-75, 125))+
        scale_y_continuous(breaks=c(1.5, 3.5),
                           labels=y.lab9)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"))+
        ggtitle("h")
    
    
    p3c <- ggplot(plotDF9)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos), height=0.5) + 
        geom_point(aes(y=id, x=interaction, fill=sig), 
                   size=4, shape=22)+
        labs(x=expression(paste("effect of LP on ", eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "none",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-100, 110))+
        scale_y_continuous(breaks=c(1.5, 3.5),
                           labels=c("",""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(1.5, 3.5, by=2),
                                               labels = y2.lab9))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("pos", "neg", "neutral"),
                           values=c("#0072B2", "#D55E00", "#999999"),
                           labels=c("Positive", "Negative", "Neutral"),
                           guide = FALSE)+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("black", "grey", "white"),
                          labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("i")
    
    
    p4a <- ggplot(plotDF4a)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=CO2_treatment)) + 
        geom_point(aes(y=id, x=P_effect_pct, fill=CO2_treatment), 
                   size=4, shape=21)+
        labs(x="LP treatment response (%)", y="NP ratio")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=18, angle=90),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "bottom",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-100, 900))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
                           labels=y.lab4)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("aCO2", "eCO2"),
                           values=c("grey", "black"),
                           labels=c(expression(aCO[2]), expression(eCO[2])), 
                           guide=F)+
        scale_fill_manual(name=expression(paste(CO[2], " treatment")),
                          limits=c("aCO2", "eCO2"),
                          values=c("grey", "black"),
                          labels=c(expression(aCO[2]), expression(eCO[2])))+
        ggtitle("j")+
        guides(fill = guide_legend(title.position = "top"))
    
    
    p4b <- ggplot(plotDF4b)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect_pct, fill=P_treatment), 
                   size=4, shape=23)+
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
              legend.position = "bottom",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-75, 125))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
                           labels=y.lab4)+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("grey", "black"),
                           labels=c("HP", "LP"),
                           guide=F)+
        scale_fill_manual(name=paste("P treatment"),
                          limits=c("eP", "aP"),
                          values=c("grey", "black"),
                          labels=c("HP", "LP"))+
        ggtitle("k")+
        guides(fill = guide_legend(title.position = "top"))
    
    p4c <- ggplot(plotDF4)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos), height=0.5) + 
        geom_point(aes(y=id, x=interaction, fill=sig), 
                   size=4, shape=22)+
        labs(x=expression(paste("effect of LP on ", eCO[2], " response (%)")), y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=18), 
              axis.text.x = element_text(size=16),
              axis.text.y=element_text(size=16),
              axis.title.y=element_text(size=18),
              legend.text=element_text(size=16),
              legend.title=element_text(size=18),
              panel.grid.major=element_blank(),
              legend.position = "bottom",
              plot.title = element_text(size = 18, face = "bold"),
              legend.background = element_rect(fill="white",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-100, 110))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5),
                           labels=c("","","",""),
                           sec.axis = sec_axis(~., name = "", breaks=seq(1.5, 7.5, by=2),
                                               labels = y2.lab4))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("pos", "neg", "neutral"),
                           values=c("#0072B2", "#D55E00", "#999999"),
                           labels=c("Positive", "Negative", "Neutral"),
                           guide = FALSE)+
        scale_fill_manual(name=expression(paste("LP x ", eCO[2])),
                          limits=c("pos", "neg", "neutral"),
                          values=c("black", "grey", "white"),
                          labels=c("Positive", "Negative", "Neutral"))+
        ggtitle("l")+
        guides(fill = guide_legend(title.position = "top"))
    
    pdf("output/metafor_summary_plot/FigureS3_other_responses.pdf", width=16, height=16)
    plot_grid(p1a, p1b, p1c,
              p2a, p2b, p2c,
              p3a, p3b, p3c,
              p4a, p4b, p4c,
              rel_heights=c(1.0, 1.0, 1.0, 1.5),
              rel_widths=c(1.0, 0.9, 1.0),
              labels=c(""), ncol=3, align="h", axis = "l")
    dev.off()
    
    
    
    
}