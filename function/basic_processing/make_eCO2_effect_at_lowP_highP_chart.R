### This script plot bar chart for all significant response ratio means and confidence interval

make_eCO2_effect_at_lowP_highP_chart <- function(sumDF) {
    
    ### This function processes the dataframe with some basic summaries
    if(!dir.exists("output/metafor_summary_plot")) {
        dir.create("output/metafor_summary_plot", showWarnings = FALSE)
    }
    
    ### prepare df
    sumDF$Category <- c(rep("Biomass", 24), rep("Concentration", 16), rep("Gas exchange", 4),
                       rep("Morphology", 8), rep("Nutrient uptake", 4), 
                       rep("Resource use efficiency", 6))
    sumDF$Pos <- sumDF$CO2_effect + sumDF$se
    sumDF$Neg <- sumDF$CO2_effect - sumDF$se
    
    ### assign color
    sumDF$sig <- ifelse(sumDF$Pos < 0, "neg", ifelse(sumDF$Neg > 0, "pos", "neutral"))
    
    ### subset
    plotDF1 <- subset(sumDF, Category == "Biomass")
    plotDF2 <- subset(sumDF, Category == "Concentration")
    plotDF3 <- subset(sumDF, Category == "Gas exchange")
    plotDF4 <- subset(sumDF, Category == "Morphology")
    plotDF5 <- subset(sumDF, Category == "Nutrient uptake")
    plotDF6 <- subset(sumDF, Category == "Resource use efficiency")
    
    ### prepare labels
    y.lab1 <- c("leaf_biomass"="Leaf",
                "stem_biomass"="Stem",
                "root_biomass"="Root",
                "total_biomass"="Total",
                "leaf_N_content"="Leaf N",
                "stem_N_content"="Stem N",
                "root_N_content"="Root N",
                "total_N_content"="Total N",
                "leaf_P_content"="Leaf P",
                "stem_P_content"="Stem P",
                "root_P_content"="Root P",
                "total_P_content"="Total P")
    
    y2.lab1 <- c(bquote(n[s]==.(plotDF1$ns[1])),
                 bquote(n[e]==.(plotDF1$ne[1])),
                 bquote(n[s]==.(plotDF1$ns[3])),
                 bquote(n[e]==.(plotDF1$ne[3])),
                 bquote(n[s]==.(plotDF1$ns[5])),
                 bquote(n[e]==.(plotDF1$ne[5])),
                 bquote(n[s]==.(plotDF1$ns[7])),
                 bquote(n[e]==.(plotDF1$ne[7])),
                 bquote(n[s]==.(plotDF1$ns[9])),
                 bquote(n[e]==.(plotDF1$ne[9])),
                 bquote(n[s]==.(plotDF1$ns[11])),
                 bquote(n[e]==.(plotDF1$ne[11])),
                 bquote(n[s]==.(plotDF1$ns[13])),
                 bquote(n[e]==.(plotDF1$ne[13])),
                 bquote(n[s]==.(plotDF1$ns[15])),
                 bquote(n[e]==.(plotDF1$ne[15])),
                 bquote(n[s]==.(plotDF1$ns[17])),
                 bquote(n[e]==.(plotDF1$ne[17])),
                 bquote(n[s]==.(plotDF1$ns[19])),
                 bquote(n[e]==.(plotDF1$ne[19])),
                 bquote(n[s]==.(plotDF1$ns[21])),
                 bquote(n[e]==.(plotDF1$ne[21])),
                 bquote(n[s]==.(plotDF1$ns[23])),
                 bquote(n[e]==.(plotDF1$ne[23])))
    
    y.lab2 <- c("leaf_N_concentration"="Leaf N",
                "stem_N_concentration"="Stem N",
                "root_N_concentration"="Root N",
                "total_N_concentration"="Total N",
                "leaf_P_concentration"="Leaf P",
                "stem_P_concentration"="Stem P",
                "root_P_concentration"="Root P",
                "total_P_concentration"="Total P")
    
    y2.lab2 <- c(bquote(n[s]==.(plotDF2$ns[1])),
                 bquote(n[e]==.(plotDF2$ne[1])),
                 bquote(n[s]==.(plotDF2$ns[3])),
                 bquote(n[e]==.(plotDF2$ne[3])),
                 bquote(n[s]==.(plotDF2$ns[5])),
                 bquote(n[e]==.(plotDF2$ne[5])),
                 bquote(n[s]==.(plotDF2$ns[7])),
                 bquote(n[e]==.(plotDF2$ne[7])),
                 bquote(n[s]==.(plotDF2$ns[9])),
                 bquote(n[e]==.(plotDF2$ne[9])),
                 bquote(n[s]==.(plotDF2$ns[11])),
                 bquote(n[e]==.(plotDF2$ne[11])),
                 bquote(n[s]==.(plotDF2$ns[13])),
                 bquote(n[e]==.(plotDF2$ne[13])),
                 bquote(n[s]==.(plotDF2$ns[15])),
                 bquote(n[e]==.(plotDF2$ne[15])))
    
    y.lab3 <- c("CO2_assimilation_rate"="A",
                "stomatal_conductance"=expression(g[s]))
    
    y2.lab3 <- c(bquote(n[s]==.(plotDF3$ns[1])),
                 bquote(n[e]==.(plotDF3$ne[1])),
                 bquote(n[s]==.(plotDF3$ns[3])),
                 bquote(n[e]==.(plotDF3$ne[3])))
    
    y.lab4 <- c("leaf_area"="Leaf area",
                "SLA"="SLA",
                "LMA"="LMA",
                "Root_length"="Root length")
    
    y2.lab4 <- c(bquote(n[s]==.(plotDF4$ns[1])),
                 bquote(n[e]==.(plotDF4$ne[1])),
                 bquote(n[s]==.(plotDF4$ns[3])),
                 bquote(n[e]==.(plotDF4$ne[3])),
                 bquote(n[s]==.(plotDF4$ns[5])),
                 bquote(n[e]==.(plotDF4$ne[5])),
                 bquote(n[s]==.(plotDF4$ns[7])),
                 bquote(n[e]==.(plotDF4$ne[7])))
    
    y.lab5 <- c("N_uptake"="N uptake",
                "P_uptake"="P uptake")
    
    y2.lab5 <- c(bquote(n[s]==.(plotDF5$ns[1])),
                 bquote(n[e]==.(plotDF5$ne[1])),
                 bquote(n[s]==.(plotDF5$ns[3])),
                 bquote(n[e]==.(plotDF5$ne[3])))
    
    y.lab6 <- c("WUE"="WUE",
                "NUE"="NUE",
                "PUE"="PUE")
    
    y2.lab6 <- c(bquote(n[s]==.(plotDF6$ns[1])),
                 bquote(n[e]==.(plotDF6$ne[1])),
                 bquote(n[s]==.(plotDF6$ns[3])),
                 bquote(n[e]==.(plotDF6$ne[3])),
                 bquote(n[s]==.(plotDF6$ns[5])),
                 bquote(n[e]==.(plotDF6$ne[5])))
    
    ### plotting
    p1 <- ggplot(plotDF1)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect, fill=P_treatment), 
                   size=4, shape=21)+
        labs(x="Response ratio", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_continuous(limits=c(-0.4, 0.6))+
        scale_y_continuous(breaks=c(1.5, 3.5, 5.5, 7.5, 
                                  9.5, 11.5, 13.5, 15.5,
                                  17.5, 19.5, 21.5, 23.5),
                         labels=y.lab1,
                         sec.axis = sec_axis(~., name = "", breaks=c(1:24),
                                             labels = y2.lab1))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("blue3", "red2"),
                           labels=c("hP", "lP"))+
        scale_fill_manual(name=paste("Means"),
                          limits=c("eP", "aP"),
                          values=c("blue3", "red2"),
                          labels=c("hP", "lP"))+
        ggtitle("Biomass")
    
    #plot(p1)

    p2 <- ggplot(plotDF2)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect, fill=P_treatment), 
                   size=4, shape=21)+
        labs(x=expression(paste(CO[2], " response ratio")), y="")+
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
              legend.position = c(0.7, 0.45),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_x_continuous(limits=c(-0.4, 0.6))+
        scale_y_continuous(breaks=c(25.5, 27.5, 29.5, 31.5, 
                                    33.5, 35.5, 37.5, 39.5),
                           labels=y.lab2,
                           sec.axis = sec_axis(~., name = "", breaks=c(25:40),
                                               labels = y2.lab2))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("blue3", "red2"),
                           labels=c("hP", "lP"))+
        scale_fill_manual(name=paste("Means"),
                          limits=c("eP", "aP"),
                          values=c("blue3", "red2"),
                          labels=c("hP", "lP"))+
        ggtitle("Nutrient concentration")
    
    #plot(p2)
    
    p3 <- ggplot(plotDF3)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect, fill=P_treatment), 
                   size=4, shape=21)+
        labs(x="Response ratio", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_continuous(limits=c(-0.4, 0.6))+
        scale_y_continuous(breaks=c(41.5, 43.5),
                           labels=y.lab3,
                           sec.axis = sec_axis(~., name = "", breaks=c(41:44),
                                               labels = y2.lab3))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("blue3", "red2"),
                           labels=c("hP", "lP"))+
        scale_fill_manual(name=paste("Means"),
                          limits=c("eP", "aP"),
                          values=c("blue3", "red2"),
                          labels=c("hP", "lP"))+
        ggtitle("Gas exchange")
    
    #plot(p3)
    
    p4 <- ggplot(plotDF4)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect, fill=P_treatment), 
                   size=4, shape=21)+
        labs(x="Response ratio", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_continuous(limits=c(-0.4, 0.6))+
        scale_y_continuous(breaks=c(45.5, 47.5, 49.5, 51.5),
                           labels=y.lab4,
                           sec.axis = sec_axis(~., name = "", breaks=c(45:52),
                                               labels = y2.lab4))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("blue3", "red2"),
                           labels=c("hP", "lP"))+
        scale_fill_manual(name=paste("Means"),
                          limits=c("eP", "aP"),
                          values=c("blue3", "red2"),
                          labels=c("hP", "lP"))+
        ggtitle("Morphology")
    
    #plot(p4)
    
    p5 <- ggplot(plotDF5)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect, fill=P_treatment), 
                   size=4, shape=21)+
        labs(x="Response ratio", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_continuous(limits=c(-0.4, 0.6))+
        scale_y_continuous(breaks=c(53.5, 55.5),
                           labels=y.lab5,
                           sec.axis = sec_axis(~., name = "", breaks=c(53:56),
                                               labels = y2.lab5))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("blue3", "red2"),
                           labels=c("hP", "lP"))+
        scale_fill_manual(name=paste("Means"),
                          limits=c("eP", "aP"),
                          values=c("blue3", "red2"),
                          labels=c("hP", "lP"))+
        ggtitle("Nutrient uptake")
    
    #plot(p5)
    
    p6 <- ggplot(plotDF6)+ 
        geom_vline(xintercept = 0.0)+
        geom_errorbarh(aes(y=id, xmin=Neg, xmax=Pos, color=P_treatment)) + 
        geom_point(aes(y=id, x=CO2_effect, fill=P_treatment), 
                   size=4, shape=21)+
        labs(x="Response ratio", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        scale_x_continuous(limits=c(-0.4, 0.6))+
        scale_y_continuous(breaks=c(57.5, 59.5, 61.5),
                           labels=y.lab6,
                           sec.axis = sec_axis(~., name = "", breaks=c(57:62),
                                               labels = y2.lab6))+
        scale_color_manual(name=paste("CIs"),
                           limits=c("eP", "aP"),
                           values=c("blue3", "red2"),
                           labels=c("hP", "lP"))+
        scale_fill_manual(name=paste("Means"),
                          limits=c("eP", "aP"),
                          values=c("blue3", "red2"),
                          labels=c("hP", "lP"))+
        ggtitle("Resource use efficiency")
    
    #plot(p6)
    
    pdf("output/metafor_summary_plot/CO2_effect_at_lowP_highP_all_results.pdf", width=6, height=20)
    plot_grid(p6, p5, p4, p3, p1, p2,
              labels="", ncol=1, align="v", axis = "l",
              rel_heights=c(0.4, 0.3, 0.5, 0.3, 1.6, 1.2))
    dev.off()
    
    
    }