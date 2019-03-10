### This script plot bar chart for all significant response ratio means and confidence interval

plot_significant_response_ratio_100 <- function() {
    
    ### prepare df
    myDF <- data.frame(c("Leaf biomass", "Stem biomass", "Root biomass", "Total biomass",
                         "Leaf N content", "Stem N content", "Root N content", "Total N content",
                         "Leaf P content", "Stem P content", "Root P content", "Total P content",
                         "Leaf N concentration", "Stem N concentration", "Root N concentration", "Total N concentration",
                         "Leaf P concentration", "Stem P concentration", "Root P concentration", "Total P concentration",
                         "CO2 assimilation rate", "Stomatal conductance", "Leaf area", "SLA", "LMA",
                         "Root length", "N uptake", "P uptake", "WUE", "NUE", "PUE"), NA, NA, NA, NA, NA, NA)
    colnames(myDF) <- c("Variable", "Mean", "Pos", "Neg", "ne", "ns", "Category")
    myDF$Category <- c(rep("Biomass", 12), rep("Concentration", 8), rep("Gas exchange", 2),
                       rep("Morphology", 4), rep("Nutrient uptake", 2), rep("Resource use efficiency", 3))
    
    ### assign values
    ## biomass
    myDF$Mean[myDF$Variable=="Leaf biomass"] <- 0.2
    myDF$Neg[myDF$Variable=="Leaf biomass"] <- 0.09
    myDF$Pos[myDF$Variable=="Leaf biomass"] <- 0.31
    myDF$ns[myDF$Variable=="Leaf biomass"] <- 17
    myDF$ne[myDF$Variable=="Leaf biomass"] <- 85

    myDF$Mean[myDF$Variable=="Stem biomass"] <- 0.16
    myDF$Neg[myDF$Variable=="Stem biomass"] <- 0.04
    myDF$Pos[myDF$Variable=="Stem biomass"] <- 0.28
    myDF$ns[myDF$Variable=="Stem biomass"] <- 6
    myDF$ne[myDF$Variable=="Stem biomass"] <- 20
    
    myDF$Mean[myDF$Variable=="Root biomass"] <- 0.15
    myDF$Neg[myDF$Variable=="Root biomass"] <- 0.05
    myDF$Pos[myDF$Variable=="Root biomass"] <- 0.26
    myDF$ns[myDF$Variable=="Root biomass"] <- 12
    myDF$ne[myDF$Variable=="Root biomass"] <- 43
    
    myDF$Mean[myDF$Variable=="Total biomass"] <- 0.13
    myDF$Neg[myDF$Variable=="Total biomass"] <- 0.04
    myDF$Pos[myDF$Variable=="Total biomass"] <- 0.22
    myDF$ns[myDF$Variable=="Total biomass"] <- 14
    myDF$ne[myDF$Variable=="Total biomass"] <- 39
    
    myDF$Mean[myDF$Variable=="Leaf N content"] <- 0.05
    myDF$Neg[myDF$Variable=="Leaf N content"] <- -0.11
    myDF$Pos[myDF$Variable=="Leaf N content"] <- 0.21
    myDF$ns[myDF$Variable=="Leaf N content"] <- 8
    myDF$ne[myDF$Variable=="Leaf N content"] <- 36
    
    myDF$Mean[myDF$Variable=="Stem N content"] <- NA
    myDF$Neg[myDF$Variable=="Stem N content"] <- NA
    myDF$Pos[myDF$Variable=="Stem N content"] <- NA
    myDF$ns[myDF$Variable=="Stem N content"] <- 2
    myDF$ne[myDF$Variable=="Stem N content"] <- 10
    
    myDF$Mean[myDF$Variable=="Root N content"] <- NA
    myDF$Neg[myDF$Variable=="Root N content"] <- NA
    myDF$Pos[myDF$Variable=="Root N content"] <- NA
    myDF$ns[myDF$Variable=="Root N content"] <- 3
    myDF$ne[myDF$Variable=="Root N content"] <- 16
    
    myDF$Mean[myDF$Variable=="Total N content"] <- NA
    myDF$Neg[myDF$Variable=="Total N content"] <- NA
    myDF$Pos[myDF$Variable=="Total N content"] <- NA
    myDF$ns[myDF$Variable=="Total N content"] <- 4
    myDF$ne[myDF$Variable=="Total N content"] <- 16
    
    myDF$Mean[myDF$Variable=="Leaf P content"] <- 0.01
    myDF$Neg[myDF$Variable=="Leaf P content"] <- -0.07
    myDF$Pos[myDF$Variable=="Leaf P content"] <- 0.08
    myDF$ns[myDF$Variable=="Leaf P content"] <- 12
    myDF$ne[myDF$Variable=="Leaf P content"] <- 51
    
    myDF$Mean[myDF$Variable=="Stem P content"] <- 0.04
    myDF$Neg[myDF$Variable=="Stem P content"] <- -0.19
    myDF$Pos[myDF$Variable=="Stem P content"] <- 0.27
    myDF$ns[myDF$Variable=="Stem P content"] <- 3
    myDF$ne[myDF$Variable=="Stem P content"] <- 14
    
    myDF$Mean[myDF$Variable=="Root P content"] <- -0.03
    myDF$Neg[myDF$Variable=="Root P content"] <- -0.15
    myDF$Pos[myDF$Variable=="Root P content"] <- 0.08
    myDF$ns[myDF$Variable=="Root P content"] <- 6
    myDF$ne[myDF$Variable=="Root P content"] <- 24
    
    myDF$Mean[myDF$Variable=="Total P content"] <- 0.13
    myDF$Neg[myDF$Variable=="Total P content"] <- -0.03
    myDF$Pos[myDF$Variable=="Total P content"] <- 0.29
    myDF$ns[myDF$Variable=="Total P content"] <- 7
    myDF$ne[myDF$Variable=="Total P content"] <- 24
    
    ## concentration
    myDF$Mean[myDF$Variable=="Leaf N concentration"] <- -0.03
    myDF$Neg[myDF$Variable=="Leaf N concentration"] <- -0.09
    myDF$Pos[myDF$Variable=="Leaf N concentration"] <- 0.02
    myDF$ns[myDF$Variable=="Leaf N concentration"] <- 7
    myDF$ne[myDF$Variable=="Leaf N concentration"] <- 39
    
    myDF$Mean[myDF$Variable=="Stem N concentration"] <- -0.12
    myDF$Neg[myDF$Variable=="Stem N concentration"] <- -0.22
    myDF$Pos[myDF$Variable=="Stem N concentration"] <- -0.01
    myDF$ns[myDF$Variable=="Stem N concentration"] <- 3
    myDF$ne[myDF$Variable=="Stem N concentration"] <- 14
    
    myDF$Mean[myDF$Variable=="Root N concentration"] <- NA
    myDF$Neg[myDF$Variable=="Root N concentration"] <- NA
    myDF$Pos[myDF$Variable=="Root N concentration"] <- NA
    myDF$ns[myDF$Variable=="Root N concentration"] <- 4
    myDF$ne[myDF$Variable=="Root N concentration"] <- 19
    
    myDF$Mean[myDF$Variable=="Total N concentration"] <- 0.05
    myDF$Neg[myDF$Variable=="Total N concentration"] <- -0.04
    myDF$Pos[myDF$Variable=="Total N concentration"] <- 0.15
    myDF$ns[myDF$Variable=="Total N concentration"] <- 4
    myDF$ne[myDF$Variable=="Total N concentration"] <- 10
    
    myDF$Mean[myDF$Variable=="Leaf P concentration"] <- -0.07
    myDF$Neg[myDF$Variable=="Leaf P concentration"] <- -0.12
    myDF$Pos[myDF$Variable=="Leaf P concentration"] <- -0.03
    myDF$ns[myDF$Variable=="Leaf P concentration"] <- 13
    myDF$ne[myDF$Variable=="Leaf P concentration"] <- 57
    
    myDF$Mean[myDF$Variable=="Stem P concentration"] <- -0.07
    myDF$Neg[myDF$Variable=="Stem P concentration"] <- -0.18
    myDF$Pos[myDF$Variable=="Stem P concentration"] <- 0.04
    myDF$ns[myDF$Variable=="Stem P concentration"] <- 4
    myDF$ne[myDF$Variable=="Stem P concentration"] <- 18
    
    myDF$Mean[myDF$Variable=="Root P concentration"] <- -0.09
    myDF$Neg[myDF$Variable=="Root P concentration"] <- -0.17
    myDF$Pos[myDF$Variable=="Root P concentration"] <- -0.02
    myDF$ns[myDF$Variable=="Root P concentration"] <- 10
    myDF$ne[myDF$Variable=="Root P concentration"] <- 37
    
    myDF$Mean[myDF$Variable=="Total P concentration"] <- NA
    myDF$Neg[myDF$Variable=="Total P concentration"] <- NA
    myDF$Pos[myDF$Variable=="Total P concentration"] <- NA
    myDF$ns[myDF$Variable=="Total P concentration"] <- 6
    myDF$ne[myDF$Variable=="Total P concentration"] <- 19
    
    ## gas exchange
    myDF$Mean[myDF$Variable=="CO2 assimilation rate"] <- 0.05
    myDF$Neg[myDF$Variable=="CO2 assimilation rate"] <- 0.01
    myDF$Pos[myDF$Variable=="CO2 assimilation rate"] <- 0.1
    myDF$ns[myDF$Variable=="CO2 assimilation rate"] <- 8
    myDF$ne[myDF$Variable=="CO2 assimilation rate"] <- 38
    
    myDF$Mean[myDF$Variable=="Stomatal conductance"] <- -0.24
    myDF$Neg[myDF$Variable=="Stomatal conductance"] <- -0.58
    myDF$Pos[myDF$Variable=="Stomatal conductance"] <- 0.11
    myDF$ns[myDF$Variable=="Stomatal conductance"] <- 4
    myDF$ne[myDF$Variable=="Stomatal conductance"] <- 13
    
    ## morphology
    myDF$Mean[myDF$Variable=="Leaf area"] <- 0.06
    myDF$Neg[myDF$Variable=="Leaf area"] <- -0.02
    myDF$Pos[myDF$Variable=="Leaf area"] <- 0.14
    myDF$ns[myDF$Variable=="Leaf area"] <- 8
    myDF$ne[myDF$Variable=="Leaf area"] <- 24
    
    myDF$Mean[myDF$Variable=="SLA"] <- 0.01
    myDF$Neg[myDF$Variable=="SLA"] <- -0.03
    myDF$Pos[myDF$Variable=="SLA"] <- 0.05
    myDF$ns[myDF$Variable=="SLA"] <- 7
    myDF$ne[myDF$Variable=="SLA"] <- 23
    
    myDF$Mean[myDF$Variable=="LMA"] <- 0.02
    myDF$Neg[myDF$Variable=="LMA"] <- -0.04
    myDF$Pos[myDF$Variable=="LMA"] <- 0.07
    myDF$ns[myDF$Variable=="LMA"] <- 7
    myDF$ne[myDF$Variable=="LMA"] <- 21
    
    myDF$Mean[myDF$Variable=="Root length"] <- -0.05
    myDF$Neg[myDF$Variable=="Root length"] <- -0.19
    myDF$Pos[myDF$Variable=="Root length"] <- 0.09
    myDF$ns[myDF$Variable=="Root length"] <- 5
    myDF$ne[myDF$Variable=="Root length"] <- 21
    
    ## nutrient uptkae
    myDF$Mean[myDF$Variable=="N uptake"] <- 0.11
    myDF$Neg[myDF$Variable=="N uptake"] <- 0.03
    myDF$Pos[myDF$Variable=="N uptake"] <- 0.19
    myDF$ns[myDF$Variable=="N uptake"] <- 7
    myDF$ne[myDF$Variable=="N uptake"] <- 30
    
    myDF$Mean[myDF$Variable=="P uptake"] <- 0.09
    myDF$Neg[myDF$Variable=="P uptake"] <- 0.000001
    myDF$Pos[myDF$Variable=="P uptake"] <- 0.18
    myDF$ns[myDF$Variable=="P uptake"] <- 7
    myDF$ne[myDF$Variable=="P uptake"] <- 35
    
    ## resource use efficiency
    myDF$Mean[myDF$Variable=="WUE"] <- -0.11
    myDF$Neg[myDF$Variable=="WUE"] <- -0.27
    myDF$Pos[myDF$Variable=="WUE"] <- 0.04
    myDF$ns[myDF$Variable=="WUE"] <- 4
    myDF$ne[myDF$Variable=="WUE"] <- 8
    
    myDF$Mean[myDF$Variable=="NUE"] <- NA
    myDF$Neg[myDF$Variable=="NUE"] <- NA
    myDF$Pos[myDF$Variable=="NUE"] <- NA
    myDF$ns[myDF$Variable=="NUE"] <- 3
    myDF$ne[myDF$Variable=="NUE"] <- 17
    
    myDF$Mean[myDF$Variable=="PUE"] <- NA
    myDF$Neg[myDF$Variable=="PUE"] <- NA
    myDF$Pos[myDF$Variable=="PUE"] <- NA
    myDF$ns[myDF$Variable=="PUE"] <- 5
    myDF$ne[myDF$Variable=="PUE"] <- 23
    
    ### assign color
    myDF$color <- ifelse(myDF$Pos < 0, "neg", ifelse(myDF$Neg > 0, "pos", "neutral"))
    
    ### subset
    plotDF1 <- subset(myDF, Category == "Biomass")
    plotDF2 <- subset(myDF, Category == "Concentration")
    plotDF3 <- subset(myDF, Category == "Gas exchange")
    plotDF4 <- subset(myDF, Category == "Morphology")
    plotDF5 <- subset(myDF, Category == "Nutrient uptake")
    plotDF6 <- subset(myDF, Category == "Resource use efficiency")
    
    ### prepare labels
    y.lab1 <- c("Leaf biomass"="Leaf",
                "Stem biomass"="Stem",
                "Root biomass"="Root",
                "Total biomass"="Total",
                "Leaf N content"="Leaf N",
                "Stem N content"="Stem N",
                "Root N content"="Root N",
                "Total N content"="Total N",
                "Leaf P content"="Leaf P",
                "Stem P content"="Stem P",
                "Root P content"="Root P",
                "Total P content"="Total P")
    
    y.lab2 <- c("Leaf N concentration"="Leaf N",
                "Stem N concentration"="Stem N",
                "Root N concentration"="Root N",
                "Total N concentration"="Total N",
                "Leaf P concentration"="Leaf P",
                "Stem P concentration"="Stem P",
                "Root P concentration"="Root P",
                "Total P concentration"="Total P")
    
    y.lab3 <- c("CO2 assimilation rate"="A",
                "Stomatal conductance"=expression(g[s]))
    
    y.lab4 <- c("Leaf area"="Leaf area",
                "SLA"="SLA",
                "LMA"="LMA",
                "Root length"="Root length")
    
    y.lab5 <- c("N uptake"="N uptake",
                "P uptake"="P uptake")
    
    y.lab6 <- c("WUE"="WUE",
                "NUE"="NUE",
                "PUE"="PUE")
    
    ### plotting
    p1 <- ggplot(plotDF1)+ 
        geom_segment(aes(y=Variable, x=Neg, 
                         yend=Variable, xend=Pos, color=color), 
                     size=6)+
        geom_point(aes(y=Variable, x=Mean, fill=color), 
                   size=4, shape=21)+
        labs(x="Response ratio", y="")+
        theme_linedraw()+
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="none")+
        geom_vline(xintercept = 0.0)+
        scale_x_continuous(limits=c(-1.0, 1.0))+
        scale_y_discrete(limits=c("Leaf biomass", "Stem biomass", "Root biomass", "Total biomass",
                                  "Leaf N content", "Stem N content", "Root N content", "Total N content",
                                  "Leaf P content", "Stem P content", "Root P content", "Total P content"), 
                         labels=y.lab1)+
        scale_color_manual(limits=c("pos", "neg", "neutral"),
                           values=c("green", "red", "grey"))+
        scale_fill_manual(limits=c("pos", "neg", "neutral"),
                           values=c("darkgreen", "brown", "black"))+
        annotate(geom="text", x=-0.95, y=c(1:12), label=expression(paste(n[s], "=")),
                 color="black")+
        annotate(geom="text", x=-0.85, y=c(1:12), label=plotDF1$ns,
                 color="black")+
        annotate(geom="text", x=-0.75, y=c(1:12), label=expression(paste(n[e], "=")),
                 color="black")+
        annotate(geom="text", x=-0.65, y=c(1:12), label=plotDF1$ne,
                 color="black")+
        annotate(geom="text", x=0.65, y=12, label="Biomass",
                 color="black")
    
    p2 <- ggplot(plotDF2)+ 
        geom_segment(aes(y=Variable, x=Neg, 
                         yend=Variable, xend=Pos, color=color), 
                     size=6)+
        geom_point(aes(y=Variable, x=Mean, fill=color), 
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
        geom_vline(xintercept = 0.0)+
        scale_x_continuous(limits=c(-1.0, 1.0))+
        scale_y_discrete(limits=c("Leaf N concentration", "Stem N concentration", "Root N concentration", "Total N concentration",
                                  "Leaf P concentration", "Stem P concentration", "Root P concentration", "Total P concentration"), 
                         labels=y.lab2)+
        scale_color_manual(limits=c("pos", "neg", "neutral"),
                           values=c("green", "red", "grey"))+
        scale_fill_manual(limits=c("pos", "neg", "neutral"),
                          values=c("darkgreen", "brown", "black"))+
        annotate(geom="text", x=-0.95, y=c(1:8), label=expression(paste(n[s], "=")),
                 color="black")+
        annotate(geom="text", x=-0.85, y=c(1:8), label=plotDF2$ns,
                 color="black")+
        annotate(geom="text", x=-0.75, y=c(1:8), label=expression(paste(n[e], "=")),
                 color="black")+
        annotate(geom="text", x=-0.65, y=c(1:8), label=plotDF2$ne,
                 color="black")+
        annotate(geom="text", x=0.65, y=8, label="Nutrient concentration",
                 color="black")
    
    p3 <- ggplot(plotDF3)+ 
        geom_segment(aes(y=Variable, x=Neg, 
                         yend=Variable, xend=Pos, color=color), 
                     size=6)+
        geom_point(aes(y=Variable, x=Mean, fill=color), 
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
        geom_vline(xintercept = 0.0)+
        scale_x_continuous(limits=c(-1.0, 1.0))+
        scale_y_discrete(limits=c("CO2 assimilation rate", "Stomatal conductance"), 
                         labels=y.lab3)+
        scale_color_manual(limits=c("pos", "neg", "neutral"),
                           values=c("green", "red", "grey"))+
        scale_fill_manual(limits=c("pos", "neg", "neutral"),
                          values=c("darkgreen", "brown", "black"))+
        annotate(geom="text", x=-0.95, y=c(1:2), label=expression(paste(n[s], "=")),
                 color="black")+
        annotate(geom="text", x=-0.85, y=c(1:2), label=plotDF3$ns,
                 color="black")+
        annotate(geom="text", x=-0.75, y=c(1:2), label=expression(paste(n[e], "=")),
                 color="black")+
        annotate(geom="text", x=-0.65, y=c(1:2), label=plotDF3$ne,
                 color="black")+
        annotate(geom="text", x=0.65, y=2, label="Gas exchange",
                 color="black")
    
    p4 <- ggplot(plotDF4)+ 
        geom_segment(aes(y=Variable, x=Neg, 
                         yend=Variable, xend=Pos, color=color), 
                     size=6)+
        geom_point(aes(y=Variable, x=Mean, fill=color), 
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
        geom_vline(xintercept = 0.0)+
        scale_x_continuous(limits=c(-1.0, 1.0))+
        scale_y_discrete(limits=c("Leaf area", "SLA", "LMA",
                                  "Root length"), 
                         labels=y.lab4)+
        scale_color_manual(limits=c("pos", "neg", "neutral"),
                           values=c("green", "red", "grey"))+
        scale_fill_manual(limits=c("pos", "neg", "neutral"),
                          values=c("darkgreen", "brown", "black"))+
        annotate(geom="text", x=-0.95, y=c(1:4), label=expression(paste(n[s], "=")),
                 color="black")+
        annotate(geom="text", x=-0.85, y=c(1:4), label=plotDF4$ns,
                 color="black")+
        annotate(geom="text", x=-0.75, y=c(1:4), label=expression(paste(n[e], "=")),
                 color="black")+
        annotate(geom="text", x=-0.65, y=c(1:4), label=plotDF4$ne,
                 color="black")+
        annotate(geom="text", x=0.65, y=4, label="Morphology",
                 color="black")
    
    p5 <- ggplot(plotDF5)+ 
        geom_segment(aes(y=Variable, x=Neg, 
                         yend=Variable, xend=Pos, color=color), 
                     size=6)+
        geom_point(aes(y=Variable, x=Mean, fill=color), 
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
        geom_vline(xintercept = 0.0)+
        scale_x_continuous(limits=c(-1.0, 1.0))+
        scale_y_discrete(limits=c("N uptake", "P uptake"), 
                         labels=y.lab5)+
        scale_color_manual(limits=c("pos", "neg", "neutral"),
                           values=c("green", "red", "grey"))+
        scale_fill_manual(limits=c("pos", "neg", "neutral"),
                          values=c("darkgreen", "brown", "black"))+
        annotate(geom="text", x=-0.95, y=c(1:2), label=expression(paste(n[s], "=")),
                 color="black")+
        annotate(geom="text", x=-0.85, y=c(1:2), label=plotDF5$ns,
                 color="black")+
        annotate(geom="text", x=-0.75, y=c(1:2), label=expression(paste(n[e], "=")),
                 color="black")+
        annotate(geom="text", x=-0.65, y=c(1:2), label=plotDF5$ne,
                 color="black")+
        annotate(geom="text", x=0.65, y=2, label="Plant nutrient uptake",
                 color="black")
    
    p6 <- ggplot(plotDF6)+ 
        geom_segment(aes(y=Variable, x=Neg, 
                         yend=Variable, xend=Pos, color=color), 
                     size=6)+
        geom_point(aes(y=Variable, x=Mean, fill=color), 
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
        geom_vline(xintercept = 0.0)+
        scale_x_continuous(limits=c(-1.0, 1.0))+
        scale_y_discrete(limits=c("WUE", "NUE", "PUE"), 
                         labels=y.lab6)+
        scale_color_manual(limits=c("pos", "neg", "neutral"),
                           values=c("green", "red", "grey"))+
        scale_fill_manual(limits=c("pos", "neg", "neutral"),
                          values=c("darkgreen", "brown", "black"))+
        annotate(geom="text", x=-0.95, y=c(1:3), label=expression(paste(n[s], "=")),
                 color="black")+
        annotate(geom="text", x=-0.85, y=c(1:3), label=plotDF6$ns,
                 color="black")+
        annotate(geom="text", x=-0.75, y=c(1:3), label=expression(paste(n[e], "=")),
                 color="black")+
        annotate(geom="text", x=-0.65, y=c(1:3), label=plotDF6$ne,
                 color="black")+
        annotate(geom="text", x=0.65, y=3, label="Resource use efficiency",
                 color="black")
    
    pdf("output/subplot_gradient/mean_confidence_interval_100.pdf", width=6, height=16)
    plot_grid(p6, p5, p4, p3, p2, p1,
              labels="", ncol=1, align="v", axis = "l",
              rel_heights=c(0.4, 0.3, 0.5, 0.3, 1.4, 2.0))
    dev.off()
    
    
}