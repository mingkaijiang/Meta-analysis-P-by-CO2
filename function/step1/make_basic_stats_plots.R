make_basic_summary_stats_plots <- function(inDF) {
    
    inDF <- inDF[inDF$Mycorrhizae_2 %in%c("ECM", "AM"),]

    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    #### Summary of treatment
    
    p1 <- ggplot()+
        geom_histogram(data=inDF, aes(Trt_aCO2), binwidth=25)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        ylab("Data entry count")+
        xlab(expression(paste(aCO[2], " (ppm)")))
    
    p2 <- ggplot()+
        geom_histogram(data=inDF, aes(Trt_eCO2), binwidth=25)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        ylab("Data entry count")+
        xlab(expression(paste(eCO[2], " (ppm)")))
    
    med <- median(inDF$Trt_eC_by_aC, na.rm=T)
    mea <- mean(inDF$Trt_eC_by_aC, na.rm=T)
    
    p3 <- ggplot()+
        geom_histogram(data=inDF, aes(Trt_eC_by_aC), binwidth=0.1)+
        geom_vline(xintercept=med)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        ylab("Data entry count")+
        xlab(expression(paste(eCO[2], " / ", aCO[2])))
    
    inDF$LP_HP <- inDF$Trt_aP / inDF$Trt_eP
    
    med <- median(inDF$LP_HP, na.rm=T)
    mea <- mean(inDF$LP_HP, na.rm=T)
    
    p4 <- ggplot()+
        geom_histogram(data=inDF, aes(LP_HP), binwidth=0.02)+
        geom_vline(xintercept=med)+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        ylab("Data entry count")+
        xlab("LP / HP")
    
    ### summary histgram of treatments
    pdf("output/step1/Figure_S2.pdf", width=8, height=8)
    plot_grid(p1, p2, p3, p4,
              labels="", ncol=2, align="v", axis = "l")
    grid.text(grid.labs,x = c(0.12, 0.62, 0.12, 0.62),
              y = c(0.95, 0.95, 0.46, 0.46),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    
    #### summary of variables
    p1 <- ggplot()+
        geom_bar(data=inDF, aes(Category),stat="count")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        ylab("Number of data entry")+
        scale_x_discrete(name="Response category",
                         breaks=c( "Biomass", "Concentration", "Morphology","Gas Exchange",
                                   "Nutrient Uptake", "Nutrient Ratio", "Resource Use Efficiency"))
    
    #### summary of vegetation types
    p2 <- ggplot()+
        geom_bar(data=inDF, aes(Vegetation_type),stat="count")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_blank(), 
              axis.text.x = element_blank(),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        ylab("Number of data entry")+
        scale_x_discrete(name="Vegetation type",
                         breaks=c( "Woody", "Nonwoody"),
                         labels=c(  "Woody", "Nonwoody"))
    
    catDF <- unique(inDF[c("Literature", "Category")])
    vegDF <- unique(inDF[c("Literature", "Vegetation_type")])
    
    p3 <- ggplot()+
        geom_bar(data=catDF, aes(Category),stat="count")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12, angle=90, hjust=1),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        ylab("Number of study")+
        scale_x_discrete(name="Category",
                         breaks=c( "Biomass", "Concentration", "Morphology","Gas Exchange",
                                   "Nutrient Uptake", "Nutrient Ratio", "Resource Use Efficiency"))
    
    #### summary of vegetation types
    p4 <- ggplot()+
        geom_bar(data=vegDF, aes(Vegetation_type),stat="count")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12, angle=90, hjust=1),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        ylab("Number of study")+
        scale_x_discrete(name="Vegetation type",
                         breaks=c(  "Woody", "Nonwoody"),
                         labels=c(  "Woody", "Nonwoody"))
    
    
    #pdf("output/step1/summary_data_variable.pdf", width=12, height=8)
    #plot_grid(p1, p2, p3, p4,
    #          labels="", ncol=2, align="v", axis = "l",
    #          rel_heights = c(0.8, 0.8, 1.6, 1.6))
    #grid.text(grid.labs,x = c(0.46, 0.95, 0.46, 0.95),
    #          y = c(0.95, 0.95, 0.46, 0.46),
    #          gp=gpar(fontsize=16, col="black", fontface="bold"))
    #dev.off()
    
    
    
    
    
    catDF <- unique(inDF[c("Literature", "Category", "Vegetation_type")])
    
    p1 <- ggplot()+
        geom_bar(data=catDF, aes(Category, fill=Vegetation_type),stat="count")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12, angle=90, hjust=1),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        ylab("Study count")+
        scale_x_discrete(name="Category",
                         breaks=c( "Biomass", "Concentration", "Morphology","Gas Exchange",
                                   "Nutrient Uptake", "Nutrient Ratio", "Resource Use Efficiency", "Other"))+
        scale_fill_discrete(name="Vegetation type")+
        theme(legend.justification=c(0,1), legend.position=c(0.65,0.9))
        
    #pdf("output/step1/data_summary_study_count_vegetation_category.pdf")
    #plot(p1)
    #dev.off()
    
    #inDF$Category <- as.factor(inDF$Category)
    
    #### summary of variables
    p1 <- ggplot()+
        geom_bar(data=inDF, aes(Category),stat="count")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12, angle=90, hjust=1),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        ylab("Number of data entry")+
        scale_x_discrete(name="",
                         breaks=c( "Biomass", "Concentration", "Morphology","Gas Exchange",
                                   "Nutrient Uptake", "Nutrient Ratio", "Resource Use Efficiency"))
    
    #### summary of vegetation types
    p2 <- ggplot()+
        geom_bar(data=inDF, aes(Vegetation_type, fill=Mycorrhizae_2),stat="count")+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12, angle=90, hjust=1),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.position="bottom",
              legend.text.align=0)+
        ylab("Number of data entry")+
        scale_x_discrete(name="",
                         breaks=c( "Woody", "Nonwoody"),
                         labels=c(  "Woody", "Nonwoody"))+
        scale_fill_manual(name="Mycorrhizal type",
                          values=c("black", "lightgrey"),
                          labels=c("AM", "ECM"))
    
    
    
    pdf("output/step1/Figure_1.pdf", width=12, height=6)
    plot_grid(p1, p2,
              labels="auto", ncol=2, align="v", axis = "l",
              rel_widths = c(1, 0.5))
    dev.off()
    
}
