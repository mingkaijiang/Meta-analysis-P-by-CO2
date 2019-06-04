make_basic_summary_stats_plots <- function() {
    ### This function processes the dataframe with some basic summaries
    if(!dir.exists("output/basic_summary")) {
        dir.create("output/basic_summary", showWarnings = FALSE)
    }
    
    test <- subDF100
    
    #### Summary of treatment
    
    p1 <- ggplot()+
        geom_histogram(data=test, aes(Trt_aCO2), binwidth=25)+
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
        geom_histogram(data=test, aes(Trt_eCO2), binwidth=100)+
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
    
    p3 <- ggplot()+
        geom_histogram(data=test, aes(Trt_eC_by_aC), binwidth=0.1)+
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
    
    p4 <- ggplot()+
        geom_histogram(data=test, aes(Trt_eP_by_aP), binwidth=0.1)+
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
        xlab("HP / LP")
    

    ### summary histgram of treatments
    pdf("output/basic_summary/summary_basic_treatment.pdf", width=8, height=8)
    grid.labs <- c("(a)", "(b)", "(c)", "(d)")
    
    plot_grid(p1, p2, p3, p4,
              labels="", ncol=2, align="v", axis = "l")
    grid.text(grid.labs,x = c(0.12, 0.62, 0.12, 0.62),
              y = c(0.95, 0.95, 0.46, 0.46),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    
    #### summary of variables
    p1 <- ggplot()+
        geom_bar(data=test, aes(Category),stat="count")+
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
        ylab("Data entry count")+
        scale_x_discrete(name="Category",
                         breaks=c( "Biomass", "Concentration", "Morphology","Gas Exchange",
                                   "Nutrient Uptake", "Nutrient Ratio", "Resource Use Efficiency", "Other"))
    
    #### summary of vegetation types
    p2 <- ggplot()+
        geom_bar(data=test, aes(Vegetation_type),stat="count")+
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
        ylab("Data entry count")+
        scale_x_discrete(name="Vegetation type",
                         breaks=c( "Grass Forb Legume", "Tree", "Crop"),
                         labels=c( "Grass & others", "Tree", "Crop"))
    
    catDF <- unique(test[c("Literature", "Category")])
    vegDF <- unique(test[c("Literature", "Vegetation_type")])
    
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
        ylab("Study count")+
        scale_x_discrete(name="Category",
                         breaks=c( "Biomass", "Concentration", "Morphology","Gas Exchange",
                                   "Nutrient Uptake", "Nutrient Ratio", "Resource Use Efficiency", "Other"))
    
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
        ylab("Study count")+
        scale_x_discrete(name="Vegetation type",
                         breaks=c( "Grass Forb Legume", "Tree", "Crop"),
                         labels=c( "Grass & others", "Tree", "Crop"))
    
    
    pdf("output/basic_summary/summary_data_variable.pdf", width=12, height=8)
    plot_grid(p1, p2, p3, p4,
              labels="", ncol=2, align="v", axis = "l",
              rel_heights = c(0.8, 0.8, 1.6, 1.6))
    grid.text(grid.labs,x = c(0.46, 0.95, 0.46, 0.95),
              y = c(0.95, 0.95, 0.46, 0.46),
              gp=gpar(fontsize=16, col="black", fontface="bold"))
    dev.off()
    
    catDF <- unique(test[c("Literature", "Category", "Vegetation_type")])
    
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
        
    pdf("output/basic_summary/data_summary_study_count_vegetation_category.pdf")
    plot(p1)
    dev.off()
    
}