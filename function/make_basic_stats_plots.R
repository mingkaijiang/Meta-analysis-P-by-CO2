make_basic_summary_stats_plots <- function(test) {
    ### This function processes the dataframe with some basic summaries
    
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
        xlab("eP / aP")
    

    ### summary histgram of treatments
    pdf("output/summary_basic_treatment.pdf", width=8, height=8)
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
        scale_x_discrete(name="Category",
                         breaks=c( "Biomass", "Concentration", "Morphology","Gas Exchange",
                                   "Nutrient Uptake", "Nutrient Ratio", "Resource Use Efficiency", "Other"))
    
    pdf("output/summary_data_variable.pdf", width=12, height=8)
    plot(p1)
    dev.off()
    
}