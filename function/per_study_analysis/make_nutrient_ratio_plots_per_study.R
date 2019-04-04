make_nutrient_ratio_plots_per_study <- function(inDF) {
    
    if(!dir.exists("output/individual_nutrient_ratio")) {
        dir.create("output/individual_nutrient_ratio", showWarnings = FALSE)
    }
    
    ### Subset biomass category
    bioDF <- subset(inDF, Category == "Nutrient Ratio")
    
    bioDF1 <- bioDF[bioDF$Variable %in% c("Total NP ratio", "Leaf NP ratio", "Stem NP ratio",
                                          "Root NP ratio", "Aboveground NP ratio"),]
    
    ### check how many studies are available for each data variable
    varDF <- unique(bioDF1[c("Literature", "Variable", "Vegetation_type")])
    
    p1 <- ggplot()+
        geom_bar(data=varDF, aes(Variable, fill=Vegetation_type),stat="count")+
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
        ylab("Study count")+
        xlab("N concentration")+
        scale_fill_discrete(name="Vegetation type")+
        scale_x_discrete(name="NP ratio", 
                         labels=c("AG", 
                                  "Leaf", 
                                  "Root",
                                  "Stem",
                                  "Total"))
    
    pdf("output/individual_nutrient_ratio/Nutrient_ratio_data_availability_plot.pdf")
    plot(p1)
    dev.off()

}