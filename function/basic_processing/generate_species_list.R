generate_species_list <- function() {
    inDF <- subDF100
    
    test <- unique(inDF[c("Vegetation_type", "Species")])
    
    write.csv(test, "output/metafor_summary_plot/species_list.csv", row.names=F)
    
}