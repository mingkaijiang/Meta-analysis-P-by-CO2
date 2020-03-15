generate_species_list <- function(inDF) {

    out <- unique(inDF[c("Vegetation_type", "Species")])
    
    write.csv(out, "output/step1/species_list.csv", row.names=F)
    
}