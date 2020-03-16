testing_correlation_relationships <- function() {
    
    reDF <- reDF100
    
    ### create directory
    if(!dir.exists("output/relationship_tables")) {
        dir.create("output/relationship_tables", showWarnings = FALSE)
    }
    
    ### find studies where we have N uptake
    test1 <- subset(reDF, Variable == "Plant N uptake")
    studies <- as.character(unique(test1$Literature))
    tDF1 <- subset(reDF, Literature%in%studies)
    
    ### subset root biomass and total root length
    tDF2 <- subset(tDF1, Variable%in%c("Root biomass", "Plant N uptake"))
    write.csv(tDF2, "output/relationship_tables/root_biomass_plant_N_uptake.csv")
    
    tDF2 <- subset(tDF1, Variable%in%c("Total root length", "Plant N uptake"))
    write.csv(tDF2, "output/relationship_tables/root_length_plant_N_uptake.csv")
    
    
    ### find studies where we have P uptake
    test1 <- subset(reDF, Variable == "Plant P uptake")
    studies <- as.character(unique(test1$Literature))
    tDF1 <- subset(reDF, Literature%in%studies)
    
    ### subset root biomass and total root length
    tDF2 <- subset(tDF1, Variable%in%c("Root biomass", "Plant P uptake"))
    write.csv(tDF2, "output/relationship_tables/root_biomass_plant_P_uptake.csv")
    
    tDF2 <- subset(tDF1, Variable%in%c("Total root length", "Plant P uptake"))
    write.csv(tDF2, "output/relationship_tables/root_length_plant_P_uptake.csv")
    
    
}