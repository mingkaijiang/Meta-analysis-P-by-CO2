test_woody_plant_mycorrhizal_effect <- function(wdDF) {
    
    bDF <- subset(wdDF, Category == "Biomass")
    lDF <- subset(bDF, Variable == "Leaf biomass")
    rDF <- subset(bDF, Variable == "Root biomass")
    tDF <- subset(bDF, Variable == "Total plant biomass")
    
    
    ### total biomas
    am.list <- c("Cecropia insignis", "Cecropia longipes", "Cecropia peltata",
                 "Ficus insipida", "Guazuma ulmifolia", "Ochroma pyramidale",
                 "Trema micrantha", "Trichospermum mexicanum", "Citrus aurantium",
                 "Gossypoim hirsutum")
    
    ecm.list <- c("Pinus densiflora", "Populus deltoides", "Eucalyptus grandis")
    

    for (i in am.list) {
        tDF$Mycorrhizae[tDF$Species == i] <- "AM"
    }
    
    for (i in ecm.list) {
        tDF$Mycorrhizae[tDF$Species == i] <- "ECM"
    }
    
    tDF <- tDF[complete.cases(tDF$Species),]
    
    res <- rma(log_interaction, v_variance, 
               mods = ~factor(Mycorrhizae), data = tDF)
    
    print(res)
    
    
    #### leaf biomass
    am.list <- c("Citrus aurantium",
                 "Gossypoim hirsutum")
    
    ecm.list <- c("Populus deltoides", "Pinus ponderosa", "Pinus radiata",
                  "Pinus caribaea")
    
    
    for (i in am.list) {
        lDF$Mycorrhizae[lDF$Species == i] <- "AM"
    }
    
    for (i in ecm.list) {
        lDF$Mycorrhizae[lDF$Species == i] <- "ECM"
    }
    
    lDF <- lDF[complete.cases(lDF$Species),]
    
    res <- rma(log_interaction, v_variance, 
               mods = ~factor(Mycorrhizae), data = lDF)
    
    print(res)
    
    
    ### root biomass
    am.list <- c("Citrus aurantium",
                 "Gossypoim hirsutum")
    
    ecm.list <- c("Populus deltoides", "Pinus ponderosa")
    
    
    for (i in am.list) {
        rDF$Mycorrhizae[rDF$Species == i] <- "AM"
    }
    
    for (i in ecm.list) {
        rDF$Mycorrhizae[rDF$Species == i] <- "ECM"
    }
    
    rDF <- rDF[complete.cases(rDF$Species),]
    
    res <- rma(log_interaction, v_variance, 
               mods = ~factor(Mycorrhizae), data = rDF)
    
    print(res)
    
    ### leaf P concentration
    lpDF <- subset(wdDF, Variable == "Leaf P concentration")
    
    
    am.list <- c("Cecropia insignis", "Cecropia longipes", "Cecropia peltata",
                 "Ficus insipida", "Guazuma ulmifolia", "Ochroma pyramidale",
                 "Trema micrantha", "Trichospermum mexicanum", "Citrus aurantium",
                 "Gossypoim hirsutum")
    
    ecm.list <- c("Pinus densiflora", "Populus deltoides")
    
    
    for (i in am.list) {
        lpDF$Mycorrhizae[lpDF$Species == i] <- "AM"
    }
    
    for (i in ecm.list) {
        lpDF$Mycorrhizae[lpDF$Species == i] <- "ECM"
    }
    
    lpDF <- lpDF[complete.cases(lpDF$Species),]
    
    res <- rma(log_interaction, v_variance, 
               mods = ~factor(Mycorrhizae), data = lpDF)
    
    print(res)
    ### significant mycorrhizal difference on leaf P concentration
    
    ### leaf N concentration
    lnDF <- subset(wdDF, Variable == "Leaf N concentration")
    
    
    am.list <- c("Cecropia insignis", "Cecropia longipes", "Cecropia peltata",
                 "Ficus insipida", "Guazuma ulmifolia", "Ochroma pyramidale",
                 "Trema micrantha", "Trichospermum mexicanum", "Citrus aurantium")
    
    ecm.list <- c("Pinus densiflora")
    
    
    for (i in am.list) {
        lnDF$Mycorrhizae[lnDF$Species == i] <- "AM"
    }
    
    for (i in ecm.list) {
        lnDF$Mycorrhizae[lnDF$Species == i] <- "ECM"
    }
    
    lnDF <- lnDF[complete.cases(lnDF$Species),]
    
    res <- rma(log_interaction, v_variance, 
               mods = ~factor(Mycorrhizae), data = lnDF)
    
    print(res)
    
    
    ### photosynthesis
    aDF <- subset(wdDF, Variable == "CO2 assimilation rate")
    
    
    am.list <- c("Cecropia insignis", "Cecropia longipes", "Cecropia peltata",
                 "Ochroma pyramidale",
                 "Trema micrantha", "Trichospermum mexicanum", "Citrus aurantium")
    
    ecm.list <- c("Pinus densiflora", "Populus deltoides", "Eucalyptus grandis",
                  "Pinus radiata")
    
    
    for (i in am.list) {
        aDF$Mycorrhizae[aDF$Species == i] <- "AM"
    }
    
    for (i in ecm.list) {
        aDF$Mycorrhizae[aDF$Species == i] <- "ECM"
    }
    
    aDF <- aDF[complete.cases(aDF$Species),]
    
    res <- rma(log_interaction, v_variance, 
               mods = ~factor(Mycorrhizae), data = aDF)
    
    print(res)
    
}