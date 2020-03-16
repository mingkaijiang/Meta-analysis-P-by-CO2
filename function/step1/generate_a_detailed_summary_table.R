generate_a_detailed_summary_table <- function(inDF) {
    outDF <- data.frame(c("Plant biomass", "N content", "P content", "N concentration", "P concentration",
                          "N:P ratio", "Leaf gas exchange", "Morphology", "Nutrient uptake", "NUE"), NA, NA,
                        NA, NA, NA, NA, NA, NA, NA, NA)
    colnames(outDF) <- c("Category", "studies", "entries", "aCO2_low", "aCO2_mean", "aCO2_high",
                         "eCO2_low", "eCO2_mean", "eCO2_high", "eC_aC", "HP_LP")
    
    
    ### plant biomass (excluding phosphorus and nitrogen contents)
    test <- subset(inDF,Variable%in%c("Total plant biomass", "Leaf biomass", "Stem biomass", "Root biomass"))
    outDF$entries[outDF$Category=="Plant biomass"] <- dim(test)[1]
    outDF$studies[outDF$Category=="Plant biomass"] <- length(unique(test$Literature))
    outDF$aCO2_low[outDF$Category=="Plant biomass"] <- min(test$Trt_aCO2)
    outDF$aCO2_mean[outDF$Category=="Plant biomass"] <- mean(test$Trt_aCO2)
    outDF$aCO2_high[outDF$Category=="Plant biomass"] <- max(test$Trt_aCO2)
    outDF$eCO2_low[outDF$Category=="Plant biomass"] <- min(test$Trt_eCO2)
    outDF$eCO2_mean[outDF$Category=="Plant biomass"] <- mean(test$Trt_eCO2)
    outDF$eCO2_high[outDF$Category=="Plant biomass"] <- max(test$Trt_eCO2)
    outDF$aCO2_low[outDF$Category=="Plant biomass"] <- min(test$Trt_aCO2)
    outDF$eC_aC[outDF$Category=="Plant biomass"] <- mean(test$Trt_eC_by_aC)
    outDF$HP_LP[outDF$Category=="Plant biomass"] <- mean(test$Trt_eP_by_aP)
    
    ### plant N content 
    test <- subset(inDF,Variable%in%c("Total plant N content", "Leaf N content", "Stem N content", "Root N content"))
    outDF$entries[outDF$Category=="N content"] <- dim(test)[1]
    outDF$studies[outDF$Category=="N content"] <- length(unique(test$Literature))
    outDF$aCO2_low[outDF$Category=="N content"] <- min(test$Trt_aCO2)
    outDF$aCO2_mean[outDF$Category=="N content"] <- mean(test$Trt_aCO2)
    outDF$aCO2_high[outDF$Category=="N content"] <- max(test$Trt_aCO2)
    outDF$eCO2_low[outDF$Category=="N content"] <- min(test$Trt_eCO2)
    outDF$eCO2_mean[outDF$Category=="N content"] <- mean(test$Trt_eCO2)
    outDF$eCO2_high[outDF$Category=="N content"] <- max(test$Trt_eCO2)
    outDF$aCO2_low[outDF$Category=="N content"] <- min(test$Trt_aCO2)
    outDF$eC_aC[outDF$Category=="N content"] <- mean(test$Trt_eC_by_aC)
    outDF$HP_LP[outDF$Category=="N content"] <- mean(test$Trt_eP_by_aP)
    
    ### plant P content 
    test <- subset(inDF,Variable%in%c("Total plant P content", "Leaf P content", "Stem P content", "Root P content"))
    outDF$entries[outDF$Category=="P content"] <- dim(test)[1]
    outDF$studies[outDF$Category=="P content"] <- length(unique(test$Literature))
    outDF$aCO2_low[outDF$Category=="P content"] <- min(test$Trt_aCO2)
    outDF$aCO2_mean[outDF$Category=="P content"] <- mean(test$Trt_aCO2)
    outDF$aCO2_high[outDF$Category=="P content"] <- max(test$Trt_aCO2)
    outDF$eCO2_low[outDF$Category=="P content"] <- min(test$Trt_eCO2)
    outDF$eCO2_mean[outDF$Category=="P content"] <- mean(test$Trt_eCO2)
    outDF$eCO2_high[outDF$Category=="P content"] <- max(test$Trt_eCO2)
    outDF$aCO2_low[outDF$Category=="P content"] <- min(test$Trt_aCO2)
    outDF$eC_aC[outDF$Category=="P content"] <- mean(test$Trt_eC_by_aC)
    outDF$HP_LP[outDF$Category=="P content"] <- mean(test$Trt_eP_by_aP)
    
    
    ### plant N concentration
    test <- subset(inDF,Variable%in%c("Total plant N concentration", "Leaf N concentration", "Stem N concentration", "Root N concentration"))
    outDF$entries[outDF$Category=="N concentration"] <- dim(test)[1]
    outDF$studies[outDF$Category=="N concentration"] <- length(unique(test$Literature))
    outDF$aCO2_low[outDF$Category=="N concentration"] <- min(test$Trt_aCO2)
    outDF$aCO2_mean[outDF$Category=="N concentration"] <- mean(test$Trt_aCO2)
    outDF$aCO2_high[outDF$Category=="N concentration"] <- max(test$Trt_aCO2)
    outDF$eCO2_low[outDF$Category=="N concentration"] <- min(test$Trt_eCO2)
    outDF$eCO2_mean[outDF$Category=="N concentration"] <- mean(test$Trt_eCO2)
    outDF$eCO2_high[outDF$Category=="N concentration"] <- max(test$Trt_eCO2)
    outDF$aCO2_low[outDF$Category=="N concentration"] <- min(test$Trt_aCO2)
    outDF$eC_aC[outDF$Category=="N concentration"] <- mean(test$Trt_eC_by_aC)
    outDF$HP_LP[outDF$Category=="N concentration"] <- mean(test$Trt_eP_by_aP)
    
    
    ### plant P concentration
    test <- subset(inDF,Variable%in%c("Total plant P concentration", "Leaf P concentration", "Stem P concentration", "Root P concentration"))
    outDF$entries[outDF$Category=="P concentration"] <- dim(test)[1]
    outDF$studies[outDF$Category=="P concentration"] <- length(unique(test$Literature))
    outDF$aCO2_low[outDF$Category=="P concentration"] <- min(test$Trt_aCO2)
    outDF$aCO2_mean[outDF$Category=="P concentration"] <- mean(test$Trt_aCO2)
    outDF$aCO2_high[outDF$Category=="P concentration"] <- max(test$Trt_aCO2)
    outDF$eCO2_low[outDF$Category=="P concentration"] <- min(test$Trt_eCO2)
    outDF$eCO2_mean[outDF$Category=="P concentration"] <- mean(test$Trt_eCO2)
    outDF$eCO2_high[outDF$Category=="P concentration"] <- max(test$Trt_eCO2)
    outDF$aCO2_low[outDF$Category=="P concentration"] <- min(test$Trt_aCO2)
    outDF$eC_aC[outDF$Category=="P concentration"] <- mean(test$Trt_eC_by_aC)
    outDF$HP_LP[outDF$Category=="P concentration"] <- mean(test$Trt_eP_by_aP)
    
    ### plant NP ratio
    test <- subset(inDF,Variable%in%c("Total NP ratio", "Leaf NP ratio", "Stem NP ratio", "Root NP ratio"))
    outDF$entries[outDF$Category=="N:P ratio"] <- dim(test)[1]
    outDF$studies[outDF$Category=="N:P ratio"] <- length(unique(test$Literature))
    outDF$aCO2_low[outDF$Category=="N:P ratio"] <- min(test$Trt_aCO2)
    outDF$aCO2_mean[outDF$Category=="N:P ratio"] <- mean(test$Trt_aCO2)
    outDF$aCO2_high[outDF$Category=="N:P ratio"] <- max(test$Trt_aCO2)
    outDF$eCO2_low[outDF$Category=="N:P ratio"] <- min(test$Trt_eCO2)
    outDF$eCO2_mean[outDF$Category=="N:P ratio"] <- mean(test$Trt_eCO2)
    outDF$eCO2_high[outDF$Category=="N:P ratio"] <- max(test$Trt_eCO2)
    outDF$aCO2_low[outDF$Category=="N:P ratio"] <- min(test$Trt_aCO2)
    outDF$eC_aC[outDF$Category=="N:P ratio"] <- mean(test$Trt_eC_by_aC)
    outDF$HP_LP[outDF$Category=="N:P ratio"] <- mean(test$Trt_eP_by_aP)
    
    ### gas exchange
    test <- subset(inDF,Variable%in%c("CO2 assimilation rate", "Stomatal conductance", "WUE"))
    outDF$entries[outDF$Category=="Leaf gas exchange"] <- dim(test)[1]
    outDF$studies[outDF$Category=="Leaf gas exchange"] <- length(unique(test$Literature))
    outDF$aCO2_low[outDF$Category=="Leaf gas exchange"] <- min(test$Trt_aCO2)
    outDF$aCO2_mean[outDF$Category=="Leaf gas exchange"] <- mean(test$Trt_aCO2)
    outDF$aCO2_high[outDF$Category=="Leaf gas exchange"] <- max(test$Trt_aCO2)
    outDF$eCO2_low[outDF$Category=="Leaf gas exchange"] <- min(test$Trt_eCO2)
    outDF$eCO2_mean[outDF$Category=="Leaf gas exchange"] <- mean(test$Trt_eCO2)
    outDF$eCO2_high[outDF$Category=="Leaf gas exchange"] <- max(test$Trt_eCO2)
    outDF$aCO2_low[outDF$Category=="Leaf gas exchange"] <- min(test$Trt_aCO2)
    outDF$eC_aC[outDF$Category=="Leaf gas exchange"] <- mean(test$Trt_eC_by_aC)
    outDF$HP_LP[outDF$Category=="Leaf gas exchange"] <- mean(test$Trt_eP_by_aP)
    
    ### morphology
    test <- subset(inDF,Variable%in%c("LAI", "SLA", "Specific root length", "Total leaf area", "Total root length", "LMA"))
    outDF$entries[outDF$Category=="Morphology"] <- dim(test)[1]
    outDF$studies[outDF$Category=="Morphology"] <- length(unique(test$Literature))
    outDF$aCO2_low[outDF$Category=="Morphology"] <- min(test$Trt_aCO2)
    outDF$aCO2_mean[outDF$Category=="Morphology"] <- mean(test$Trt_aCO2)
    outDF$aCO2_high[outDF$Category=="Morphology"] <- max(test$Trt_aCO2)
    outDF$eCO2_low[outDF$Category=="Morphology"] <- min(test$Trt_eCO2)
    outDF$eCO2_mean[outDF$Category=="Morphology"] <- mean(test$Trt_eCO2)
    outDF$eCO2_high[outDF$Category=="Morphology"] <- max(test$Trt_eCO2)
    outDF$aCO2_low[outDF$Category=="Morphology"] <- min(test$Trt_aCO2)
    outDF$eC_aC[outDF$Category=="Morphology"] <- mean(test$Trt_eC_by_aC)
    outDF$HP_LP[outDF$Category=="Morphology"] <- mean(test$Trt_eP_by_aP)
    
    ### nutrient use efficiency
    test <- subset(inDF,Variable%in%c("NUE", "PUE"))
    outDF$entries[outDF$Category=="NUE"] <- dim(test)[1]
    outDF$studies[outDF$Category=="NUE"] <- length(unique(test$Literature))
    outDF$aCO2_low[outDF$Category=="NUE"] <- min(test$Trt_aCO2)
    outDF$aCO2_mean[outDF$Category=="NUE"] <- mean(test$Trt_aCO2)
    outDF$aCO2_high[outDF$Category=="NUE"] <- max(test$Trt_aCO2)
    outDF$eCO2_low[outDF$Category=="NUE"] <- min(test$Trt_eCO2)
    outDF$eCO2_mean[outDF$Category=="NUE"] <- mean(test$Trt_eCO2)
    outDF$eCO2_high[outDF$Category=="NUE"] <- max(test$Trt_eCO2)
    outDF$aCO2_low[outDF$Category=="NUE"] <- min(test$Trt_aCO2)
    outDF$eC_aC[outDF$Category=="NUE"] <- mean(test$Trt_eC_by_aC)
    outDF$HP_LP[outDF$Category=="NUE"] <- mean(test$Trt_eP_by_aP)
    
    ### nutrient uptake
    test <- subset(inDF,Category=="Nutrient Uptake")
    outDF$entries[outDF$Category=="Nutrient uptake"] <- dim(test)[1]
    outDF$studies[outDF$Category=="Nutrient uptake"] <- length(unique(test$Literature))
    outDF$aCO2_low[outDF$Category=="Nutrient uptake"] <- min(test$Trt_aCO2)
    outDF$aCO2_mean[outDF$Category=="Nutrient uptake"] <- mean(test$Trt_aCO2)
    outDF$aCO2_high[outDF$Category=="Nutrient uptake"] <- max(test$Trt_aCO2)
    outDF$eCO2_low[outDF$Category=="Nutrient uptake"] <- min(test$Trt_eCO2)
    outDF$eCO2_mean[outDF$Category=="Nutrient uptake"] <- mean(test$Trt_eCO2)
    outDF$eCO2_high[outDF$Category=="Nutrient uptake"] <- max(test$Trt_eCO2)
    outDF$aCO2_low[outDF$Category=="Nutrient uptake"] <- min(test$Trt_aCO2)
    outDF$eC_aC[outDF$Category=="Nutrient uptake"] <- mean(test$Trt_eC_by_aC)
    outDF$HP_LP[outDF$Category=="Nutrient uptake"] <- mean(test$Trt_eP_by_aP)
    
    
    write.csv(outDF, "output/step1/detailed_summary_table.csv", row.names=F)
}