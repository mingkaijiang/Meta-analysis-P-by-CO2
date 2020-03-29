reduce_duplicated_time_data_entries <- function(inDF) {
    
    ### prepare outputDF
    outDF <- c()
    
    ### generate literature to loop the data
    lit.list <- unique(inDF$Literature)
    
    ### loop based on literature
    for (i in 1:length(lit.list)) {
        tmpDF <- subset(inDF, Literature==lit.list[i])
        
        ### generate species list to loop through the species
        species.list <- unique(tmpDF$Species)
        
        for (k in 1:length(species.list)) {
            tmpDF2 <- subset(tmpDF, Species==species.list[k])
            
            ### create variable list
            var.list <- unique(tmpDF2$Variable)
            
            for (j in 1:length(var.list)) {
                tmpDF3 <- subset(tmpDF2, Variable == var.list[j])
                
                ### check experimental duration
                dur.list <- unique(tmpDF3$Experiment_duration) 
                l <- length(dur.list) 
                
                if (l == 1) {
                    outDF <- rbind(outDF, tmpDF3)
                } else {
                    max.dur <- max(dur.list)
                    tmpDF4 <- subset(tmpDF3, Experiment_duration == max.dur)
                    outDF <- rbind(outDF, tmpDF4)
                }
            }
        }
    }
    
    write.csv(outDF, "output/step2/P_by_CO2_data_V8_simplified.csv")
    
    return(outDF)
}