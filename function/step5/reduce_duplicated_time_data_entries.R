reduce_duplicated_time_data_entries <- function(inDF) {
    
    ### prepare outputDF
    outDF <- c()
    
    ### generate literature to loop the data
    lit.list <- unique(inDF$Literature)
    
    ### loop
    for (i in 1:length(lit.list)) {
        tmpDF <- subset(inDF, Literature==lit.list[i])
        
        ### create variable list
        var.list <- unique(tmpDF$Variable)
        
        for (j in 1:length(var.list)) {
            tmpDF2 <- subset(tmpDF, Variable == var.list[j])
            
            ### check experimental duration
            dur.list <- unique(tmpDF2$Experiment_duration) 
            l <- length(dur.list) 
            
            if (l == 1) {
                outDF <- rbind(outDF, tmpDF2)
            } else {
                max.dur <- max(dur.list)
                tmpDF3 <- subset(tmpDF2, Experiment_duration == max.dur)
                outDF <- rbind(outDF, tmpDF3)
            }
            
        }
    }
    
    write.csv(outDF, "output/step5/P_by_CO2_data_V8_simplified.csv")
    
    return(outDF)
}