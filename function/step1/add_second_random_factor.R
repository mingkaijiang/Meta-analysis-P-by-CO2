add_second_random_factor <- function(inDF) {
    
    outDF <- c()
    
    inDF$random_factor2 <- c(1:1202)
    
    study.list <- unique(inDF$Literature)
    
    for (i in study.list) {
        subDF1 <- subset(inDF, Literature == i) 
        var.list <- unique(subDF1$Variable)
        
        for (j in var.list) {
            subDF2 <- subset(subDF1, Variable == j)
            
            ### count number of replicated data entries
            n <- dim(subDF2)[1]
            
            subDF2$random_factor2 <- c(1:n)
            
            outDF <- rbind(outDF, subDF2)
        }
    }
    
    return(outDF)
}