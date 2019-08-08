process_Thompson_2019 <- function() {
    inDF <- read.csv("data/Thompson_etal_PCO2.csv")
    
    sumDF <- summaryBy(total.p+per.n+np+rgr+photosynthesis~treat.CO2+treat.P+species, 
                       FUN=c(mean, sd), keep.names=T, data=inDF, na.rm=T)
    
    
    write.csv(sumDF, "output/metafor_summary_plot/Thompson_processed.csv", row.names=F)
    
    
}