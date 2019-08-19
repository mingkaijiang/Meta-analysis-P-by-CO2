calculate_mean_median_experimental_duration <- function() {
    inDF <- subDF100
    
    inDF$Experiment_duration <- as.character(inDF$Experiment_duration)
    inDF$Experiment_duration <- gsub("growing season", "100", inDF$Experiment_duration)
    inDF$Experiment_duration <- as.numeric(inDF$Experiment_duration)
    
    summary(inDF$Experiment_duration)
}