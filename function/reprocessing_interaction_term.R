reprocessing_interaction_term <- function(inDF) {
    ### This script reprocesses the data
    ### based on example given by Baig et al. 2015 GCB
    ### to recalculate the interaction response ratio (r), 
    ### and its variance.
    ### The variance can be calculated based on standard deviation
    ### but many data points do not report this value
    ### therefore, for these entries (i.e. missing standard deviation), 
    ### I used the mean sd of the same variable as a proxy. 
    
    
    ### calculate r, the response ratio of the P by CO2 interaction
    ### this should be the same as column Interaction_multiplicative_aCaP
    inDF$interaction <- (inDF$eCeP_mean/inDF$aCeP_mean)/(inDF$eCaP_mean/inDF$aCaP_mean)

    ### Calculate the log of response ratio to linearize the data
    inDF$log_interaction <- log(inDF$interaction)
    
    ### calculate v, the variance of the log response ratio for P by CO2 interaction
    inDF$v_variance <- (inDF$eCeP_sd^2/(inDF$Sample.Size*(inDF$eCeP_mean)^2))+
        (inDF$eCaP_sd^2/(inDF$Sample.Size*(inDF$eCaP_mean)^2))+
        (inDF$aCeP_sd^2/(inDF$Sample.Size*(inDF$aCeP_mean)^2))+
        (inDF$aCaP_sd^2/(inDF$Sample.Size*(inDF$aCaP_mean)^2))
    
    
    ### estimate overall interaction mean and variance for each variable
    outDF <- unique(inDF[c("Literature", "Category", "Variable")])
    
    return(outDF)
}