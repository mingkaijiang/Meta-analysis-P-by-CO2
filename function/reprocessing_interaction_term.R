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
    #inDF$interaction <- (inDF$eCeP_mean/inDF$aCeP_mean)/(inDF$eCaP_mean/inDF$aCaP_mean)
    
    ### this should be the same as column Interaction_multiplicative_aCeP
    inDF$interaction <- (inDF$eCaP_mean/inDF$aCaP_mean)/(inDF$eCeP_mean/inDF$aCeP_mean)
    
    ### Calculate the log of response ratio to linearize the data
    inDF$log_interaction <- log(inDF$interaction)
    
    ### calculate v, the variance of the log response ratio for P by CO2 interaction
    #inDF$v_variance <- (inDF$eCeP_sd^2/(inDF$Sample.Size*(inDF$eCeP_mean)^2))+
    #    (inDF$eCaP_sd^2/(inDF$Sample.Size*(inDF$eCaP_mean)^2))+
    #    (inDF$aCeP_sd^2/(inDF$Sample.Size*(inDF$aCeP_mean)^2))+
    #    (inDF$aCaP_sd^2/(inDF$Sample.Size*(inDF$aCaP_mean)^2))
    
    inDF$v_variance <- (inDF$eCaP_sd^2/(inDF$Sample.Size*(inDF$eCaP_mean)^2))+
        (inDF$eCeP_sd^2/(inDF$Sample.Size*(inDF$eCeP_mean)^2))+
        (inDF$aCeP_sd^2/(inDF$Sample.Size*(inDF$aCeP_mean)^2))+
        (inDF$aCaP_sd^2/(inDF$Sample.Size*(inDF$aCaP_mean)^2))
    
    ### many studies do not report sd, hence variance needs to be calculated using proxies
    ### here variance for missing data of each study is calculated assuming
    ### mean variance of the remaining studies within each data variable
    
    for (i in unique(inDF$Variable)) {
        
        ### calcualte mean variance for the subset
        test1 <- subset(inDF, Variable == i)
        mean.variance <- mean(test1$v_variance, na.rm=T)
        
        ### assign the mean variance to missing data
        inDF$v_variance[inDF$Variable==i] <- ifelse(is.na(inDF$v_variance[inDF$Variable==i]), mean.variance, 
                                                    inDF$v_variance[inDF$Variable==i])
        
        inDF$v_variance[inDF$Variable==i & is.na(inDF$v_variance)] <- mean.variance
    }
    
    inDF$Vegetation_type <- as.character(inDF$Vegetation_type)
    
    inDF$Vegetation_type[inDF$Vegetation_type=="Grass Forb Legume"] <- "GFL"
    
    outDF <- inDF
    
    
    return(outDF)
}