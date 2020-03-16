make_step1_reprocessing_p_effect_term <- function(inDF) {
    ### This script reprocesses the data
    ### based on example given by Baig et al. 2015 GCB
    ### to recalculate the p effect response ratio (r), 
    ### and its variance.
    ### The variance can be calculated based on standard deviation
    ### but many data points do not report this value
    ### therefore, for these entries (i.e. missing standard deviation), 
    ### I used the mean sd of the same variable as a proxy. 
    

    inDF$P_aCO2 <- inDF$aCaP_mean/inDF$aCeP_mean
    inDF$P_eCO2 <- inDF$eCaP_mean/inDF$eCeP_mean
    
    ####### P effect, limitation (i.e. eP is control)
    inDF$log_P_aCO2 <- log(inDF$aCaP_mean/inDF$aCeP_mean)
    inDF$log_P_eCO2 <- log(inDF$eCaP_mean/inDF$eCeP_mean)
    
    #inDF$log_P_aCO2 <- (inDF$aCaP_mean/inDF$aCeP_mean) - 1
    #inDF$log_P_eCO2 <- (inDF$eCaP_mean/inDF$eCeP_mean) - 1
    
    inDF$variance_p_aCO2 <- (inDF$aCaP_sd^2/(inDF$Sample.Size*(inDF$aCaP_mean)^2))+
        (inDF$aCeP_sd^2/(inDF$Sample.Size*(inDF$aCeP_mean)^2))
    
    inDF$variance_p_eCO2 <- (inDF$eCaP_sd^2/(inDF$Sample.Size*(inDF$eCaP_mean)^2))+
        (inDF$eCeP_sd^2/(inDF$Sample.Size*(inDF$eCeP_mean)^2))
    
    ### many studies do not report sd, hence variance needs to be calculated using proxies
    ### here variance for missing data of each study is calculated assuming
    ### mean variance of the remaining studies within each data variable
    
    for (i in unique(inDF$Variable)) {
        
        ### calcualte mean variance for the subset
        test1 <- subset(inDF, Variable == i)
        mean.variance.aCO2 <- mean(test1$variance_p_aCO2, na.rm=T)
        mean.variance.eCO2 <- mean(test1$variance_p_eCO2, na.rm=T)
        
        mean.aCO2.mean <- mean(test1$P_aCO2, na.rm=T)
        mean.eCO2.mean <- mean(test1$P_eCO2, na.rm=T)
        
        variance.aCO2.perct <- mean.variance.aCO2 / mean.aCO2.mean
        variance.eCO2.perct <- mean.variance.eCO2 / mean.eCO2.mean
        
        ### assign the mean variance to missing data
        inDF$variance_p_aCO2[inDF$Variable==i] <- ifelse(is.na(inDF$variance_p_aCO2[inDF$Variable==i]), variance.aCO2.perct * inDF$P_aCO2[inDF$Variable==i], 
                                                    inDF$variance_p_aCO2[inDF$Variable==i])
        
        inDF$variance_p_aCO2[inDF$Variable==i & is.na(inDF$variance_p_aCO2)] <- inDF$P_aCO2[inDF$Variable==i & is.na(inDF$variance_p_aCO2)] * variance.aCO2.perct
        
        ### assign the mean variance to missing data
        inDF$variance_p_eCO2[inDF$Variable==i] <- ifelse(is.na(inDF$variance_p_eCO2[inDF$Variable==i]), variance.eCO2.perct * inDF$P_eCO2[inDF$Variable==i], 
                                                         inDF$variance_p_eCO2[inDF$Variable==i])
        
        inDF$variance_p_eCO2[inDF$Variable==i & is.na(inDF$variance_p_eCO2)] <- inDF$P_eCO2[inDF$Variable==i & is.na(inDF$variance_p_eCO2)] * variance.eCO2.perct
    }
    
    outDF <- inDF

    return(outDF)
}