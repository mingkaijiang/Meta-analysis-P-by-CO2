reprocessing_co2_effect_term <- function(inDF) {
    ### This script reprocesses the data
    ### based on example given by Baig et al. 2015 GCB
    ### to recalculate the co2 effect response ratio (r), 
    ### and its variance.
    ### The variance can be calculated based on standard deviation
    ### but many data points do not report this value
    ### therefore, for these entries (i.e. missing standard deviation), 
    ### I used the mean sd of the same variable as a proxy. 
    
    inDF$co2_eP <- inDF$eCeP_mean/inDF$aCeP_mean
    inDF$co2_aP <- inDF$eCaP_mean/inDF$aCaP_mean

    ### mean effect, under eP condition
    inDF$log_co2_eP <- log(inDF$eCeP_mean/inDF$aCeP_mean)
    inDF$log_co2_aP <- log(inDF$eCaP_mean/inDF$aCaP_mean)
    
    #inDF$log_co2_eP <- (inDF$eCeP_mean/inDF$aCeP_mean) - 1
    #inDF$log_co2_aP <- (inDF$eCaP_mean/inDF$aCaP_mean) - 1
    
    
    ### variance, under eP condition
    inDF$variance_co2_eP <- (inDF$eCeP_sd^2/(inDF$Sample.Size*(inDF$eCeP_mean)^2))+
        (inDF$aCeP_sd^2/(inDF$Sample.Size*(inDF$aCeP_mean)^2))
    
    inDF$variance_co2_aP <- (inDF$eCaP_sd^2/(inDF$Sample.Size*(inDF$eCaP_mean)^2))+
        (inDF$aCaP_sd^2/(inDF$Sample.Size*(inDF$aCaP_mean)^2))
    
    ### many studies do not report sd, hence variance needs to be calculated using proxies
    ### here variance for missing data of each study is calculated assuming
    ### mean variance of the remaining studies within each data variable
    
    for (i in unique(inDF$Variable)) {
        
        ### calcualte mean variance for the subset
        test1 <- subset(inDF, Variable == i)
        mean.variance.eP <- mean(test1$variance_co2_eP, na.rm=T)
        mean.variance.aP <- mean(test1$variance_co2_aP, na.rm=T)
        mean.eP.mean <- mean(test1$co2_eP, na.rm=T)
        mean.aP.mean <- mean(test1$co2_aP, na.rm=T)
        
        variance.eP.perct <- mean.variance.eP / mean.eP.mean
        variance.aP.perct <- mean.variance.eP / mean.aP.mean
        
        ### assign the mean variance to missing data
        inDF$variance_co2_eP[inDF$Variable==i] <- ifelse(is.na(inDF$variance_co2_eP[inDF$Variable==i]), variance.eP.perct * inDF$co2_eP[inDF$Variable==i],
                                                    inDF$variance_co2_eP[inDF$Variable==i])
        
        inDF$variance_co2_eP[inDF$Variable==i & is.na(inDF$variance_co2_eP)] <- inDF$co2_eP[inDF$Variable==i & is.na(inDF$variance_co2_eP)] * variance.eP.perct
        
        
        
        inDF$variance_co2_aP[inDF$Variable==i] <- ifelse(is.na(inDF$variance_co2_aP[inDF$Variable==i]), variance.aP.perct * inDF$co2_aP[inDF$Variable==i],
                                                         inDF$variance_co2_aP[inDF$Variable==i])
        
        inDF$variance_co2_aP[inDF$Variable==i & is.na(inDF$variance_co2_aP)] <- inDF$co2_aP[inDF$Variable==i & is.na(inDF$variance_co2_aP)] * variance.aP.perct
    }
    
    outDF <- inDF

    return(outDF)
}