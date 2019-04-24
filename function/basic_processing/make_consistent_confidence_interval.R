make_consistent_confidence_interval <- function(inDF, return.option="all_se") {
    ### This function process the raw data and checks for confidence interval
    ### Many studies report CIs as SE, or SD, but many others don't report what CI they use
    ### Here, option "all_se" returns a dataframe that automatically fills missing CI with SE
    ### Alternatively, option "only_se" returns a dataframe where computable SE is provided (i.e. excluding all missing CIs). 
    ### And, ignore the least significance difference ones
    
    nrow <- nrow(inDF)
    
    ### split the data frame
    myDF1 <- subset(inDF, Confidence.interval.type=="SE") 
    myDF2 <- subset(inDF, Confidence.interval.type=="LSD") 
    myDF3 <- subset(inDF, Confidence.interval.type=="SD") 
    myDF4 <- subset(inDF, Confidence.interval.type=="2SE") 
    myDF5 <- subset(inDF, !Confidence.interval.type%in%c("SE", "LSD", "SD", "2SE")) 
    
    ### Convert SD to SE
    myDF3$aCaP_pos <- myDF3$aCaP_mean + (myDF3$aCaP_pos - myDF3$aCaP_mean) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    myDF3$aCaP_pos <- myDF3$aCaP_mean + (myDF3$aCaP_pos - myDF3$aCaP_mean) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    
    myDF3$aCaP_neg <- myDF3$aCaP_mean - (myDF3$aCaP_mean - myDF3$aCaP_neg) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    myDF3$aCaP_neg <- myDF3$aCaP_mean - (myDF3$aCaP_mean - myDF3$aCaP_neg) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    
    myDF3$aCeP_pos <- myDF3$aCeP_mean + (myDF3$aCeP_pos - myDF3$aCeP_mean) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    myDF3$aCeP_pos <- myDF3$aCeP_mean + (myDF3$aCeP_pos - myDF3$aCeP_mean) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    
    myDF3$aCeP_neg <- myDF3$aCeP_mean - (myDF3$aCeP_mean - myDF3$aCeP_neg) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    myDF3$aCeP_neg <- myDF3$aCeP_mean - (myDF3$aCeP_mean - myDF3$aCeP_neg) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    
    myDF3$eCaP_pos <- myDF3$eCaP_mean + (as.numeric(myDF3$eCaP_pos) - myDF3$eCaP_mean) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    myDF3$eCaP_pos <- myDF3$eCaP_mean + (myDF3$eCaP_pos - myDF3$eCaP_mean) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    
    myDF3$eCaP_neg <- myDF3$eCeP_mean - (myDF3$eCaP_mean - myDF3$eCaP_neg) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    myDF3$eCaP_neg <- myDF3$eCeP_mean - (myDF3$eCaP_mean - myDF3$eCaP_neg) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    
    
    myDF3$eCeP_pos <- myDF3$eCeP_mean + (myDF3$eCeP_pos - myDF3$eCeP_mean) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    myDF3$eCeP_pos <- myDF3$eCeP_mean + (myDF3$eCeP_pos - myDF3$eCeP_mean) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    
    myDF3$eCeP_neg <- myDF3$eCeP_mean - (myDF3$eCeP_mean - myDF3$eCeP_neg) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    myDF3$eCeP_neg <- myDF3$eCeP_mean - (myDF3$eCeP_mean - myDF3$eCeP_neg) / sqrt(as.numeric(as.character(myDF3$Sample.Size)))
    
    myDF3$Confidence.interval.type <- "SE"
    
    ### Convert 2SE to SE
    #myDF4$aCaP_pos <- myDF4$aCaP_mean + (myDF4$aCaP_pos - myDF4$aCaP_mean) / 2
    #myDF4$aCaP_pos <- myDF4$aCaP_mean + (myDF4$aCaP_pos - myDF4$aCaP_mean) / 2
    #
    #myDF4$aCeP_pos <- myDF4$aCeP_mean + (myDF4$aCeP_pos - myDF4$aCeP_mean) / 2
    #myDF4$aCeP_pos <- myDF4$aCeP_mean + (myDF4$aCeP_pos - myDF4$aCeP_mean) / 2
    #
    #myDF4$aCeP_neg <- myDF4$aCeP_mean - (myDF4$aCeP_mean - myDF4$aCeP_neg) / 2
    #myDF4$aCeP_neg <- myDF4$aCeP_mean - (myDF4$aCeP_mean - myDF4$aCeP_neg) / 2
    #
    #myDF4$eCaP_pos <- myDF4$eCaP_mean + (myDF4$eCaP_pos - myDF4$eCaP_mean) / 2
    #myDF4$eCaP_pos <- myDF4$eCaP_mean + (myDF4$eCaP_pos - myDF4$eCaP_mean) / 2
    #
    #myDF4$eCaP_neg <- myDF4$eCeP_mean - (myDF4$eCaP_mean - myDF4$eCaP_neg) / 2
    #myDF4$eCaP_neg <- myDF4$eCeP_mean - (myDF4$eCaP_mean - myDF4$eCaP_neg) / 2
    #
    #
    #myDF4$eCeP_pos <- myDF4$eCeP_mean + (myDF4$eCeP_pos - myDF4$eCeP_mean) / 2
    #myDF4$eCeP_pos <- myDF4$eCeP_mean + (myDF4$eCeP_pos - myDF4$eCeP_mean) / 2
    #
    #myDF4$eCeP_neg <- myDF4$eCeP_mean - (myDF4$eCeP_mean - myDF4$eCeP_neg) / 2
    #myDF4$eCeP_neg <- myDF4$eCeP_mean - (myDF4$eCeP_mean - myDF4$eCeP_neg) / 2
    #
    #myDF4$Confidence.interval.type <- "SE"
    
    ### No CI information available, consider it as SE
    myDF5$Confidence.interval.type <- "SE"
    
    ### return
    #outDF1 <- rbind(myDF1, myDF3, myDF4, myDF5)
    outDF2 <- rbind(myDF1, myDF3, myDF4)
    outDF1 <- rbind(myDF1, myDF3, myDF5)
    
    ### add sd information 
    outDF1$aCaP_sd <- (outDF1$aCaP_pos - outDF1$aCaP_neg) / 2 * sqrt(as.numeric(as.character(outDF1$Sample.Size)))
    outDF1$eCaP_sd <- (outDF1$eCaP_pos - outDF1$eCaP_neg) / 2 * sqrt(as.numeric(as.character(outDF1$Sample.Size)))
    outDF1$aCeP_sd <- (outDF1$aCeP_pos - outDF1$aCeP_neg) / 2 * sqrt(as.numeric(as.character(outDF1$Sample.Size)))
    outDF1$eCeP_sd <- (outDF1$eCeP_pos - outDF1$eCeP_neg) / 2 * sqrt(as.numeric(as.character(outDF1$Sample.Size)))
    
    
    ### return
    if(return.option=="all_se") {
        return(outDF1)
    } else {
        return(outDF2)
    }
}