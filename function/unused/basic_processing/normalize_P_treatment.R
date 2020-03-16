normalize_P_treatment <- function(myDF) {
    inDF <- myDF
    
    studies <- unique(myDF$Literature)
    
    for (i in 1:length(studies)) {
        HP <- max(inDF[inDF$Literature==studies[i], "Trt_eP"])
        
        inDF[inDF$Literature == studies[i], "Trt_eP_norm"] <- inDF[inDF$Literature == studies[i], "Trt_eP"] / HP
        inDF[inDF$Literature == studies[i], "Trt_aP_norm"] <- inDF[inDF$Literature == studies[i], "Trt_aP"] / HP
        
    }
    
    test <- subset(inDF, Trt_eP_norm < 1)
    unique(test$Literature)
    
}