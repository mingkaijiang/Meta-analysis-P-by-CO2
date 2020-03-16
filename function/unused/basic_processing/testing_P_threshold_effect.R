testing_P_threshold_effect <- function() {
    
    testDF <- subset(reDF100, Variable=="Leaf biomass")
    
    
    with(testDF, plot(log_co2_eP~Trt_aCO2))
    with(testDF, point(log_co2_aP~Trt_aCO2,fill=T))
    
    
    ggplot(testDF) +
        geom_point(aes(Trt_aCO2, log_co2_eP), col="blue3")+
        geom_point(aes(Trt_aCO2, log_co2_aP), col="red2")+
        geom_smooth(method=lm, formula = y ~ splines::bs(x, 3), aes(Trt_aCO2, log_co2_eP), col="blue3")+
        geom_smooth(method=lm, formula = y ~ splines::bs(x, 3), aes(Trt_aCO2, log_co2_aP), col="red2")
    
    ggplot(testDF) +
        geom_point(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_point(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")+
        geom_smooth(method=gam, aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_smooth(method=gam, aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")
    
    ggplot(testDF) +
        geom_point(aes(Trt_eP_by_aP, log_co2_eP), col="blue3")+
        geom_point(aes(Trt_eP_by_aP, log_co2_aP), col="red2")+
        geom_smooth(method=gam, aes(Trt_eP_by_aP, log_co2_eP), col="blue3")+
        geom_smooth(method=gam, aes(Trt_eP_by_aP, log_co2_aP), col="red2")
    
    testDF <- subset(reDF100, Variable=="CO2 assimilation rate")
    testDF1 <- subset(testDF, Literature == "Thomas et al. 2006")
    testDF2 <- subset(testDF, Literature == "Lewis et al. 2010")
    testDF3 <- subset(testDF, Literature == "Fleisher et al. 2012")
    testDF4 <- subset(testDF, Literature == "Kogawara et al. 2005")
    testDF6 <- subset(testDF, Literature == "Conroy et al. 1990a")
    testDF7 <- subset(testDF, Literature == "Syvertsen and Graham 1999")
    testDF8 <- subset(testDF, Literature == "Almeida et al. 1999")
    testDF9 <- subset(testDF, Literature == "Imai and Adachi 1996")
    testDF10 <- subset(testDF, Literature == "Singh et al. 2014")
    testDF11 <- subset(testDF, Literature == "Dey et al. 2017a")
    
    ggplot(testDF1) +
        geom_point(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_point(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")+
        geom_smooth(method=gam, aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_smooth(method=gam, aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")
    
    ggplot(testDF2) +
        geom_point(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_point(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")+
        geom_smooth(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_smooth(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")
    
    
    ggplot(testDF3) +
        geom_point(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_point(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")+
        geom_smooth(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_smooth(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")
    
    ggplot(testDF4) +
        geom_point(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_point(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")+
        geom_smooth(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_smooth(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")
    
    ggplot(testDF6) +
        geom_point(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_point(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")+
        geom_smooth(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_smooth(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")
    
    ggplot(testDF7) +
        geom_point(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_point(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")+
        geom_smooth(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_smooth(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")
    
    ggplot(testDF8) +
        geom_point(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_point(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")+
        geom_smooth(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_smooth(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")
    
    ggplot(testDF9) +
        geom_point(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_point(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")+
        geom_smooth(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_smooth(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")
    
    ggplot(testDF10) +
        geom_point(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_point(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")+
        geom_smooth(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_smooth(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")
    
    ggplot(testDF11) +
        geom_point(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_point(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")+
        geom_smooth(aes(Trt_eP_by_aP, eCeP_over_aCeP), col="blue3")+
        geom_smooth(aes(Trt_eP_by_aP, eCaP_over_aCaP), col="red2")
    
    
}
