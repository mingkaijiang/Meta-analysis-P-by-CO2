metafor_statistics_biomass_100_mixed_effect_model <- function(reDF, intDF) {
    
    ### create directory
    if(!dir.exists("output/statistics_biomass_100_mixed_effect_model")) {
        dir.create("output/statistics_biomass_100_mixed_effect_model", showWarnings = FALSE)
    }
    
    ### prepare the df
    reDF$Pot_volume <- as.numeric(reDF$Pot_volume)
    reDF$Experiment_duration <- as.numeric(reDF$Experiment_duration)
    
    ### assign co2 predictors
    eco2.conc <- c(500, 600, 700, 800)
    aco2.conc <- c(350, 350, 350, 350)
    
    
    ### create storage df to store all outputs, available for later plotting
    outDF <- data.frame(rep(c("biomass", "leaf", "stem", "root", "total", 
                              "concentration", "leafP", "stemN", "rootP",
                              "uptake", "Nupt", "Pupt", 
                              "A", "gs", "RL"), each=4), rep(aco2.conc, by = 15), 
                        rep(eco2.conc, by = 15), NA, NA, NA, NA)
    
    colnames(outDF) <- c("Variable", "aCO2", "eCO2", "hP_mean", "hP_se", "lP_mean", "lP_se")
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Leaf biomass"| Variable=="Stem biomass" | Variable=="Root biomass" | Variable=="Total biomass")
    
    mod1 <- gam(log_co2_aP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_aP/mean(variance_co2_aP))
    pred1 <- predict(mod1, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    mod2 <- gam(log_co2_eP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_eP/mean(variance_co2_eP))
    pred2 <- predict(mod2, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    outDF$hP_mean[outDF$Variable == "biomass"] <- pred2$fit
    outDF$hP_se[outDF$Variable == "biomass"] <- pred2$se.fit
    
    outDF$lP_mean[outDF$Variable == "biomass"] <- pred1$fit
    outDF$lP_se[outDF$Variable == "biomass"] <- pred1$se.fit
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Leaf biomass")
    
    mod1 <- gam(log_co2_aP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_aP/mean(variance_co2_aP))
    pred1 <- predict(mod1, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    mod2 <- gam(log_co2_eP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_eP/mean(variance_co2_eP))
    pred2 <- predict(mod2, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    outDF$hP_mean[outDF$Variable == "leaf"] <- pred2$fit
    outDF$hP_se[outDF$Variable == "leaf"] <- pred2$se.fit
    
    outDF$lP_mean[outDF$Variable == "leaf"] <- pred1$fit
    outDF$lP_se[outDF$Variable == "leaf"] <- pred1$se.fit
    
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Stem biomass")
    
    mod1 <- gam(log_co2_aP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_aP/mean(variance_co2_aP))
    pred1 <- predict(mod1, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    mod2 <- gam(log_co2_eP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_eP/mean(variance_co2_eP))
    pred2 <- predict(mod2, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    outDF$hP_mean[outDF$Variable == "stem"] <- pred2$fit
    outDF$hP_se[outDF$Variable == "stem"] <- pred2$se.fit
    
    outDF$lP_mean[outDF$Variable == "stem"] <- pred1$fit
    outDF$lP_se[outDF$Variable == "stem"] <- pred1$se.fit
    
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Root biomass")
    
    mod1 <- gam(log_co2_aP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_aP/mean(variance_co2_aP))
    pred1 <- predict(mod1, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    mod2 <- gam(log_co2_eP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_eP/mean(variance_co2_eP))
    pred2 <- predict(mod2, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    outDF$hP_mean[outDF$Variable == "root"] <- pred2$fit
    outDF$hP_se[outDF$Variable == "root"] <- pred2$se.fit
    
    outDF$lP_mean[outDF$Variable == "root"] <- pred1$fit
    outDF$lP_se[outDF$Variable == "root"] <- pred1$se.fit
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Total biomass")
    
    mod1 <- gam(log_co2_aP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_aP/mean(variance_co2_aP))
    pred1 <- predict(mod1, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    mod2 <- gam(log_co2_eP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_eP/mean(variance_co2_eP))
    pred2 <- predict(mod2, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    outDF$hP_mean[outDF$Variable == "total"] <- pred2$fit
    outDF$hP_se[outDF$Variable == "total"] <- pred2$se.fit
    
    outDF$lP_mean[outDF$Variable == "total"] <- pred1$fit
    outDF$lP_se[outDF$Variable == "total"] <- pred1$se.fit
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Leaf P concentration" | Variable=="Stem N concentration" | Variable=="Root P concentration" )
    
    mod1 <- gam(log_co2_aP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_aP/mean(variance_co2_aP))
    pred1 <- predict(mod1, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    mod2 <- gam(log_co2_eP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_eP/mean(variance_co2_eP))
    pred2 <- predict(mod2, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    outDF$hP_mean[outDF$Variable == "concentration"] <- pred2$fit
    outDF$hP_se[outDF$Variable == "concentration"] <- pred2$se.fit
    
    outDF$lP_mean[outDF$Variable == "concentration"] <- pred1$fit
    outDF$lP_se[outDF$Variable == "concentration"] <- pred1$se.fit
    
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Leaf P concentration")
    
    mod1 <- gam(log_co2_aP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_aP/mean(variance_co2_aP))
    pred1 <- predict(mod1, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    mod2 <- gam(log_co2_eP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_eP/mean(variance_co2_eP))
    pred2 <- predict(mod2, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    outDF$hP_mean[outDF$Variable == "leafP"] <- pred2$fit
    outDF$hP_se[outDF$Variable == "leafP"] <- pred2$se.fit
    
    outDF$lP_mean[outDF$Variable == "leafP"] <- pred1$fit
    outDF$lP_se[outDF$Variable == "leafP"] <- pred1$se.fit
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Stem N concentration")
    
    mod1 <- gam(log_co2_aP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_aP/mean(variance_co2_aP))
    pred1 <- predict(mod1, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    mod2 <- gam(log_co2_eP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_eP/mean(variance_co2_eP))
    pred2 <- predict(mod2, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    outDF$hP_mean[outDF$Variable == "stemN"] <- pred2$fit
    outDF$hP_se[outDF$Variable == "stemN"] <- pred2$se.fit
    
    outDF$lP_mean[outDF$Variable == "stemN"] <- pred1$fit
    outDF$lP_se[outDF$Variable == "stemN"] <- pred1$se.fit
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Root P concentration")
    
    mod1 <- gam(log_co2_aP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_aP/mean(variance_co2_aP))
    pred1 <- predict(mod1, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    mod2 <- gam(log_co2_eP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_eP/mean(variance_co2_eP))
    pred2 <- predict(mod2, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    outDF$hP_mean[outDF$Variable == "rootP"] <- pred2$fit
    outDF$hP_se[outDF$Variable == "rootP"] <- pred2$se.fit
    
    outDF$lP_mean[outDF$Variable == "rootP"] <- pred1$fit
    outDF$lP_se[outDF$Variable == "rootP"] <- pred1$se.fit
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Plant P uptake")
    
    mod1 <- gam(log_co2_aP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_aP/mean(variance_co2_aP))
    pred1 <- predict(mod1, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    mod2 <- gam(log_co2_eP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_eP/mean(variance_co2_eP))
    pred2 <- predict(mod2, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    outDF$hP_mean[outDF$Variable == "Pupt"] <- pred2$fit
    outDF$hP_se[outDF$Variable == "Pupt"] <- pred2$se.fit
    
    outDF$lP_mean[outDF$Variable == "Pupt"] <- pred1$fit
    outDF$lP_se[outDF$Variable == "Pupt"] <- pred1$se.fit
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Plant N uptake")
    
    mod1 <- gam(log_co2_aP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_aP/mean(variance_co2_aP))
    pred1 <- predict(mod1, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    mod2 <- gam(log_co2_eP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_eP/mean(variance_co2_eP))
    pred2 <- predict(mod2, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    outDF$hP_mean[outDF$Variable == "Nupt"] <- pred2$fit
    outDF$hP_se[outDF$Variable == "Nupt"] <- pred2$se.fit
    
    outDF$lP_mean[outDF$Variable == "Nupt"] <- pred1$fit
    outDF$lP_se[outDF$Variable == "Nupt"] <- pred1$se.fit
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Plant N uptake" | Variable=="Plant P uptake" )
    
    mod1 <- gam(log_co2_aP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_aP/mean(variance_co2_aP))
    pred1 <- predict(mod1, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    mod2 <- gam(log_co2_eP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_eP/mean(variance_co2_eP))
    pred2 <- predict(mod2, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    outDF$hP_mean[outDF$Variable == "uptake"] <- pred2$fit
    outDF$hP_se[outDF$Variable == "uptake"] <- pred2$se.fit
    
    outDF$lP_mean[outDF$Variable == "uptake"] <- pred1$fit
    outDF$lP_se[outDF$Variable == "uptake"] <- pred1$se.fit
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="CO2 assimilation rate")
    
    mod1 <- gam(log_co2_aP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_aP/mean(variance_co2_aP))
    pred1 <- predict(mod1, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    mod2 <- gam(log_co2_eP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_eP/mean(variance_co2_eP))
    pred2 <- predict(mod2, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    outDF$hP_mean[outDF$Variable == "A"] <- pred2$fit
    outDF$hP_se[outDF$Variable == "A"] <- pred2$se.fit
    
    outDF$lP_mean[outDF$Variable == "A"] <- pred1$fit
    outDF$lP_se[outDF$Variable == "A"] <- pred1$se.fit
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Stomatal conductance")
    
    mod1 <- gam(log_co2_aP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_aP/mean(variance_co2_aP))
    pred1 <- predict(mod1, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    mod2 <- gam(log_co2_eP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_eP/mean(variance_co2_eP))
    pred2 <- predict(mod2, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    outDF$hP_mean[outDF$Variable == "gs"] <- pred2$fit
    outDF$hP_se[outDF$Variable == "gs"] <- pred2$se.fit
    
    outDF$lP_mean[outDF$Variable == "gs"] <- pred1$fit
    outDF$lP_se[outDF$Variable == "gs"] <- pred1$se.fit
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Total root length")
    
    mod1 <- gam(log_co2_aP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_aP/mean(variance_co2_aP))
    pred1 <- predict(mod1, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    mod2 <- gam(log_co2_eP ~ Trt_eCO2 + Trt_aCO2, data=tDF, 
                weights = variance_co2_eP/mean(variance_co2_eP))
    pred2 <- predict(mod2, list(Trt_eCO2 = eco2.conc, Trt_aCO2 = aco2.conc), interval = "confidence", se.fit = TRUE)
    
    outDF$hP_mean[outDF$Variable == "RL"] <- pred2$fit
    outDF$hP_se[outDF$Variable == "RL"] <- pred2$se.fit
    
    outDF$lP_mean[outDF$Variable == "RL"] <- pred1$fit
    outDF$lP_se[outDF$Variable == "RL"] <- pred1$se.fit
    
    
    
    
    
    ########################## Plotting ###################################
    p1 <- ggplot(outDF[outDF$Variable == "biomass",]) +
        geom_point(aes(eCO2, hP_mean), color="black") +
        geom_errorbar(aes(eCO2, ymin=hP_mean-hP_se, ymax=hP_mean+hP_se), color="grey") +
        geom_point(aes(eCO2, lP_mean), color="red") +
        geom_errorbar(aes(eCO2, ymin=lP_mean-lP_se, ymax=lP_mean+lP_se), color="pink") 
    
    plot(p1)
    

}
