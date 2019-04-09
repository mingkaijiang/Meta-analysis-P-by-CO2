metafor_statistics_mixed_effect_model <- function(reDF, intDF) {
    
    ### create directory
    if(!dir.exists("output/statistics_mixed_effect_model")) {
        dir.create("output/statistics_mixed_effect_model", showWarnings = FALSE)
    }
    
    ### prepare the df
    reDF$Pot_volume <- as.numeric(reDF$Pot_volume)
    reDF$Experiment_duration <- as.numeric(reDF$Experiment_duration)
    #reDF <- subset(reDF, Trt_eP_by_aP <= 25)
    #reDF <- subset(reDF, Trt_eCO2 >= 550)
    
    ### assign co2 predictors
    Trt_eCO2 <- c(550, 600, 650, 700)
    Trt_eC_by_aC <- c(1.25, 1.5, 1.75, 2)
    
    Trt_aCO2 <- c(350, 350, 350, 350)
    Trt_eP_by_aP <- c(10, 10, 10, 10)
    
    ### create storage df to store all outputs, available for later plotting
    outDF <- data.frame(rep(c("biomass", "leaf", "stem", "root", "total", 
                              "concentration", "leafP", "stemN", "rootP",
                              "uptake", "Nupt", "Pupt", 
                              "A", "gs", "RL"), each=4), rep(Trt_aCO2, by = 15), 
                        rep(Trt_eCO2, by = 15), NA, NA, NA, NA)
    
    colnames(outDF) <- c("Variable", "aCO2", "eCO2", "hP_mean", "hP_se", "lP_mean", "lP_se")
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Leaf biomass"| Variable=="Stem biomass" | Variable=="Root biomass" | Variable=="Total plant biomass")

    mod1 <- rma(log_co2_aP, variance_co2_aP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred1 <- predict(mod1, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    mod2 <- rma(log_co2_eP, variance_co2_eP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred2 <- predict(mod2, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    
    outDF$hP_mean[outDF$Variable == "biomass"] <- pred2$pred
    outDF$hP_se[outDF$Variable == "biomass"] <- pred2$se
    
    outDF$lP_mean[outDF$Variable == "biomass"] <- pred1$pred
    outDF$lP_se[outDF$Variable == "biomass"] <- pred1$se
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Leaf biomass")
    
    mod1 <- rma(log_co2_aP, variance_co2_aP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred1 <- predict(mod1, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    mod2 <- rma(log_co2_eP, variance_co2_eP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred2 <- predict(mod2, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    outDF$hP_mean[outDF$Variable == "leaf"] <- pred2$pred
    outDF$hP_se[outDF$Variable == "leaf"] <- pred2$se
    
    outDF$lP_mean[outDF$Variable == "leaf"] <- pred1$pred
    outDF$lP_se[outDF$Variable == "leaf"] <- pred1$se
    
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Stem biomass")
    
    mod1 <- rma(log_co2_aP, variance_co2_aP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred1 <- predict(mod1, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    mod2 <- rma(log_co2_eP, variance_co2_eP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred2 <- predict(mod2, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    outDF$hP_mean[outDF$Variable == "stem"] <- pred2$pred
    outDF$hP_se[outDF$Variable == "stem"] <- pred2$se
    
    outDF$lP_mean[outDF$Variable == "stem"] <- pred1$pred
    outDF$lP_se[outDF$Variable == "stem"] <- pred1$se
    
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Root biomass")
    
    mod1 <- rma(log_co2_aP, variance_co2_aP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred1 <- predict(mod1, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    mod2 <- rma(log_co2_eP, variance_co2_eP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred2 <- predict(mod2, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    outDF$hP_mean[outDF$Variable == "root"] <- pred2$pred
    outDF$hP_se[outDF$Variable == "root"] <- pred2$se
    
    outDF$lP_mean[outDF$Variable == "root"] <- pred1$pred
    outDF$lP_se[outDF$Variable == "root"] <- pred1$se
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Total plant biomass")
    
    mod1 <- rma(log_co2_aP, variance_co2_aP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred1 <- predict(mod1, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    mod2 <- rma(log_co2_eP, variance_co2_eP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred2 <- predict(mod2, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    outDF$hP_mean[outDF$Variable == "total"] <- pred2$pred
    outDF$hP_se[outDF$Variable == "total"] <- pred2$se
    
    outDF$lP_mean[outDF$Variable == "total"] <- pred1$pred
    outDF$lP_se[outDF$Variable == "total"] <- pred1$se
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable%in%c("Leaf P concentration", "Stem N concentration", "Root P concentration", "Leaf N concentration", "Stem P concentration", "Total N concentration"))
    
    mod1 <- rma(log_co2_aP, variance_co2_aP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred1 <- predict(mod1, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    mod2 <- rma(log_co2_eP, variance_co2_eP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred2 <- predict(mod2, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    outDF$hP_mean[outDF$Variable == "concentration"] <- pred2$pred
    outDF$hP_se[outDF$Variable == "concentration"] <- pred2$se
    
    outDF$lP_mean[outDF$Variable == "concentration"] <- pred1$pred
    outDF$lP_se[outDF$Variable == "concentration"] <- pred1$se
    
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Leaf P concentration")
    
    mod1 <- rma(log_co2_aP, variance_co2_aP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred1 <- predict(mod1, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    mod2 <- rma(log_co2_eP, variance_co2_eP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred2 <- predict(mod2, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    outDF$hP_mean[outDF$Variable == "leafP"] <- pred2$pred
    outDF$hP_se[outDF$Variable == "leafP"] <- pred2$se
    
    outDF$lP_mean[outDF$Variable == "leafP"] <- pred1$pred
    outDF$lP_se[outDF$Variable == "leafP"] <- pred1$se
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Stem N concentration")
    
    mod1 <- rma(log_co2_aP, variance_co2_aP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred1 <- predict(mod1, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    mod2 <- rma(log_co2_eP, variance_co2_eP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred2 <- predict(mod2, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    outDF$hP_mean[outDF$Variable == "stemN"] <- pred2$pred
    outDF$hP_se[outDF$Variable == "stemN"] <- pred2$se
    
    outDF$lP_mean[outDF$Variable == "stemN"] <- pred1$pred
    outDF$lP_se[outDF$Variable == "stemN"] <- pred1$se
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Root P concentration")
    
    mod1 <- rma(log_co2_aP, variance_co2_aP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred1 <- predict(mod1, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    mod2 <- rma(log_co2_eP, variance_co2_eP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred2 <- predict(mod2, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    outDF$hP_mean[outDF$Variable == "rootP"] <- pred2$pred
    outDF$hP_se[outDF$Variable == "rootP"] <- pred2$se
    
    outDF$lP_mean[outDF$Variable == "rootP"] <- pred1$pred
    outDF$lP_se[outDF$Variable == "rootP"] <- pred1$se
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Plant P uptake")
    
    mod1 <- rma(log_co2_aP, variance_co2_aP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred1 <- predict(mod1, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    mod2 <- rma(log_co2_eP, variance_co2_eP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred2 <- predict(mod2, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    outDF$hP_mean[outDF$Variable == "Pupt"] <- pred2$pred
    outDF$hP_se[outDF$Variable == "Pupt"] <- pred2$se
    
    outDF$lP_mean[outDF$Variable == "Pupt"] <- pred1$pred
    outDF$lP_se[outDF$Variable == "Pupt"] <- pred1$se
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Plant N uptake")
    
    mod1 <- rma(log_co2_aP, variance_co2_aP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred1 <- predict(mod1, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    mod2 <- rma(log_co2_eP, variance_co2_eP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred2 <- predict(mod2, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    outDF$hP_mean[outDF$Variable == "Nupt"] <- pred2$pred
    outDF$hP_se[outDF$Variable == "Nupt"] <- pred2$se
    
    outDF$lP_mean[outDF$Variable == "Nupt"] <- pred1$pred
    outDF$lP_se[outDF$Variable == "Nupt"] <- pred1$se
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Plant N uptake" | Variable=="Plant P uptake" )
    
    mod1 <- rma(log_co2_aP, variance_co2_aP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred1 <- predict(mod1, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    mod2 <- rma(log_co2_eP, variance_co2_eP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred2 <- predict(mod2, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    outDF$hP_mean[outDF$Variable == "uptake"] <- pred2$pred
    outDF$hP_se[outDF$Variable == "uptake"] <- pred2$se
    
    outDF$lP_mean[outDF$Variable == "uptake"] <- pred1$pred
    outDF$lP_se[outDF$Variable == "uptake"] <- pred1$se
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="CO2 assimilation rate")
    
    mod1 <- rma(log_co2_aP, variance_co2_aP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred1 <- predict(mod1, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    mod2 <- rma(log_co2_eP, variance_co2_eP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred2 <- predict(mod2, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    outDF$hP_mean[outDF$Variable == "A"] <- pred2$pred
    outDF$hP_se[outDF$Variable == "A"] <- pred2$se
    
    outDF$lP_mean[outDF$Variable == "A"] <- pred1$pred
    outDF$lP_se[outDF$Variable == "A"] <- pred1$se
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Stomatal conductance")
    
    mod1 <- rma(log_co2_aP, variance_co2_aP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred1 <- predict(mod1, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    mod2 <- rma(log_co2_eP, variance_co2_eP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred2 <- predict(mod2, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    outDF$hP_mean[outDF$Variable == "gs"] <- pred2$pred
    outDF$hP_se[outDF$Variable == "gs"] <- pred2$se
    
    outDF$lP_mean[outDF$Variable == "gs"] <- pred1$pred
    outDF$lP_se[outDF$Variable == "gs"] <- pred1$se
    
    ####################### subset the dataframe for the right variable ##############################
    ### we need to weigh the studies by their variance!!!
    
    tDF <- subset(reDF, Variable=="Total root length")
    
    mod1 <- rma(log_co2_aP, variance_co2_aP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred1 <- predict(mod1, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    mod2 <- rma(log_co2_eP, variance_co2_eP,
                mods = ~ Trt_eCO2 + Trt_aCO2 + Trt_eP_by_aP, 
                data = tDF, knha=T,
                control=list(stepadj=0.5))
    pred2 <- predict(mod2, newmods=cbind(Trt_eCO2, Trt_aCO2, Trt_eP_by_aP))
    
    outDF$hP_mean[outDF$Variable == "RL"] <- pred2$pred
    outDF$hP_se[outDF$Variable == "RL"] <- pred2$se
    
    outDF$lP_mean[outDF$Variable == "RL"] <- pred1$pred
    outDF$lP_se[outDF$Variable == "RL"] <- pred1$se
    
    
    #### convert outDF into plotDF
    outDF1 <- outDF[,c("Variable", "aCO2", "eCO2", "hP_mean", "hP_se")]
    outDF2 <- outDF[,c("Variable", "aCO2", "eCO2", "lP_mean", "lP_se")]
    colnames(outDF1) <- colnames(outDF2) <- c("Variable", "aCO2", "eCO2", "mean.v", "se")
    outDF1$P_trt <- "hP"
    outDF2$P_trt <- "lP"
    plotDF <- rbind(outDF1, outDF2)
    
    plotDF$eCO2 <- as.factor(plotDF$eCO2)
    
    ########################## Plotting ###################################
    p1 <- ggplot(plotDF[plotDF$Variable == "biomass",],
                 aes(eCO2, mean.v*100, fill=P_trt)) +
        geom_bar(stat="identity", 
                 position=position_dodge()) +
        geom_errorbar(aes(x=eCO2, ymin=(mean.v-se)*100, ymax=(mean.v+se)*100), 
                      width=.2, position=position_dodge(.9))+
        ylab(expression(paste("Biomass "* CO[2], " response (%)"))) +
        xlab(expression(paste(eCO[2], " treatment (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.7, 0.35),
              legend.background = element_rect(fill="grey",
                                               size=0.5, linetype="solid", 
                                               colour ="black"))+
        scale_fill_manual(name=paste("P treatment"),
                           breaks=c("hP", "lP"),
                           values=c("grey","orange"),
                           labels=c("hP", "lP"))+
        ylim(c(-30,40))
        
    
    p2 <- ggplot(plotDF[plotDF$Variable == "leaf",],
                 aes(eCO2, mean.v*100, fill=P_trt)) +
        geom_bar(stat="identity", 
                 position=position_dodge()) +
        geom_errorbar(aes(x=eCO2, ymin=(mean.v-se)*100, ymax=(mean.v+se)*100), 
                      width=.2, position=position_dodge(.9))+
        ylab(expression(paste("Leaf biomass "* CO[2], " response (%)"))) +
        xlab(expression(paste(eCO[2], " treatment (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.1, 0.9))+
        scale_fill_manual(name=paste("P treatment"),
                          breaks=c("hP", "lP"),
                          values=c("grey","orange"),
                          labels=c("hP", "lP"))
    
    
    p3 <- ggplot(plotDF[plotDF$Variable == "stem",],
                 aes(eCO2, mean.v*100, fill=P_trt)) +
        geom_bar(stat="identity", 
                 position=position_dodge()) +
        geom_errorbar(aes(x=eCO2, ymin=(mean.v-se)*100, ymax=(mean.v+se)*100), 
                      width=.2, position=position_dodge(.9))+
        ylab(expression(paste("Stem biomass "* CO[2], " response (%)"))) +
        xlab(expression(paste(eCO[2], " treatment (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.1, 0.9))+
        scale_fill_manual(name=paste("P treatment"),
                          breaks=c("hP", "lP"),
                          values=c("grey","orange"),
                          labels=c("hP", "lP"))
    
    
    p4 <- ggplot(plotDF[plotDF$Variable == "root",],
                 aes(eCO2, mean.v*100, fill=P_trt)) +
        geom_bar(stat="identity", 
                 position=position_dodge()) +
        geom_errorbar(aes(x=eCO2, ymin=(mean.v-se)*100, ymax=(mean.v+se)*100), 
                      width=.2, position=position_dodge(.9))+
        ylab(expression(paste("Root biomass "* CO[2], " response (%)"))) +
        xlab(expression(paste(eCO[2], " treatment (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.1, 0.9))+
        scale_fill_manual(name=paste("P treatment"),
                          breaks=c("hP", "lP"),
                          values=c("grey","orange"),
                          labels=c("hP", "lP"))
    
    
    p5 <- ggplot(plotDF[plotDF$Variable == "total",],
                 aes(eCO2, mean.v*100, fill=P_trt)) +
        geom_bar(stat="identity", 
                 position=position_dodge()) +
        geom_errorbar(aes(x=eCO2, ymin=(mean.v-se)*100, ymax=(mean.v+se)*100), 
                      width=.2, position=position_dodge(.9))+
        ylab(expression(paste("Total biomass "* CO[2], " response (%)"))) +
        xlab(expression(paste(eCO[2], " treatment (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.1, 0.9))+
        scale_fill_manual(name=paste("P treatment"),
                          breaks=c("hP", "lP"),
                          values=c("grey","orange"),
                          labels=c("hP", "lP"))
    
    
    p6 <- ggplot(plotDF[plotDF$Variable == "concentration",],
                 aes(eCO2, mean.v*100, fill=P_trt)) +
        geom_bar(stat="identity", 
                 position=position_dodge()) +
        geom_errorbar(aes(x=eCO2, ymin=(mean.v-se)*100, ymax=(mean.v+se)*100), 
                      width=.2, position=position_dodge(.9))+
        ylab(expression(paste("Concentration "* CO[2], " response (%)"))) +
        xlab(expression(paste(eCO[2], " treatment (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = "none")+
        scale_fill_manual(name=paste("P treatment"),
                          breaks=c("hP", "lP"),
                          values=c("grey","orange"),
                          labels=c("hP", "lP"))+
        ylim(c(-30,40))
    
    p7 <- ggplot(plotDF[plotDF$Variable == "leafP",],
                 aes(eCO2, mean.v*100, fill=P_trt)) +
        geom_bar(stat="identity", 
                 position=position_dodge()) +
        geom_errorbar(aes(x=eCO2, ymin=(mean.v-se)*100, ymax=(mean.v+se)*100), 
                      width=.2, position=position_dodge(.9))+
        ylab(expression(paste("leaf P conc "* CO[2], " response (%)"))) +
        xlab(expression(paste(eCO[2], " treatment (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.1, 0.9))+
        scale_fill_manual(name=paste("P treatment"),
                          breaks=c("hP", "lP"),
                          values=c("grey","orange"),
                          labels=c("hP", "lP"))
    
    p8 <- ggplot(plotDF[plotDF$Variable == "stemN",],
                 aes(eCO2, mean.v*100, fill=P_trt)) +
        geom_bar(stat="identity", 
                 position=position_dodge()) +
        geom_errorbar(aes(x=eCO2, ymin=(mean.v-se)*100, ymax=(mean.v+se)*100), 
                      width=.2, position=position_dodge(.9))+
        ylab(expression(paste("Stem N conc "* CO[2], " response (%)"))) +
        xlab(expression(paste(eCO[2], " treatment (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.1, 0.9))+
        scale_fill_manual(name=paste("P treatment"),
                          breaks=c("hP", "lP"),
                          values=c("grey","orange"),
                          labels=c("hP", "lP"))
    
    
    p9 <- ggplot(plotDF[plotDF$Variable == "rootP",],
                 aes(eCO2, mean.v*100, fill=P_trt)) +
        geom_bar(stat="identity", 
                 position=position_dodge()) +
        geom_errorbar(aes(x=eCO2, ymin=(mean.v-se)*100, ymax=(mean.v+se)*100), 
                      width=.2, position=position_dodge(.9))+
        ylab(expression(paste("root P conc "* CO[2], " response (%)"))) +
        xlab(expression(paste(eCO[2], " treatment (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = c(0.1, 0.9))+
        scale_fill_manual(name=paste("P treatment"),
                          breaks=c("hP", "lP"),
                          values=c("grey","orange"),
                          labels=c("hP", "lP"))
    
    
    p10 <- ggplot(plotDF[plotDF$Variable == "uptake",],
                 aes(eCO2, mean.v*100, fill=P_trt)) +
        geom_bar(stat="identity", 
                 position=position_dodge()) +
        geom_errorbar(aes(x=eCO2, ymin=(mean.v-se)*100, ymax=(mean.v+se)*100), 
                      width=.2, position=position_dodge(.9))+
        ylab(expression(paste("Nutrient uptake "* CO[2], " response (%)"))) +
        xlab(expression(paste(eCO[2], " treatment (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = "none")+
        scale_fill_manual(name=paste("P treatment"),
                          breaks=c("hP", "lP"),
                          values=c("grey","orange"),
                          labels=c("hP", "lP"))+
        ylim(c(-30,40))
    
    
    p11 <- ggplot(plotDF[plotDF$Variable == "A",],
                  aes(eCO2, mean.v*100, fill=P_trt)) +
        geom_bar(stat="identity", 
                 position=position_dodge()) +
        geom_errorbar(aes(x=eCO2, ymin=(mean.v-se)*100, ymax=(mean.v+se)*100), 
                      width=.2, position=position_dodge(.9))+
        ylab(expression(paste("Photosynthesis "* CO[2], " response (%)"))) +
        xlab(expression(paste(eCO[2], " treatment (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              #legend.justification = c(0, 1), 
              legend.position = "none")+
        scale_fill_manual(name=paste("P treatment"),
                          breaks=c("hP", "lP"),
                          values=c("grey","orange"),
                          labels=c("hP", "lP"))+
        ylim(c(-30,40))
    
    
    p12 <- ggplot(plotDF[plotDF$Variable == "gs",],
                  aes(eCO2, mean.v*100, fill=P_trt)) +
        geom_bar(stat="identity", 
                 position=position_dodge()) +
        geom_errorbar(aes(x=eCO2, ymin=(mean.v-se)*100, ymax=(mean.v+se)*100), 
                      width=.2, position=position_dodge(.9))+
        ylab(expression(paste("stomatal conductance "* CO[2], " response (%)"))) +
        xlab(expression(paste(eCO[2], " treatment (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = "none")+
        scale_fill_manual(name=paste("P treatment"),
                          breaks=c("hP", "lP"),
                          values=c("grey","orange"),
                          labels=c("hP", "lP"))
    
    p13 <- ggplot(plotDF[plotDF$Variable == "RL",],
                  aes(eCO2, mean.v*100, fill=P_trt)) +
        geom_bar(stat="identity", 
                 position=position_dodge()) +
        geom_errorbar(aes(x=eCO2, ymin=(mean.v-se)*100, ymax=(mean.v+se)*100), 
                      width=.2, position=position_dodge(.9))+
        ylab(expression(paste("Root length "* CO[2], " response (%)"))) +
        xlab(expression(paste(eCO[2], " treatment (ppm)")))+
        theme_linedraw() +
        theme(panel.grid.minor=element_blank(),
              axis.title.x = element_text(size=14), 
              axis.text.x = element_text(size=12),
              axis.text.y=element_text(size=12),
              axis.title.y=element_text(size=14),
              legend.text=element_text(size=12),
              legend.title=element_text(size=14),
              panel.grid.major=element_blank(),
              legend.justification = c(0, 1), 
              legend.position = "none")+
        scale_fill_manual(name=paste("P treatment"),
                          breaks=c("hP", "lP"),
                          values=c("grey","orange"),
                          labels=c("hP", "lP"))+
        ylim(c(-30,50))
    
    #plot(p13)
    
    #plot(p2)
    #plot(p4)
    #plot(p5)
    #plot(p7)
    #plot(p8)
    #plot(p9)
    
    
    
    ### summary histgram of treatments
    pdf("output/statistics_mixed_effect_model/biomass_prediction_response.pdf", width=12, height=14)
    
    plot_grid(p11, p10, p6, p13, p1, 
              labels="AUTO", ncol=2, align="v", axis = "l",
              rel_heights=c(1,1,1,1.5,1))
    dev.off()


}
