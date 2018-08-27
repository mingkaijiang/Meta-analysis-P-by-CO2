##### Script to process raw data on P by CO2 literature 
##### Author: Mingkai Jiang

##### Master script


#### library
require(readxl)


#### check input files
myDF <- read_excel("data/PxCO2_literature_raw.xlsx", sheet = "biomass",
                   skip=2, col_names=F)
myDF <- as.data.frame(myDF)
colnames(myDF) <- c("Ref", "Variable", "Unit", "aCO2", "eCO2", "aP", "eP", "P_unit", "P_add_freq","Sample_size", 
                    "aCaP_mean", "aCaP_plus", "aCaP_minus","aCaP_variance",
                    "aCeP_mean", "aCeP_plus", "aCeP_minus","aCeP_variance",
                    "eCaP_mean", "eCaP_plus", "eCaP_minus","eCaP_variance",
                    "eCeP_mean", "eCeP_plus", "eCeP_minus","eCeP_variance",
                    "leafP_conc_aCaP", "leafP_conc_aCeP", "leafP_conc_eCaP", "leafP_conc_eCeP",
                    "Species", "Soil_weight", "Pot_volume", "Experiment_duration",
                    "eC_by_aC", "eP_by_aP", "aCeP_by_aCaP", "eCaP_by_aCaP", "eCeP_by_aCaP",
                    "Additive_interaction", "Multiplicative_interaction")

### change class of numeric variables
myDF$aCO2 <- as.numeric(myDF$aCO2)
myDF$eCO2 <- as.numeric(myDF$eCO2)
myDF$aP <- as.numeric(myDF$aP)
myDF$eP <- as.numeric(myDF$eP)
myDF$Sample_size <- as.numeric(myDF$Sample_size)
myDF$aCaP_mean <- as.numeric(myDF$aCaP_mean)
myDF$aCeP_mean <- as.numeric(myDF$aCeP_mean)
myDF$eCaP_mean <- as.numeric(myDF$eCaP_mean)
myDF$eCeP_mean <- as.numeric(myDF$eCeP_mean)

myDF$aCaP_plus <- as.numeric(myDF$aCaP_plus)
myDF$aCeP_plus <- as.numeric(myDF$aCeP_plus)
myDF$eCaP_plus <- as.numeric(myDF$eCaP_plus)
myDF$eCeP_plus <- as.numeric(myDF$eCeP_plus)

myDF$aCaP_minus <- as.numeric(myDF$aCaP_minus)
myDF$aCeP_minus <- as.numeric(myDF$aCeP_minus)
myDF$eCaP_minus <- as.numeric(myDF$eCaP_minus)
myDF$eCeP_minus <- as.numeric(myDF$eCeP_minus)

myDF$aCaP_variance <- as.numeric(myDF$aCaP_variance)
myDF$aCeP_variance <- as.numeric(myDF$aCeP_variance)
myDF$eCaP_variance <- as.numeric(myDF$eCaP_variance)
myDF$eCeP_variance <- as.numeric(myDF$eCeP_variance)

myDF$leafP_conc_aCaP <- as.numeric(myDF$leafP_conc_aCaP)
myDF$leafP_conc_eCaP <- as.numeric(myDF$leafP_conc_eCaP)
myDF$leafP_conc_aCeP <- as.numeric(myDF$leafP_conc_aCeP)
myDF$leafP_conc_eCeP <- as.numeric(myDF$leafP_conc_eCeP)

myDF$Experiment_duration <- as.numeric(myDF$Experiment_duration)

### Redo calculations
myDF$eC_by_aC <- myDF$eCO2/myDF$aCO2
myDF$eP_by_aP <- myDF$eP/myDF$aP

myDF$aCeP_by_aCaP <- myDF$aCeP_mean/myDF$aCaP_mean
myDF$eCaP_by_aCaP <- myDF$eCaP_mean/myDF$aCaP_mean
myDF$eCeP_by_aCaP <- myDF$eCeP_mean/myDF$aCaP_mean

myDF$Multiplicative_interaction <- (myDF$eCeP_mean/myDF$eCaP_mean)/(myDF$aCeP_mean/myDF$aCaP_mean) - 1
myDF$Additive_interaction <- (eCeP_by_aCaP-1) - (eCaP_by_aCaP-1) - (aCeP_by_aCaP - 1)


### Get variable names
var.names <- unique(myDF$Variable)

### Prepare df to store variance information
stDF <- data.frame(var.names, NA, NA, NA, NA, NA)
colnames(stDF) <- c("Variable","aCaP", "aCeP", "eCaP", "eCeP", "Sample_size")

### assign variance and sample size
for (i in 1:length(var.names)) {
    subDF <- subset(myDF, Variable==var.names[i])
    
    ### calculate median for each sd to fill the NAs
    stDF[stDF$Variable == var.names[i], "aCaP"] <- median(subDF$aCaP_variance, na.rm=T)
    stDF[stDF$Variable == var.names[i], "aCeP"] <- median(subDF$aCeP_variance, na.rm=T)
    stDF[stDF$Variable == var.names[i], "eCaP"] <- median(subDF$eCaP_variance, na.rm=T)
    stDF[stDF$Variable == var.names[i], "eCeP"] <- median(subDF$eCeP_variance, na.rm=T)
    stDF[stDF$Variable == var.names[i], "Sample_size"] <- median(subDF$Sample_size, na.rm=T)
    
}

for (i in 1:length(var.names)) {
    myDF[myDF$Variable==var.names[i], "aCaP_variance"][is.na(myDF[myDF$Variable==var.names[i],"aCaP_variance"])] <- stDF[stDF$Variable == var.names[i], "aCaP"]
    myDF[myDF$Variable==var.names[i], "aCeP_variance"][is.na(myDF[myDF$Variable==var.names[i],"aCeP_variance"])] <- stDF[stDF$Variable == var.names[i], "aCeP"]
    myDF[myDF$Variable==var.names[i], "eCaP_variance"][is.na(myDF[myDF$Variable==var.names[i],"eCaP_variance"])] <- stDF[stDF$Variable == var.names[i], "eCaP"]
    myDF[myDF$Variable==var.names[i], "eCeP_variance"][is.na(myDF[myDF$Variable==var.names[i],"eCeP_variance"])] <- stDF[stDF$Variable == var.names[i], "eCeP"]
    
    myDF[myDF$Variable==var.names[i], "Sample_size"][is.na(myDF[myDF$Variable==var.names[i],"Sample_size"])] <- stDF[stDF$Variable == var.names[i], "Sample_size"]
    
}

### Recalculate +sd and -sd
myDF$aCaP_plus <- myDF$aCaP_mean + myDF$aCaP_mean * myDF$aCaP_variance
myDF$aCaP_minus <- myDF$aCaP_mean - myDF$aCaP_mean * myDF$aCaP_variance

myDF$aCeP_plus <- myDF$aCeP_mean + myDF$aCeP_mean * myDF$aCeP_variance
myDF$aCeP_minus <- myDF$aCeP_mean - myDF$aCeP_mean * myDF$aCeP_variance

myDF$eCaP_plus <- myDF$eCaP_mean + myDF$eCaP_mean * myDF$eCaP_variance
myDF$eCaP_minus <- myDF$eCaP_mean - myDF$eCaP_mean * myDF$eCaP_variance

myDF$eCeP_plus <- myDF$eCeP_mean + myDF$eCeP_mean * myDF$eCeP_variance
myDF$eCeP_minus <- myDF$eCeP_mean - myDF$eCeP_mean * myDF$eCeP_variance

myDF$aCeP_by_aCaP <- as.numeric(myDF$aCeP_by_aCaP)
myDF$eCaP_by_aCaP <- as.numeric(myDF$eCaP_by_aCaP)
myDF$eCeP_by_aCaP <- as.numeric(myDF$eCeP_by_aCaP)

write.csv(myDF, "data/test_biomass.csv")

### Plotting
require(ggplot2)

#### Total biomass
pdf("output/summary_total_biomass.pdf")
### plot some basic information
par(mfrow=c(2,2))
hist(myDF$aCO2, xlab = "aCO2 (ppm)", main=NA)
hist(myDF$eCO2, xlab = "eCO2 (ppm)", main=NA)
hist(myDF$eC_by_aC, xlab = "eCO2/aCO2", main=NA)
hist(myDF$eP_by_aP, xlab = "eP/aP", main=NA)
legend("topright", paste0("n=", nrow(myDF)))

### Plot treatment ratio
par(mfrow=c(1,2))
test <- subset(myDF, eP_by_aP <= 100 & Variable == "Total biomass")

hist(test$eP_by_aP, xlab = "eP/aP", main=NA)
legend("topright", paste0("n=", nrow(test)))

hist(test$eC_by_aC, xlab = "eC/aC", main=NA)
legend("topright", paste0("n=", nrow(test)))

### Plot response ratio
par(mfrow=c(1,1))

with(test, barplot(eCaP_by_aCaP, horiz=T, col=as.factor(Ref), 
                   xlab="eCaP/aCaP", main="Effect of CO2"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.7)

with(test, barplot(aCeP_by_aCaP, horiz=T, col=as.factor(Ref), 
                   xlab="aCeP/aCaP", main="Effect of P"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.6)


with(test, barplot(eCeP_by_aCaP, horiz=T, col=as.factor(Ref), 
                   xlab="eCeP/aCaP", main="Total response"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.7)


with(test, barplot(Multiplicative_interaction, horiz=T, col=as.factor(Ref), 
                   xlab="Interaction", main="Effect of CO2 by P interaction (multiplicative)"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.7)

with(test, barplot(Additive_interaction, horiz=T, col=as.factor(Ref), 
                   xlab="Interaction", main="Effect of CO2 by P interaction (additive)"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.7)

dev.off()

#### leaf biomass
pdf("output/summary_leaf_biomass.pdf")
### plot some basic information
par(mfrow=c(2,2))
hist(myDF$aCO2, xlab = "aCO2 (ppm)", main=NA)
hist(myDF$eCO2, xlab = "eCO2 (ppm)", main=NA)
hist(myDF$eC_by_aC, xlab = "eCO2/aCO2", main=NA)
hist(myDF$eP_by_aP, xlab = "eP/aP", main=NA)
legend("topright", paste0("n=", nrow(myDF)))

### Plot treatment ratio
par(mfrow=c(1,2))
test <- subset(myDF, eP_by_aP <= 100 & Variable == "Leaf biomass")

hist(test$eP_by_aP, xlab = "eP/aP", main=NA)
legend("topright", paste0("n=", nrow(test)))

hist(test$eC_by_aC, xlab = "eC/aC", main=NA)
legend("topright", paste0("n=", nrow(test)))

### Plot response ratio
par(mfrow=c(1,1))

with(test, barplot(eCaP_by_aCaP, horiz=T, col=as.factor(Ref), 
                   xlab="eCaP/aCaP", main="Effect of CO2"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.7)

with(test, barplot(aCeP_by_aCaP, horiz=T, col=as.factor(Ref), 
                   xlab="aCeP/aCaP", main="Effect of P"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.6)


with(test, barplot(eCeP_by_aCaP, horiz=T, col=as.factor(Ref), 
                   xlab="eCeP/aCaP", main="Total response"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.7)


with(test, barplot(Multiplicative_interaction, horiz=T, col=as.factor(Ref), 
                   xlab="Interaction", main="Effect of CO2 by P interaction (multiplicative)"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.7)

with(test, barplot(Additive_interaction, horiz=T, col=as.factor(Ref), 
                   xlab="Interaction", main="Effect of CO2 by P interaction (additive)"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.7)

dev.off()

#### Root biomass
pdf("output/summary_root_biomass.pdf")
### plot some basic information
par(mfrow=c(2,2))
hist(myDF$aCO2, xlab = "aCO2 (ppm)", main=NA)
hist(myDF$eCO2, xlab = "eCO2 (ppm)", main=NA)
hist(myDF$eC_by_aC, xlab = "eCO2/aCO2", main=NA)
hist(myDF$eP_by_aP, xlab = "eP/aP", main=NA)
legend("topright", paste0("n=", nrow(myDF)))

### Plot treatment ratio
par(mfrow=c(1,2))
test <- subset(myDF, eP_by_aP <= 100 & Variable == "Root biomass")

hist(test$eP_by_aP, xlab = "eP/aP", main=NA)
legend("topright", paste0("n=", nrow(test)))

hist(test$eC_by_aC, xlab = "eC/aC", main=NA)
legend("topright", paste0("n=", nrow(test)))

### Plot response ratio
par(mfrow=c(1,1))

with(test, barplot(eCaP_by_aCaP, horiz=T, col=as.factor(Ref), 
                   xlab="eCaP/aCaP", main="Effect of CO2"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.7)

with(test, barplot(aCeP_by_aCaP, horiz=T, col=as.factor(Ref), 
                   xlab="aCeP/aCaP", main="Effect of P"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.6)


with(test, barplot(eCeP_by_aCaP, horiz=T, col=as.factor(Ref), 
                   xlab="eCeP/aCaP", main="Total response"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.7)


with(test, barplot(Multiplicative_interaction, horiz=T, col=as.factor(Ref), 
                   xlab="Interaction", main="Effect of CO2 by P interaction (multiplicative)"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.7)

with(test, barplot(Additive_interaction, horiz=T, col=as.factor(Ref), 
                   xlab="Interaction", main="Effect of CO2 by P interaction (additive)"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.7)

dev.off()


#### Stem biomass
pdf("output/summary_stem_biomass.pdf")
### plot some basic information
par(mfrow=c(2,2))
hist(myDF$aCO2, xlab = "aCO2 (ppm)", main=NA)
hist(myDF$eCO2, xlab = "eCO2 (ppm)", main=NA)
hist(myDF$eC_by_aC, xlab = "eCO2/aCO2", main=NA)
hist(myDF$eP_by_aP, xlab = "eP/aP", main=NA)
legend("topright", paste0("n=", nrow(myDF)))

### Plot treatment ratio
par(mfrow=c(1,2))
test <- subset(myDF, eP_by_aP <= 100 & Variable == "Stem biomass")

hist(test$eP_by_aP, xlab = "eP/aP", main=NA)
legend("topright", paste0("n=", nrow(test)))

hist(test$eC_by_aC, xlab = "eC/aC", main=NA)
legend("topright", paste0("n=", nrow(test)))

### Plot response ratio
par(mfrow=c(1,1))

with(test, barplot(eCaP_by_aCaP, horiz=T, col=as.factor(Ref), 
                   xlab="eCaP/aCaP", main="Effect of CO2"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.7)

with(test, barplot(aCeP_by_aCaP, horiz=T, col=as.factor(Ref), 
                   xlab="aCeP/aCaP", main="Effect of P"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.6)


with(test, barplot(eCeP_by_aCaP, horiz=T, col=as.factor(Ref), 
                   xlab="eCeP/aCaP", main="Total response"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.7)


with(test, barplot(Multiplicative_interaction, horiz=T, col=as.factor(Ref), 
                   xlab="Interaction", main="Effect of CO2 by P interaction (multiplicative)"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.7)

with(test, barplot(Additive_interaction, horiz=T, col=as.factor(Ref), 
                   xlab="Interaction", main="Effect of CO2 by P interaction (additive)"))
legend("topright", fill=unique(as.factor(test$Ref)), 
       legend=unique(as.factor(test$Ref)), cex=0.7)

dev.off()