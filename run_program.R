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

write.csv(myDF, "data/test_biomass.csv")

### Plotting
require(ggplot2)


hist(myDF$aCO2)
hist(myDF$eCO2)
hist(myDF$eC_by_aC)
hist(myDF$eP_by_aP)

test <- subset(myDF, eP_by_aP <= 100)


with(test[test$Variable == "Total biomass",], barplot(eCaP_by_aCaP, horiz=T, names.arg = Ref))
with(test[test$Variable == "Total biomass",], barplot(aCeP_by_aCaP, horiz=T))
with(test[test$Variable == "Total biomass",], barplot(eCeP_by_aCaP, horiz=T))
