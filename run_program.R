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
