##### Script to process raw data on P by CO2 literature 
##### Author: Mingkai Jiang

##### Master script
### source
source("prepare.R")

#### check input files
myDF <- read.csv("data/P_by_CO2_data_cleaned_no_eq_V1.csv",strip.white=T)

myDF <- as.data.frame(myDF)

### make numbers numeric
myDF$Interaction_additive_aCaP <- as.numeric(as.character(myDF$Interaction_additive_aCaP))
myDF$Interaction_multiplicative_aCaP <- as.numeric(as.character(myDF$Interaction_multiplicative_aCaP))

myDF$Interaction_additive_aCaP_aCeP <- as.numeric(as.character(myDF$Interaction_additive_aCeP))
myDF$Interaction_multiplicative_aCeP <- as.numeric(as.character(myDF$Interaction_multiplicative_aCeP))

### Basic statistics that summarize 
### number of studies
### number of data entries
### species numbers
### vegetation type
### CO2 and P treatment
### etc. 
make_basic_summary_stats_plots(myDF)




### Comment:
### need to find a way to constrain P addition range
### right now eP can be 1000 times bigger than aP. 
### Check with stat book how to weigh the studies
### and what to do for those that don't have a variance. 
### Consider eC and aP as the treatment, so what is interaction then?
### Also, need to compute per study response ratio



