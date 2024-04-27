#############################################################################
# NAME: DATA MANAGEMENT 
rm(list=ls())
BRFSS <- read.csv("/Users/aileenandrade/Desktop/Spring Quarter/Epi 514/BRFSS project/Data/BRFSS") 

library(tidyverse)
library(haven)
library(foreign)


View(BRFSS)
names(BRFSS)

grep("sex", names(BRFSS), value=TRUE) #check for different sex variables

#renaming variables working with 
names(BRFSS)[names(BRFSS) == "X_STATE"] <- "state"
names(BRFSS)[names(BRFSS) == "BIRTHSEX"] <- "sex"
names(BRFSS)[names(BRFSS) == "RRHCARE4"] <- "HlthDiscrim"
names(BRFSS)[names(BRFSS) == "CERVSCRN"] <- "CervScrnEver"
names(BRFSS)[names(BRFSS) == "CRVCLHPV"] <- "CervScrnHPV"
names(BRFSS)[names(BRFSS) == "CRVCLPAP"] <- "CervScrnPAP"
names(BRFSS)[names(BRFSS) == "HADHYST2"] <- "HadHyst"
names(BRFSS)[names(BRFSS) == "RRCLASS3"] <- "RaceOthers"
names(BRFSS)[names(BRFSS) == "INCOME3"] <- "income"
names(BRFSS)[names(BRFSS) == "EMPLOY1"] <- "employ"
names(BRFSS)[names(BRFSS) == "PRIMINSR"] <- "insurance"
names(BRFSS)[names(BRFSS) == "EDUCA"] <- "edu"
names(BRFSS)[names(BRFSS) == "_AGE80"] <- "age"
names(BRFSS)[names(BRFSS) == "GENHLTH"] <- "GenHlth"


names(BRFSS)

colnames(BRFSS) <- trimws(colnames(BRFSS)) #removing any blank spaces in column names


#cutting down data set to what we need


BRFSS <- BRFSS[, c("state", "sex", "HlthDiscrim", 
                   "CervScrnEver", "CervScrnHPV", "CervScrnPAP", "HadHyst", "RaceOthers", 
                   "income", "employ", "insurance", "edu", "age", "GenHlth")]

names(brfss) #check names included are correct



