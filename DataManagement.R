#############################################################################
# NAME: DATA MANAGEMENT 
rm(list=ls())
BRFSS <- read.csv("/Users/mirtamaravilla/Documents/UW Courses/Spring Qtr 2024/EPI 514 /EPI 514 Research/BRFSS") 

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
names(BRFSS)[names(BRFSS) == "X_AGE80"] <- "age"
names(BRFSS)[names(BRFSS) == "GENHLTH"] <- "GenHlth"


names(BRFSS)

colnames(BRFSS) <- trimws(colnames(BRFSS)) #removing any blank spaces in column names


#cutting down data set to what we need
BRFSS <- BRFSS[,c("state", "sex","HlthDiscrim", "CervScrnEver", "CervScrnHPV", 
                  "CervScrnPAP", "HadHyst", "RaceOthers", "income", "employ",
                  "insurance", "edu", "age", "GenHlth")]

names(BRFSS) #check names included are correct




#################### EXPOSURE (Mirta) ###################
#checking race variables
table(BRFSS$HlthDiscrim)

#Setting values to missing (highlighted red in data dictionary)
BRFSS$HlthDiscrim[BRFSS$HlthDiscrim==5] <- NA 
BRFSS$HlthDiscrim[BRFSS$HlthDiscrim==6] <- NA 
BRFSS$HlthDiscrim[BRFSS$HlthDiscrim==7] <- NA 
BRFSS$HlthDiscrim[BRFSS$HlthDiscrim==9] <- NA
table(BRFSS$HlthDiscrim)

#converting into labeled factor variables 
BRFSS$HlthDiscrim.f <- factor(BRFSS$HlthDiscrim,
                           levels = 1:4, 
                           labels = c("Worse than others", "same as others", 
                                      "better than others", "worse than some,better than others"))
#checking 
table(BRFSS$HlthDiscrim.f)


#################### OUTCOMES (Aileen) ########################

#check cervical cancer variables

summary(BRFSS$CervScrnEver)
summary(BRFSS$CervScrnHPV)
summary(BRFSS$CervScrnPAP)
summary(BRFSS$HadHyst)

#Cervical Screen ever variable

BRFSS$CervScrnEver[BRFSS$CervScrnEver==2] <- 0 #assigning 0 for no


BRFSS$CervScrnEver[BRFSS$CervScrnEver==7] <- NA #set missing values
BRFSS$CervScrnEver[BRFSS$CervScrnEver==9] <- NA #set missing values

summary(BRFSS$CervScrnEver) #check


#Cervical Screen with HPV test

BRFSS$CervScrnHPV[BRFSS$CervScrnHPV==2] <- 0 #assigning 0 for no


BRFSS$CervScrnHPV[BRFSS$CervScrnHPV==7] <- NA #set missing values
BRFSS$CervScrnHPV[BRFSS$CervScrnHPV==9] <- NA #set missing values

summary(BRFSS$CervScrnHPV) #check

#Cervical Screen with PAP test

BRFSS$CervScrnPAP[BRFSS$CervScrnPAP==2] <- 0 #assigning 0 for no


BRFSS$CervScrnPAP[BRFSS$CervScrnPAP==7] <- NA #set missing values
BRFSS$CervScrnPAP[BRFSS$CervScrnPAP==9] <- NA #set missing values

summary(BRFSS$CervScrnPAP) #check

#Hist of hysterectomy

BRFSS$HadHyst[BRFSS$HadHyst==2] <- 0 #assigning 0 for no


BRFSS$HadHyst[BRFSS$HadHyst==7] <- NA #set missing values
BRFSS$HadHyst[BRFSS$HadHyst==9] <- NA #set missing values

summary(BRFSS$HadHyst) #check


########################## COVARIATES (Mirta/Samantha) ####################

#INSURANCE 
table(BRFSS$insurance)

#setting values to missing (red highlight)
BRFSS$insurance[BRFSS$insurance==77] <- NA
BRFSS$insurance[BRFSS$insurance==99] <- NA

#converting to factor 
BRFSS$insurance_f<- BRFSS$insurance[BRFSS$insurance <=10] <- 1
BRFSS$insurance_f<- BRFSS$insurance[BRFSS$insurance==88] <- 0

#converting to labeled factor 
BRFSS$insurance.f <- factor(BRFSS$insurance_f,
                      levels = 0:1,
                      labels = c("No", "Yes"))
#checking
table(BRFSS$insurance_f)

