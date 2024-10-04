rm(list=ls())
install.packages("survival")
install.packages("survminer")
install.packages("haven")
install.packages("tidyverse")
library("gridExtra")
getwd()
library(haven)
library(dplyr)
library(tidyverse)
library(readxl)


library(survival) # this is the cornerstone command for survival analysis in R
library(ggplot2)
library(ggpubr)
library(survminer)
install.packages("descriptr")
library(descriptr)
library(grid)
library(gridExtra)

TLM<- read_excel("E:/Manuscripts/SEER Databse/Tongue/Tongue_lung_metastatsis/Tongue_lung_metastasis_analysis_dataset.xlsx")


TLM$Age_cat<-as.factor(TLM$Metastasis_lung)
TLM$Age_cat2<-as.factor(TLM$Age_cat2)
TLM$Age_cat3<-as.factor(TLM$Age_cat3)
TLM$Age_cat4<-as.factor(TLM$Age_cat4)
TLM$Sex<-as.factor(TLM$Sex)
TLM$Marital_status_diagnosis<-as.factor(TLM$Marital_status_diagnosis)
TLM$Marital_Status <-as.factor(TLM$Marital_Status)
TLM$Median_household_cat<-as.factor(TLM$Median_household_cat)
TLM$Residence<-as.factor(TLM$Residence)
TLM$Race<-as.factor(TLM$Race)
TLM$Median_household_cat<-as.factor(TLM$Median_household_cat)
TLM$Site_tongue<-as.factor(TLM$Site_tongue)
TLM$TStage<-as.factor(TLM$TStage)
TLM$MStage<-as.factor(TLM$MStage)
TLM$NStage<-as.factor(TLM$NStage)
TLM$Stage<-as.factor(TLM$Stage)
TLM$Surgery<-as.factor(TLM$Surgery)
TLM$Radiation<-as.factor(TLM$Radiation)
TLM$Chemotherapy<-as.factor(TLM$Chemotherapy)
TLM$days_diagnosis_treatment_cat<-as.factor(TLM$days_diagnosis_treatment_cat)
TLM$Metastasis_lung<-as.factor(TLM$Metastasis_lung)
TLM$OS_6months<-as.factor(TLM$OS_6months)
TLM$OS_12months<-as.factor(TLM$OS_12months)
TLM$OS_24months<-as.factor(TLM$OS_24months)
TLM$OS_60months<-as.factor(TLM$OS_60months)
TLM$CSS_6months<-as.factor(TLM$CSS_6months)
TLM$CSS_12months<-as.factor(TLM$CSS_12months)
TLM$CSS_24months<-as.factor(TLM$CSS_24months)
TLM$CSS_60months<-as.factor(TLM$CSS_60months)
TLM$Metastasis_spread<-as.factor(TLM$Metastasis_spread)

TLM_cox_subset<-TLM %>% 
  filter(Metastasis_lung=="Yes")


###Characteristics of patients (Chi-square)

addmargins(table(TLM$Metastasis_lung))
prop.table(table(TLM$Metastasis_lung))*100

###AGE

addmargins(table(TLM$Age_cat4))
prop.table(table(TLM$Age_cat4))*100

table(TLM$Age_cat4,TLM$Metastasis_lung)
prop.table(table(TLM$Age_cat4,TLM$Metastasis_lung),2)*100

TLM %>% 
  select(Metastasis_lung, Age_cat4) %>% 
  table() %>% 
  chisq.test()

###SEX

addmargins(table(TLM$Sex))
prop.table(table(TLM$Sex))*100

table(TLM$Sex,TLM$Metastasis_lung)
prop.table(table(TLM$Sex,TLM$Metastasis_lung),2)*100

TLM %>% 
  select(Metastasis_lung, Sex) %>% 
  table() %>% 
  chisq.test()

###Marital Status
addmargins(table(TLM$Marital_Status))
prop.table(table(TLM$Marital_Status))*100

table(TLM$Marital_Status,TLM$Metastasis_lung)
prop.table(table(TLM$Marital_Status,TLM$Metastasis_lung),2)*100

TLM %>% 
  select(Metastasis_lung, Marital_Status) %>% 
  table() %>% 
  chisq.test()


###Residence
addmargins(table(TLM$Residence))
prop.table(table(TLM$Residence))*100

table(TLM$Residence,TLM$Metastasis_lung)
prop.table(table(TLM$Residence,TLM$Metastasis_lung),2)*100

TLM %>% 
  select(Metastasis_lung, Residence) %>% 
  table() %>% 
  chisq.test()


###Race
addmargins(table(TLM$Race))
prop.table(table(TLM$Race))*100

table(TLM$Race,TLM$Metastasis_lung)
prop.table(table(TLM$Race,TLM$Metastasis_lung),2)*100

TLM %>% 
  select(Metastasis_lung, Race) %>% 
  table() %>% 
  chisq.test()

###Monthly Income

addmargins(table(TLM$Median_household_cat))
prop.table(table(TLM$Median_household_cat))*100
TLM$Median_household_cat<- relevel(TLM$Median_household_cat, ref = "< $40,000")


table(TLM$Median_household_cat,TLM$Metastasis_lung)
prop.table(table(TLM$Median_household_cat,TLM$Metastasis_lung),2)*100

TLM %>% 
  select(Metastasis_lung, Median_household_cat) %>% 
  table() %>% 
  chisq.test()

###Site_tongue
addmargins(table(TLM$Site_tongue))
prop.table(table(TLM$Site_tongue))*100

table(TLM$Site_tongue,TLM$Metastasis_lung)
prop.table(table(TLM$Site_tongue,TLM$Metastasis_lung),2)*100

TLM %>% 
  select(Metastasis_lung, Site_tongue) %>% 
  table() %>% 
  chisq.test()

###TStage
addmargins(table(TLM$TStage))
prop.table(table(TLM$TStage))*100

table(TLM$TStage,TLM$Metastasis_lung)
prop.table(table(TLM$TStage,TLM$Metastasis_lung),2)*100

TLM %>% 
  select(Metastasis_lung, TStage) %>% 
  table() %>% 
  chisq.test()


###Nstage
addmargins(table(TLM$NStage))
prop.table(table(TLM$NStage))*100

table(TLM$NStage,TLM$Metastasis_lung)
prop.table(table(TLM$NStage,TLM$Metastasis_lung),2)*100

TLM %>% 
  select(Metastasis_lung, NStage) %>% 
  table() %>% 
  chisq.test()


###Metastasis_number
addmargins(table(TLM$Metastasis_spread))
prop.table(table(TLM$Metastasis_spread))*100

table(TLM$Metastasis_spread,TLM$Metastasis_lung)
prop.table(table(TLM$Metastasis_spread,TLM$Metastasis_lung),2)*100

TLM %>% 
  select(Metastasis_lung, Metastasis_spread) %>% 
  table() %>% 
  chisq.test()


###days_from_diagnosis_to treatment
addmargins(table(TLM$days_diagnosis_treatment_cat))
prop.table(table(TLM$days_diagnosis_treatment_cat))*100

table(TLM$days_diagnosis_treatment_cat,TLM$Metastasis_lung)
prop.table(table(TLM$days_diagnosis_treatment_cat,TLM$Metastasis_lung),2)*100

TLM %>% 
  select(Metastasis_lung, days_diagnosis_treatment_cat) %>% 
  table() %>% 
  chisq.test()



###Surgery_performed
addmargins(table(TLM$Surgery))
prop.table(table(TLM$Surgery))*100

table(TLM$Surgery,TLM$Metastasis_lung)
prop.table(table(TLM$Surgery,TLM$Metastasis_lung),2)*100

TLM %>% 
  select(Metastasis_lung, Surgery) %>% 
  table() %>% 
  chisq.test()

###Radiology_performed
addmargins(table(TLM$Radiation))
prop.table(table(TLM$Radiation))*100

table(TLM$Radiation,TLM$Metastasis_lung)
prop.table(table(TLM$Radiation,TLM$Metastasis_lung),2)*100

TLM %>% 
  select(Metastasis_lung, Radiation) %>% 
  table() %>% 
  chisq.test()

###Chemotherapy_performed
addmargins(table(TLM$Chemotherapy))
prop.table(table(TLM$Chemotherapy))*100

table(TLM$Chemotherapy,TLM$Metastasis_lung)
prop.table(table(TLM$Chemotherapy,TLM$Metastasis_lung),2)*100

TLM %>% 
  select(Metastasis_lung, Chemotherapy) %>% 
  table() %>% 
  chisq.test()

###Vital_Status
addmargins(table(TLM$Vital_Status))
prop.table(table(TLM$Vital_Status))*100

addmargins(table(TLM_cox_subset$Vital_Status))
prop.table(table(TLM_cox_subset$Vital_Status))*100



##########--------Risk factors for lung metastasis---#############
####Incidence####
(prop.table(table(TLM$Metastasis_lung)))*100
round(prop.table(table(TLM$Metastasis_lung))*100)


###Logistic

###Age
levels (TLM$Metastasis_lung)
TLM$Age_cat4<- relevel(TLM$Age_cat4, ref = "upto 40")


Log_TLM_Age<-glm(TLM$Metastasis_lung ~ TLM$Age_cat4,
                 family=binomial(link=logit))
summary(Log_TLM_Age)
exp(cbind(Odds_RAtio=coef(Log_TLM_Age), confint(Log_TLM_Age)))


#Sex

Log_TLM_Sex<-glm(TLM$Metastasis_lung ~ TLM$Sex,
                 family=binomial(link=logit))
summary(Log_TLM_Sex)
exp(cbind(Odds_RAtio=coef(Log_TLM_Sex), confint(Log_TLM_Sex)))


###Marital_Status_diagnosis_cat2

Log_TLM_MS2<-glm(TLM$Metastasis_lung ~ TLM$Marital_Status,
                 family=binomial(link=logit))
summary(Log_TLM_MS2)
exp(cbind(Odds_RAtio=coef(Log_TLM_MS2), confint(Log_TLM_MS2)))


###Residence

Log_TLM_Residence<-glm(TLM$Metastasis_lung ~ TLM$Residence,
                       family=binomial(link=logit))
summary(Log_TLM_Residence)
exp(cbind(Odds_RAtio=coef(Log_TLM_Residence), confint(Log_TLM_Residence)))


###Race

TLM$Race<- relevel(TLM$Race, ref = "White")
Log_TLM_Race<-glm(TLM$Metastasis_lung ~ TLM$Race,
                  family=binomial(link=logit))
summary(Log_TLM_Race)
exp(cbind(Odds_RAtio=coef(Log_TLM_Race), confint(Log_TLM_Race)))



###Site_tongue

Log_TLM_Site_tongue<-glm(TLM$Metastasis_lung ~ TLM$Site_tongue,
                         family=binomial(link=logit))
summary(Log_TLM_Site_tongue)
exp(cbind(Odds_RAtio=coef(Log_TLM_Site_tongue), confint(Log_TLM_Site_tongue)))


###TStage

Log_TLM_TStage<-glm(TLM$Metastasis_lung ~ TLM$TStage,
                    family=binomial(link=logit))
summary(Log_TLM_TStage)
exp(cbind(Odds_RAtio=coef(Log_TLM_TStage), confint(Log_TLM_TStage)))


###NStage

Log_TLM_NStage<-glm(TLM$Metastasis_lung ~ TLM$NStage,
                    family=binomial(link=logit))
summary(Log_TLM_NStage)
exp(cbind(Odds_RAtio=coef(Log_TLM_NStage), confint(Log_TLM_NStage)))



Log_TLM<-glm(TLM$Metastasis_lung ~ TLM$Age_cat4
             +TLM$Sex
             +TLM$Marital_Status
             +TLM$Race
             +TLM$Site_tongue
             +TLM$TStage
             +TLM$NStage,
             family=binomial(link=logit))
summary(Log_TLM)
exp(cbind(Odds_RAtio=coef(Log_TLM), confint(Log_TLM)))



#############-SurvivalAnalysis-##############


TLM_metastasis_subset<- TLM %>% 
  filter(Metastasis_lung=="Yes")
summary(TLM_metastasis_subset$Survival_months)


####OverallSurvival/Mortality####
addmargins(table(TLM$Vital_Status))
prop.table(table(TLM$Vital_Status))*100

addmargins(table(TLM_metastasis_subset$Vital_Status))
table(TLM_metastasis_subset$Vital_Status)
prop.table(table(TLM_metastasis_subset$Vital_Status))*100


TLM_OS<-TLM %>% 
  filter(Vital_Status=="Dead")
summary(TLM_OS$Survival_months)

TLM_metastasis_OS<-TLM_metastasis_subset %>% 
  filter(Vital_Status=="Dead")
summary(TLM_metastasis_OS$Survival_months)


table(TLM_metastasis_subset$Vital_Status)
prop.table(table(TLM_metastasis_subset$Vital_Status))*100
table(TLM_metastasis_subset$OS_12months)
prop.table(table(TLM_metastasis_subset$OS_12months))*100
table(TLM_metastasis_subset$OS_24months)
prop.table(table(TLM_metastasis_subset$OS_24months))*100
table(TLM_metastasis_subset$OS_60months)
prop.table(table(TLM_metastasis_subset$OS_60months))*100

####Surgery
table(TLM_metastasis_OS$Surgery)
prop.table(table(TLM_metastasis_OS$Surgery))*100
TLM_metastasis_OS %>% 
  t.test(Survival_months~Surgery, data=., alternative = "two.sided")
TLM_metastasis_OS_surgery<-TLM_metastasis_OS %>% 
  filter(Surgery=="Yes")
summary(TLM_metastasis_OS_surgery$Survival_months)

####Radiation
table(TLM_metastasis_OS$Radiation)
prop.table(table(TLM_metastasis_OS$Radiation))*100
TLM_metastasis_OS %>% 
  t.test(Survival_months~Radiation, data=., alternative = "two.sided")
TLM_metastasis_OS_Radiation<-TLM_metastasis_OS %>% 
  filter(Radiation=="Yes")
summary(TLM_metastasis_OS_Radiation$Survival_months)


####Chemotherapy
table(TLM_metastasis_OS$Chemotherapy)
prop.table(table(TLM_metastasis_OS$Chemotherapy))*100
TLM_metastasis_OS %>% 
  t.test(Survival_months~Chemotherapy, data=., alternative = "two.sided")
TLM_metastasis_OS_Chemotherapy<-TLM_metastasis_OS %>% 
  filter(Chemotherapy=="Yes")
summary(TLM_metastasis_OS_Chemotherapy$Survival_months)




#####Cancer-specific-Survival/Mortality#####

addmargins(table(TLM$CSS_def))
prop.table(table(TLM$CSS_def))*100

addmargins(table(TLM_metastasis_subset$CSS_def))
table(TLM_metastasis_subset$CSS_def)
prop.table(table(TLM_metastasis_subset$CSS_def))*100

TLM_CSS<-TLM %>% 
  filter(CSS_def=="Dead (attributable to this cancer dx)")
summary(TLM_CSS$Survival_months)

TLM_metastasis_CSS<-TLM_metastasis_subset %>% 
  filter(CSS_def=="Dead (attributable to this cancer dx)")
summary(TLM_metastasis_CSS$Survival_months)

table(TLM_metastasis_subset$CSS_def)
prop.table(table(TLM_metastasis_subset$CSS_def))*100
table(TLM_metastasis_subset$CSS_12months)
prop.table(table(TLM_metastasis_subset$CSS_12months))*100
table(TLM_metastasis_subset$CSS_24months)
prop.table(table(TLM_metastasis_subset$CSS_24months))*100
table(TLM_metastasis_subset$CSS_60months)
prop.table(table(TLM_metastasis_subset$CSS_60months))*100



####Surgery
table(TLM_metastasis_CSS$Surgery)
prop.table(table(TLM_metastasis_CSS$Surgery))*100
TLM_metastasis_CSS %>% 
  t.test(Survival_months~Surgery, data=., alternative = "two.sided")
TLM_metastasis_CSS_surgery<-TLM_metastasis_CSS %>% 
  filter(Surgery=="Yes")
summary(TLM_metastasis_CSS_surgery$Survival_months)

####Radiation
table(TLM_metastasis_CSS$Radiation)
prop.table(table(TLM_metastasis_CSS$Radiation))*100
TLM_metastasis_CSS %>% 
  t.test(Survival_months~Radiation, data=., alternative = "two.sided")
TLM_metastasis_CSS_Radiation<-TLM_metastasis_OS %>% 
  filter(Radiation=="Yes")
summary(TLM_metastasis_CSS_Radiation$Survival_months)


####Chemotherapy
table(TLM_metastasis_CSS$Chemotherapy)
prop.table(table(TLM_metastasis_CSS$Chemotherapy))*100
TLM_metastasis_CSS %>% 
  t.test(Survival_months~Chemotherapy, data=., alternative = "two.sided")
TLM_metastasis_CSS_Chemotherapy<-TLM_metastasis_CSS %>% 
  filter(Chemotherapy=="Yes")
summary(TLM_metastasis_CSS_Chemotherapy$Survival_months)


#################---COX Regression---########################

###OS
TLM$Survival_months<-as.numeric(TLM$Survival_months)
TLM_cox_subset$Survival_months<-as.numeric(TLM_cox_subset$Survival_months)


res.cox_age <- coxph(Surv(Survival_months, Vital_Status_lung) ~ Age_cat4, 
                     data = TLM_cox_subset)
summary(res.cox_age)


res.cox_sex <- coxph(Surv(Survival_months, Vital_Status_lung) ~ Sex, 
                     data = TLM_cox_subset)
summary(res.cox_sex)

res.cox_MS<- coxph(Surv(Survival_months, Vital_Status_lung) ~ Marital_Status, 
                   data = TLM_cox_subset)
summary(res.cox_MS)


res.cox_residence<- coxph(Surv(Survival_months, Vital_Status_lung) ~ Residence, 
                          data = TLM_cox_subset)
summary(res.cox_residence)

res.cox_race <- coxph(Surv(Survival_months, Vital_Status_lung) ~ Race, 
                      data = TLM_cox_subset)
summary(res.cox_race)


TLM_cox_subset$Median_household_cat<- relevel(TLM_cox_subset$Median_household_cat, ref = "< $40,000")
res.cox_MI <- coxph(Surv(Survival_months, Vital_Status_lung) ~ Median_household_cat, 
                      data = TLM_cox_subset)
summary(res.cox_MI)

res.cox_site <- coxph(Surv(Survival_months, Vital_Status_lung) ~ Site_tongue, 
                      data = TLM_cox_subset)
summary(res.cox_site)

summary(res.cox_MI)

res.cox_mspread <- coxph(Surv(Survival_months, Vital_Status_lung) ~ Metastasis_spread, 
                      data = TLM_cox_subset)
summary(res.cox_mspread)

TLM_cox_subset$days_diagnosis_treatment_cat<- relevel(TLM_cox_subset$days_diagnosis_treatment_cat, ref="Upto 30 days")
res.cox_ddt<- coxph(Surv(Survival_months, Vital_Status_lung) ~ days_diagnosis_treatment_cat, 
                    data = TLM_cox_subset)
summary(res.cox_ddt)

res.cox_surgery<- coxph(Surv(Survival_months, Vital_Status_lung) ~ Surgery, 
                        data = TLM_cox_subset)
summary(res.cox_surgery)

res.cox_radio<- coxph(Surv(Survival_months, Vital_Status_lung) ~ Radiation, 
                      data = TLM_cox_subset)
summary(res.cox_radio)

res.cox_chemo<- coxph(Surv(Survival_months, Vital_Status_lung) ~ Chemotherapy, 
                      data = TLM_cox_subset)
summary(res.cox_chemo)


res.cox_Site<- coxph(Surv(Survival_months, Vital_Status_lung) ~ Site_tongue, 
                     data = TLM_cox_subset)
summary(res.cox_Site)

res.cox_Tstage<- coxph(Surv(Survival_months, Vital_Status_lung) ~ TStage, 
                       data = TLM_cox_subset)
summary(res.cox_Tstage)

res.cox_Nstage<- coxph(Surv(Survival_months, Vital_Status_lung) ~ NStage, 
                       data = TLM_cox_subset)
summary(res.cox_Nstage)
###res.cox_all <- coxph(Surv(Survival_months, Vital_Status_lung) ~ Age_cat4+ Sex + Marital_Status
                     ##+Race+Residence+Site_tongue+days_diagnosis_treatment_cat
                     ##+Surgery+Chemotherapy+Radiation,
                     ###data =  TLM_cox_subset)
###summary(res.cox_all)

res.cox_all <- coxph(Surv(Survival_months, Vital_Status_lung) ~ Race+Site_tongue+TStage+Metastasis_spread+Chemotherapy,
                     data =  TLM_cox_subset)
summary(res.cox_all)

fit <- surv_fit(Surv(Survival_months, Vital_Status_lung) ~ Sex + Marital_Status
                +Race+Site_tongue+Metastasis_spread+Chemotherapy,  data = TLM_cox_subset)


######Cancer-specific Survival#####
res.cox_CSSage <- coxph(Surv(Survival_months, CSS) ~ Age_cat4, 
                     data = TLM_cox_subset)
summary(res.cox_CSSage)


res.cox_CSSsex <- coxph(Surv(Survival_months, CSS) ~ Sex, 
                     data = TLM_cox_subset)
summary(res.cox_CSSsex)

res.cox_CSSMS<- coxph(Surv(Survival_months, CSS) ~ Marital_Status, 
                   data = TLM_cox_subset)
summary(res.cox_CSSMS)


res.cox_CSSresidence<- coxph(Surv(Survival_months, CSS) ~ Residence, 
                          data = TLM_cox_subset)
summary(res.cox_CSSresidence)

res.cox_race <- coxph(Surv(Survival_months, CSS) ~ Race, 
                      data = TLM_cox_subset)
summary(res.cox_race)


TLM_cox_subset$Median_household_cat<- relevel(TLM_cox_subset$Median_household_cat, ref = "< $40,000")
res.cox_CSSMI <- coxph(Surv(Survival_months, CSS) ~ Median_household_cat, 
                    data = TLM_cox_subset)
summary(res.cox_CSSMI)

res.cox_CSSsite <- coxph(Surv(Survival_months, CSS) ~ Site_tongue, 
                      data = TLM_cox_subset)
summary(res.cox_CSSsite)

res.cox_CSmspread <- coxph(Surv(Survival_months, CSS) ~ Metastasis_spread, 
                         data = TLM_cox_subset)
summary(res.cox_CSmspread)

TLM_cox_subset$days_diagnosis_treatment_cat<- relevel(TLM_cox_subset$days_diagnosis_treatment_cat, ref="Upto 30 days")
res.cox_CSSddt<- coxph(Surv(Survival_months, CSS) ~ days_diagnosis_treatment_cat, 
                    data = TLM_cox_subset)
summary(res.cox_CSSddt)

res.cox_CSSsurgery<- coxph(Surv(Survival_months, CSS) ~ Surgery, 
                        data = TLM_cox_subset)
summary(res.cox_CSSsurgery)

res.cox_CSSradio<- coxph(Surv(Survival_months, CSS) ~ Radiation, 
                      data = TLM_cox_subset)
summary(res.cox_CSSradio)

res.cox_CSSchemo<- coxph(Surv(Survival_months, CSS) ~ Chemotherapy, 
                      data = TLM_cox_subset)
summary(res.cox_CSSchemo)


res.cox_CSSTstage<- coxph(Surv(Survival_months, CSS) ~ TStage, 
                       data = TLM_cox_subset)
summary(res.cox_CSSTstage)

res.cox_CSSNstage<- coxph(Surv(Survival_months, CSS) ~ NStage, 
                       data = TLM_cox_subset)
summary(res.cox_CSSNstage)
###res.cox_CSSall <- coxph(Surv(Survival_months, CSS) ~ Age_cat4+ Sex + Marital_Status
                     ###+Race+Residence+Site_tongue+days_diagnosis_treatment_cat
                     ###+Surgery+Chemotherapy+Radiation,
                     ###data =  TLM_cox_subset)
###summary(res.cox_CSSall)

res.cox_CSSall <- coxph(Surv(Survival_months, CSS) ~ Sex 
                     +Race+Site_tongue+TStage+Chemotherapy,
                     data =  TLM_cox_subset)
summary(res.cox_CSSall)

CSSfit <- surv_fit(Surv(Survival_months, CSS) ~ Sex + Marital_Status
                +Race+Site_tongue+Chemotherapy,  data = TLM_cox_subset)


####figures
####Logistic plot

dat<- data.frame(
  Index= c(1,2,3,4,5,6,7,8,9,10,11,12),
  label=c("Above 40 vs Upto 40","Male vs Female", "Single/Separated/Divorced/Widowed vs Married",
          "Black vs White","Others vs White","Base of tongue vs Anterior part", "T2 vs T1", "T3 vs T1", "T4 vs T1", "N1 vs N0", "N2 vs N0", "N3 vs N0"),
  OR=c(1.56, 1.40, 1.42,1.22, 0.86, 2.43, 1.81, 4.47,4.86, 1.97, 4.02, 7.11),
  UL=c(0.64, 0.95, 1.08, 0.75, 0.40, 1.70, 1.07, 2.73, 2.96, 1.04, 2.34, 3.85),
  LL=c(5.15, 2.12, 1.87, 1.88, 1.62, 3.54, 3.17, 7.67, 8.35, 3.89, 7.45, 13.85),
  CI=c("0.64,5.15", "0.95,2.12", "1.08, 1.87","0.75, 1.88", "0.40, 1.62",
       "1.70, 3.54", "1.07, 3.17", "2,73, 7.67","2.96, 8.35", "1.04, 3.89",
       "2.34, 7.45", "3.85,13.85"))

dat

plot1 <- ggplot(dat, aes(y = Index, x = OR)) +
  geom_point(shape = 18, size = 5) +  
  geom_errorbarh(aes(xmin = LL, xmax = UL), height = 0.25) +
  geom_vline(xintercept = 1, color = "red", linetype = "dashed", cex = 1, alpha = 0.5) +
  scale_y_continuous(name = "", breaks=1:12, labels = dat$label, trans = "reverse") +
  xlab("Odds Ratio (95% CI)") + 
  ylab(" ") + 
  theme_bw() +
  theme(panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.y = element_text(size =  12, colour = "black"),
        axis.text.x.bottom = element_text(size =  12, colour = "black"),
        axis.title.x = element_text(size = 12, colour = "black"))
plot1

table_base <- ggplot(dat, aes(y=label)) +
  ylab(NULL) + xlab("  ") + 
  theme(plot.title = element_text(hjust = 0.5, size=12), 
        axis.text.x = element_text(color="white", hjust = -3, size = 20), ## This is used to help with alignment
        axis.line = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_blank(),
        axis.title.y = element_blank(), 
        legend.position = "none",
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.background = element_blank())

table_base

###tab1 <- table_base + 
  labs(title = "space") +
  geom_text(aes(y = rev(Index), x = 1, label = sprintf("%0.1f", round(OR, digits = 1))), size = 4) + ## decimal places
  ggtitle("OR")  ###If one digit in ODDS RAtio


tab1 <- table_base + 
  labs(title = "space") +
  geom_text(aes(y = rev(Index), x = 1, label = OR), size = 4) + ## decimal places
  ggtitle("OR")
tab1

tab2 <- table_base +
  geom_text(aes(y = rev(Index), x = 1, label = CI), size = 4) + 
  ggtitle("95% CI")
tab2

lay <-  matrix(c(1,1,1,1,1,1,1,1,1,1,2,3,3), nrow = 1)
grid.arrange(plot1, tab1, tab2, layout_matrix = lay)



# 1. Generate the survival curve 
km_OS_fit<- survfit(Surv(Survival_months, Vital_Status_lung)~Metastasis_lung, data = TLM)
km_CSS_fit<- survfit(Surv(Survival_months, CSS)~Metastasis_lung, data = TLM)

race_subset<- subset(TLM_cox_subset, Race !="Other")
km_OS_race_fit<- survfit(Surv(Survival_months, Vital_Status_lung)~Race, data = race_subset)
km_OS_site_fit<- survfit(Surv(Survival_months, Vital_Status_lung)~Site_tongue, data = TLM_cox_subset)
km_OS_MSpread_fit<- survfit(Surv(Survival_months, Vital_Status_lung)~Metastasis_spread, data = TLM_cox_subset)
km_OS_chemo_fit<- survfit(Surv(Survival_months, Vital_Status_lung)~Chemotherapy, data = TLM_cox_subset)



km_CSS_race_fit<- survfit(Surv(Survival_months, CSS)~Race, data = race_subset)
km_CSS_site_fit<- survfit(Surv(Survival_months, CSS)~Site_tongue, data = TLM_cox_subset)
km_CSS_MSpread_fit<- survfit(Surv(Survival_months, CSS)~Metastasis_spread, data = TLM_cox_subset)
km_CSS_chemo_fit<- survfit(Surv(Survival_months, CSS)~Chemotherapy, data = TLM_cox_subset)

# 2. Plot the curve 

plot(km_site_fit)
plot(km_chemo_fit)


##customized colors

custom_colors<- c("#E7B800", "#2E9FDF", "#FF0000")


###Figure2

###OS
ggsurvplot(
  km_OS_fit,
  data=TLM,
  pval=TRUE, palette=custom_colors,
  pval.coord=c(30, .5), ###to change p-value position
  pval.size=6, ###to change p-value legend size
  ggtheme= theme_minimal(),
  xlab= "Time in months",
  ylab= "Overall survival probability",
  break.time.by=12,
  break.y.by=.20,
  surv.scale="percent",
  xlim=c(0,60),
  font.main= c(12, "bold"),
  font.x= c(15, "bold"),
  font.y= c(15, "bold"),
  font.tickslab= c(10, "plain"),
  font.legend= c(16, "plain")+
    theme(
    legend.text = element_text(family = "Times", size = 12, face = "bold"),
    legend.title = element_text(family = "Times", size = 12, face = "bold"),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  )
)


####CSS
ggsurvplot(
  km_CSS_fit,
  data=TLM,
  pval=TRUE, palette=custom_colors,
  pval.coord=c(30, .5), ###to change p-value position
  pval.size=6, ###to change p-value legend size
  ggtheme= theme_minimal(),
  xlab= "Time in months",
  ylab= "Cancer-specific survival probability",
  break.time.by=12,
  break.y.by=.20,
  surv.scale="percent",
  xlim=c(0,60),
  font.main= c(12, "bold"),
  font.x= c(15, "bold"),
  font.y= c(15, "bold"),
  font.tickslab= c(10, "plain"),
  font.legend= c(16, "plain")+
    theme(
      legend.text = element_text(family = "Times", size = 12, face = "bold"),
      legend.title = element_text(family = "Times", size = 12, face = "bold"),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
)

#########Figure3#######

#####----race------####

ggsurvplot(
  km_OS_race_fit,
  data=TLM,
  pval=TRUE, palette=custom_colors,
  pval.coord=c(30, .5), ###to change p-value position
  pval.size=6, ###to change p-value legend size
  ggtheme= theme_minimal(),
  xlab= "Time in months",
  ylab= "Overall survival probability",
  break.time.by=12,
  break.y.by=.20,
  surv.scale="percent",
  xlim=c(0,60),
  font.main= c(12, "bold"),
  font.x= c(15, "bold"),
  font.y= c(15, "bold"),
  font.tickslab= c(10, "plain"),
  font.legend= c(16, "plain")+
    theme(
      legend.text = element_text(family = "Times", size = 12, face = "bold"),
      legend.title = element_text(family = "Times", size = 12, face = "bold"),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
)

########-----site-------#######

ggsurvplot(
  km_OS_site_fit,
  data=TLM,
  pval=TRUE, palette=custom_colors,
  pval.coord=c(30, .5), ###to change p-value position
  pval.size=6, ###to change p-value legend size
  ggtheme= theme_minimal(),
  xlab= "Time in months",
  ylab= "Overall survival probability",
  break.time.by=12,
  break.y.by=.20,
  surv.scale="percent",
  xlim=c(0,60),
  font.main= c(12, "bold"),
  font.x= c(15, "bold"),
  font.y= c(15, "bold"),
  font.tickslab= c(10, "plain"),
  font.legend= c(16, "plain")+
    theme(
      legend.text = element_text(family = "Times", size = 12, face = "bold"),
      legend.title = element_text(family = "Times", size = 12, face = "bold"),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
)


########-----Extent of distant metastasis-------#######

ggsurvplot(
  km_OS_MSpread_fit,
  data=TLM,
  pval=TRUE, palette=custom_colors,
  pval.coord=c(30, .5), ###to change p-value position
  pval.size=6, ###to change p-value legend size
  ggtheme= theme_minimal(),
  xlab= "Time in months",
  ylab= "Overall survival probability",
  break.time.by=12,
  break.y.by=.20,
  surv.scale="percent",
  xlim=c(0,60),
  font.main= c(12, "bold"),
  font.x= c(15, "bold"),
  font.y= c(15, "bold"),
  font.tickslab= c(10, "plain"),
  font.legend= c(16, "plain")+
    theme(
      legend.text = element_text(family = "Times", size = 12, face = "bold"),
      legend.title = element_text(family = "Times", size = 12, face = "bold"),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
)


########-----Chemotherapy-------#######

ggsurvplot(
  km_OS_chemo_fit,
  data=TLM,
  pval=TRUE, palette=custom_colors,
  pval.coord=c(30, .5), ###to change p-value position
  pval.size=6, ###to change p-value legend size
  ggtheme= theme_minimal(),
  xlab= "Time in months",
  ylab= "Overall survival probability",
  break.time.by=12,
  break.y.by=.20,
  surv.scale="percent",
  xlim=c(0,60),
  font.main= c(12, "bold"),
  font.x= c(15, "bold"),
  font.y= c(15, "bold"),
  font.tickslab= c(10, "plain"),
  font.legend= c(16, "plain")+
    theme(
      legend.text = element_text(family = "Times", size = 12, face = "bold"),
      legend.title = element_text(family = "Times", size = 12, face = "bold"),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
)

#########Figure4#######

#####----race------####

ggsurvplot(
  km_CSS_race_fit,
  data=TLM,
  pval=TRUE, palette=custom_colors,
  pval.coord=c(30, .5), ###to change p-value position
  pval.size=6, ###to change p-value legend size
  ggtheme= theme_minimal(),
  xlab= "Time in months",
  ylab= "Cancer-specific survival probability",
  break.time.by=12,
  break.y.by=.20,
  surv.scale="percent",
  xlim=c(0,60),
  font.main= c(12, "bold"),
  font.x= c(15, "bold"),
  font.y= c(15, "bold"),
  font.tickslab= c(10, "plain"),
  font.legend= c(16, "plain")+
    theme(
      legend.text = element_text(family = "Times", size = 12, face = "bold"),
      legend.title = element_text(family = "Times", size = 12, face = "bold"),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
)


########-----site-------#######

ggsurvplot(
  km_CSS_site_fit,
  data=TLM,
  pval=TRUE, palette=custom_colors,
  pval.coord=c(30, .5), ###to change p-value position
  pval.size=6, ###to change p-value legend size
  ggtheme= theme_minimal(),
  xlab= "Time in months",
  ylab= "Cancer-specific survival probability",
  break.time.by=12,
  break.y.by=.20,
  surv.scale="percent",
  xlim=c(0,60),
  font.main= c(12, "bold"),
  font.x= c(15, "bold"),
  font.y= c(15, "bold"),
  font.tickslab= c(10, "plain"),
  font.legend= c(16, "plain")+
    theme(
      legend.text = element_text(family = "Times", size = 12, face = "bold"),
      legend.title = element_text(family = "Times", size = 12, face = "bold"),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
)



########-----Chemotherapy-------#######

ggsurvplot(
  km_CSS_chemo_fit,
  data=TLM,
  pval=TRUE, palette=custom_colors,
  pval.coord=c(30, .5), ###to change p-value position
  pval.size=6, ###to change p-value legend size
  ggtheme= theme_minimal(),
  xlab= "Time in months",
  ylab= "Cancer-specific survival probability",
  break.time.by=12,
  break.y.by=.20,
  surv.scale="percent",
  xlim=c(0,60),
  font.main= c(12, "bold"),
  font.x= c(15, "bold"),
  font.y= c(15, "bold"),
  font.tickslab= c(10, "plain"),
  font.legend= c(16, "plain")+
    theme(
      legend.text = element_text(family = "Times", size = 12, face = "bold"),
      legend.title = element_text(family = "Times", size = 12, face = "bold"),
      panel.background = element_rect(fill = "white"),
      plot.background = element_rect(fill = "white")
    )
)





