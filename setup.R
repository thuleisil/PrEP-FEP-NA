library(haven)
library(tidyverse)
library(summarytools)
library(networktools)
library(qgraph)
library(bootnet)
library(psychonetrics)



prep_df <- read_sav(file = "data/FEP-PrEP-Roma.sav") 


prep_df_mod <- prep_df %>% 
  mutate(gender = as.factor(GENDER)) %>% 
  mutate(age = ETA,
         race = as.factor(ETNIA),
         religion = as.factor(Regione_Nascita),
         ses = as.factor(STATOCIV),
         housing_condition = as.factor(DOMICIL),
         school_years = ANNISCOL,
         occupation = as.factor(Occupazione),
         referral = as.factor(INVIO),
         previous_psychiatrist_contact = as.factor(PRE_CONTATTO),
         previous_suicide_attempt = as.factor(PRE_TS),
         diagnosis_t0 = as.factor(T0_DIAGNOSI),
         dui = DUI,
         substance_abuse = as.factor(ABUSO_Sostanze),
         antipsychotics_t0 = as.factor(ANTIPSIC1),
         ) %>% 
  mutate(across(c(ANTIDEP1, ANTIDEP2, STABILIZ1, STABILIZ2, BENZO), ~as.factor(.x)))



prep_df_drop <- prep_df %>% 
  select(contains("PANSS"), ETA, GENDER, ETNIA, ANTIPSIC1, ANTIDEP1, STABILIZ1, BENZO) %>% 
  drop_na()




