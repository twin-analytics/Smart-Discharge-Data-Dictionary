# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 04_SELECTION CRITERIA   ############
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Apply selection criteria for analysis
# This script will not run on its own - it only exists 
#  to be sourced in other scripts

# For data manipulations
library(tidyverse)
library(Hmisc)
library(summarytools)
library(broom)
library(openxlsx)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ELIGIBILITY CRITERIA      #########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Admitted to the hospital with a proven or suspected infection
infection_exclude <- subset(dat_clean, infection_adm == "No")
infection_exclude %>% 
  select(studyid_adm, redcap_event_name, infection_adm) %>% 
  print(n=Inf)
dat_clean <- subset(dat_clean, infection_adm != "No"|is.na(infection_adm))
n_distinct(infection_exclude$studyid_adm)
nrow(infection_exclude)

# Pilot data
any(is.na(dat_clean$is_pilot_adm))
sum(is.na(dat_clean$is_pilot_adm))

pilot_exclude <-subset(dat_clean, is_pilot_adm == "Yes")
dat_clean <- subset(dat_clean, is_pilot_adm != "Yes"|is.na(is_pilot_adm))
n_distinct(dat_clean$is_pilot_adm); table(dat_clean$is_pilot_adm)
nrow(pilot_exclude)

# Testing data
table(dat_clean$site_adm)
sum(is.na(dat_clean$site_adm))

site_exclude <- subset(dat_clean, site_adm == "Testing")
dat_clean <- subset(dat_clean, site_adm != "Testing"|is.na(site_adm))
dat_clean$site_adm <- droplevels(dat_clean$site_adm)
unique(dat_clean$site_adm)

# Eligible age
any(is.na(dat_clean$ageadmit_adm))
sum(is.na(dat_clean$ageadmit_adm))
age_exclude <- subset(dat_clean, ageadmit_adm == "No")
dat_clean <- subset(dat_clean, ageadmit_adm != "No" | is.na(ageadmit_adm))
nrow(age_exclude)
table(dat_clean$ageadmit_adm)


