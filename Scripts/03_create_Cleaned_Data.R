###############################################################
#  PROJECT: REDCap Multi-country Pediatric Sepsis Dataset
#  SCRIPT: 03_Create_Cleaned_Data.R
#  PURPOSE: Combine, clean, and prepare harmonized datasets 
#           from Uganda, Rwanda and Tanzania for further analysis.

###############################################################

# ~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD LIBRARIES    #######
# ~~~~~~~~~~~~~~~~~~~~~~~~~

install.packages("tidyverse")
install.packages("xlsx")
install.packages("expss")
install.packages("janitor")
install.packages("DataExplorer")
install.packages("SmartEDA")
install.packages("dlookr")
install.packages("here")

# Core set of libraries for data manipulation
library(tidyverse) # Data wrangling and transformation
library(expss)     # Manage and preserve variable labels
library(janitor)   # Clean variable names
library(here)      # Simplify relative file paths
library(dplyr)     # Data manipulation (part of tidyverse)
library(sjlabelled)# Label handling for survey data
library(Hmisc)     # Label management, summary stats
library(labelled)

# For Exploratory Data Analysis
library(DataExplorer)# Automated data exploration
library(SmartEDA)    # Data profiling
library(dlookr)      # Data diagnosis and visualization

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOAD DATA  AND PREPARE DATASETS #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load country-specific REDCap label scripts
# Rename the loaded data and remove the original 'data' object.
# This is done to prevent potential overwriting when loading the next dataset.

source("Scripts/01_Redcap_Labels.r")
dat_uganda <- data
rm(data)   
  
source("Scripts/02_RedCap_Labels.R")
dat_rwanda_tz <- data %>% 
  filter(redcap_data_access_group != "uganda")
rm(data)

# Quick checks
unique(dat_uganda$redcap_event_name)
unique(dat_rwanda_tz$redcap_event_name)
dim(dat_uganda)
dim(dat_rwanda_tz)
glimpse(dat_uganda)
glimpse(dat_rwanda_tz)

# Identify variables that differ between datasets
setdiff(colnames(dat_uganda), colnames(dat_rwanda_tz))
setdiff(colnames(dat_rwanda_tz)[!grepl("\\.factor$", colnames(dat_rwanda_tz))], colnames(dat_uganda))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# HANDLE LABELLED VARIABLES BEFORE MERGING #####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Count labelled variables
sum(sapply(dat_uganda, is.labelled))
sum(sapply(dat_rwanda_tz, is.labelled))

# Save variable labels before merging
label_vars_uganda <- get_label(dat_uganda)
label_vars_rwanda <- get_label(dat_rwanda_tz)

# Temporarily remove labels (to avoid bind_rows() errors)
dat_uganda_clean <- remove_all_labels(dat_uganda)
dat_rwanda_clean <- remove_all_labels(dat_rwanda_tz)

# Merge datasets safely
dat_raw <- bind_rows(dat_uganda_clean, dat_rwanda_clean)

# Reapply labels
label(dat_raw) <- as.list(c(label_vars_uganda, label_vars_rwanda))[names(dat_raw)]

str(dat_raw)
glimpse(dat_raw)

# ~~~~~~~~~~~~~~~~~~~~~~~
# HANDLE FACTOR VARIABLES ####
# ~~~~~~~~~~~~~~~~~~~~~~~

# Replace factors cols ####

# REDCap automatically creates duplicate factor  versions of each variable
# Replace the original version of variables with the factor versions

# Note that some of the levels are not consistent (levels are sometimes 1,0 or 0,1)
#  This is how it's coded in REDCap though so just need to be careful...

factor_vars <- grep("\\.factor", colnames(dat_raw), value = TRUE)
nonfactor_equivalent <- gsub("\\.factor", "", factor_vars)

# Replace the non-factor version with the factor version of variables
# Make sure to preserve the labels before overwriting
#  Note: Factor version of columns do not have labels
label_vars <- sapply(dat_raw %>% 
                       select(-all_of(factor_vars)), label)

dat_raw[, nonfactor_equivalent] <- dat_raw[, factor_vars]

dat_raw <- dat_raw %>% 
  select(-all_of(factor_vars))

# Add the labels back
label(dat_raw) <- as.list(label_vars)

# Clean the data
dat_clean <- dat_raw %>% 
  clean_names()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# REMOVE AUTOPSY & QUALITY OF LIFE (FSS/PedsQL)  SECTIONS ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Verbal Autopsy not needed and fss and pedsql (Word of Matt)

# dat_clean <- dat_clean %>%
#   filter(redcap_event_name != "Autopsy")

# Identify which row each form starts/ends
# Store form index and and colnames into a data frame
form_index <- data.frame(
  EndIndex = grep("complete", colnames(dat_clean)),
  ColName = grep("complete", colnames(dat_clean), value = TRUE)) %>% 
  mutate(StartIndex = c(0, EndIndex[-length(EndIndex)]) + 1)
  
# Identify the ranges for the forms you want to remove
remove_forms <- c(#"followup_post_discharge_autopsy_complete", 
                  "fss_and_pedsql_complete", 
                  "fss_and_pedsql_patient_details_complete")

# Find their start and end indices
# remove_indices <- purrr::map2(
#   form_index$StartIndex[form_index$ColName %in% remove_forms],
#   form_index$EndIndex[form_index$ColName %in% remove_forms],
#   ~ .x:.y
# ) %>% unlist()
# 
# colnames(dat_clean)[remove_indices]

# Remove those columns
# dat_clean <- dat_clean[, -remove_indices]

# 
# autopsy_cols <- form_index$StartIndex[form_index$ColName == "followup_post_discharge_autopsy_complete"] :
#   form_index$EndIndex[form_index$ColName == "followup_post_discharge_autopsy_complete"]
# colnames(dat_clean)[autopsy_cols]

fss_pedsql <- form_index$StartIndex[form_index$ColName == "fss_and_pedsql_patient_details_complete"] :
  form_index$EndIndex[form_index$ColName == "fss_and_pedsql_complete"]
colnames(dat_clean)[fss_pedsql]

grep("consent", form_index$ColName, value = TRUE)
unique(form_index$ColName)

consent_cols <- form_index$StartIndex[form_index$ColName == "consent_form_storage_complete"] : 
  form_index$EndIndex[form_index$ColName == "consent_form_storage_complete"]
colnames(dat_clean)[consent_cols]


dat_clean <- dat_clean %>% 
  select(-all_of(c(fss_pedsql, consent_cols)))


# Remove empty columns 
empty_cols <- names(dat_clean)[
  sapply(dat_clean, function(x) all(is.na(x) | x == ""))
]

 constant_cols <- names(dat_clean)[
  sapply(dat_clean, function(x) length(unique(na.omit(x))) <= 1)
]
 
constant_cols

unique(dat_clean$otherstudy_checkbox_adm_6); unique(dat_clean$admitabx_adm_11)
unique(dat_clean$hivquestions_adm_4); unique(dat_clean$respinterv_dis_3)

# dat_clean <- remove_empty(dat_clean, which=c("rows", "cols"), quiet = FALSE)
 # dat_clean <- remove_constant(dat_clean, na.rm = FALSE)

dat_clean <- dat_clean %>% 
  select(-all_of(c(empty_cols, constant_cols)))


## ~~~~~~~~~~~~~~~~~~
## Preserve labels ####
## ~~~~~~~~~~~~~~~~~~

# Subsequent data manipulations may remove labels since tidyverse
#  functions are not compatible with labels and will get overwritten
# Need to make sure to store the labels so we can re-apply them again later
label_vars <- sapply(dat_clean, label)


# HTML Report
# diagnose_web_report(dat_clean) # dlookr
# create_report(dat_clean) # DataExplorer


## ~~~~~~~~~~~~~~~~~~
## NA if unknown ####
## ~~~~~~~~~~~~~~~~~~

# Replace unknown, don't know, doesn't know with NA
# Define columns where 97 = Unknown
Unknown_97 <- c("vaccmeasles_adm",
                "vaccpneumoc_adm",
                "vaccdpt_adm",
                "deliverytype_adm",
                "exclbreastfed_adm",
                "totalbreastfed_adm",
                "priorhealth_adm",
                "momhiv_adm",
                "pddcaregiverpresent_fol",
                "accident_pda",
                "accidentintent_pda")

# Define columns where 97 = Don’t know
Dont_know_97 <- c("illnessduration_pda",
                  "fever_pda",
                  "feverdays_pda",
                  "feveruntildeath_pda",
                  "feverseverity_pda",
                  "stools_pda",
                  "stoolsfreq_pda",
                  "stooluntildeath_pda",
                  "cough_pda",
                  "coughdays_pda",
                  "coughseverity_pda",
                  "breathdiff_pda",
                  "breathdiffdays_pda",
                  "breathfast_pda",
                  "breathfastdays_pda",
                  "indraw_pda",
                  "grunt_pda",
                  "convulsion_pda",
                  "unconscious_pda",
                  "unconshours_pda",
                  "stiffneck_pda",
                  "fontanelle_pda",
                  "skinrash_pda",
                  "skinrashdays_pda",
                  "skinflake_pda",
                  "haircolor_pda",
                  "belly_pda",
                  "anemia_pda",
                  "armpitswell_pda",
                  "bleeding_pda",
                  "skinblack_pda",
                  "causeknown_pda",
                  "certissued_pda",
                  "certavail_pda")

# Define columns where 97 = Unsure
unsure_97 <- c("oxygenavail_adm")

# Combine them into one vector
not_sure <- c(Unknown_97, Dont_know_97, unsure_97)

# Replace 97 or text equivalents ("Dont know", "Unsure", etc.) with NA
dat_clean <- dat_clean %>%
  mutate(across(any_of(not_sure), 
                ~ factor(case_when(
                  . %in% c(97, "97", "Dont know", "Don't know", 
                           "Not sure", "Unsure", "Unknown") ~ NA,
                  TRUE ~ .)
                )))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FILL DOWN NON-FOLLOWUP VARIABLES ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

followup_cols <- grep("_fol", colnames(dat_clean), value = TRUE)

view(subset(dat_clean, studyid_adm == "0002-1A-CH-004"))

dat_clean <- dat_clean %>%
  group_by(studyid_adm) %>%
  fill(-all_of(followup_cols), .direction = "down") %>%
  ungroup()

levels(dat_clean$redcap_event_name)
autopsy_rows <- dat_clean %>% 
  filter(redcap_event_name == "Autopsy")

autopsy_rows %>% 
  select(studyid_adm, ageadmit_adm, studygroup_adm, infection_adm, excludeadmit_adm,
         consenttype_adm, consentobtained_adm, admitdate_adm, attendant_adm, attendantsex_adm) %>% 
  glimpse()
View(autopsy_rows)

adm_rows <- dat_clean %>% 
  filter(redcap_event_name == c("hospitalization and discharge"))
glimpse(adm_rows)

event_rows <- dat_clean %>% 
  filter(redcap_event_name %in% c("2 month discharge", "4 month discharge", "6 month discharge", "12 month discharge", "Autopsy"))

autopsy_rows %>% 
  select(studyid_adm, redcap_event_name, infection_adm, malariastatuspos_adm, death_dis) %>% 
  print(n=Inf)

table(dat_clean$infection_adm); table(autopsy_rows$malariastatuspos_adm)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FILL DOWN AUTOPSY ROWS   ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
followup_cols <- grep("_fol", colnames(dat_clean), value = TRUE)

dat_clean <- dat_clean %>%
  group_by(studyid_adm) %>%
  fill(-all_of(followup_cols), .direction = "up") %>%
  ungroup()


# Then apply the filter
dat_clean <- dat_clean %>% 
  filter(redcap_event_name != "Autopsy")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ONLY FINAL VISIT         ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Quick checks
dat_subset <- dat_clean %>%
  arrange(redcap_event_name) %>% 
  group_by(studyid_adm) %>% 
  slice_tail(n=1)

dat_clean <- dat_subset

# Currently, for yes/no variables, yes is the reference group
# We need to make it so no is the reference

levels(dat_clean$infection_adm) # Yes is the reference group
factor_vars <- lapply(dat_clean, levels)
factor_vars$infection_adm

yesno_vars <- sapply(factor_vars, identical, c("Yes", "No"))
yesno_vars <- names(yesno_vars[yesno_vars])  # keeps only the TRUE elements (columns that are Yes/No)
dat_clean <- dat_clean %>% 
  mutate(across(all_of(yesno_vars), ~relevel(., ref = "No")))





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# SAVE WORKSPACE             ########
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Only keep final datasets
redcap_date <- "2025-10-27"
to_keep <- c("dat_uganda", "dat_rwanda_tz", "dat_raw", "dat_subset", "dat_clean")
rm(list = setdiff(ls(), to_keep))

save.image(paste0("Workspace/03_Create_Cleaned_Data (", redcap_date, ").RData"))

