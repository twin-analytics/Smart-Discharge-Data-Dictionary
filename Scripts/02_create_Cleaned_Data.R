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
library(tidyverse) # For data manipulaton
library(expss)     # For data lableling
library(janitor)   # Data cleaning
library(here)      # file path

# For Exploratory Data Analysis
library(DataExplorer)
library(SmartEDA)
library(dlookr)

# ~~~~~~~~~~~~~~~~
# LOAD DATA  #####
# ~~~~~~~~~~~~~~~~

source("Scripts/01_Redcap_Labels.r")

# Rename the file
dat_raw <- data

unique(dat_raw$redcap_event_name)
sort(unique(dat_raw$redcap_event_name))

dim(dat_raw)
glimpse(dat_raw)


## ~~~~~~~~~~~~~~~~~~~
## Replace factors cols ####
## ~~~~~~~~~~~~~~~~~~~

# REDCap automatically creates factor versions of each variable
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

dat_clean <- dat_raw %>% 
  select(-all_of(factor_vars))

# Clean the data
dat_clean <- dat_clean %>% 
  clean_names()

# Verbal Autopsy not needed(Word of Matt)
dat_clean <- dat_clean %>%
  filter(redcap_event_name != "Autopsy")

empty_cols <- names(dat_clean)[
  sapply(dat_clean, function(x) all(is.na(x) | x == ""))
]

# constant_cols <- names(dat_clean)[
#   sapply(dat_clean, function(x) length(unique(na.omit(x))) == 1)
# ]

# dat_clean_raw <- remove_empty(dat_clean_raw, which=c("rows", "cols"), quiet = FALSE)
# dat_clean_raw <- remove_constant(dat_clean_raw, na.rm = FALSE, quiet = FALSE)

dat_clean <- dat_clean %>% 
  select(-all_of(c(empty_cols)))


## ~~~~~~~~~~~~~~~~~~
## Preserve labels ####
## ~~~~~~~~~~~~~~~~~~

# Subsequent data manipulations may remove labels since tidyverse
#  functions are not compatible with labels and will get overwritten
# Need to make sure to store the labels so we can re-apply them again later
label_vars <- sapply(dat_clean, label)


# HTML Report
diagnose_web_report(dat_clean) # dlookr
create_report(dat_clean) # DataExplorer


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
                ~ dplyr::case_when(
                  . %in% c(97, "97", "Dont know", "Don't know", 
                           "Not sure", "Unsure", "Unknown") ~ NA,
                  TRUE ~ .
                )))

