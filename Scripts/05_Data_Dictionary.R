# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Install and load the required packages               ####
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if(!require(haven)){
  install.packages("haven", dependencies = TRUE)
  library(haven)
}

if(!require(tidyverse)){
  install.packages("tidyverse", dependencies = TRUE)
  library(tidyverse)
}

if(!require(summarytools)){
  install.packages("summarytools", dependencies = TRUE)
  library(summarytools)
}

library(skimr)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import Datasets               ##### 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Source the cleaned script and the minimal selection criteria
source("Scripts/03_create_Cleaned_Data.R")
source("Scripts/04_Selection_Criteria.R")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DATA DICTIONARY         ######
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Identify variables that are not relevant to exclude from the data dictionary e.g
# redundant, administrative, identifying information, exclusion criteria variables

# consent_vars <- grep("consent_", colnames(dat_clean), value = TRUE)

# comment_vars <- dat_clean %>%
#   select(grep("_complete", colnames(dat_clean), value = TRUE))

exclude <- c("studyid_adm",
             "studyid_ref",
             "studyidcheck_ref",
             "studyid_dama",
             "studyidcheck_dama",
             "studyid_fol",
             "studyidcheck_fol",
             "studyid_pda",
             "studyidcheck_pda",
             "creationdate_adm",
             "uploaddate_adm",
             "appversion_adm",
             "is_pilot_adm",
             "username_adm",
             "nursename_adm",
             "noteligible_adm",
             "nursenameother_adm",
             "nursenameother_dis",
             "nursename_dis",
              grep("phone", colnames(dat_clean), value = TRUE),
              grep("comment", colnames(dat_clean), value = TRUE),
              grep("_complete", colnames(dat_clean), value = TRUE)
)

dat_clean <- dat_clean %>% 
  select(-any_of(exclude))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
# Create Data Dictionary       ######
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Data dictionary for mom dataset
cat("Creating data dictionary ...\n")
dat_codebook <- dfSummary(dat_clean, graph.magnif = 0.5)

# HTML Report
# Create a temporary file path to save the HTML report
output_file <- "Results/Data_Dictionary.html"
print(dat_codebook, method = "browser", file = output_file)
cat("Data dictionary saved to:", output_file, "\n")

# PDF Report
output_file_pdf <- "Results/Data_Dictionary.pdf"
pagedown::chrome_print(
  input  = output_file_html,
  output = output_file_pdf
)
cat("PDF data dictionary saved to:", output_file_pdf, "\n")


# ~~~~~~~~~~~~~~~~~~~
# Quick Checks   ####
# ~~~~~~~~~~~~~~~~~~~

missing_summary <- dat_clean %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(cols = everything(),
               names_to = "variable",
               values_to = "n_missing") %>%
  mutate(
    n_total = nrow(dat_clean),
    pct_missing = 100 * n_missing / n_total
  ) %>%
  arrange(desc(pct_missing))

head(missing_summary, 20)

print(missing_summary, n=Inf)


site_ids <- dat_clean %>%
  filter(is.na(site_adm))
nrow(site_ids)

missing_consent <- dat_clean %>% 
  filter(is.na(consenttype_adm)) %>% 
  select(studyid_adm, site_adm)
missing_consent

missing_consentobtained <- dat_clean %>% 
  filter(is.na(consentobtained_adm)) %>% 
  select(studyid_adm, site_adm)
missing_consentobtained

View(dat_clean %>% 
    filter(studyid_adm %in% c("0002-2E-CH-004", "0002-9L-PH-071")))

missing_sex <- dat_clean %>% 
  filter(is.na(sex_adm)) %>% 
  select(studyid_adm, site_adm)
missing_sex

missing_referral <- dat_clean %>% 
  filter(is.na(isreferral_adm)) %>% 
  select(studyid_adm, site_adm)
missing_referral

missing_momalive <- dat_clean %>% 
  filter(is.na(momalive_adm)) %>% 
  select(studyid_adm, site_adm)
missing_momalive

missing_momage <- dat_clean %>% 
  filter(is.na(momage_adm)) %>% 
  select(studyid_adm, site_adm)
missing_momage


# Check labels
label(dat_clean$exclbreastfed_adm)


dat_clean2 <- dat_clean
dat_clean2 <- remove_labels(dat_clean2)
skimr::skim(data = dat_clean2)
skim_without_charts(data = dat_clean2)
