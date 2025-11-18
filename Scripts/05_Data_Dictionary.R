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

comment_vars <- dat_clean %>%
  select(grep("_complete", colnames(dat_clean), value = TRUE))

exclude <- c("studyid_adm",
             "creationdate_adm",
             "uploaddate_adm",
             "appversion_adm",
             "is_pilot_adm",
             "username_adm",
             "nursename_adm",
             "noteligible_adm",
             "nursenameother_adm",
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

# Create a temporary file path to save the HTML report
output_file <- "Results/Data_Dictionary.html"

# Save the mom data dictionary as an HTML file
print(dat_codebook, method = "browser", file = output_file)
cat("Data dictionary saved to:", output_file, "\n")

