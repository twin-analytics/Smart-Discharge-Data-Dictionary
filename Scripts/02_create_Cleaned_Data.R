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

# Core set of libraries for data manipulation
library(tidyverse) # For data manipulaton
library(expss)     # For data lableling
library(janitor)   # Data cleaning

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

unique(dat_raw$Event.Name)
sort(unique(dat_raw$Event.Name))

# Verbal Autopsy not needed(Word of Matt)
dat_raw <- dat_raw %>%
  filter(!Event.Name %in% c("Autopsy", "then please try to reload this page.</span></p>"))


# Clean the data
dat_clean_raw <- dat_raw %>% 
  clean_names()

empty_cols <- names(dat_clean_raw)[
  sapply(dat_clean_raw, function(x) all(is.na(x) | x == ""))
]

constant_cols <- names(dat_clean_raw)[
  sapply(dat_clean_raw, function(x) length(unique(na.omit(x))) == 1)
]

# dat_clean_raw <- remove_empty(dat_clean_raw, which=c("rows", "cols"), quiet = FALSE)
# dat_clean_raw <- remove_constant(dat_clean_raw, na.rm = FALSE, quiet = FALSE)

dat_clean_raw <- dat_clean_raw %>% 
  select(-all_of(c(empty_cols, constant_cols)))

diagnose_web_report(dat_clean_raw)

