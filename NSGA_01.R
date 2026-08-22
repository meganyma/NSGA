#---
#title: "NSGA 02032026"
#author: "Chibuzo Obasi"
#date: "2026-02-03"
#output: html_document
#editor_options: 
#  chunk_output_type: console
#---
R.version.string
getwd()
setwd("/Users/baofuma/NSGA")

knitr::opts_chunk$set(echo = TRUE)

# ============================================================
# 0) Install + load packages (install only once)
# ============================================================

pkgs <- c("tidyverse","janitor","gtsummary","MASS","ordinal","flextable","officer")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(janitor)
library(gtsummary)
library(MASS)      # glm.nb
library(ordinal)   # clm
library(flextable)
library(officer)

############################################################
# FINAL COMPLETE R SCRIPT (START → FINISH)
# Dataset: COVID8_22_22.csv
# Outputs: Table 1 + Regression tables exported to Word
############################################################

# ============================================================
# 0) Packages (install if missing)
# ============================================================
pkgs <- c("tidyverse", "janitor", "gtsummary", "MASS", "ordinal", "flextable", "officer")

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(janitor)
library(gtsummary)
library(MASS)       # glm.nb
library(ordinal)    # clm
library(flextable)
library(officer)

############################################################
# FINAL 
############################################################

# ============================================================
# 0) Packages (install if missing)
# ============================================================
pkgs <- c("tidyverse", "janitor", "gtsummary", "MASS", "ordinal", "flextable", "officer")

to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(to_install) > 0) install.packages(to_install)

library(tidyverse)
library(janitor)
library(gtsummary)
library(MASS)       # glm.nb
library(ordinal)    # clm
library(flextable)
library(officer)

# ============================================================
# 1) Load data + clean names
# ============================================================
dat <- read.csv("NSGA_COVID8_22_22.csv", stringsAsFactors = FALSE) %>%
  clean_names() %>%
  mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
  mutate(across(where(is.character), ~ if_else(.x == "Unknown", NA_character_, .x))) %>%
  mutate(across(where(is.numeric), ~ ifelse(is.finite(.x), .x, NA_real_)))

# ============================================================
# 2) Remove/handle Unknown/DK + basic range cleaning
# ============================================================

# caregiving: keep only 1=Yes, 2=No; others set to NA
dat <- dat %>%
  mutate(
    caregiving = if_else(caregiving %in% c(1, 2), caregiving, NA_real_),
    caregivingimpact = if_else(caregivingimpact %in% c(1, 2), caregivingimpact, NA_real_)
    
  )
table(dat$caregiving, useNA = "ifany")  
table(dat$caregiving, dat$caregivingimpact, useNA = "ifany")
# Age continuous and bounded
dat <- dat %>%
  mutate(
    age = as.numeric(age),
    age = if_else(age >= 50 & age <= 120, age, NA_real_)
  )
table(dat$age,  useNA = "ifany")
# Healthy days outcomes: 0–30
dat <- dat %>%
  mutate(
    healthydays_physical = if_else(healthydays_physical >= 0 & healthydays_physical <= 30,
                                   healthydays_physical, NA_real_),
    healthydays_mental   = if_else(healthydays_mental >= 0 & healthydays_mental <= 30,
                                   healthydays_mental, NA_real_)
  )
table(dat$caregiving, dat$healthydays_physical, useNA = "ifany")
# Exercise days: 0–7
dat <- dat %>%
  mutate(
    exercise_days_07 = if_else(exercise_days >= 0 & exercise_days <= 7, exercise_days, NA_real_)
  )
table(dat$gender, useNA = "ifany")
dat <- dat %>%
  mutate(
    gender = factor(
      if_else(gender %in% c("Male", "Female"), gender, NA_character_),
      levels = c("Male", "Female")
    )
  )
table(dat$gender, useNA = "ifany")
# ============================================================
# 3) Key recodes (Caregiving, Race, Marital, Employment, Income)
# ============================================================

# Caregiving status
dat <- dat %>%
  mutate(
    caregiving_clean = factor(
      caregiving,
      levels = c(1, 2),
      labels = c("Caregiver", "Non-caregiver")
    )
  )

# Race: White vs Non-White (based on race_white 0/1)
dat <- dat %>%
  mutate(
    race_white_bin = case_when(
      race_white == 1 ~ "White",
      race_white == 0 ~ "Non-White",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("White", "Non-White"))
  )

# Marital status: Married/Partnered vs Not married
dat <- dat %>%
  mutate(
    marital_binary = case_when(
      maritalstatus_currentmaritalstatus %in% c(
        "CurrentMaritalStatus_Married",
        "CurrentMaritalStatus_LivingWithPartner"
      ) ~ "Married/Partnered",
      maritalstatus_currentmaritalstatus %in% c(
        "CurrentMaritalStatus_Divorced",
        "CurrentMaritalStatus_NeverMarried",
        "CurrentMaritalStatus_Separated",
        "CurrentMaritalStatus_Widowed",
        "CurrentMaritalStatus_Other"
      ) ~ "Not married",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Married/Partnered", "Not married"))
  )

# Employment: Employed vs Unemployed
# If labels differ, run: table(dat$employment, useNA="ifany") and adjust.
dat <- dat %>%
  mutate(
    employment_binary = case_when(
      employment %in% c(
        "EmploymentStatus_EmployedFulltime",
        "EmploymentStatus_EmployedParttime",
        "EmploymentStatus_Other"
      ) ~ "Employed",
      employment %in% c(
        "EmploymentStatus_Retired",
        "EmploymentStatus_Unemployed"
      ) ~ "Unemployed",
      TRUE ~ NA_character_
    ) %>% factor(levels = c("Employed", "Unemployed"))
  )

# Income: income_cont 1–7 (used in BOTH Table 1 and regressions)
# Category meanings can be explained in Results/Discussion.
# If labels differ, run: table(dat$income_annualincome, useNA="ifany") and adjust.
dat <- dat %>%
  mutate(
    income_cont = case_when(
      income_annualincome == "AnnualIncome_less35k"   ~ 1,
      income_annualincome == "AnnualIncome_35k50k"    ~ 2,
      income_annualincome == "AnnualIncome_50k75k"    ~ 3,
      income_annualincome == "AnnualIncome_75k100k"   ~ 4,
      income_annualincome == "AnnualIncome_100k150k"  ~ 5,
      income_annualincome == "AnnualIncome_150k200k"  ~ 6,
      income_annualincome == "AnnualIncome_more200k"  ~ 7,
      TRUE ~ NA_real_
    )
  )
dat <- dat %>%
  mutate(
    education_cat = case_when(
      educationlevel_highestgrade %in% c(1,2) ~ "High school and less",
      educationlevel_highestgrade == 3        ~ "Some college",
      educationlevel_highestgrade %in% c(4, 5)         ~ "College graduate",
      TRUE               ~ NA_character_
    ) %>% factor(levels = c("High school and less",
                            "Some college",
                            "College graduate"))
  )

dat <- dat %>%
  mutate(
    
    # count of conditions (0–7)
    chronic_count = rowSums(
      across(c(cancer_history, chronicdisease_history, chroniclung_history, diabetes_history,
               heartcondition_history,highbp_history, stroke_history)),
      na.rm = TRUE
    )
  )
table(dat$chronic_count, useNA = "ifany")
# ============================================================
# 4) Recode health/QoL/social variables to 1–5 (1=Poor, 5=Excellent)
# ============================================================

dat <- dat %>%
  mutate(
    gen_health_15 = case_when(
      overallhealth_generalhealth %in% c("Poor", "GeneralHealth_Poor", 1) ~ 1,
      overallhealth_generalhealth %in% c("Fair", "GeneralHealth_Fair", 2) ~ 2,
      overallhealth_generalhealth %in% c("Good", "GeneralHealth_Good", 3) ~ 3,
      overallhealth_generalhealth %in% c("Very good","Very Good","VeryGood","GeneralHealth_VeryGood", 4) ~ 4,
      overallhealth_generalhealth %in% c("Excellent","GeneralHealth_Excellent", 5) ~ 5,
      TRUE ~ NA_real_
    ),
    quality_of_life_15 = case_when(
      overallhealth_generalquality %in% c("Poor", "GeneralQuality_Poor", 1) ~ 1,
      overallhealth_generalquality %in% c("Fair", "GeneralQuality_Fair", 2) ~ 2,
      overallhealth_generalquality %in% c("Good", "GeneralQuality_Good", 3) ~ 3,
      overallhealth_generalquality %in% c("Very good","Very Good","VeryGood","GeneralQuality_VeryGood", 4) ~ 4,
      overallhealth_generalquality %in% c("Excellent","GeneralQuality_Excellent", 5) ~ 5,
      TRUE ~ NA_real_
    ),
    phys_health_15 = case_when(
      overallhealth_generalphysicalhealth %in% c("Poor", "GeneralPhysicalHealth_Poor", 1) ~ 1,
      overallhealth_generalphysicalhealth %in% c("Fair", "GeneralPhysicalHealth_Fair", 2) ~ 2,
      overallhealth_generalphysicalhealth %in% c("Good", "GeneralPhysicalHealth_Good", 3) ~ 3,
      overallhealth_generalphysicalhealth %in% c("Very good","Very Good","VeryGood","GeneralPhysicalHealth_VeryGood", 4) ~ 4,
      overallhealth_generalphysicalhealth %in% c("Excellent","GeneralPhysicalHealth_Excellent", 5) ~ 5,
      TRUE ~ NA_real_
    ),
    mental_health_15 = case_when(
      overallhealth_generalmentalhealth %in% c("Poor", "GeneralMentalHealth_Poor", 1) ~ 1,
      overallhealth_generalmentalhealth %in% c("Fair", "GeneralMentalHealth_Fair", 2) ~ 2,
      overallhealth_generalmentalhealth %in% c("Good", "GeneralMentalHealth_Good", 3) ~ 3,
      overallhealth_generalmentalhealth %in% c("Very good","Very Good","VeryGood","GeneralMentalHealth_VeryGood", 4) ~ 4,
      overallhealth_generalmentalhealth %in% c("Excellent","GeneralMentalHealth_Excellent", 5) ~ 5,
      TRUE ~ NA_real_
    ),
    social_sat1_15 = case_when(
      overallhealth_socialsatisfaction %in% c("Poor","SocialSatisfaction_Poor", 1) ~ 1,
      overallhealth_socialsatisfaction %in% c("Fair","SocialSatisfaction_Fair", 2) ~ 2,
      overallhealth_socialsatisfaction %in% c("Good","SocialSatisfaction_Good", 3) ~ 3,
      overallhealth_socialsatisfaction %in% c("Very good","Very Good","VeryGood","SocialSatisfaction_VeryGood", 4) ~ 4,
      overallhealth_socialsatisfaction %in% c("Excellent","SocialSatisfaction_Excellent", 5) ~ 5,
      TRUE ~ NA_real_
    ),
    social_sat2_15 = case_when(
      overallhealth_socialsatisfaction_2 %in% c("Poor","SocialSatisfaction_Poor", 1) ~ 1,
      overallhealth_socialsatisfaction_2 %in% c("Fair","SocialSatisfaction_Fair", 2) ~ 2,
      overallhealth_socialsatisfaction_2 %in% c("Good","SocialSatisfaction_Good", 3) ~ 3,
      overallhealth_socialsatisfaction_2 %in% c("Very good","Very Good","VeryGood","SocialSatisfaction_VeryGood", 4) ~ 4,
      overallhealth_socialsatisfaction_2 %in% c("Excellent","SocialSatisfaction_Excellent", 5) ~ 5,
      TRUE ~ NA_real_
    ),
    
    # PROMIS loneliness: assume numeric 1–5 (higher=worse), reverse so higher=better
    promis_soc_15 = case_when(
      promis_soc261_e8ee76 %in% c(1,2,3,4,5) ~ 6 - as.numeric(promis_soc261_e8ee76),
      TRUE ~ NA_real_
    )
  )
dim(dat)


dat %>%
  filter(!is.na(caregiving_clean), age >= 50,!is.na(exercise_days_07)) %>%
  group_by(caregiving_clean) %>%
  summarise(
    mean_days = mean(exercise_days_07, na.rm = TRUE),
    sd = sd(exercise_days, na.rm = TRUE),
    n = n()
  )

dat %>%
  filter(!is.na(caregiving_clean), age >= 50) %>%
  summarise(
    mean_days = mean(exercise_days_07, na.rm = TRUE),
    sd = sd(exercise_days, na.rm = TRUE),
    n = n()
  )

tbl_vars <- c("caregiving_clean", "healthydays_mental", "healthydays_physical",
              "age", "gender", "race_white_bin", "marital_binary",
              "education_cat", "income_cont","employment_binary",
              "gen_health_15", "chronic_count")
