library(survey)
library(gtsummary)
library(dplyr)
options(survey.lonely.psu = "adjust")
# ---- shared settings ----
tbl_vars <- c("caregiving_clean", "healthydays_mental", "healthydays_physical",
              "age", "gender", "race_white_bin", "marital_binary",
              "education_cat", "income_cont", "employment_binary",
              "gen_health_15", "chronic_count")

stat_list <- list(all_continuous()  ~ "{mean} ({sd})",
                  all_categorical() ~ "{n} ({p}%)")

brfss <- brfss %>%
  dplyr::select(all_of(c(tbl_vars, "_PSU", "_STSTR", "finalwt"))) %>%
  mutate(complete_case = complete.cases(dplyr::select(., all_of(tbl_vars))))

brfss <- brfss %>%
  mutate(caregiving_clean = relevel(caregiving_clean, ref = "Non-caregiver")) %>%
  mutate(gender = relevel(gender, ref = "Male")) %>%
  mutate(employment_binary = relevel(employment_binary, ref = "Unemployed")) %>%
  mutate(education_cat = relevel(education_cat, ref = "College graduate"))

# ============================================================
# 6) Variable labels (clean names for Table 1 + regressions)
# IMPORTANT: label_list must be a NAMED LIST (not a named vector)
# ============================================================
var_label_map <- tibble::tibble(
  variable = c(
    "caregiving_clean", "healthydays_mental", "healthydays_physical", "age", "gender", "race_white_bin", 
    "marital_binary",
    "education_cat","income_cont", "employment_binary",  "gen_health_15","chronic_count"
  ),
  label = c(
    "Caregiving status","Mentally unhealthy days","Physically unhealthy days",
    "Age (years)","Gender","Race","Marital status","Education (highest level)",
    "Annual household income",
    "Employment status",
    "Self-rated general health",
    "Chronic condition count"
  )
)

label_list <- as.list(var_label_map$label)
names(label_list) <- var_label_map$variable

# ---- BRFSS: WEIGHTED ----
# design built on full caregiver-module rows, subset at analysis time

table(brfss$`_STSTR`,useNA = "ifany")
brfss_svy   <- svydesign(ids = ~1, strata = ~`_STSTR`, weights = ~finalwt,
                         data = brfss)      

brfss_svy50 <- subset(brfss_svy, age >= 50 & complete_case)

table(brfss$caregiving_clean,useNA = "ifany")
svy_all <- subset(brfss_svy, !is.na(caregiving_clean) & age >= 50)
sum(weights(svy_all))
sum(weights(brfss_svy50))          # should be ~58 million
nrow(brfss_svy50$variables)        # should be ~117,922 wait — check this

brfss_svy$variables %>%
  filter(!is.na(caregiving_clean), age >= 50) %>%
  summarise(across(all_of(tbl_vars), ~sum(is.na(.)))) %>%
  t()

setdiff(tbl_vars, names(label_list))   # variables with no label — should be empty
setdiff(names(label_list), tbl_vars)   # labels with no variable — "source" will show here


# ---- NSGA: UNWEIGHTED ----
dim(dat)
nsga_analytic <- dat %>%
  dplyr::select(all_of(tbl_vars)) %>%
  mutate(complete_case = complete.cases(dplyr::select(., all_of(tbl_vars)))) %>%
  filter(age >= 50 & complete_case) %>%
  mutate(caregiving_clean = relevel(caregiving_clean, ref = "Non-caregiver")) %>%
  mutate(gender = relevel(gender, ref = "Male")) %>%
  mutate(employment_binary = relevel(employment_binary, ref = "Unemployed")) %>%
  mutate(education_cat = relevel(education_cat, ref = "College graduate"))
  
dim(nsga_analytic)

brfss_clean <- brfss %>% 
  filter(age >= 50 & complete_case)%>%
  mutate(source = "BRFSS") 
nsga_analytic <- nsga_analytic %>% 
  filter(age >= 50 & complete_case)%>%
  mutate(source = "NSGA") 

combined_unweight <- bind_rows(nsga_analytic,brfss_clean)
