




tbl_vars_noinc <- c("caregiving_clean", "healthydays_mental", "healthydays_physical",
              "age", "gender", "race_white_bin", "marital_binary",
              "education_cat", "employment_binary",
              "gen_health_15", "chronic_count")


dat_wo_income <- brfss %>%
  dplyr::select(all_of(tbl_vars_noinc)) %>%
  mutate(complete_case = complete.cases(dplyr::select(., all_of(tbl_vars_noinc)))) %>%
  filter(age >= 50 & complete_case) %>%
  mutate(caregiving_clean = relevel(caregiving_clean, ref = "Non-caregiver")) %>%
  mutate(gender = relevel(gender, ref = "Male")) %>%
  mutate(employment_binary = relevel(employment_binary, ref = "Unemployed")) %>%
  mutate(education_cat = relevel(education_cat, ref = "College graduate"))

nsga_wo_income <- MASS::glm.nb(healthydays_mental ~ caregiving_clean + age + gender +
                             race_white_bin + marital_binary + education_cat +
                             employment_binary + chronic_count,
                           data = dat_wo_income)

nsga_wo_income_exp<- gtsummary::tbl_regression(nsga_wo_income, exponentiate = TRUE, label = label_list) %>% 
  gtsummary::bold_labels()
nsga_wo_income_exp

tbl_vars <- c("caregiving_clean", "healthydays_mental", "healthydays_physical",
              "age", "gender", "race_white_bin", "marital_binary",
              "education_cat", "income_cont", "employment_binary",
              "gen_health_15", "chronic_count")
dat_w_income <- brfss %>%
  dplyr::select(all_of(tbl_vars)) %>%
  mutate(complete_case = complete.cases(dplyr::select(., all_of(tbl_vars)))) %>%
  filter(age >= 50 & complete_case) %>%
  mutate(caregiving_clean = relevel(caregiving_clean, ref = "Non-caregiver")) %>%
  mutate(gender = relevel(gender, ref = "Male")) %>%
  mutate(employment_binary = relevel(employment_binary, ref = "Unemployed")) %>%
  mutate(education_cat = relevel(education_cat, ref = "College graduate"))

nsga_w_income <- MASS::glm.nb(healthydays_mental ~ caregiving_clean + age + gender +
                                 race_white_bin + marital_binary + education_cat +
                                 employment_binary + chronic_count,
                               data = dat_w_income)

nsga_w_income_exp<- gtsummary::tbl_regression(nsga_w_income, exponentiate = TRUE, label = label_list) %>% 
  gtsummary::bold_labels()
nsga_w_income_exp

nrow(dat_w_income)
nrow(dat_wo_income)

table(dat_w_income$caregiving_clean)

#NSGA missing rate
dat %>%
  dplyr::select(all_of( c("caregiving_clean","age"))) %>%
  mutate(complete_case = complete.cases(dplyr::select(., all_of(c("caregiving_clean","age"))))) %>%
  filter(age >= 50 & complete_case) %>%
  summarise(
    n = n()
  )  
dat %>%
  dplyr::select(all_of( c("caregiving_clean","income_cont","age"))) %>%
  mutate(complete_case = complete.cases(dplyr::select(., all_of(c("caregiving_clean","income_cont","age"))))) %>%
  filter(age >= 50 & complete_case) %>%
  summarise(
    n = n()
  )  
4616/5300

#BRFSS missing rate

brfss%>%filter(!is.na(caregiving_clean) & age >=50 & !is.na(income_cont))%>%
                summarise(
                  n = n()
                )
brfss %>%
  dplyr::select(all_of( c("caregiving_clean","income_cont","age"))) %>%
  mutate(complete_case = complete.cases(dplyr::select(., all_of(c("caregiving_clean","income_cont","age"))))) %>%
  filter(age >= 50 & complete_case) %>%
  summarise(
    n = n()
  )  

brfss %>%
  dplyr::select(all_of( c("caregiving_clean","age"))) %>%
  mutate(complete_case = complete.cases(dplyr::select(., all_of(c("caregiving_clean","age"))))) %>%
  filter(age >= 50 & complete_case) %>%
  summarise(
    n = n()
  )
124483/156449


