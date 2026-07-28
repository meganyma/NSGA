#---------
#dat <- read.csv("brfss_data_040326.csv", stringsAsFactors = FALSE) %>%
#  clean_names() %>%
#  mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
#  mutate(across(where(is.character), ~ if_else(.x == "Unknown", NA_character_, .x))) %>%
#  mutate(across(where(is.numeric), ~ ifelse(is.finite(.x), .x, NA_real_)))
------------


library(haven)
brfss0 <- read_xpt("LLCP2021.XPT")

brfss0 <- brfss0 %>%
  dplyr::select(CAREGIV1,`_AGE80`, MENTHLTH, PHYSHLTH, `_TOTINDA`, `_RACE`,MARITAL,EMPLOY1,INCOME3,EDUCA,GENHLTH,
                DIABETE4,CHCSCNCR,BPHIGH6,`_MICHD`,CHCOCNCR,CVDSTRK3,
                CHCCOPD3,CHCKDNY2,SEXVAR,
                `_PSU`,`_STSTR`,`_LLCPWT`,`_STATE`)
brfss0 %>% filter(!is.na(CAREGIV1)) %>% count(`_STATE`) %>% print(n = 60)
brfss0 %>% filter(!is.na(CAREGIV1)) %>% count(CAREGIV1) %>% print(n = 60)

brfss1 <- read_xpt("LLCP21V1.XPT")
brfss1 <- brfss1 %>%
  dplyr::select(CAREGIV1,`_AGE80`, MENTHLTH, PHYSHLTH, `_TOTINDA`, `_RACE`,MARITAL,EMPLOY1,INCOME3,EDUCA,GENHLTH,
                DIABETE4,CHCSCNCR,BPHIGH6,`_MICHD`,CHCOCNCR,CVDSTRK3,
                CHCCOPD3,CHCKDNY2,SEXVAR,
                `_PSU`,`_STSTR`,`_LCPWTV1`,`_STATE`)
brfss1 %>% filter(!is.na(CAREGIV1)) %>% count(`_STATE`) %>% print(n = 60)
brfss1 %>%
  filter(`_STATE` == 39, !is.na(CAREGIV1)) %>%
  count(CAREGIV1)

brfss2 <- read_xpt("LLCP21V2.XPT")
brfss2 <- brfss2 %>%
  dplyr::select(CAREGIV1,`_AGE80`, MENTHLTH, PHYSHLTH, `_TOTINDA`, `_RACE`,MARITAL,EMPLOY1,INCOME3,EDUCA,GENHLTH,
                DIABETE4,CHCSCNCR,BPHIGH6,`_MICHD`,CHCOCNCR,CVDSTRK3,
                CHCCOPD3,CHCKDNY2,SEXVAR,
                `_PSU`,`_STSTR`,`_LCPWTV2`,`_STATE`)
brfss2 %>% filter(!is.na(CAREGIV1)) %>% count(`_STATE`) %>% print(n = 60)
brfss2 %>% filter(`_STATE` == 34, !is.na(CAREGIV1)) %>% count(CAREGIV1)

# NJ caregiver responses in the common file
brfss0 %>% filter(`_STATE` == 34, CAREGIV1 %in% 1:2) %>% nrow()
# NJ caregiver responses in V2
brfss2 %>% filter(`_STATE` == 34, CAREGIV1 %in% 1:2) %>% nrow()

s_common <- brfss0 %>% filter(CAREGIV1 %in% 1:2) %>% distinct(`_STATE`) %>% pull()
s_v1     <- brfss1 %>% filter(CAREGIV1 %in% 1:2) %>% distinct(`_STATE`) %>% pull()
s_v2     <- brfss2 %>% filter(CAREGIV1 %in% 1:2) %>% distinct(`_STATE`) %>% pull()

intersect(s_common, s_v1)   # expect empty
intersect(s_common, s_v2)   # New Jersey may appear here
intersect(s_v1, s_v2)       # expect empty

# Ohio: common vs V1
n_oh_common <- brfss0  %>% filter(`_STATE` == 39, CAREGIV1 %in% 1:2) %>% nrow()
n_oh_v1     <- brfss1 %>% filter(`_STATE` == 39, CAREGIV1 %in% 1:2) %>% nrow()

# New Jersey: common vs V2
n_nj_common <- brfss0  %>% filter(`_STATE` == 34, CAREGIV1 %in% 1:2) %>% nrow()
n_nj_v2     <- brfss2 %>% filter(`_STATE` == 34, CAREGIV1 %in% 1:2) %>% nrow()

c(oh_common = n_oh_common, oh_v1 = n_oh_v1, nj_common = n_nj_common, nj_v2 = n_nj_v2)

p_oh_common <- n_oh_common / (n_oh_common + n_oh_v1)
p_oh_v1     <- n_oh_v1     / (n_oh_common + n_oh_v1)
p_nj_common <- n_nj_common / (n_nj_common + n_nj_v2)
p_nj_v2     <- n_nj_v2     / (n_nj_common + n_nj_v2)

common <- brfss0 %>%
  mutate(finalwt = case_when(
    `_STATE` == 39 ~ `_LLCPWT` * p_oh_common,
    `_STATE` == 34 ~ `_LLCPWT` * p_nj_common,
    TRUE           ~ `_LLCPWT`
  ))
v1 <- brfss1 %>%
  mutate(finalwt = if_else(`_STATE` == 39, `_LCPWTV1` * p_oh_v1, `_LCPWTV1`))

v2 <- brfss2 %>%
  mutate(finalwt = if_else(`_STATE` == 34, `_LCPWTV2` * p_nj_v2, `_LCPWTV2`))

brfss <- bind_rows(common, v1, v2)
brfss %>% count(CAREGIV1)
# ============================================================
# 2) Remove/handle Unknown/DK + basic range cleaning
# ============================================================

# caregiving: keep only 1=Yes, 2=No; others set to NA
brfss <- brfss %>%
  mutate(caregiving = if_else(CAREGIV1 %in% c(1, 2), CAREGIV1, NA_real_))

table(brfss$caregiving)
brfss <- brfss %>%
  mutate(
    age = as.numeric(`_AGE80`),
    age = if_else(age >= 50 & age <= 120, age, NA_real_)
  )

table(brfss$age,useNA = "ifany")

table(brfss$MENTHLTH)
brfss <- brfss %>%
  mutate(
    healthydays_physical = case_when(
      PHYSHLTH == 88            ~ 0,
      PHYSHLTH >= 1 & PHYSHLTH <= 30 ~ PHYSHLTH,
      TRUE                      ~ NA_real_    # 77, 99, blank
    ),
    healthydays_mental = case_when(
      MENTHLTH == 88            ~ 0,
      MENTHLTH >= 1 & MENTHLTH <= 30 ~ MENTHLTH,
      TRUE                      ~ NA_real_
    )
  )
table(brfss$healthydays_mental)
#
brfss <- brfss %>%
  mutate(pa_level = case_when(
    `_TOTINDA` == 1 ~ 1,
    `_TOTINDA` == 2 ~ 0,
    TRUE            ~ NA_real_
  ))
table(brfss$`_TOTINDA`, brfss$pa_level, useNA = "ifany")
# ============================================================
# 3) Key recodes (Caregiving, Race, Marital, Employment, Income)
# ============================================================
brfss <- brfss %>%
  mutate(
    caregiving_clean = factor(
      caregiving,
      levels = c(1, 2),
      labels = c("Caregiver", "Non-caregiver")
    ),
    caregiving_clean = relevel(caregiving_clean, ref = "Non-caregiver")
  )
table(brfss$caregiving, brfss$caregiving_clean, useNA = "ifany")

# Race: White vs Non-White (based on race_white 0/1)
brfss <- brfss %>%
  mutate(
    race_white_bin = case_when(
      `_RACE` == 1              ~ "White",          # White non-Hispanic
      `_RACE` %in% 2:8          ~ "Non-White",      # all other categories incl. Hispanic
      TRUE                      ~ NA_character_      # 9 / blank
    ) %>% factor(levels = c("White", "Non-White"))
  )
table(brfss$`_RACE`, brfss$race_white_bin, useNA = "ifany")


# Marital status: Married/Partnered vs Not married
brfss <- brfss %>%
  mutate(
    marital_binary = case_when(
      MARITAL %in% c(1, 6)        ~ "Married/Partnered",   # Married, unmarried couple
      MARITAL %in% c(2, 3, 4, 5)  ~ "Not married",         # Divorced, Widowed, Separated, Never married
      TRUE                        ~ NA_character_           # 9 / blank
    ) %>% factor(levels = c("Married/Partnered", "Not married"))
  )
table(brfss$MARITAL, brfss$marital_binary, useNA = "ifany")
# Employment: Employed vs Unemployed
brfss <- brfss %>%
  mutate(
    employment_binary = case_when(
      EMPLOY1 %in% c(1, 2)              ~ "Employed",     # wages, self-employed
      EMPLOY1 %in% c(3, 4, 5, 6, 7, 8)  ~ "Unemployed",   # out of work, homemaker, student, retired, unable
      TRUE                              ~ NA_character_    # 9 / blank
    ) %>% factor(levels = c("Employed", "Unemployed"))
  )
table(brfss$EMPLOY1, brfss$employment_binary, useNA = "ifany")

brfss <- brfss %>%
  mutate(
    income_cont = case_when(
      INCOME3 %in% 1:5  ~ 1,   # < $35k   (BRFSS <10k through 25-34,999)
      INCOME3 == 6      ~ 2,   # $35k–50k
      INCOME3 == 7      ~ 3,   # $50k–75k
      INCOME3 == 8      ~ 4,   # $75k–100k
      INCOME3 == 9      ~ 5,   # $100k–150k
      INCOME3 == 10     ~ 6,   # $150k–200k
      INCOME3 == 11     ~ 7,   # $200k+
      TRUE              ~ NA_real_   # 77, 99, blank
    )
  )
table(brfss$INCOME3, brfss$income_cont, useNA = "ifany")
table(brfss$income_cont, useNA = "ifany")

brfss <- brfss %>%
  mutate(
    education_cat = case_when(
      EDUCA %in% c(1, 2) ~ "Less than high school",
      EDUCA %in% c(3, 4) ~ "High school graduate",
      EDUCA == 5         ~ "Some college",
      EDUCA == 6         ~ "College graduate",
      TRUE               ~ NA_character_
    ) %>% factor(levels = c("Less than high school",
                            "High school graduate",
                            "Some college",
                            "College graduate"))
  )

table(brfss$EDUCA,brfss$education_cat, useNA = "ifany")


brfss <- brfss %>%
  mutate(
    gen_health_15 = case_when(
      GENHLTH == 1 ~ 5,   # Excellent
      GENHLTH == 2 ~ 4,   # Very good
      GENHLTH == 3 ~ 3,   # Good
      GENHLTH == 4 ~ 2,   # Fair
      GENHLTH == 5 ~ 1,   # Poor
      TRUE         ~ NA_real_   # 7, 9, blank
    )
  )

table(brfss$GENHLTH, brfss$gen_health_15, useNA = "ifany")

brfss <- brfss %>%
  mutate(
    # binary recode each condition: 1 = has it, 0 = doesn't, NA = DK/refused/missing
    diabetes_history   = case_when(DIABETE4 == 1 ~ 1,
                         DIABETE4 %in% c(2, 3, 4) ~ 0,
                         TRUE ~ NA_real_),
    heartcondition_history   = case_when(`_MICHD` == 1 ~ 1, `_MICHD` == 2 ~ 0, TRUE ~ NA_real_), #heartdisease
    cancer_history  = case_when(CHCOCNCR == 1 | CHCSCNCR == 1 ~ 1, CHCOCNCR == 2 & CHCSCNCR == 2 ~ 0, TRUE ~ NA_real_),
    stroke_history = case_when(CVDSTRK3 == 1 ~ 1, CVDSTRK3 == 2 ~ 0, TRUE ~ NA_real_), #stroke
    chronicdisease_history  = case_when(CHCKDNY2 == 1 ~ 1, CHCKDNY2 == 2 ~ 0, TRUE ~ NA_real_), #kidney
    highbp_history = case_when(
      BPHIGH6 == 1            ~ 1,   # Yes
      BPHIGH6 %in% c(2, 3, 4) ~ 0,   # pregnancy-only, No, borderline
      TRUE                    ~ NA_real_   # 7, 9, blank
    ),
    chroniclung_history = case_when(
      CHCCOPD3 == 1 ~ 1,      
      CHCCOPD3 == 2 ~ 0,
      TRUE          ~ NA_real_
    )
  )%>%
  mutate(
    # count of conditions (0–7)
    chronic_count = rowSums(
      across(c(cancer_history, chronicdisease_history, chroniclung_history, diabetes_history,
               heartcondition_history,highbp_history, stroke_history)),
      na.rm = TRUE
    )
  )
table(brfss$chronic_count, useNA = "ifany")



brfss <- brfss %>%
  mutate(
    caregiving_clean = factor(
      caregiving,
      levels = c(1, 2),
      labels = c("Caregiver", "Non-caregiver")
    )
  )

brfss <- brfss %>%
  mutate(
    gender = case_when(
      SEXVAR == 1 ~ "Male",
      SEXVAR == 2 ~ "Female",
      TRUE        ~ NA_character_
    ) %>% factor(levels = c("Male", "Female"))
  )
brfss_clean <- brfss %>%
  dplyr::select(
    caregiving_clean, healthydays_mental, healthydays_physical, age, gender, race_white_bin, marital_binary,
    education_cat,income_cont, employment_binary,  gen_health_15,chronic_count
  ) %>%
  tidyr::drop_na() %>%
  droplevels()

summary(brfss)

table(brfss_clean$caregiving_clean, useNA = "ifany")
table(brfss_clean$healthydays_mental, useNA = "ifany")
table(brfss_clean$healthydays_physical, useNA = "ifany")
table(brfss_clean$age, useNA = "ifany")
table(brfss_clean$gender, useNA = "ifany")


