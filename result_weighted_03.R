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

# ---- Column 1: BRFSS weighted (percentage only) ----
tbl_brfss_wt <- tbl_svysummary(
  brfss_svy50,
  include = all_of(tbl_vars),
  label = label_list,
  type = list(gen_health_15 ~ "continuous"),
  statistic = list(all_continuous()  ~ "{mean} ({sd})",
                   all_categorical() ~ "{p}%"),        # weighted → % only
  digits = list(age ~ 1, healthydays_mental ~ 2, healthydays_physical ~ 2),
  missing = "no"
) %>%
  modify_header(stat_0 ~ "**BRFSS (weighted)**")

# ---- Columns 2 & 3: unweighted BRFSS and NSGA, split by source ----
tbl_unwt <- tbl_summary(
  combined_unweight,
  by = source,
  include = all_of(tbl_vars),
  label = label_list,
  type = list(gen_health_15 ~ "continuous"),
  statistic = stat_list,                                # unweighted → n (%)
  digits = list(age ~ 1, healthydays_mental ~ 2, healthydays_physical ~ 2),
  missing = "no"
)
# stat_1 / stat_2 headers come from the factor levels of `source`
label_list
# ---- merge into one 3-column table ----
tbl_1 <- tbl_merge(
  list(tbl_brfss_wt, tbl_unwt),
  tab_spanner = c("**Weighted**", "**Unweighted**")
) %>%
  bold_labels()

tbl_1

#######################
# regress on healthydays_mental 
#######################
m_brfss <- svyglm(healthydays_mental ~ caregiving_clean  +
                    age + gender + race_white_bin + marital_binary +education_cat+income_cont+ employment_binary + 
                    chronic_count,
                  design = subset(brfss_svy, complete_case), family = quasipoisson())
tbl_m_brfss <- gtsummary::tbl_regression(m_brfss, exponentiate = TRUE, label = label_list) %>% gtsummary::bold_labels()
tbl_m_brfss 
# NSGA: unweighted negative binomial
m_nsga <- MASS::glm.nb(healthydays_mental ~ caregiving_clean  +
                         age + gender + race_white_bin + marital_binary +education_cat+income_cont+ employment_binary + 
                         chronic_count, data = nsga_analytic)
tbl_m_nsga <- gtsummary::tbl_regression(m_nsga, exponentiate = TRUE, label = label_list) %>% gtsummary::bold_labels()
tbl_m_nsga 

# extract on the log scale (NOT the exponentiated IRR)
b_brfss <- coef(m_brfss)["caregiving_cleanCaregiver"]
s_brfss <- sqrt(diag(vcov(m_brfss)))["caregiving_cleanCaregiver"]

b_nsga  <- coef(m_nsga)["caregiving_cleanCaregiver"]
s_nsga  <- sqrt(diag(vcov(m_nsga)))["caregiving_cleanCaregiver"]

# difference in log-IRRs
diff    <- b_nsga - b_brfss
se_diff <- sqrt(s_brfss^2 + s_nsga^2)

z <- diff / se_diff
p <- 2 * pnorm(-abs(z))

# back to the IRR-ratio scale, directly comparable to the interaction term
exp(diff)                                   # ratio of IRRs
exp(diff + c(-1.96, 1.96) * se_diff)        # 95% CI
c(z = z, p = p)
#######################
# regress on healthydays_physical 
#######################
m_brfss_phy <- svyglm(healthydays_physical ~ caregiving_clean  +
                    age + gender + race_white_bin + marital_binary +education_cat+income_cont+ employment_binary + 
                    chronic_count,
                  design = subset(brfss_svy, complete_case), family = quasipoisson())
tbl_m_brfss_phy <- gtsummary::tbl_regression(m_brfss_phy , exponentiate = TRUE, label = label_list) %>% gtsummary::bold_labels()
tbl_m_brfss_phy 
# NSGA: unweighted negative binomial
m_nsga_phy <- MASS::glm.nb(healthydays_physical ~ caregiving_clean  +
                         age + gender + race_white_bin + marital_binary +education_cat+income_cont+ employment_binary + 
                         chronic_count, data = nsga_analytic)
tbl_m_nsga_phy <- gtsummary::tbl_regression(m_nsga_phy, exponentiate = TRUE, label = label_list) %>% gtsummary::bold_labels()
tbl_m_nsga_phy 

# extract on the log scale (NOT the exponentiated IRR)
b_brfss_phy <- coef(m_brfss_phy)["caregiving_cleanCaregiver"]
s_brfss_phy <- sqrt(diag(vcov(m_brfss_phy)))["caregiving_cleanCaregiver"]

b_nsga_phy  <- coef(m_nsga_phy)["caregiving_cleanCaregiver"]
s_nsga_phy  <- sqrt(diag(vcov(m_nsga_phy)))["caregiving_cleanCaregiver"]

# difference in log-IRRs
diff    <- b_nsga_phy - b_brfss_phy
se_diff <- sqrt(s_brfss_phy^2 + s_nsga_phy^2)

z <- diff / se_diff
p <- 2 * pnorm(-abs(z))

# back to the IRR-ratio scale, directly comparable to the interaction term
exp(diff)                                   # ratio of IRRs
exp(diff + c(-1.96, 1.96) * se_diff)        # 95% CI
c(z = z, p = p)

#######################
# linear regress on self rated general health 
#######################

m_brfss_gh <- svyglm(gen_health_15 ~ caregiving_clean + age + gender + race_white_bin +
                       marital_binary + education_cat + income_cont + employment_binary +
                       chronic_count,
                     design = subset(brfss_svy, complete_case))     # Gaussian

tbl_m_brfss_gh <- tbl_regression(m_brfss_gh, label = label_list) %>% bold_labels()   

m_nsga_gh <- lm(gen_health_15 ~ caregiving_clean + age + gender + race_white_bin +
                  marital_binary + education_cat + income_cont + employment_binary +
                  chronic_count, data = nsga_analytic)


tbl_m_nsga_gh <- tbl_regression(m_nsga_gh, label = label_list) %>% bold_labels()      

# difference in adjusted caregiving effect (points on the 1-5 scale)
b_brfss_gh <- coef(m_brfss_gh)["caregiving_cleanCaregiver"]
s_brfss_gh <- sqrt(diag(vcov(m_brfss_gh)))["caregiving_cleanCaregiver"]
b_nsga_gh  <- coef(m_nsga_gh)["caregiving_cleanCaregiver"]
s_nsga_gh  <- sqrt(diag(vcov(m_nsga_gh)))["caregiving_cleanCaregiver"]

diff    <- b_nsga_gh - b_brfss_gh
se_diff <- sqrt(s_brfss_gh^2 + s_nsga_gh^2)
z <- diff / se_diff
p <- 2 * pnorm(-abs(z))
c(diff = diff, se = se_diff, z = z, p = p)



#NSGA summary by caregiving
nsga_unwt <- tbl_summary(
  nsga_analytic,
  by = caregiving_clean,
  include = all_of(tbl_vars),
  label = label_list,
  type = list(gen_health_15 ~ "continuous"),
  statistic = stat_list,                                # unweighted → n (%)
  digits = list(age ~ 1, healthydays_mental ~ 2, healthydays_physical ~ 2),
  missing = "no"
)

nsga_unwt

#Brfss by caregiving
brfss_wt <- tbl_svysummary(
  brfss_svy50,
  by = caregiving_clean,
  include = all_of(tbl_vars),
  label = label_list,
  type = list(gen_health_15 ~ "continuous"),
  statistic = list(all_continuous()  ~ "{mean} ({sd})",
                   all_categorical() ~ "{p}%"),        # weighted → % only
  digits = list(age ~ 1, healthydays_mental ~ 2, healthydays_physical ~ 2),
  missing = "no"
)

# ============================================================
# 10) Export ALL tables to ONE Word document (NEW NAME)
# ============================================================
out_file <- "ALL_Tables_Final.docx"

doc <- officer::read_docx() %>%
  officer::body_add_par("Descriptive and Regression Tables (Final)", style = "heading 1") %>%
  officer::body_add_par("Pooled Sample Characteristics by Dataset", style = "heading 2") %>%
  flextable::body_add_flextable(gtsummary::as_flex_table(tbl_1)) %>%
  officer::body_add_par("Adjusted Negative Binomial Model Predicting Mentally Unhealthy Days Among NSGA", style = "heading 2") %>%
  flextable::body_add_flextable(gtsummary::as_flex_table(tbl_m_nsga)) %>% 
  officer::body_add_par("Adjusted Negative Binomial Model Predicting Physically Unhealthy Days Among NSGA", style = "heading 2") %>%
  flextable::body_add_flextable(gtsummary::as_flex_table(tbl_m_nsga_phy)) %>%
  officer::body_add_par("Adjusted Weighted Quasi-Poisson Model Predicting Mentally Unhealthy Days Among BRFSS Respondents", style = "heading 2") %>%
  flextable::body_add_flextable(gtsummary::as_flex_table(tbl_m_brfss)) %>%
  officer::body_add_par("Adjusted Weighted Quasi-Poisson Model Predicting Physically Unhealthy Days Among BRFSS Respondents", style = "heading 2") %>%
  flextable::body_add_flextable(gtsummary::as_flex_table(tbl_m_brfss_phy)) %>%
  officer::body_add_par("Adjusted Linear Regression Model Predicting Self-Rated General Health Among NSGA", style = "heading 2") %>%
  flextable::body_add_flextable(gtsummary::as_flex_table(tbl_m_nsga_gh)) %>%
  officer::body_add_par("Adjusted Weighted Linear Regression Model Predicting Self-Rated General Health Among BRFSS", style = "heading 2") %>%
  flextable::body_add_flextable(gtsummary::as_flex_table(tbl_m_brfss_gh)) %>%
  officer::body_add_par("Unweighted Characteristics NSGA", style = "heading 2") %>%
  flextable::body_add_flextable(gtsummary::as_flex_table(nsga_unwt)) %>%
  officer::body_add_par("Weighted Characteristics BRFSS", style = "heading 2") %>%
  flextable::body_add_flextable(gtsummary::as_flex_table(brfss_wt))  
  
print(doc, target = out_file)

message("Saved Word file here: ", file.path(getwd(), out_file))
list.files(pattern = "ALL_Tables_Final")