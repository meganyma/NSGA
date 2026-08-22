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


list.files(pattern = "ALL_Tables_Final")
