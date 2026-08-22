# =============================================================
# Table 1: Sample characteristics, weighted BRFSS vs. NSGA
#   - BRFSS: survey-weighted means/proportions with design-based SEs
#   - NSGA:  unweighted means/proportions
#   - p-value: z-test for the difference between two independent
#              estimates (BRFSS design-based SE + NSGA SE)
#   - SMD:    standardized difference, a sample-size-independent
#             measure of how much the samples differ
#
# Education collapsed to 3 categories (high school or less,
# some college, college graduate) to match the regression models.
# =============================================================

library(survey)

# -------------------------------------------------------------
# 0. Inputs
#    brfss_svy50   = survey design, 50+, complete cases
#    nsga_analytic = NSGA analytic data frame
# -------------------------------------------------------------
des  <- brfss_svy50
nsga <- nsga_analytic



# =============================================================
# 1. CONTINUOUS variables: weighted mean vs. unweighted mean
# =============================================================
cont_row <- function(var, label) {
  f  <- as.formula(paste0("~", var))
  
  sm   <- svymean(f, des, na.rm = TRUE)
  m_b  <- as.numeric(sm)
  se_b <- as.numeric(SE(sm))
  sd_b <- sqrt(as.numeric(svyvar(f, des, na.rm = TRUE)))
  
  x    <- nsga[[var]]; x <- x[!is.na(x)]
  m_n  <- mean(x); sd_n <- sd(x); se_n <- sd_n / sqrt(length(x))
  
  z   <- (m_n - m_b) / sqrt(se_n^2 + se_b^2)
  p   <- 2 * pnorm(-abs(z))
  smd <- (m_n - m_b) / sqrt((sd_n^2 + sd_b^2) / 2)
  
  data.frame(
    Characteristic = label,
    BRFSS = sprintf("%.2f (%.2f)", m_b, sd_b),
    NSGA  = sprintf("%.2f (%.2f)", m_n, sd_n),
    p     = p, SMD = smd,
    stringsAsFactors = FALSE
  )
}

# =============================================================
# 2. CATEGORICAL variables: weighted % vs. unweighted n (%)
#    One row per level; p and SMD computed per level.
# =============================================================
cat_rows <- function(var, label, level_labels = NULL) {
  f  <- as.formula(paste0("~", var))
  
  sm   <- svymean(f, des, na.rm = TRUE)
  p_b  <- as.numeric(sm)
  se_b <- as.numeric(SE(sm))
  lv   <- sub(paste0("^", var), "", names(sm))
  
  tabn <- table(nsga[[var]])
  n    <- sum(tabn)
  cnt  <- as.numeric(tabn[lv]); cnt[is.na(cnt)] <- 0
  p_n  <- cnt / n
  se_n <- sqrt(p_n * (1 - p_n) / n)
  
  z    <- (p_n - p_b) / sqrt(se_n^2 + se_b^2)
  pval <- 2 * pnorm(-abs(z))
  smd  <- (p_n - p_b) / sqrt((p_n*(1-p_n) + p_b*(1-p_b)) / 2)
  
  labs <- if (is.null(level_labels)) lv else level_labels[lv]
  
  rbind(
    data.frame(Characteristic = label, BRFSS = "", NSGA = "",
               p = NA_real_, SMD = NA_real_, stringsAsFactors = FALSE),
    data.frame(
      Characteristic = paste0("   ", labs),
      BRFSS = sprintf("%.1f%%", 100 * p_b),
      NSGA  = sprintf("%s (%.1f%%)", format(cnt, big.mark = ","), 100 * p_n),
      p = pval, SMD = smd,
      stringsAsFactors = FALSE
    )
  )
}

# =============================================================
# 3. Assemble the table
# =============================================================
tab1 <- rbind(
  cat_rows("caregiving_clean",  "Caregiving status"),
  cont_row("healthydays_mental",   "Mentally unhealthy days"),
  cont_row("healthydays_physical", "Physically unhealthy days"),
  cont_row("gen_health_15",        "Self-rated general health"),
  cont_row("age",                  "Age (years)"),
  cat_rows("gender",            "Gender"),
  cat_rows("race_white_bin",    "Race"),
  cat_rows("marital_binary",    "Marital status"),
  cat_rows("education_cat",    "Education"),
  cat_rows("employment_binary", "Employment status"),
  cont_row("chronic_count",     "Chronic condition count")
)

# format p and SMD for display
tab1$`p value` <- ifelse(is.na(tab1$p), "",
                         ifelse(tab1$p < 0.001, "<0.001", sprintf("%.3f", tab1$p)))
tab1$`SMD`     <- ifelse(is.na(tab1$SMD), "", sprintf("%.2f", tab1$SMD))
tab1_out <- tab1[, c("Characteristic", "BRFSS", "NSGA", "p value", "SMD")]

print(tab1_out, row.names = FALSE)
write.csv(tab1_out, "table1_weighted_vs_nsga.csv", row.names = FALSE)

# =============================================================
# NOTE ON INTERPRETATION
# With N = 117,922 (BRFSS) vs 4,410 (NSGA), nearly every comparison
# will be p < .001 even when the absolute difference is trivial.
# The SMD column is the more informative one: |SMD| > 0.10 is the
# conventional threshold for a meaningful between-group difference.
# Consider reporting SMD alongside (or instead of) p in the manuscript.
# =============================================================

###########
#basic summary - bsga
dat %>%
  dplyr::select(all_of( tbl_vars)) %>%
  mutate(complete_case = complete.cases(dplyr::select(., all_of(tbl_vars)))) %>%
  filter(age >= 50 & complete_case) %>%
  group_by(caregiving_clean) %>%
  summarise(
    n = n(),
    mean_ment = mean(healthydays_mental),
    mean_phy = mean(healthydays_physical)
  )

#basic summary - brfss unweighted
brfss %>%
  dplyr::select(all_of( tbl_vars)) %>%
  mutate(complete_case = complete.cases(dplyr::select(., all_of(tbl_vars)))) %>%
  filter(age >= 50 & complete_case) %>%
  group_by(caregiving_clean) %>%
  summarise(
    n = n(),
    mean_ment = mean(healthydays_mental),
    mean_phy = mean(healthydays_physical)
  )
#basic summary - brfss weighted
svyby(~healthydays_mental + healthydays_physical + gen_health_15,
      ~caregiving_clean, brfss_svy50, svymean, na.rm = TRUE)


