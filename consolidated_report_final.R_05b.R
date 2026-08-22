# =============================================================
# Extract Average Marginal Effects (AME) with 95% CI
# for all six models -> consolidated table 
#
# 6 models = 3 outcomes x 2 datasets:
#   Model 1: NSGA  mental        (glm.nb, unweighted)
#   Model 2: BRFSS mental        (weighted NB via bootstrap)
#   Model 3: NSGA  physical      (glm.nb, unweighted)
#   Model 4: BRFSS physical      (weighted NB via bootstrap)
#   Model 5: NSGA  self-rated    (lm, unweighted)
#   Model 6: BRFSS self-rated    (svyglm gaussian, weighted)
#
# Cell format: AME (95% CI)
# =============================================================
remove.packages(c("glmmTMB", "TMB"))
install.packages("TMB")
install.packages("glmmTMB")
library(glmmTMB)
library(marginaleffects)
library(survey)
library(MASS)

# helper: tidy AME table with CI from a standard fitted model
ame_tidy <- function(model, wts = NULL) {
  if (is.null(wts)) {
    a <- avg_comparisons(model)              # no wts argument at all
  } else {
    a <- avg_comparisons(model, wts = wts)
  }
  data.frame(
    term     = a$term,
    contrast = a$contrast,
    AME      = round(a$estimate, 3),
    low      = round(a$conf.low, 3),
    high     = round(a$conf.high, 3),
    p        = a$p.value
  )
}

# -------------------------------------------------------------
# NSGA models (unweighted) -- direct, no bootstrap
# -------------------------------------------------------------
ame_m1 <- ame_tidy(m_nsga_mental)     # Model 1
ame_m3 <- ame_tidy(m_nsga_phy)       # Model 3
ame_m5 <- ame_tidy(m_nsga_gh)         # Model 5 (linear; AME = coefficient)

# -------------------------------------------------------------
# BRFSS self-rated health (svyglm gaussian, weighted) -- direct
# marginaleffects supports svyglm; pass sampling weights
# -------------------------------------------------------------
w_gh   <- weights(subset(brfss_svy, complete_case & age >= 50), "sampling")
ame_m6 <- ame_tidy(m_brfss_gh, wts = w_gh)   # Model 6

# -------------------------------------------------------------
# BRFSS count models (weighted NB) -- via bootstrap
# Return ALL covariate AMEs as a named vector on each replicate;
# withReplicates gives design-based SEs, CI = est +/- 1.96*SE
# -------------------------------------------------------------


ames_brfss_mental <- readRDS("withrep_results_healthydays_mental.rds")$ames
ames_brfss_phys   <- readRDS("withrep_results_healthydays_physical.rds")$ames

# ------------------------------------------------------------
# 1. Standardize each to columns: term, AME, low, high
#    (marginaleffects uses estimate/conf.low/conf.high;
#     bootstrap uses AME/low/high — normalize both)
# ------------------------------------------------------------
norm_ame <- function(df) {
  df <- as.data.frame(df)
  
  # build a unified term key
  if ("contrast" %in% names(df)) {
    # marginaleffects format: term + contrast separate -> combine as "term|contrast"
    term_key <- paste0(df$term, "|", df$contrast)
  } else {
    # bootstrap format: term already "term|contrast"
    term_key <- df$term
  }
  
  data.frame(
    term = term_key,
    AME  = if ("AME" %in% names(df)) df$AME else df$estimate,
    low  = if ("low" %in% names(df)) df$low else df$conf.low,
    high = if ("high" %in% names(df)) df$high else df$conf.high,
    stringsAsFactors = FALSE
  )
}

# ------------------------------------------------------------
# 2. Format each model's cells as "AME (low, high)"
# ------------------------------------------------------------

cell <- function(df, model_name) {
  d <- norm_ame(df)
  d[[model_name]] <- sprintf("%.2f\n(%.2f, %.2f)", d$AME, d$low, d$high)
  d[, c("term", model_name)]
}
# ------------------------------------------------------------
# 3. Merge all six into one wide table, joined on term
# ------------------------------------------------------------
tab <- Reduce(function(a, b) full_join(a, b, by = "term"), list(
  cell(ame_m1,  "NSGA_Mental"),
  cell(ames_brfss_mental, "BRFSS_Mental"),
  cell(ame_m3,    "NSGA_Physical"),
  cell(ames_brfss_phys,   "BRFSS_Physical"),
  cell(ame_m5,      "NSGA_SelfRated"),
  cell(ame_m6,     "BRFSS_SelfRated")
))


# ------------------------------------------------------------
# 4. Order rows: caregiving first, then covariates
# ------------------------------------------------------------
row_order <- c(
  "caregiving_clean|Caregiver - Non-caregiver",
  "age|+1",
  "gender|Female - Male",
  "race_white_bin|Non-White - White",
  "marital_binary|Not married - Married/Partnered",
  "education_cat|Some college - College graduate",
  "education_cat|High school and less - College graduate",
  "income_cont|+1",
  "employment_binary|Employed - Unemployed",
  "chronic_count|+1"
)
tab <- tab[match(row_order, tab$term), ]

# nice row labels
tab$term <- recode(tab$term,
                   "caregiving_clean|Caregiver - Non-caregiver"            = "Caregiver (vs. non-caregiver)",
                   "age|+1"                                                = "Age (+1 year)",
                   "gender|Female - Male"                                  = "Female (vs. male)",
                   "race_white_bin|Non-White - White"                      = "Non-White (vs. White)",
                   "marital_binary|Not married - Married/Partnered"        = "Not married (vs. married/partnered)",
                   "education_cat|Some college - College graduate"         = "Some college (vs. college graduate)",
                   "education_cat|High school and less - College graduate" = "High school or less (vs. college graduate)",
                   "income_cont|+1"                                        = "Income (+1)",
                   "employment_binary|Employed - Unemployed"               = "Employed (vs. not employed)",
                   "chronic_count|+1"                                      = "Chronic conditions (+1)"
)

print(tab, row.names = FALSE)
# ------------------------------------------------------------
# 6. Export
# ------------------------------------------------------------
write.csv(tab, "consolidated_AME_table.csv", row.names = FALSE)



# ------------------------------------------------------------
#    Caregiving AME + SE for each of the 6 models
#    Fill these from your results (AME and its SE).
#    SE = (high - low) / (2 * 1.96) .
# ------------------------------------------------------------
get_caregiving <- function(df, pattern = "caregiving") {
  row <- df[grepl(pattern, df$term), ]
  ame  <- row$AME
  se   <- (row$high - ame) / 1.96
  c(ame = ame, se = se)
}
# mental

ame_nsga_mental  <- get_caregiving(ame_m1)["ame"]  
se_nsga_mental  <- get_caregiving(ame_m1)["se"] 
ame_brfss_mental <- get_caregiving(ames_brfss_mental)["ame"]  
se_brfss_mental <- get_caregiving(ames_brfss_mental)["se"]  


# physical
ame_nsga_phys    <- get_caregiving(ame_m3)["ame"]   
se_nsga_phys    <- get_caregiving(ame_m3)["se"]       
ame_brfss_phys   <- get_caregiving(ames_brfss_phys)["ame"]   
se_brfss_phys   <- get_caregiving(ames_brfss_phys)["se"]        

# self-rated health (scale points, 1-5)
get_caregiving(ame_m5)["ame"]
ame_nsga_gh      <- get_caregiving(ame_m5)["ame"] 
se_nsga_gh      <- get_caregiving(ame_m5)["se"]       
ame_brfss_gh     <- get_caregiving(ame_m6)["ame"] 
se_brfss_gh     <- get_caregiving(ame_m6)["se"]       

# ------------------------------------------------------------
# 2. z-test for difference between two independent AMEs
# ------------------------------------------------------------
compare <- function(ame_n, se_n, ame_b, se_b) {
  diff    <- ame_n - ame_b
  se_diff <- sqrt(se_n^2 + se_b^2)
  z       <- diff / se_diff
  p       <- 2 * pnorm(-abs(z))
  c(NSGA = ame_n, BRFSS = ame_b, Difference = diff,
    SE_diff = se_diff, z = z, p = p)
}

rows <- rbind(
  Mental        = compare(unname(ame_nsga_mental), unname(se_nsga_mental), 
                          unname(ame_brfss_mental), unname(se_brfss_mental)),
  Physical      = compare(unname(ame_nsga_phys),   unname(se_nsga_phys),   
                          unname(ame_brfss_phys),   unname(se_brfss_phys)),
  SelfRated     = compare(unname(ame_nsga_gh),     unname(se_nsga_gh),     
                          unname(ame_brfss_gh),     unname(se_brfss_gh))
)

comparison_tab <- data.frame(
  Outcome = c("Mentally unhealthy days", "Physically unhealthy days",
              "Self-rated general health"),
  `NSGA AME`       = sprintf("%.2f", rows[, "NSGA"]),
  `BRFSS AME`      = sprintf("%.2f", rows[, "BRFSS"]),
  Difference       = sprintf("%.2f", rows[, "Difference"]),
  `95% CI of diff` = sprintf("(%.2f, %.2f)",
                             rows[, "Difference"] - 1.96*rows[, "SE_diff"],
                             rows[, "Difference"] + 1.96*rows[, "SE_diff"]),
  p                = ifelse(rows[, "p"] < 0.001, "<0.001", sprintf("%.3f", rows[, "p"])),
  check.names = FALSE, row.names = NULL
)

print(comparison_tab, row.names = FALSE)
write.csv(comparison_tab, "cross_dataset_comparison.csv", row.names = FALSE)

######
# IRR comparison — mental
irr_brfss_mental <- readRDS("withrep_results_healthydays_mental.rds")$coefs
row           <- irr_brfss_mental[grepl("caregiving", irr_brfss_mental$term), ]
b_brfss_mental   <- log(row$IRR)
s_brfss_mental   <- (log(row$IRR_high) - b_brfss_mental) / 1.96


b_nsga_mental <- coef(m_nsga_mental)["caregiving_cleanCaregiver"]
s_nsga_mental <- sqrt(diag(vcov(m_nsga_mental)))["caregiving_cleanCaregiver"]

gtsummary::tbl_regression(m_nsga_mental, exponentiate = TRUE, label = label_list) %>% gtsummary::bold_labels()


diff    <- b_nsga_mental - b_brfss_mental
se_diff <- sqrt(s_brfss_mental^2 + s_nsga_mental^2)
z       <- diff / se_diff
p       <- 2 * pnorm(-abs(z))

exp(diff)                              # ratio of IRRs (NSGA / BRFSS)
exp(diff + c(-1.96, 1.96) * se_diff)   # 95% CI
c(z = z, p = p)


######
# IRR comparison — physical
irr_brfss_phy <- readRDS("withrep_results_healthydays_physical.rds")$coefs
row           <- irr_brfss_phy[grepl("caregiving", irr_brfss_phy$term), ]
b_brfss_phy   <- log(row$IRR)
s_brfss_phy   <- (log(row$IRR_high) - b_brfss_phy) / 1.96


b_nsga_phy <- coef(m_nsga_phy)["caregiving_cleanCaregiver"]
s_nsga_phy <- sqrt(diag(vcov(m_nsga_phy)))["caregiving_cleanCaregiver"]

gtsummary::tbl_regression(m_nsga_phy, exponentiate = TRUE, label = label_list) %>% gtsummary::bold_labels()


diff    <- b_nsga_phy - b_brfss_phy
se_diff <- sqrt(s_brfss_phy^2 + s_nsga_phy^2)
z       <- diff / se_diff
p       <- 2 * pnorm(-abs(z))

exp(diff)                              # ratio of IRRs (NSGA / BRFSS)
exp(diff + c(-1.96, 1.96) * se_diff)   # 95% CI
c(z = z, p = p)
