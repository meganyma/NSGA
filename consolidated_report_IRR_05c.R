
# -------------------------------------------------------------
# 1. NSGA models (unweighted glm.nb) -> IRR directly from coef
# -------------------------------------------------------------
irr_tidy <- function(model) {
  b  <- coef(model)
  se <- sqrt(diag(vcov(model)))
  data.frame(
    term     = names(b),
    IRR      = round(exp(b), 3),
    IRR_low  = round(exp(b - 1.96 * se), 3),
    IRR_high = round(exp(b + 1.96 * se), 3),
    p        = 2 * pnorm(-abs(b / se)),
    row.names = NULL,
    stringsAsFactors = FALSE
  )
}

irr_m1 <- irr_tidy(m_nsga_mental)   # Model 1: NSGA mental
irr_m3 <- irr_tidy(m_nsga_phy)      # Model 3: NSGA physical

# -------------------------------------------------------------
# 2. BRFSS count models -> IRR already stored in the bootstrap
#    results ($coefs has term, IRR, IRR_low, IRR_high, p)
# -------------------------------------------------------------
irr_brfss_mental <- readRDS("withrep_results_healthydays_mental.rds")$coefs
irr_brfss_phys   <- readRDS("withrep_results_healthydays_physical.rds")$coefs

# -------------------------------------------------------------
# 3. Format each model's cells as "IRR (low, high)"
#    (both sources already share glm.nb coefficient naming,
#     so no term-key construction is needed)
# -------------------------------------------------------------
cell_irr <- function(df, model_name) {
  d <- as.data.frame(df)
  d[[model_name]] <- sprintf("%.2f\n(%.2f, %.2f)", d$IRR, d$IRR_low, d$IRR_high)
  d[, c("term", model_name)]
}

tab_irr <- Reduce(function(a, b) full_join(a, b, by = "term"), list(
  cell_irr(irr_m1,           "NSGA_Mental"),
  cell_irr(irr_brfss_mental, "BRFSS_Mental"),
  cell_irr(irr_m3,           "NSGA_Physical"),
  cell_irr(irr_brfss_phys,   "BRFSS_Physical")
))

# -------------------------------------------------------------
# 4. Order rows (caregiving first) and relabel; drop intercept
# -------------------------------------------------------------
row_order <- c(
  "caregiving_cleanCaregiver",
  "age",
  "genderFemale",
  "race_white_binNon-White",
  "marital_binaryNot married",
  "education_catSome college",
  "education_catHigh school and less",
  "income_cont",
  "employment_binaryEmployed",
  "chronic_count"
)
tab_irr <- tab_irr[match(row_order, tab_irr$term), ]

tab_irr$term <- recode(tab_irr$term,
                       "caregiving_cleanCaregiver"         = "Caregiver (vs. non-caregiver)",
                       "age"                               = "Age (+1 year)",
                       "genderFemale"                      = "Female (vs. male)",
                       "race_white_binNon-White"           = "Non-White (vs. White)",
                       "marital_binaryNot married"         = "Not married (vs. married/partnered)",
                       "education_catSome college"         = "Some college (vs. college graduate)",
                       "education_catHigh school and less" = "High school or less (vs. college graduate)",
                       "income_cont"                       = "Income (+1)",
                       "employment_binaryEmployed"         = "Employed (vs. not employed)",
                       "chronic_count"                     = "Chronic conditions (+1)"
)

print(tab_irr, row.names = FALSE)
write.csv(tab_irr, "consolidated_IRR_table.csv", row.names = FALSE)


# =============================================================
# 5. Cross-dataset comparison on the IRR (relative) scale
#    z-test on the difference of independent log-IRRs;
#    exp(difference) = ratio of IRRs (NSGA / BRFSS)
# =============================================================

# pull caregiving log-coefficient + SE from an IRR table
get_caregiving_log <- function(df, pattern = "caregiving") {
  row <- df[grepl(pattern, df$term), ]
  if (nrow(row) != 1) stop("Expected exactly 1 caregiving row, found ", nrow(row))
  beta <- log(row$IRR)
  se   <- (log(row$IRR_high) - beta) / 1.96
  c(beta = beta, se = se)
}

# NOTE ON PRECISION: the stored IRRs are rounded to 3 decimals, so the
# back-transformed SE carries small rounding error. For exact values,
# pull from the raw withReplicates objects instead:
#   res <- readRDS("withrep_full_healthydays_mental.rds")
#   est <- coef(res); se <- sqrt(diag(attr(res, "var")))
#   i <- grep("^coef\\|caregiving", names(est))
#   c(beta = unname(est[i]), se = unname(se[i]))

cg_nsga_mental  <- get_caregiving_log(irr_m1)
cg_brfss_mental <- get_caregiving_log(irr_brfss_mental)
cg_nsga_phys    <- get_caregiving_log(irr_m3)
cg_brfss_phys   <- get_caregiving_log(irr_brfss_phys)

compare_irr <- function(b_n, s_n, b_b, s_b) {
  diff    <- b_n - b_b                       # difference of log-IRRs
  se_diff <- sqrt(s_n^2 + s_b^2)
  z       <- diff / se_diff
  p       <- 2 * pnorm(-abs(z))
  c(IRR_NSGA   = exp(b_n),
    IRR_BRFSS  = exp(b_b),
    IRR_ratio  = exp(diff),                  # >1 = larger relative assoc. in NSGA
    ratio_low  = exp(diff - 1.96 * se_diff),
    ratio_high = exp(diff + 1.96 * se_diff),
    z = z, p = p)
}

rows_irr <- rbind(
  Mental   = compare_irr(unname(cg_nsga_mental["beta"]), unname(cg_nsga_mental["se"]),
                         unname(cg_brfss_mental["beta"]), unname(cg_brfss_mental["se"])),
  Physical = compare_irr(unname(cg_nsga_phys["beta"]),   unname(cg_nsga_phys["se"]),
                         unname(cg_brfss_phys["beta"]),  unname(cg_brfss_phys["se"]))
)

comparison_irr <- data.frame(
  Outcome            = c("Mentally unhealthy days", "Physically unhealthy days"),
  `NSGA IRR`         = sprintf("%.2f", rows_irr[, "IRR_NSGA"]),
  `BRFSS IRR`        = sprintf("%.2f", rows_irr[, "IRR_BRFSS"]),
  `Ratio of IRRs`    = sprintf("%.2f", rows_irr[, "IRR_ratio"]),
  `95% CI of ratio`  = sprintf("(%.2f, %.2f)", rows_irr[, "ratio_low"], rows_irr[, "ratio_high"]),
  p                  = ifelse(rows_irr[, "p"] < 0.001, "<0.001", sprintf("%.3f", rows_irr[, "p"])),
  check.names = FALSE, row.names = NULL
)

print(comparison_irr, row.names = FALSE)
write.csv(comparison_irr, "cross_dataset_comparison_IRR.csv", row.names = FALSE)


# =============================================================
# 6. Self-rated health (linear models) -- coefficients, not IRRs
#    Report separately; a linear coefficient is the change in
#    scale points, not a rate ratio.
# =============================================================
b_tidy <- function(model) {
  b  <- coef(model)
  se <- sqrt(diag(vcov(model)))
  data.frame(term = names(b),
             B    = round(b, 3),
             low  = round(b - 1.96 * se, 3),
             high = round(b + 1.96 * se, 3),
             p    = 2 * pnorm(-abs(b / se)),
             row.names = NULL, stringsAsFactors = FALSE)
}
b_m5 <- b_tidy(m_nsga_gh)     # NSGA self-rated health
b_m6 <- b_tidy(m_brfss_gh)    # BRFSS self-rated health (svyglm)

# caregiving comparison on the linear scale
cg5 <- b_m5[grepl("caregiving", b_m5$term), ]
cg6 <- b_m6[grepl("caregiving", b_m6$term), ]
s5  <- (cg5$high - cg5$B) / 1.96
s6  <- (cg6$high - cg6$B) / 1.96
d   <- cg5$B - cg6$B
sd_ <- sqrt(s5^2 + s6^2)
cat(sprintf("\nSelf-rated health: NSGA B = %.3f, BRFSS B = %.3f, diff = %.3f, z = %.2f, p = %.3f\n",
            cg5$B, cg6$B, d, d/sd_, 2*pnorm(-abs(d/sd_))))
