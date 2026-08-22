# =============================================================
# SERIAL withReplicates full bundle (coef + AME + predictions)
#   - education AME via manual predictions-difference
#   - NA-padded returns (fixed length/order) so withReplicates
#     never breaks on a failure
#   - withReplicates computes the design-based variance itself
#     (independent cross-check of the parallel hand-rolled SEs)
# =============================================================

library(MASS)
library(marginaleffects)
library(survey)

# ------------------------------------------------------------ SETTINGS
OUTCOME <- "healthydays_mental"     # or "healthydays_physical"
ENGINE  <- "glm.nb"                 # keep consistent with NSGA

model_formula <- as.formula(paste0(
  OUTCOME, " ~ caregiving_clean + age + gender + race_white_bin + ",
  "marital_binary + education_cat + income_cont + employment_binary + chronic_count"))

AME_VARS <- c("caregiving_clean", "age", "gender", "race_white_bin",
              "marital_binary", "education_cat", "income_cont",
              "employment_binary", "chronic_count")

# ------------------------------------------------------------ extractor
# One function used for BOTH the base template and each replicate,
# so lengths/names always match. Returns a named numeric vector.
extract_all <- function(fit, d) {
  if (ENGINE == "glmmTMB") {
    cf <- setNames(glmmTMB::fixef(fit)$cond,
                   paste0("coef|", names(glmmTMB::fixef(fit)$cond)))
  } else {
    cf <- setNames(coef(fit), paste0("coef|", names(coef(fit))))
  }
  
  ame <- unlist(lapply(AME_VARS, function(v) {
    if (v == "education_cat") {
      lv  <- levels(d$education_cat); ref <- lv[1]
      p_ref <- avg_predictions(fit, newdata = transform(d, education_cat = ref),
                               wts = d$.w, type = "response")$estimate
      out <- vapply(setdiff(lv, ref), function(l) {
        p_l <- avg_predictions(fit, newdata = transform(d, education_cat = l),
                               wts = d$.w, type = "response")$estimate
        p_l - p_ref
      }, numeric(1))
      out[!is.finite(out)] <- NA_real_
      setNames(out, paste0("ame|education_cat|", setdiff(lv, ref), " - ", ref))
    } else {
      a   <- avg_comparisons(fit, variables = v, newdata = d,
                             wts = d$.w, type = "response")
      val <- a$estimate; val[!is.finite(val)] <- NA_real_
      setNames(val, paste0("ame|", a$term, "|", a$contrast))
    }
  }))
  
  pr  <- avg_predictions(fit, by = "caregiving_clean", newdata = d,
                         wts = d$.w, type = "response")
  prd <- setNames(pr$estimate, paste0("pred|", pr$caregiving_clean))
  prd[!is.finite(prd)] <- NA_real_
  
  c(cf, ame, prd)
}

# ------------------------------------------------------------ base template
cat("Fitting base to build template...\n")
d0 <- brfss_rep$variables
d0$.w <- weights(brfss_rep, "sampling")
fit0 <- if (ENGINE == "glmmTMB")
  glmmTMB::glmmTMB(model_formula, family = glmmTMB::nbinom2, weights = .w, data = d0) else
    glm.nb(model_formula, data = d0, weights = .w)
TEMPLATE <- extract_all(fit0, d0)
NMS <- names(TEMPLATE); LEN <- length(TEMPLATE)
cat("Template:", LEN, "quantities. Base caregiving AME:",
    round(TEMPLATE[grep("caregiving_clean\\|Caregiver", NMS)][1], 3), "\n\n")

# ------------------------------------------------------------ bootstrap
counter <- 0; fails <- 0; start_time <- Sys.time()

res <- withReplicates(brfss_rep, function(w, data) {
  counter <<- counter + 1
  
  # progress
  el <- as.numeric(difftime(Sys.time(), start_time, units = "secs"))
  if (counter > 1) {
    rem <- (el / (counter - 1)) * (200 - counter + 1)
    cat(sprintf("Rep %d/200 (%.0f%%) | %.1f min | ETA %.1f min\n",
                counter, 100*counter/200, el/60, rem/60))
  } else {
    cat(sprintf("Rep %d/200 | starting...\n", counter))
  }
  flush.console()
  
  out <- tryCatch({
    d <- data; d$.w <- w
    fit <- if (ENGINE == "glmmTMB")
      glmmTMB::glmmTMB(model_formula, family = glmmTMB::nbinom2, weights = .w, data = d) else
        glm.nb(model_formula, data = d, weights = .w)
    v <- extract_all(fit, d)
    v[NMS]                                 # force fixed length/order
  }, error = function(e) {
    fails <<- fails + 1
    cat(sprintf("!!! Rep %d FAILED: %s\n", counter, conditionMessage(e)))
    setNames(rep(NA_real_, LEN), NMS)      # NA-padded, same length (NOT NULL)
  })
  
  out
})

cat(sprintf("\nDone. Fit failures: %d\n", fails))

# ------------------------------------------------------------ extract
# withReplicates gives estimate + its own design-based SE
est <- coef(res)
se  <- sqrt(diag(attr(res, "var")))

ic <- grepl("^coef\\|", names(est)); ia <- grepl("^ame\\|", names(est)); ip <- grepl("^pred\\|", names(est))

coefs <- data.frame(term = sub("^coef\\|","",names(est)[ic]),
                    IRR = round(exp(est[ic]),3),
                    IRR_low = round(exp(est[ic]-1.96*se[ic]),3),
                    IRR_high= round(exp(est[ic]+1.96*se[ic]),3),
                    p = signif(2*pnorm(-abs(est[ic]/se[ic])),3), row.names=NULL)
ames <- data.frame(term = sub("^ame\\|","",names(est)[ia]),
                   AME = round(est[ia],3),
                   low = round(est[ia]-1.96*se[ia],3),
                   high= round(est[ia]+1.96*se[ia],3),
                   p = signif(2*pnorm(-abs(est[ia]/se[ia])),3), row.names=NULL)
preds <- data.frame(group = sub("^pred\\|","",names(est)[ip]),
                    pred = round(est[ip],3),
                    low = round(est[ip]-1.96*se[ip],3),
                    high= round(est[ip]+1.96*se[ip],3), row.names=NULL)

cat("\n--- COEFFICIENTS (IRR) ---\n"); print(coefs)
cat("\n--- AMEs (days) ---\n");        print(ames)
cat("\n--- PREDICTED MEANS ---\n");    print(preds)

saveRDS(res, paste0("withrep_full_", OUTCOME, ".rds"))
saveRDS(list(coefs=coefs, ames=ames, preds=preds),
        paste0("withrep_results_", OUTCOME, ".rds"))

# ------------------------------------------------------------ CROSS-CHECK
# Compare this withReplicates SE for caregiving AME against your
# parallel hand-formula result (~0.266). They should match closely.
cg <- "caregiving_clean|Caregiver - Non-caregiver"
cat(sprintf("\nCROSS-CHECK caregiving AME SE (withReplicates): %.4f\n",
            se[paste0("ame|", cg)]))
cat("Parallel hand-formula gave ~0.266; plain sd ~0.254.\n")
