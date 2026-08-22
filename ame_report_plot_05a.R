install.packages("marginaleffects")   # if needed
library(marginaleffects)
library(MASS)
m_nsga_mental <- MASS::glm.nb(healthydays_mental ~ caregiving_clean + age + gender +
                          race_white_bin + marital_binary + education_cat +
                          income_cont + employment_binary + chronic_count,
                        data = nsga_analytic)

# average marginal effect of caregiving (difference in predicted days)
avg_comparisons(m_nsga_mental, variables = "caregiving_clean")

# NSGA, mental — adjusted predicted days by caregiving status
avg_predictions(m_nsga_mental, by = "caregiving_clean")


m_nsga_phy <- MASS::glm.nb(healthydays_physical ~ caregiving_clean + age + gender +
                                race_white_bin + marital_binary + education_cat +
                                income_cont + employment_binary + chronic_count,
                              data = nsga_analytic)

# average marginal effect of caregiving (difference in predicted days)
avg_comparisons(m_nsga_phy, variables = "caregiving_clean")
avg_predictions(m_nsga_phy, by = "caregiving_clean")
# self-rated general health (linear model — AME is the difference on the 1-5 scale)
m_nsga_gh <- lm(gen_health_15 ~ caregiving_clean + age + gender + race_white_bin +
                  marital_binary + education_cat + income_cont + employment_binary +
                  chronic_count, data = nsga_analytic)
avg_comparisons(m_nsga_gh, variables = "caregiving_clean")
avg_predictions(m_nsga_gh, by = "caregiving_clean")



#self-rated general health
m_brfss_gh <- svyglm(gen_health_15 ~ caregiving_clean + age + gender + race_white_bin +
                       marital_binary + education_cat + income_cont + employment_binary +
                       chronic_count,
                     design = subset(brfss_svy, complete_case))     # Gaussian
design_gh <- subset(brfss_svy, complete_case & age >= 50)


avg_comparisons(m_brfss_gh, variables = "caregiving_clean", wts = weights(design_gh, "sampling"))
avg_predictions(m_brfss_gh, by = "caregiving_clean",
                wts = weights(design_gh, "sampling"))
#plot
library(ggplot2)

fig_dat <- data.frame(
  outcome = c(rep("A. Mentally unhealthy days", 4),
              rep("B. Physically unhealthy days", 4)),
  dataset = rep(c("BRFSS","BRFSS","NSGA","NSGA"), 2),
  caregiver = factor(rep(c("Non-caregiver","Caregiver"), 4),
                     levels = c("Non-caregiver","Caregiver")),
  pred = c(3.217, 4.726, 1.61, 2.42,
           4.660, 4.941, 2.36, 2.51),
  se   = c(0.0798, 0.2372, 0.0685, 0.2201,
           0.1165, 0.2122, 0.0932, 0.2145)
)

fig_dat$low  <- fig_dat$pred - 1.96 * fig_dat$se
fig_dat$high <- fig_dat$pred + 1.96 * fig_dat$se
fig_dat



ggplot(fig_dat, aes(x = caregiver, y = pred, color = dataset, group = dataset)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(aes(ymin = low, ymax = high),
                position = position_dodge(width = 0.4), width = 0.15) +
  facet_wrap(~ outcome) +                         # Panel A / Panel B
  labs(title = "Caregiver Associations with Unhealthy Days by Dataset",
       x = NULL, y = "Adjusted predicted unhealthy days (95% CI)",
       color = "Sample") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")



fig2 <- data.frame(
  dataset = c("BRFSS", "BRFSS", "NSGA", "NSGA"),
  caregiver = c("Non-caregiver", "Caregiver", "Non-caregiver", "Caregiver"),
  pred = c(3.330, 3.340, 4.300, 4.230),
  se   = c(0.0096, 0.0147, 0.0113, 0.0256)
)
fig2$caregiver <- factor(fig2$caregiver, levels = c("Non-caregiver", "Caregiver"))
fig2$low  <- fig2$pred - 1.96 * fig2$se
fig2$high <- fig2$pred + 1.96 * fig2$se

ggplot(fig2, aes(x = caregiver, y = pred, color = dataset, group = dataset)) +
  geom_point(position = position_dodge(width = 0.4), size = 3) +
  geom_errorbar(aes(ymin = low, ymax = high),
                position = position_dodge(width = 0.4), width = 0.15) +
  labs(title = "Caregiver Associations with Self-rated General Health by Dataset",
       x = NULL,
       y = "Adjusted predicted self-rated health\n(1 = poor to 5 = excellent; higher = better)",
       color = "Sample") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "bottom")

# =============================================================
