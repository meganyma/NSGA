library(ggplot2)

fig_dat <- data.frame(
  outcome = c("Mentally unhealthy days", "Mentally unhealthy days",
              "Physically unhealthy days", "Physically unhealthy days"),
  dataset = c("BRFSS", "NSGA", "BRFSS", "NSGA"),
  IRR  = c(1.27, 1.44, 1.02, 1.04),    
  low  = c(1.17, 1.19, 0.95, 0.87),    
  high = c(1.36, 1.75, 1.09, 1.25)     
)
fig_dat$bar_name <- c("Mental — BRFSS", "Mental — NSGA",
                      "Physical — BRFSS", "Physical — NSGA")

p <- ggplot(fig_dat, aes(x = IRR, y = outcome, color = dataset)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(aes(xmin = low, xmax = high),
                 position = position_dodge(width = 0.5), height = 0.2) +
  labs(title = "Caregiver Associations with Unhealthy Days by Dataset",
       x = "Adjusted IRR (95% CI)", y = NULL, color = "Sample") +
  theme_minimal(base_size = 13)

fig_dat$bar_name <- c("Mental — BRFSS", "Mental — NSGA",
                      "Physical — BRFSS", "Physical — NSGA")

p <- ggplot(fig_dat, aes(x = IRR, y = bar_name, color = dataset)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = low, xmax = high), height = 0.2) +
  labs(title = "Caregiver Associations with Unhealthy Days by Dataset",
       x = "Adjusted IRR, Caregiver vs. non-caregiver, (95% CI)", y = NULL, color = "Dataset") +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

ggsave("figure2.png", p, width = 8, height = 5, dpi = 300)