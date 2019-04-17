library(tidyverse)
library(foreach)
library(doParallel)
library(parallel)
library(cowplot)
library(tidybayes)
library(Hmisc)

#### Load Simulated Data Necessary for Figures
load("~/Desktop/academic/research/other/probability/rct_adjust_sim_data.RData")

#### Population Generation -----------------------------------------------------
set.seed(001)

# Population size
n <- 5e6

# Vector of causal effects for 10 variables
CY <-  c(
  runif(3, min = 0.4, max = 0.5) *
    sample(x = c(-1, 1), size = 3, replace = TRUE),
  runif(7, min = 0.15, max = 0.3) *
    sample(x = c(-1, 1), size = 7, replace = TRUE))
  
# Generate population
df <-
  tibble(
    C1 = rnorm(n), C2 = rnorm(n), C3 = rnorm(n), C4 = rnorm(n), C5 = rnorm(n),
    C6 = rnorm(n), C7 = rnorm(n), C8 = rnorm(n), C9 = rnorm(n), C10 = rnorm(n),
    N1 = rnorm(n), N2 = rnorm(n), N3 = rnorm(n), N4 = rnorm(n), N5 = rnorm(n),
    N6 = rnorm(n), N7 = rnorm(n), N8 = rnorm(n), N9 = rnorm(n), N10 = rnorm(n),
    Y = rnorm(n, C1 * CY[1] + C2 * CY[2] + C3 * CY[3] + C4 * CY[4] + 
                 C5 * CY[5] + C6 * CY[6] + C7 * CY[7] + C8 * CY[8] + 
                 C9 * CY[9] + C10 * CY[10]),
    Y_no = (Y - mean(Y)) / sd(Y),
    Y_trt = Y_no + 0.25
  )

# Estimated sample size for beta 0.8 and alpha 0.05 = 502

#### Population Plot -----------------------------------------------------------
total_col <- "#006200"
N_col <- "#970800"
C_col <- "#55068B"

cor_plt <- 
  ggplot() + 
  geom_point(data = df %>% sample_n(size = 1000) %>%
               gather(key = "key", value = "value", -Y_no, -Y_trt, -Y) %>%
               mutate(key = factor(key, levels = c("N1", "N2", "N3", "N4",  "N5",
                                                   "N6", "N7", "N8", "N9", "N10",
                                                   "C1", "C2", "C3", "C4", "C5",
                                                   "C6", "C7", "C8", "C9", "C10")),
                      type = if_else(key %in% c("N1", "N2", "N3", "N4",  "N5", 
                                                "N6", "N7", "N8", "N9", "N10"),
                                     "N", "C"),
                      type = factor(type, levels = c("N", "C"))),
             aes(x = value, y = Y_no, color = type),
             shape = 19, alpha = 0.5) + 
  geom_smooth(data = df %>% sample_n(size = 50000) %>%
                gather(key = "key", value = "value", -Y_no, -Y_trt, -Y) %>%
                mutate(key = factor(key, levels = c("N1", "N2", "N3", "N4",  "N5",
                                                    "N6", "N7", "N8", "N9", "N10",
                                                    "C1", "C2", "C3", "C4", "C5",
                                                    "C6", "C7", "C8", "C9", "C10"))),
              aes(x = value, y = Y_no),
              method = "lm", color = "black") + 
  scale_color_manual(breaks = c("N", "C"),
                     values = c(N_col, C_col)) + 
  facet_wrap(~ key) +
  labs(x = "Variable Value",
       y = "Outcome Value (Y)") + 
  scale_x_continuous(breaks = c(-2.5, 0, 2.5),
                     limits = c(-3, 3)) + 
  scale_y_continuous(breaks = c(-2.5, 0, 2.5),
                     limits = c(-3, 3)) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    strip.background = element_rect(fill = "white", color = "white"),
    strip.text = element_text(face = "bold", size = 10),
    axis.title = element_text(size = 15),
    legend.position = "none"
  )

cor_title <- ggdraw() + 
  draw_label("Baseline Associations Between Outcome and Non-Predictors (N) & Predictors (C)\nUnder No Treatment in the Full Population",
             fontface = 'bold', fontfamily = "Gill Sans MT",
             size = 16.5)

cor_grid <- plot_grid(cor_title, cor_plt, ncol = 1, rel_heights = c(0.1, 1))

cor_caption <- ggdraw() + 
  draw_label("Graphic by Ben Andrew | @BenYAndrew",
             fontfamily = "Gill Sans MT",
             size = 10,
             hjust = -1)

cor_grid_b <- plot_grid(cor_grid, cor_caption, ncol = 1, 
                         rel_heights = c(1, 0.05)) + 
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))

ggsave("figures/rct_adjust_1.jpeg", cor_grid_b,
       height = 10, width = 10, device = "jpeg")

#### Null Simulation -----------------------------------------------------------

# Simulate sample from the population without any intervention

# Function to test differences in variables and record sample number
t_test <- function(v) {
  
  out <- t.test(random_sample[, v] ~ random_sample$group, 
                alternative = "two.sided", var.equal = TRUE)
  
  tibble(
    variable = v,
    p = out$p.value,
    sample = s,
    diff = out$estimate[1] - out$estimate[2]
  )
}

var_list <- c("N1", "N2", "N3", "N4", "N5", "N6", "N7", "N8",  "N9", "N10", 
              "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10",
              "Y_no")

# Run in parallel
n_cores <- detectCores()
clust <- makeCluster(n_cores)
doParallel::registerDoParallel(clust, cores = 4)

total_samples <- 10000
sample_size <- 500

null_sim <- 
  foreach(s = 1:total_samples, .packages = c("tidyverse"),
          .combine = "rbind") %dopar% {
            
    set.seed(s)
    
    random_sample <- 
      as.data.frame(
        sample_n(df, size = sample_size,          # Random sample
                 replace = FALSE) %>%
        mutate(group = if_else(row_number() %in%  # Randomly assign group
                               sample(x = 1:sample_size, 
                                      size = sample_size / 2, 
                                      replace = FALSE),
                               1, 0))
      )
    
    map_df(.x = var_list, 
           .f = t_test)
  
  }

# Stop cluster after simulation
stopCluster(clust)

#### Null Simulation Plots -----------------------------------------------------
null_sim_cov <- 
  null_sim %>% 
  filter(!variable %in% c("Y_no")) %>%
  mutate(type = if_else(variable %in% c("N1", "N2", "N3", "N4", "N5", 
                                        "N6", "N7", "N8",  "N9", "N10"),
                        "N", "C"))

p_less_t <- sum(null_sim_cov$p < 0.05)
p_less_perc_t <- round(p_less_t / nrow(null_sim_cov) * 100, 2)
total_p_t <- nrow(null_sim_cov)
p_label_t <- paste0(paste0(paste0(paste0(paste0(p_less_t, " / "), total_p_t), " ("), 
                           p_less_perc_t), "%) P-values < 0.05")

null_p_plot_total <-
  ggplot(
    data = null_sim_cov %>% 
      mutate(low = if_else(p < 0.05, 1, 0)),
    aes(x = p)) + 
  geom_histogram(aes(fill = factor(low)),
                 binwidth = 0.004) + 
  geom_vline(xintercept = 0.05, linetype = "dashed", size = 0.75) + 
  scale_fill_manual(values = c(total_col, "#EB9300")) +  
  annotate(geom = "text",
           label = p_label_t,
           x = 0.08, 
           y = 950,
           hjust = 0,
           fontface = "bold",
           size = 3) + 
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1000)) + 
  labs(
    x = "P-value",
    y = "Count",
    subtitle = "All Variables"
  ) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none"
  )

null_cov_n <-
  null_sim_cov %>%
  filter(type == "N")

p_less_n <- sum(null_cov_n$p < 0.05)
p_less_perc_n <- round(p_less_n / nrow(null_cov_n) * 100, 2)
total_p_n <- nrow(null_cov_n)
p_label_n <- paste0(paste0(paste0(paste0(paste0(p_less_n, " / "), total_p_n), " ("), 
                p_less_perc_n), "%) P-values < 0.05")

null_p_plot_n <-
  ggplot(
    data = null_sim_cov %>% 
           mutate(low = if_else(p < 0.05, 1, 0)) %>%
           filter(type == "N"),
    aes(x = p)) + 
  geom_histogram(aes(fill = factor(low)),
                 binwidth = 0.004) + 
  geom_vline(xintercept = 0.05, linetype = "dashed", size = 0.75) + 
  scale_fill_manual(values = c(N_col, "#EB9300")) +  
  annotate(geom = "text",
           label = p_label_n,
           x = 0.08, 
           y = 950,
           hjust = 0,
           fontface = "bold",
           size = 3) + 
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1000)) + 
  labs(
    x = "P-value",
    y = "Count",
    subtitle = "Non-Predictor Variables"
  ) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none"
  )

null_cov_c <-
  null_sim_cov %>%
  filter(type == "C")

p_less_c <- sum(null_cov_c$p < 0.05)
p_less_perc_c <- round(p_less_c / nrow(null_cov_c) * 100, 2)
total_p_c <- nrow(null_cov_c)
p_label_c <- paste0(paste0(paste0(paste0(paste0(p_less_c, " / "), total_p_c), " ("), 
                           p_less_perc_c), "%) P-values < 0.05")

null_p_plot_c <-
  ggplot(
    data = null_sim_cov %>% 
      mutate(low = if_else(p < 0.05, 1, 0)) %>%
      filter(type == "N"),
    aes(x = p)) + 
  geom_histogram(aes(fill = factor(low)),
                 binwidth = 0.004) + 
  geom_vline(xintercept = 0.05, linetype = "dashed", size = 0.75) + 
  scale_fill_manual(values = c(C_col, "#EB9300")) +  
  annotate(geom = "text",
           label = p_label_c,
           x = 0.08, 
           y = 950,
           hjust = 0,
           fontface = "bold",
           size = 3) + 
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 1000)) + 
  labs(
    x = "P-value",
    y = "Count",
    subtitle = "Predictor Variables"
  ) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none"
  )

null_p_grid <- plot_grid(null_p_plot_total,
                         null_p_plot_n, null_p_plot_c,
                         ncol = 3, align = "hv")

null_p_title <- ggdraw() + 
  draw_label("P-values for Comparison of Variables between Randomized Groups in 10,000 Samples (n = 500) Under No Treatment",
             fontface = 'bold', fontfamily = "Gill Sans MT",
             size = 13.5)

null_p_grid_b <- plot_grid(null_p_title, null_p_grid, ncol = 1, 
                           rel_heights = c(0.1, 1))

null_p_caption <- ggdraw() + 
  draw_label("Graphic by Ben Andrew | @BenYAndrew",
             fontfamily = "Gill Sans MT",
             size = 8,
             hjust = -1.8)

null_p_grid_c <- plot_grid(null_p_grid_b, null_p_caption, ncol = 1,
                           rel_heights = c(1, 0.05)) + 
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))

ggsave("figures/rct_adjust_2.jpeg", null_p_grid_c,
       height = 6, width = 11, device = "jpeg")

# Plot distribution of number of p values < 0.05 in each sample
dist_data_t <- null_sim_cov %>% 
  group_by(sample) %>% 
  summarise(n_p = sum(p < 0.05))

null_p_dist_t <-
  ggplot(
    data = dist_data_t,
    aes(x = n_p)
  ) + 
  geom_histogram(binwidth = 1,
                 fill = total_col,
                 color = "black") + 
  labs(
    x = "Number of P-values < 0.05",
    y = "Number of Samples",
    subtitle = "All Variables"
  ) + 
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(-0.5, 5.5),
                  ylim = c(0, 6500)) + 
  scale_x_continuous(breaks = c(0:5),
                     labels = c(0:5)) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none"
  )
  
dist_data_n <- null_cov_n %>% 
  group_by(sample) %>% 
  summarise(n_p = sum(p < 0.05))

null_p_dist_n <-
  ggplot(
    data = dist_data_n,
    aes(x = n_p)
  ) + 
  geom_histogram(binwidth = 1,
                 fill = N_col,
                 color = "black") + 
  labs(
    x = "Number of P-values < 0.05",
    y = "Number of Samples",
    subtitle = "Non-Predictor Variables"
  ) + 
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(-0.5, 5.5),
                  ylim = c(0, 6500)) + 
  scale_x_continuous(breaks = c(0:5),
                     labels = c(0:5)) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none"
  )

dist_data_c <- null_cov_c %>% 
  group_by(sample) %>% 
  summarise(n_p = sum(p < 0.05))

null_p_dist_c <-
  ggplot(
    data = dist_data_c,
    aes(x = n_p)
  ) + 
  geom_histogram(binwidth = 1,
                 fill = C_col,
                 color = "black") + 
  labs(
    x = "Number of P-values < 0.05",
    y = "Number of Samples",
    subtitle = "Predictor Variables"
  ) + 
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(xlim = c(-0.5, 5.5),
                  ylim = c(0, 6500)) + 
  scale_x_continuous(breaks = c(0:5),
                     labels = c(0:5)) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none"
  )

null_dist_grid <- plot_grid(null_p_dist_t, null_p_dist_n, null_p_dist_c,
                            align = "hv", ncol = 3)

null_dist_title <- ggdraw() + 
  draw_label("Distribution of Variable Imbalance Between Randomized Groups in 10,000 Samples (n = 500) Under No Treatment",
             fontface = 'bold', fontfamily = "Gill Sans MT", size = 12)

null_dist_grid_b <- plot_grid(null_dist_title, null_dist_grid, 
                              ncol = 1, rel_heights = c(0.1, 1))

null_caption <- ggdraw() + 
  draw_label("Graphic by Ben Andrew | @BenYAndrew",
             fontfamily = "Gill Sans MT",
             size = 8,
             hjust = -1.5)

null_dist_grid_c <- plot_grid(null_dist_grid_b, null_caption, ncol = 1, 
                              rel_heights = c(1, 0.05)) + 
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))

ggsave("figures/rct_adjust_3.jpeg", null_dist_grid_c,
       height = 6, width = 10, device = "jpeg")

# Plot assoc. of Y significant diff vs. covariate significant diffs
y_samples <- 
  null_sim %>%
  filter(variable == "Y_no") %>%
  mutate(p_y = p)

sig_y_dist <- 
  null_sim %>% 
  left_join(y_samples %>% select(sample, p_y), by = "sample") %>%
  filter(variable != "Y_no") %>%
  mutate(type = if_else(variable %in% c("N1", "N2", "N3", "N4", "N5", 
                                        "N6", "N7", "N8",  "N9", "N10"),
                        "N", "C"),
         sig = if_else(p < 0.05, 1, 0)) %>%
  select(-variable) %>%
  group_by(sample, type) %>%
  summarise(
    sig_n = sum(sig),
    p_y = mean(p_y)
  ) %>%
  spread(key = type, value = sig_n) %>%
  ungroup() %>%
  rename(sig_C = C,
         sig_N = N) %>%
  mutate(sig_T = sig_C + sig_N)

total_sig_y <-
  ggplot(
    data = sig_y_dist %>% group_by(sig_T) %>% 
      summarise(sig_y = mean(p_y < 0.05),
                n = n(),
                n_sig = sum(p_y < 0.05)),
    aes(x = sig_T,
        y = sig_y)
  ) + 
  geom_bar(stat = "identity",
           fill = total_col) + 
  geom_text(aes(x = sig_T, y = sig_y + 0.01,
                label = paste0(paste0(n_sig, "/"), n)),
            fontface = "bold",
            family = "Gill Sans MT",
            size = 2.25) + 
  labs(
    x = "Number of Imbalanced Variables",
    y = "Proportion of Samples with Imbalanced Outcome",
    subtitle = "All Variables"
  ) + 
  scale_x_continuous(breaks = 0:5, labels = 0:5) + 
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25),
                     limits = c(0, 0.3), expand = c(0, 0)) + 
  coord_cartesian(xlim = c(-0.5, 5.5),
                  ylim = c(0, 0.3)) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none"
  )

n_sig_y <-
  ggplot(
    data = sig_y_dist %>% group_by(sig_N) %>% 
      summarise(sig_y = mean(p_y < 0.05),
                n = n(),
                n_sig = sum(p_y < 0.05)),
    aes(x = sig_N,
        y = sig_y)
  ) + 
  geom_bar(stat = "identity",
           fill = N_col) + 
  geom_text(aes(x = sig_N, y = sig_y + 0.01,
                label = paste0(paste0(n_sig, "/"), n)),
            fontface = "bold",
            family = "Gill Sans MT",
            size = 2.25) + 
  labs(
    x = "Number of Imbalanced Variables",
    y = "Proportion of Samples with Imbalanced Outcome",
    subtitle = "Non-Predictor Variables"
  ) + 
  scale_x_continuous(breaks = 0:5, labels = 0:5) + 
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25),
                     limits = c(0, 0.3), expand = c(0, 0)) + 
  coord_cartesian(xlim = c(-0.5, 5.5),
                  ylim = c(0, 0.3)) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none"
  )

c_sig_y <-
  ggplot(
    sig_y_dist %>% group_by(sig_C) %>% 
      summarise(sig_y = mean(p_y < 0.05),
                n = n(),
                n_sig = sum(p_y < 0.05)),
    aes(x = sig_C,
        y = sig_y)
  ) + 
  geom_bar(stat = "identity",
           fill = C_col) + 
  geom_text(aes(x = sig_C, y = sig_y + 0.01,
                label = paste0(paste0(n_sig, "/"), n)),
            fontface = "bold",
            family = "Gill Sans MT",
            size = 2.25) + 
  labs(
    x = "Number of Imbalanced Variables",
    y = "Proportion of Samples with Imbalanced Outcome",
    subtitle = "Predictor Variables"
  ) + 
  scale_x_continuous(breaks = 0:5, labels = 0:5) + 
  scale_y_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25),
                     limits = c(0, 0.3), expand = c(0, 0)) + 
  coord_cartesian(xlim = c(-0.5, 5.5),
                  ylim = c(0, 0.3)) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none"
  )

sig_y_grid <- plot_grid(total_sig_y, n_sig_y, c_sig_y,
                        align = "hv", ncol = 3)

sig_y_title <- ggdraw() + 
  draw_label("Relationship Between Variable Imbalance and Outcome Imbalance in 10,000 Samples (n = 500) Under No Treatment",
             fontface = 'bold', fontfamily = "Gill Sans MT", size = 13)

sig_y_grid_b <- plot_grid(sig_y_title, sig_y_grid, ncol = 1, 
                          rel_heights = c(0.1, 1))

sig_y_caption <- ggdraw() + 
  draw_label("Graphic by Ben Andrew | @BenYAndrew",
             fontfamily = "Gill Sans MT",
             size = 8, hjust = -1.8)

sig_y_grid_c <- plot_grid(sig_y_grid_b, sig_y_caption, ncol = 1, 
                          rel_heights = c(1, 0.05)) + 
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))

ggsave("figures/rct_adjust_4.jpeg", sig_y_grid_c,
       height = 6, width = 11, device = "jpeg")

# Plots of mean variable P-value vs. Y p-value
p_y_dist <- 
  null_sim %>% 
  left_join(y_samples %>% select(sample, p_y), by = "sample") %>%
  filter(variable != "Y_no") %>%
  mutate(type = if_else(variable %in% c("N1", "N2", "N3", "N4", "N5", 
                                        "N6", "N7", "N8",  "N9", "N10"),
                        "N", "C")) %>% 
  select(-variable) %>%
  group_by(sample, type) %>%
  summarise(
    p_v = mean(p),
    p_y = mean(p_y)
  ) 

c_pv_y <-
  ggplot(
    p_y_dist %>% filter(type == "C"),
    aes(x = p_v,
        y = p_y)
  ) + 
  geom_point(color = C_col,
             alpha = 0.2) + 
  geom_smooth(method = "lm",
              color = "black") + 
  labs(
    x = "Mean of Variable P-Values",
    y = "Outcome P-Value",
    subtitle = "Predictor Variables"
  ) + 
  scale_x_continuous(breaks = c(0.25, 0.5, 0.75),
                     limits = c(0.15, 0.85)) + 
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     limits = c(0, 1)) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none"
  )

n_pv_y <-
  ggplot(
    p_y_dist %>% filter(type == "N"),
    aes(x = p_v,
        y = p_y)
  ) + 
  geom_point(color = N_col,
             alpha = 0.2) + 
  geom_smooth(method = "lm",
              color = "black") + 
  labs(
    x = "Mean of Variable P-Values",
    y = "Outcome P-Value",
    subtitle = "Non-Predictor Variables"
  ) + 
  scale_x_continuous(breaks = c(0.25, 0.5, 0.75),
                     limits = c(0.15, 0.85)) + 
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     limits = c(0, 1)) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none"
  )

t_pv_y <-
  ggplot(
    p_y_dist,
    aes(x = p_v,
        y = p_y)
  ) + 
  geom_point(color = total_col,
             alpha = 0.2) + 
  geom_smooth(method = "lm",
              color = "black") + 
  labs(
    x = "Mean of Variable P-Values",
    y = "Outcome P-Value",
    subtitle = "All Variables"
  ) + 
  scale_x_continuous(breaks = c(0.25, 0.5, 0.75),
                     limits = c(0.15, 0.85)) + 
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     limits = c(0, 1)) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none"
  )

pv_grid <- plot_grid(t_pv_y, n_pv_y, c_pv_y,
                        align = "hv", ncol = 3)

pv_title <- ggdraw() + 
  draw_label("Relationship Between Variable Imbalance and Outcome Imbalance in 10,000 Samples (n = 500) Under No Treatment",
             fontface = 'bold', fontfamily = "Gill Sans MT", size = 13)

pv_grid_b <- plot_grid(pv_title, pv_grid, ncol = 1, rel_heights = c(0.1, 1))

pv_caption <- ggdraw() + 
  draw_label("Graphic by Ben Andrew | @BenYAndrew",
             fontfamily = "Gill Sans MT",
             size = 8, hjust = -1.8)

pv_grid_c <- plot_grid(pv_grid_b, pv_caption, ncol = 1, 
                       rel_heights = c(1, 0.05)) + 
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))

ggsave("figures/rct_adjust_5.jpeg", pv_grid_c,
       height = 6, width = 11, device = "jpeg")

#### Treatment and Model Simulation --------------------------------------------
adjustment_list <- var_list[-which(var_list %in% c("Y_no", "Y_trt", "Y"))]
c_list <- c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10")

# Function to find imbalanced variables
find_imbalance <- function() {
  
  out <- character()
  
  for (i in adjustment_list){
    
    if (t.test(random_sample[, i] ~ random_sample$group,
               var.equal = TRUE, alternative = "two.sided")$p.value < 0.05) {
      out <- c(out, i)
    }
    
  }
  
  if (length(out) == 0){
    form_all <- "Y_assign ~ group"
    form_c <- "Y_assign ~ group"
  } else {
    form_all <- paste0("Y_assign ~ group + ", paste(out, collapse = " + "))
    if (length(out[which(out %in% c_list)]) == 0){
      form_c <- "Y_assign ~ group"
    } else {
      form_c <- paste0("Y_assign ~ group + ", paste(out[which(out %in% c_list)], collapse = " + "))
    }
  }
  
  return(list(formula(form_all), 
              formula(form_c)))
  
}

# Model choice function

model_choice <- function(s, sample_size) {

  # No adjustment
  no_res <- lm(Y_assign ~ group, data = random_sample)
  no_p <- summary(no_res)$coefficients["group", "Pr(>|t|)"]
  no_se <- summary(no_res)$coefficients["group", "Std. Error"] 
  no_est <- summary(no_res)$coefficients["group", "Estimate"] 

  # Adjust for all variables
  all_res <- lm(Y_assign ~ group + C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10 + 
                  N1 + N2 + N3 + N4 + N5 + N6 + N7 + N8 + N9 + N10, data = random_sample)
  all_p <- summary(all_res)$coefficients["group", "Pr(>|t|)"]
  all_se <- summary(all_res)$coefficients["group", "Std. Error"]  
  all_est <- summary(all_res)$coefficients["group", "Estimate"] 
  
  # Adjust for all predictors
  c_res <- lm(Y_assign ~ group + C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10,
              data = random_sample)
  c_p <- summary(c_res)$coefficients["group", "Pr(>|t|)"]
  c_se <- summary(c_res)$coefficients["group", "Std. Error"]  
  c_est <- summary(c_res)$coefficients["group", "Estimate"] 
  
  # Adjust for tope 3 predictors
  top_res <- lm(Y_assign ~ group + C1 + C2 + C3,
              data = random_sample)
  top_p <- summary(c_res)$coefficients["group", "Pr(>|t|)"]
  top_se <- summary(c_res)$coefficients["group", "Std. Error"]  
  top_est <- summary(c_res)$coefficients["group", "Estimate"] 
  
  # Find imbalances
  imbal <- find_imbalance()
  
  # Adjust for imbalanced variables
  imbal_res <- lm(formula = imbal[[1]],
                  data = random_sample)
  imbal_p <- summary(imbal_res)$coefficients["group", "Pr(>|t|)"]
  imbal_se <- summary(imbal_res)$coefficients["group", "Std. Error"] 
  imbal_est <- summary(imbal_res)$coefficients["group", "Estimate"] 
  
  # Adjust for imbalanced predictors
  pi_res <- lm(formula = imbal[[2]],
               data = random_sample)
  pi_p <- summary(pi_res)$coefficients["group", "Pr(>|t|)"]
  pi_se <- summary(pi_res)$coefficients["group", "Std. Error"]
  pi_est <- summary(pi_res)$coefficients["group", "Estimate"] 
  
  # R2 for all predictors base model
  base_all <- lm(Y_assign ~ C1 + C2 + C3 + C4 + C5 + C6 + C7 + C8 + C9 + C10, 
                 data = random_sample)
  base_all_r2 <- summary(base_all)$r.squared
  base_all_adj_r2 <- summary(base_all)$adj.r.squared
  
  # R2 for top three predictors base model
  base_top <- lm(Y_assign ~ C1 + C2 + C3, 
                 data = random_sample)
  base_top_r2 <- summary(base_top)$r.squared
  base_top_adj_r2 <- summary(base_top)$adj.r.squared
  
  
  data.frame(
    sample = s,
    sample_size = sample_size,
    no_est = no_est,
    no_se = no_se,
    no_p = no_p,
    all_est = all_est,
    all_se = all_se,
    all_p = all_p,
    c_est = c_est,
    c_se = c_se,
    c_p = c_p,
    imbal_est = imbal_est,
    imbal_se = imbal_se,
    imbal_p = imbal_p,
    pi_est = pi_est,
    pi_se = pi_se,
    pi_p = pi_p,
    top_est = top_est,
    top_se = top_se,
    top_p = top_p,
    base_all_r2 = base_all_r2,
    base_all_adj_r2 = base_all_adj_r2,
    base_top_r2 = base_top_r2,
    base_top_adj_r2 = base_top_adj_r2
    )
  
}

#### Treatment Simulation ------------------------------------------------------
treat_sim <- function(total_samples, sample_size, condense = "n") {
  
  print(sample_size)
  
  out <- 
    foreach(s = 1:total_samples, .packages = c("tidyverse"),
            .combine = "rbind",
            .export = ls(globalenv())) %dopar% {
              
              set.seed(s)
              
              random_sample <- 
                as.data.frame(
                  sample_n(df, size = sample_size,            # Random sample
                           replace = FALSE) %>%
                    mutate(group = if_else(row_number() %in%  # Randomly assign group
                                             sample(x = 1:sample_size, 
                                                    size = sample_size / 2, 
                                                    replace = FALSE),
                                           1, 0),
                           Y_assign = if_else(group == 1, Y_trt, Y_no)) # Determine Y counterfactual assignment
                )
              
              model_choice(s, sample_size)
              
            }
  
  if (condense == "y") {
    
    out %>% 
      select(sample, sample_size, contains("_p")) %>%
      gather(key = "key", value = "value", -sample, -sample_size) %>%
      group_by(sample_size, key) %>%
      summarise(num_p_low = sum(value < 0.05),
                num_p = n(),
                pwr = num_p_low / num_p) %>%
      ungroup()
    
  } else if (condense == "n") {
    
    out 
    
  }
  
}

# Simulations for plotting
total_samples <- 10000

arg_list <-
  list(total_samples = total_samples,
       sample_size = c(300, 500))

n_cores <- detectCores()
clust <- makeCluster(n_cores)
doParallel::registerDoParallel(clust, cores = 4)

treat_sim_result <-
  pmap_df(arg_list,
          ~ treat_sim(total_samples = ..1,
                      sample_size = ..2))

stopCluster(clust)

treat_sim_result <-
  as_tibble(treat_sim_result)

#### Treatment Plots (n = 300) -------------------------------------------------
facet_labels <- as_labeller(
  c(no_p = "No Adjustment", 
    imbal_p = "All Imbalanced",
    pi_p = "Imbal. Predictors",
    all_p = "All Variables",
    top_p = "Strong Predictors",
    c_p = "All Predictors"))

est_dens_300 <- 
  ggplot(
    data = treat_sim_result %>% 
      filter(sample_size == 300) %>%
      select(sample, contains("_est")) %>% 
      gather(key = "key", value = "value", -sample) %>%
      mutate(key = factor(key, levels = c("no_est", "imbal_est", 
                                          "pi_est", "all_est", "top_est", "c_est"))),
    aes(x = value)
  ) + 
  geom_halfeyeh(aes(y = 1),
                size = 4,
                .width = 0.89,
                fill = "#4848D1") + 
  geom_vline(xintercept = 0.25, linetype = "dashed") + 
  coord_cartesian(xlim = c(-0.2, 0.6),
                  ylim = c(0.95, 2)) + 
  scale_x_continuous(breaks = c(-0.2, 0, 0.2, 0.4, 0.6)) + 
  labs(
    x = expression(paste(hat(beta)[treat], " [median, 89% credible interval]")),
    y = "Density"
  ) + 
  facet_grid(key ~ .) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_text(margin = margin(0, 0, 0, 0, "cm")),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.title.x = element_text(size = 9.5)
)

se_dens_300 <-
  ggplot(
    data = treat_sim_result %>% 
      filter(sample_size == 300) %>%
      select(sample, contains("_se")) %>% 
      gather(key = "key", value = "value", -sample) %>%
      mutate(key = factor(key, levels = c("no_se", "imbal_se", 
                                          "pi_se", "all_se", "top_se", "c_se"))),
    aes(x = value)
  ) + 
  geom_halfeyeh(aes(y = 1),
                size = 4,
                .width = 0.89,
                fill = "#9E4900") + 
  coord_cartesian(xlim = c(0.05, 0.14),
                  ylim = c(0.95, 2)) + 
  scale_x_continuous(breaks = c(0.05, 0.08, 0.11, 0.14)) + 
  labs(
    x = expression(paste(paste("SE of ", hat(beta)[treat]), " [median, 89% credible interval]")),
    y = "Density"
  ) + 
  facet_grid(key ~ .) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_text(margin = margin(0, 0, 0, 0, "cm")),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.title.x = element_text(size = 9.5)
  )

p_dens_300 <-
  ggplot(
    data = treat_sim_result %>% 
      filter(sample_size == 300) %>%
      select(sample, contains("_p")) %>% 
      gather(key = "key", value = "value", -sample) %>%
      mutate(key = factor(key, levels = c("no_p", "imbal_p", 
                                          "pi_p", "all_p", "top_p", "c_p")),
             low = if_else(value < 0.05, 1, 0)),
    aes(x = value)
  ) + 
  geom_histogram(aes(y = ..count.. / total_samples,
                     fill = factor(low)),
                 binwidth = 0.02,
                 center = 0) +
  geom_text(data = treat_sim_result %>% 
              filter(sample_size == 300) %>%
              select(sample, contains("_p")) %>% 
              gather(key = "key", value = "value", -sample) %>%
              mutate(key = factor(key, levels = c("no_p", "imbal_p", 
                                                  "pi_p", "all_p", "top_p", "c_p")),
                     low = if_else(value < 0.05, 1, 0)) %>%
              group_by(key) %>%
              summarise(pwr = mean(low)),
            aes(label = paste("Power:", round(pwr, 3))),
            x = 0.02, y = 0.8,
            hjust = 0,
            size = 3.5,
            fontface = "bold",
            family = "Gill Sans MT") +
  scale_fill_manual(values = c("grey30", "#EB9300")) +  
  labs(
    x = expression(paste("P-value for ", hat(beta)[treat])),
    y = "Proportion of Samples"
  ) + 
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0),
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  scale_x_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3)) +
  coord_cartesian(xlim = c(-0.005, 0.3),
                  ylim = c(0, 0.95)) +
  facet_grid(key ~ ., labeller = facet_labels) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none",
    strip.background = element_rect(fill = "white",
                                    color = "white"),
    strip.text = element_text(face = "bold",
                              size = 9),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.title.x = element_text(size = 9.5)
  )

dens_grid_300 <- plot_grid(est_dens_300, NULL, se_dens_300, NULL, p_dens_300,
                           align = "hv", ncol = 5,
                           rel_widths = c(1, 0, 1, 0, 1)) + 
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))

dens_300_title <- ggdraw() + 
  draw_label("Results of Various Modeling Strategies in 10,000 Random Samples (n = 300)",
             fontface = 'bold', fontfamily = "Gill Sans MT", size = 15)

right_label <- ggdraw() + 
  draw_label("Adjustment Strategy",
             fontface = "bold", fontfamily = "Gill Sans MT",
             size = 12, angle = 270)

dens_300_caption <- ggdraw() + 
  draw_label("Graphic by Ben Andrew | @BenYAndrew",
             fontfamily = "Gill Sans MT",
             size = 7, hjust = -1.3)

dens_grid_300_b <- plot_grid(dens_grid_300, right_label, ncol = 2, 
                             rel_widths = c(1, 0.05))

dens_grid_300_c <- plot_grid(dens_300_title, dens_grid_300_b, ncol = 1, 
                             rel_heights = c(0.1, 1)) + 
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))



dens_grid_300_d <- plot_grid(dens_grid_300_c, dens_300_caption, ncol = 1, 
                             rel_heights = c(1, 0.05)) + 
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))

ggsave("figures/rct_adjust_6.jpeg", dens_grid_300_d,
       height = 8.5, width = 8, device = "jpeg")

#### Treatment Plots (n = 500) -------------------------------------------------
est_dens_500 <- 
  ggplot(
    data = treat_sim_result %>% 
      filter(sample_size == 500) %>%
      select(sample, contains("_est")) %>% 
      gather(key = "key", value = "value", -sample) %>%
      mutate(key = factor(key, levels = c("no_est", "imbal_est", 
                                          "pi_est", "all_est", "top_est", "c_est"))),
    aes(x = value)
  ) + 
  geom_halfeyeh(aes(y = 1),
                size = 4,
                .width = 0.89,
                fill = "#4848D1") + 
  geom_vline(xintercept = 0.25, linetype = "dashed") + 
  coord_cartesian(xlim = c(-0.2, 0.6),
                  ylim = c(0.95, 2)) + 
  scale_x_continuous(breaks = c(-0.2, 0, 0.2, 0.4, 0.6)) + 
  labs(
    x = expression(paste(hat(beta)[treat], " [median, 89% credible interval]")),
    y = "Density"
  ) + 
  facet_grid(key ~ .) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_text(margin = margin(0, 0, 0, 0, "cm")),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.title.x = element_text(size = 9.5)
  )

se_dens_500 <-
  ggplot(
    data = treat_sim_result %>% 
      filter(sample_size == 500) %>%
      select(sample, contains("_se")) %>% 
      gather(key = "key", value = "value", -sample) %>%
      mutate(key = factor(key, levels = c("no_se", "imbal_se", 
                                          "pi_se", "all_se", "top_se", "c_se"))),
    aes(x = value)
  ) + 
  geom_halfeyeh(aes(y = 1),
                size = 4,
                .width = 0.89,
                fill = "#9E4900") + 
  coord_cartesian(xlim = c(0.05, 0.14),
                  ylim = c(0.95, 2)) + 
  scale_x_continuous(breaks = c(0.05, 0.08, 0.11, 0.14)) + 
  labs(
    x = expression(paste(paste("SE of ", hat(beta)[treat]), " [median, 89% credible interval]")),
    y = "Density"
  ) + 
  facet_grid(key ~ .) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none",
    strip.background = element_blank(),
    strip.text = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_text(margin = margin(0, 0, 0, 0, "cm")),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.title.x = element_text(size = 9.5)
  )

p_dens_500 <-
  ggplot(
    data = treat_sim_result %>% 
      filter(sample_size == 500) %>%
      select(sample, contains("_p")) %>% 
      gather(key = "key", value = "value", -sample) %>%
      mutate(key = factor(key, levels = c("no_p", "imbal_p", 
                                          "pi_p", "all_p", "top_p", "c_p")),
             low = if_else(value < 0.05, 1, 0)),
    aes(x = value)
  ) + 
  geom_histogram(aes(y = ..count.. / total_samples,
                     fill = factor(low)),
                 binwidth = 0.02,
                 center = 0) +
  geom_text(data = treat_sim_result %>% 
              filter(sample_size == 500) %>%
              select(sample, contains("_p")) %>% 
              gather(key = "key", value = "value", -sample) %>%
              mutate(key = factor(key, levels = c("no_p", "imbal_p", 
                                                  "pi_p", "all_p", "top_p", "c_p")),
                     low = if_else(value < 0.05, 1, 0)) %>%
              group_by(key) %>%
              summarise(pwr = mean(low)),
            aes(label = paste("Power:", round(pwr, 3))),
            x = 0.02, y = 0.8,
            hjust = 0,
            size = 3.5,
            fontface = "bold",
            family = "Gill Sans MT") +
  scale_fill_manual(values = c("grey30", "#EB9300")) +  
  labs(
    x = expression(paste("P-value for ", hat(beta)[treat])),
    y = "Proportion of Samples"
  ) + 
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0),
                     breaks = c(0, 0.2, 0.4, 0.6, 0.8)) +
  scale_x_continuous(breaks = c(0, 0.05, 0.1, 0.15, 0.2, 0.25, 0.3)) +
  coord_cartesian(xlim = c(-0.005, 0.3),
                  ylim = c(0, 0.95)) +
  facet_grid(key ~ ., labeller = facet_labels) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none",
    strip.background = element_rect(fill = "white",
                                    color = "white"),
    strip.text = element_text(face = "bold",
                              size = 9),
    plot.margin = margin(0, 0, 0, 0, "cm"),
    axis.title.x = element_text(size = 9.5)
  )

dens_grid_500 <- plot_grid(est_dens_500, NULL, se_dens_500, NULL, p_dens_500,
                           align = "hv", ncol = 5,
                           rel_widths = c(1, 0, 1, 0, 1)) + 
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))

dens_500_title <- ggdraw() + 
  draw_label("Results of Various Modeling Strategies in 10,000 Random Samples (n = 500)",
             fontface = 'bold', fontfamily = "Gill Sans MT", size = 15)

right_label <- ggdraw() + 
  draw_label("Adjustment Strategy",
             fontface = "bold", fontfamily = "Gill Sans MT",
             size = 12, angle = 270)

dens_500_caption <- ggdraw() + 
  draw_label("Graphic by Ben Andrew | @BenYAndrew",
             fontfamily = "Gill Sans MT",
             size = 7, hjust = -1.3)

dens_grid_500_b <- plot_grid(dens_grid_500, right_label, ncol = 2, 
                             rel_widths = c(1, 0.05))

dens_grid_500_c <- plot_grid(dens_500_title, dens_grid_500_b, ncol = 1, 
                             rel_heights = c(0.1, 1)) + 
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))

dens_grid_500_d <- plot_grid(dens_grid_500_c, dens_500_caption, ncol = 1, 
                             rel_heights = c(1, 0.05)) + 
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))

ggsave("figures/rct_adjust_7.jpeg", dens_grid_500_d,
       height = 8.5, width = 8, device = "jpeg")

#### Sample Size Simulation ----------------------------------------------------
total_samples <- 1500

arg_list <-
  list(total_samples = total_samples,
       sample_size = c(100, 150, 200, 250, 300, 350, 400, 
                       450, 500, 550, 600, 650, 700),
       condense = "y")

n_cores <- detectCores()
clust <- makeCluster(n_cores)
doParallel::registerDoParallel(clust, cores = 4)

sample_sim_result <-
  pmap_df(arg_list,
          ~ treat_sim(total_samples = ..1,
                      sample_size = ..2,
                      condense = ..3))

stopCluster(clust)

sample_sim <-
  as_tibble(treat_sim)

#### Sample Size Simulation Plots ----------------------------------------------
sample_sim_plot <-
  sample_sim_result %>%
  group_by(key) %>%
  mutate(
    pwr_est = Hmisc::binconf(x = num_p_low, n = num_p, method = "wilson")[ , "PointEst"],
    pwr_upper = Hmisc::binconf(x = num_p_low, n = num_p, method = "wilson")[ , "Upper"],
    pwr_lower = Hmisc::binconf(x = num_p_low, n = num_p, method = "wilson")[ , "Lower"]
  ) %>%
  ungroup () %>%
  mutate(
    key = factor(key, levels = c("c_p", "top_p", "all_p", "pi_p", "imbal_p", "no_p"))
  ) 

pd <- position_dodge(50) 

samp_include <- c(100, 150, 200, 250, 300, 350, 400, 
                  450, 500, 550, 600, 650, 700)

sample_plt <-
  ggplot(data = sample_sim_plot %>%
           filter(sample_size %in% samp_include)) + 
  geom_point(aes(x = sample_size,
                 y = pwr_est,
                 color = key),
             position = pd) +
  geom_errorbar(aes(x = sample_size,
                    ymin = pwr_lower,
                    ymax = pwr_upper,
                    color = key),
                position = pd,
                width = 25) + 
  geom_hline(yintercept = 0.8,
             linetype = "dashed",
             color = "grey80",
             alpha = 0.8) + 
  scale_x_continuous(breaks = samp_include) + 
  scale_y_continuous(breaks = c(0.2, 0.4, 0.6, 0.8, 1)) + 
  scale_color_brewer(type = "qual", palette = "Dark2",
                     breaks = c("c_p", "top_p", "all_p", "pi_p", "imbal_p", "no_p"),
                     labels = c("All Predictors", "Strong Predictors",
                                "All Variables", "Imbalanced Predictors",
                                "All Imbalanced", "No Adjustment"),
                     name = "Adjustment Strategy") + 
  labs(
    x = "Sample Size",
    y = expression(paste(paste(paste("Power", " ("), alpha), " = 0.05)"))
  ) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = c(0.9, 0.3)
  )

sample_title <- ggdraw() + 
  draw_label("Simulated Power vs. Sample Size Using Various Modeling Strategies",
             fontface = 'bold', fontfamily = "Gill Sans MT", size = 15)

sample_caption <- ggdraw() + 
  draw_label("Graphic by Ben Andrew | @BenYAndrew",
             fontfamily = "Gill Sans MT", size = 8, hjust = -1.5)

sample_grid <- plot_grid(sample_title, sample_plt, ncol = 1, 
                         rel_heights = c(0.1, 1)) + 
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))

sample_grid_b <- plot_grid(sample_grid, sample_caption, ncol = 1, 
                           rel_heights = c(1, 0.05)) + 
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))

ggsave("figures/rct_adjust_8.jpeg", sample_grid_b,
       height = 6, width = 10, device = "jpeg")
  
#### Power Ratio Simulation
total_samples <- 500

arg_list <-
  list(total_samples = total_samples,
       sample_size = seq(100, 700, by = 10),
       condense = "n")

n_cores <- detectCores()
clust <- makeCluster(n_cores)
doParallel::registerDoParallel(clust, cores = 4)

ratio_data <-
  pmap_df(arg_list,
          ~ treat_sim(total_samples = ..1,
                      sample_size = ..2,
                      condense = ..3))

stopCluster(clust)

ratio_data_plt <-
  as_tibble(ratio_data) %>%
  select(sample, sample_size, c_p, no_p, base_all_r2) %>%
  mutate(
    base_all_r2_diff = 1 - base_all_r2
  ) %>%
  group_by(sample_size) %>%
  summarise(pwr_no = mean(no_p < 0.05),
            pwr_c = mean(c_p < 0.05),
            base_all_r2_diff = mean(base_all_r2_diff),
            base_all_r2 = mean(base_all_r2)) %>%
  ungroup() %>%
  mutate(
    sample_size_2 = sample_size * base_all_r2_diff
  )
    
adj_col <- "#00D600"
unadj_col <- "#FF7400"
mod_col <- "#D60079"
ann_val <- "$n_{adjusted} =  n_{unadjusted} \\times (1 - R^{2})$"
ann_val_2 <- "$with\\,\ R^{2}\\,\ from\\,\ a\\,\ predictor-only\\,\ model$"

ratio_plt <-
  ggplot(
    data = ratio_data_plt
  ) + 
  stat_smooth(aes(x = sample_size, y = pwr_c),
              color = "grey80",
              alpha = 0.5, size = 5,
              geom = "line",
              method = "loess") +
  geom_segment(aes(x = sample_size, xend = sample_size_2,
                   y = pwr_no, yend = pwr_no),
               color = "black", size = 0.5,
               linetype = "dashed") + 
  geom_point(aes(x = sample_size, y = pwr_c),
             color = adj_col,
             size = 3) + 
  geom_point(aes(x = sample_size, y = pwr_no),
             color = unadj_col,
             size = 3) + 
  geom_point(aes(x = sample_size_2, y = pwr_no),
             color = mod_col,
             size = 3) + 
  annotate(geom = "text",
           label = "Unadjusted Model",
           color = unadj_col,
           x = 250, y = 0.4,
           hjust = 0,
           family = "Gill Sans MT",
           fontface = "bold") + 
  geom_segment(x = 245, xend = 210, y = 0.40, yend = 0.415, 
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.05, "inches")),
               col = unadj_col) + 
  annotate(geom = "text",
           label = "Adjusted Model",
           color = adj_col,
           x = 250, y = 0.875,
           hjust = 1,
           family = "Gill Sans MT",
           fontface = "bold") + 
  geom_segment(x = 255, xend = 290, y = 0.873, yend = 0.858, 
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.05, "inches")),
               col = adj_col) + 
  annotate(geom = "text",
           label = "Extrapolated\nAdjusted Model",
           color = mod_col,
           x = 185, y = 0.8,
           hjust = 1,
           family = "Gill Sans MT",
           fontface = "bold") + 
  geom_segment(x = 190, xend = 220, y = 0.778, yend = 0.755, 
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.05, "inches")),
               col = mod_col) + 
  annotate(geom = "text",
           label = "Adjusted Model\nSmoothed Trend",
           color = "grey50",
           x = 375, y = 1,
           hjust = 1,
           family = "Gill Sans MT",
           fontface = "bold") + 
  geom_segment(x = 380, xend = 410, y = 0.98, yend = 0.96, 
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.05, "inches")),
               col = "grey50") + 
  geom_rect(xmin = 435, xmax = 700, ymin = 0.52, ymax = 0.62,
            linetype = 1, color = "white", fill = "grey") + 
  annotate(geom = "text", label = "For Models with Equal Power:",
           x = 450, y = 0.6,
           hjust = 0, family = "Gill Sans MT",
           fontface = "bold.italic",
           size = 3.5) + 
  annotate(geom = "text", label = TeX(ann_val, output = "character"),
           parse = TRUE, 
           x = 450, y = 0.57, fontface = "bold",
           family = "Gill Sans MT", hjust = 0,
           size = 3.5) + 
  annotate(geom = "text", label = TeX(ann_val_2, output = "character"),
           parse = TRUE,
           x = 450, y = 0.54,
           hjust = 0, family = "Gill Sans MT",
           fontface = "bold",
           size = 3.5) + 
  labs(
    x = "Sample Size",
    y = expression(paste(paste(paste("Power", " ("), alpha), " = 0.05)"))
  ) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    axis.title = element_text(size = 13),
    axis.text = element_text(size = 10)
  )

ratio_title <- ggdraw() + 
  draw_label("Approximation of the Sample Size Needed to Obtain \nEqual Power in an Adjusted vs. Unadjusted Model",
             fontface = 'bold', fontfamily = "Gill Sans MT", size = 15)

ratio_caption <- ggdraw() + 
  draw_label("Graphic by Ben Andrew | @BenYAndrew",
             fontfamily = "Gill Sans MT", size = 8, hjust = -0.75)

ratio_grid <- plot_grid(ratio_title, ratio_plt, ncol = 1, 
                         rel_heights = c(0.1, 1)) + 
  theme(plot.margin = margin(0, 0, 0, 0, "cm"))

ratio_grid_b <- plot_grid(ratio_grid, ratio_caption, ncol = 1, 
                           rel_heights = c(1, 0.05)) + 
  theme(plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))

ggsave("figures/rct_adjust_9.jpeg", ratio_grid_b,
       height = 7, width = 7, device = "jpeg")

