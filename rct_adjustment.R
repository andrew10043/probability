library(tidyverse)
library(foreach)
library(doParallel)
library(parallel)
library(cowplot)

#### Population Generation -----------------------------------------------------
set.seed(007)

# Population size
n <- 1e6 

# Vector of causal effects for 10 variables
CY <-  runif(10, min = 0.1, max = 0.5) * 
  sample(x = c(-1, 1), size = 10, replace = TRUE) 

# Generate population
df <-
  tibble(
    C1 = rnorm(n),
    C2 = rnorm(n),
    C3 = rnorm(n),
    C4 = rnorm(n),
    C5 = rnorm(n),
    C6 = rnorm(n),
    C7 = rnorm(n),
    C8 = rnorm(n),
    C9 = rnorm(n),
    C10 = rnorm(n),
    N1 = rnorm(n),
    N2 = rnorm(n),
    N3 = rnorm(n),
    N4 = rnorm(n),
    N5 = rnorm(n),
    N6 = rnorm(n),
    N7 = rnorm(n),
    N8 = rnorm(n),
    N9 = rnorm(n),
    N10 = rnorm(n),
    Y = rnorm(n, C1 * CY[1] + C2 * CY[2] + C3 * CY[3] +  
                 C4 * CY[4] + C5 * CY[5] + C6 * CY[6] + 
                 C7 * CY[7] + C8 * CY[8] + C9 * CY[9] + 
                 C10 * CY[10])
  )

#### Population Plot -----------------------------------------------------------
cor_plot <- 
  ggplot(
    data = df %>% sample_n(size = 50000) %>%
      gather(key = "key", value = "value", -Y) %>%
      mutate(key = factor(key, levels = c("N1", "N2", "N3", "N4",  "N5", "N6", 
                                          "N7", "N8", "N9", "N10", "C1", "C2", 
                                          "C3", "C4", "C5", "C6", "C7", "C8", 
                                          "C9", "C10"))),
    aes(x = value,
        y = Y)
  ) + 
  geom_point(alpha = 0.05) + 
  geom_smooth(method = "lm", color = "blue") + 
  facet_wrap(~ key) +
  labs(x = "Variable Value",
       y = "Outcome Value (Y)") + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    strip.background = element_rect(fill = "white",
                                    color = "white"),
    strip.text = element_text(face = "bold")
  )

#### Null Simulation -----------------------------------------------------------

# Simulate sample from the population without any intervention
total_samples <- 5000
sample_size <- 500

# Function to test differences in variables and record sample number
t_test <- function(v) {
  tibble(
    variable = v,
    p = t.test(random_sample[, v] ~ random_sample$group, alternative = "two.sided",
               var.equal = TRUE)$p.value,
    sample = s
  )
}

var_list <- c("N1", "N2", "N3", "N4", "N5", "N6", "N7", "N8",  "N9", "N10", 
              "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8", "C9", "C10",
              "Y")

# Run in parallel
n_cores <- detectCores()
clust <- makeCluster(n_cores)
doParallel::registerDoParallel(clust, cores = 4)

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
total_col <- "#006200"
N_col <- "#970800"
C_col <- "#55068B"

null_sim_cov <- 
  null_sim %>% 
  filter(variable != "Y") %>%
  mutate(type = if_else(variable %in% c("N1", "N2", "N3", "N4", "N5", 
                                        "N6", "N7", "N8",  "N9", "N10"),
                        "N", "C"))

p_less_t <- sum(null_sim_cov$p < 0.05)
p_less_perc_t <- round(p_less_t / nrow(null_sim_cov) * 100, 2)
total_p_t <- nrow(null_sim_cov)
p_label_t <- paste0(paste0(paste0(paste0(paste0(p_less_t, "/"), total_p_t), " ("), 
                           p_less_perc_t), "%) p-values < 0.05")

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
           x = 0.07, 
           y = 500,
           hjust = 0,
           fontface = "bold",
           size = 3) + 
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 525)) + 
  labs(
    x = "p-value",
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
p_label_n <- paste0(paste0(paste0(paste0(paste0(p_less_n, "/"), total_p_n), " ("), 
                p_less_perc_n), "%) p-values < 0.05")

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
           x = 0.07, 
           y = 500,
           hjust = 0,
           fontface = "bold",
           size = 3) + 
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 525)) + 
  labs(
    x = "p-value",
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
p_label_c <- paste0(paste0(paste0(paste0(paste0(p_less_c, "/"), total_p_c), " ("), 
                           p_less_perc_c), "%) p-values < 0.05")

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
           x = 0.07, 
           y = 500,
           hjust = 0,
           fontface = "bold",
           size = 3) + 
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 525)) + 
  labs(
    x = "p-value",
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
    x = "Number of p-values < 0.05",
    y = "Number of Samples",
    subtitle = "All Variables"
  ) + 
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(-0.5, 5),
                  ylim = c(0, 3000)) + 
  scale_x_continuous(breaks = c(0:5),
                     labels = c(0:5)) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none"
  )
  
null_p_dist_n <-
  ggplot(
    data = dist_data_n,
    aes(x = n_p)
  ) + 
  geom_histogram(binwidth = 1,
                 fill = N_col,
                 color = "black") + 
  labs(
    x = "Number of p-values < 0.05",
    y = "Number of Samples",
    subtitle = "Non-Predictor Variables"
  ) + 
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(-0.5, 5),
                  ylim = c(0, 3000)) + 
  scale_x_continuous(breaks = c(0:5),
                     labels = c(0:5)) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none"
  )

null_p_dist_c <-
  ggplot(
    data = dist_data_n,
    aes(x = n_p)
  ) + 
  geom_histogram(binwidth = 1,
                 fill = C_col,
                 color = "black") + 
  labs(
    x = "Number of p-values < 0.05",
    y = "Number of Samples",
    subtitle = "Predictor Variables"
  ) + 
  scale_y_continuous(expand = c(0, 0)) + 
  coord_cartesian(xlim = c(-0.5, 5),
                  ylim = c(0, 3000)) + 
  scale_x_continuous(breaks = c(0:5),
                     labels = c(0:5)) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none"
  )

null_dist_grid <- plot_grid(null_p_dist_t,
                            null_p_dist_n,
                            null_p_dist_c,
                            align = "hv",
                            ncol = 3)

# Plot assoc. of Y significant diff vs. covariate significant diffs
y_samples <- 
  null_sim %>%
  filter(variable == "Y") %>%
  mutate(p_y = p)

sig_y_dist <- 
  null_sim %>% 
  left_join(y_samples %>% select(sample, p_y), by = "sample") %>%
  filter(variable != "Y") %>%
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
  geom_text(aes(x = sig_T, y = sig_y + 0.02,
                label = paste0(paste0(n_sig, "/"), n)),
            fontface = "bold",
            family = "Gill Sans MT",
            size = 2.25) + 
  labs(
    x = "Number of Imbalanced Variables (Total)",
    y = "Proportion of Samples with Imbalanced Outcome",
    subtitle = "All Variables"
  ) + 
  scale_x_continuous(breaks = 0:6, labels = 0:6) + 
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     limits = c(0, 1.1), expand = c(0, 0)) + 
  coord_cartesian(xlim = c(-0.5, 6.5)) + 
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
  geom_text(aes(x = sig_N, y = sig_y + 0.02,
                label = paste0(paste0(n_sig, "/"), n)),
            fontface = "bold",
            family = "Gill Sans MT",
            size = 2.25) + 
  labs(
    x = "Number of Imbalanced Non-Predictor Variables",
    y = "Proportion of Samples with Imbalanced Outcome",
    subtitle = "Non-Predictor Variables"
  ) + 
  scale_x_continuous(breaks = 0:6, labels = 0:6) + 
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     limits = c(0, 1.1), expand = c(0, 0)) + 
  coord_cartesian(xlim = c(-0.5, 6.5)) + 
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
  geom_text(aes(x = sig_C, y = sig_y + 0.02,
                label = paste0(paste0(n_sig, "/"), n)),
            fontface = "bold",
            family = "Gill Sans MT",
            size = 2.25) + 
  labs(
    x = "Number of Imbalanced Predictor Variables",
    y = "Proportion of Samples with Imbalanced Outcome",
    subtitle = "Predictor Variables"
  ) + 
  scale_x_continuous(breaks = 0:6, labels = 0:6) + 
  scale_y_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     limits = c(0, 1.1), expand = c(0, 0)) + 
  coord_cartesian(xlim = c(-0.5, 6.5)) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none"
  )

sig_y_grid <- plot_grid(total_sig_y, n_sig_y, c_sig_y,
                        align = "hv", ncol = 3)

#### Treatment and Model Simulation --------------------------------------------
