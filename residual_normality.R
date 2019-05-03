library(tidyverse)
library(doParallel)
library(parallel)
library(effsize)
library(RColorBrewer)
library(cowplot)

# Load data for plotting

## load("residual_normality_sim.RData")

# Simulation random number set-up ----------------------------------------------
draw <- 1:1e8

i_seq <- seq(0, 2, by = 0.01)
n_samp <- 1000

random_nums <- replicate(n_samp, 
                         sample(draw, size = length(i_seq)), 
                         simplify = FALSE)

# Simulation function ----------------------------------------------------------
simulate_difference <- function(n) {
  
  out <- 

    foreach(s = 1:n_samp,
            .combine = "rbind") %:%
    foreach(i = 1:length(i_seq), 
            .packages = c("tidyverse", "effsize"),
            .combine = "rbind",
            .export = ls(globalenv())) %dopar% {
              
              set.seed(random_nums[[s]][i])

              df <- 
                tibble(
                  a = c(rnorm(n/2, mean = 0, sd = 1),
                        rnorm(n/2, mean = i_seq[i], sd = 1)),
                  x = rep(c(1, 0), each = n/2)
                )
              
             data.frame(
               s = s,
               i = i,
               resid_sw = shapiro.test(residuals(lm(a ~ x, data = df)))$p.value,
               depen_sw = shapiro.test(df$a)$p.value,
               cd = cohen.d(df$a ~ factor(df$x))$estimate,
               n = n
             )
              
              
            }

}

# Simulation -------------------------------------------------------------------
n_cores <- detectCores()
clust <- makeCluster(n_cores)
doParallel::registerDoParallel(clust, cores = 4)

arg_list <- list(n = c(500, 1000, 2000, 5000))

sim_result <-
  pmap_df(arg_list,
          ~ simulate_difference(n = ..1))

stopCluster(clust)

# Plots ------------------------------------------------------------------------
facet_labels <- as_labeller(
  c(`500` = "n = 500",
    `1000` = "n = 1000",
    `2000` = "n = 2000", 
    `5000` = "n = 5000"))

pal <- brewer.pal(8, name = "Dark2")

plt_1 <- 
  ggplot(
    data = sim_result,
    aes(x = cd,
        y = depen_sw)
  ) + 
  geom_point(data = sim_result %>% sample_frac(0.5),
             color = pal[3],
             shape = 19,
             size = 2,
             alpha = 0.005) + 
  geom_smooth(
    data = sim_result,
    color = "black",
    se = FALSE,
    size = 1
  ) +
  scale_x_continuous(limits = c(0, 2)) + 
  facet_wrap(~ n, labeller = facet_labels) + 
  labs(
    x = "Cohen's d",
    y = "Shapiro-Wilk p-value",
    subtitle = "Dependent Variable"
  ) + 
  geom_hline(yintercept = 0.05,
             linetype = "dashed") +
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey80",
                                    color = "white")
  )

plt_2 <- 
  ggplot(
    data = sim_result,
    aes(x = cd,
        y = resid_sw)
  ) + 
  geom_point(data = sim_result %>% sample_frac(0.5),
             color = pal[4],
             shape = 19,
             size = 2,
             alpha = 0.005) + 
  geom_smooth(
    data = sim_result,
    color = "black",
    se = FALSE,
    size = 1
  ) +
  scale_x_continuous(limits = c(0, 2)) + 
  facet_wrap(~ n, labeller = facet_labels) + 
  labs(
    x = "Cohen's d",
    y = "Shapiro-Wilk p-value",
    subtitle = "Model Residuals"
  ) + 
  geom_hline(yintercept = 0.05,
             linetype = "dashed") +
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey80",
                                    color = "white")
  )

grid_1 <- plot_grid(plt_1, plt_2, align = "hv", ncol = 2)

grid_1_title <- ggdraw() + 
  draw_label("Relationship between Effect Size & Sample Size and Tests of the Normality Assumption",
             fontface = 'bold', fontfamily = "Gill Sans MT", size = 12)

grid_1_b <- plot_grid(grid_1_title, grid_1, ncol = 1, rel_heights = c(0.1, 1))

grid_1_caption <- ggdraw() + 
  draw_label("Graphic by Ben Andrew | @BenYAndrew",
             fontfamily = "Gill Sans MT",
             size = 8, hjust = -1.6)

grid_1_c <- plot_grid(grid_1_b, grid_1_caption, ncol = 1, rel_heights = c(1, 0.05))


ggsave("figures/residual_normality_1.jpeg", grid_1_c,
       height = 6, width = 10, device = "jpeg")

plt_3 <- 
  ggplot(
    data = sim_result %>%
      group_by(round(cd, 3), n) %>%
      summarise(
        prop_depen = mean(depen_sw < 0.05)
      ),
    aes(x = `round(cd, 3)`,
        y = prop_depen)
  ) + 
  geom_point(color = pal[5],
             shape = 19,
             size = 2,
             alpha = 0.05) + 
  geom_smooth(
    color = "black",
    se = FALSE,
    size = 1,
    method = "loess",
    span = 0.2
  ) +
  geom_hline(yintercept = 0.80,
             linetype = "dashed") +
  scale_x_continuous(limits = c(0, 2)) + 
  facet_wrap(~ n, labeller = facet_labels) + 
  labs(
    x = "Cohen's d",
    y = "Proportion of Shapiro-Wilk p-values < 0.05",
    subtitle = "Dependent Variable"
  ) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey80",
                                    color = "white")
  )

plt_4 <- 
  ggplot(
    data = sim_result %>%
      group_by(round(cd, 3), n) %>%
      summarise(
        prop_resid = mean(resid_sw < 0.05)
      ),
    aes(x = `round(cd, 3)`,
        y = prop_resid)
  ) + 
  geom_point(color = pal[6],
             shape = 19,
             size = 2,
             alpha = 0.05) + 
  geom_smooth(
    color = "black",
    se = FALSE,
    size = 1
  ) +
  geom_hline(yintercept = 0.80,
             linetype = "dashed") +
  scale_x_continuous(limits = c(0, 2)) + 
  facet_wrap(~ n, labeller = facet_labels) + 
  labs(
    x = "Cohen's d",
    y = "Proportion of Shapiro-Wilk p-values < 0.05",
    subtitle = "Model Residuals"
  ) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey80",
                                    color = "white")
  )
 
grid_2 <- plot_grid(plt_3, plt_4, align = "hv", ncol = 2)

grid_2_title <- ggdraw() + 
  draw_label("Relationship between Effect Size & Sample Size and Tests of the Normality Assumption",
             fontface = 'bold', fontfamily = "Gill Sans MT", size = 12)

grid_2_b <- plot_grid(grid_2_title, grid_2, ncol = 1, rel_heights = c(0.1, 1))

grid_2_caption <- ggdraw() + 
  draw_label("Graphic by Ben Andrew | @BenYAndrew",
             fontfamily = "Gill Sans MT",
             size = 8, hjust = -1.6)

grid_2_c <- plot_grid(grid_2_b, grid_2_caption, ncol = 1, rel_heights = c(1, 0.05))


ggsave("figures/residual_normality_2.jpeg", grid_2_c,
       height = 6, width = 10, device = "jpeg")

# Quick data for examples 
set.seed(123)

df <- 
  tibble(
    a = c(rnorm(1000, 0, 1),
          rnorm(1000, 0, 1),
          rnorm(1000, 2, 1),
          rnorm(1000, 0.75, 1)),
    x = rep(c(0, 1), each = 2000),
    cat = rep(c("big", "small", "big", "small"), each = 1000)
  )

facet_labels <- as_labeller(
  c("big" = "Cohen's d = 2",
    "small" = "Cohen's d = 0.8"))

plt_5 <-
  ggplot(
    data = df,
    aes(x = a)
  ) + 
  geom_density(
    aes(fill = factor(x)),
    alpha = 0.5
  ) +
  scale_fill_viridis_d(guide = FALSE) + 
  coord_cartesian(ylim = c(0, 0.4)) + 
  facet_wrap(~ cat, labeller = facet_labels) + 
  labs(
    x = "Outcome",
    y = "Density",
    subtitle = "Dependent Variable (stratified by group)"
  ) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey80", color = "white")
  )

plt_6 <-
  ggplot(
    data = df,
    aes(x = a)
  ) + 
  geom_density(
    fill = pal[7],
    alpha = 0.75
  ) +
  geom_text(
    data = tibble(lab = c("Shapiro-Wilk p < 0.001", "Shapiro-Wilk p = 0.31"),
                  x = -3.75,
                  y = 0.45,
                  cat = c("big", "small")),
    aes(x = x, y = y, label = lab),
    family = "Gill Sans MT",
    fontface = "bold",
    size = 3,
    hjust = 0) + 
  coord_cartesian(ylim = c(0, 0.46)) + 
  facet_wrap(~ cat, labeller = facet_labels) + 
  labs(
    x = "Outcome",
    y = "Density",
    subtitle = "Dependent Variable (condensed)"
  ) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey80", color = "white")
  )

plt_7 <-
  ggplot(
    data = df,
    aes(x = residuals(lm(a ~ x)))
  ) + 
  geom_density(
    fill = pal[1],
    alpha = 0.75
  ) +
  geom_text(
    data = tibble(lab = c("Shapiro-Wilk p = 0.35", "Shapiro-Wilk p = 0.81"),
                  x = -3.75,
                  y = 0.45,
                  cat = c("big", "small")),
    aes(x = x, y = y, label = lab),
    family = "Gill Sans MT",
    fontface = "bold",
    size = 3,
    hjust = 0) + 
  coord_cartesian(ylim = c(0, 0.46)) + 
  facet_wrap(~ cat, labeller = facet_labels) + 
  labs(
    x = "Outcome",
    y = "Density",
    subtitle = "Model Residuals"
  ) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    strip.text = element_text(face = "bold"),
    strip.background = element_rect(fill = "grey80", color = "white")
  )

grid_3 <- plot_grid(plt_5, plt_6, plt_7, ncol = 1, align = "hv")

grid_3_title <- ggdraw() + 
  draw_label("Dependent Variable and Model Residual Distributions\nfor Varying Levels of Effect Size",
             fontface = 'bold', fontfamily = "Gill Sans MT", size = 12)

grid_3_b <- plot_grid(grid_3_title, grid_3, ncol = 1, rel_heights = c(0.1, 1))

grid_3_caption <- ggdraw() + 
  draw_label("Graphic by Ben Andrew | @BenYAndrew",
             fontfamily = "Gill Sans MT",
             size = 8, hjust = -0.25)

grid_3_c <- plot_grid(grid_3_b, grid_3_caption, ncol = 1, rel_heights = c(1, 0.03))

ggsave("figures/residual_normality_3.jpeg", grid_3_c,
       height = 10, width = 5, device = "jpeg")

