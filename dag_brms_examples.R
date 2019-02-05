library(tidyverse)
library(brms)
library(ggdag)
library(tidybayes)
library(cowplot)

## Example 1: post-treatment bias (i.e., conditioning on a mediator)
n <- 200
set.seed(191)

# Generate simulated data
df <-
  tibble(
    h0 = rnorm(n, 10, 2), # baseline ht
    treatment = rep(0:1, each = n/2), # treatment status
    fungus = rbinom(n, size = 1, prob = 0.5 - treatment * 0.4), # fungus prob.
    h1 = h0 + rnorm(n, 5 - 3 * fungus) # final ht as function of fungus & h0
  )

# Generate DAG
dag_1 <- 
  dagify(H1 ~ H0 + Fungus,
         Fungus ~ Treatment) %>% 
  ggdag(text_size = 3,
        node = FALSE,
        text_col = "black") + 
  labs(x = NULL, y = NULL) + 
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(family = "Gill Sans MT"))

# Models
m1 <- 
  brm(
    data = df,
    family = gaussian,
    h1 ~ 1 + h0 + treatment + fungus,
    prior = c(prior(normal(0, 5), class = Intercept),
              prior(normal(0, 5), class = b),
              prior(exponential(1), class = sigma)),
    iter = 2000, warmup = 500, chains = 4, cores = 4)

m2 <- update(m1, h1 ~ 1 + h0 + treatment)

# Plots
plt_1 <-
  posterior_samples(m1) %>%
  as_tibble() %>%
  ggplot(aes(x = b_treatment, y = 0)) + 
  geom_halfeyeh(point_interval = median_qi, .width = 0.89,
                fill = "#8E1900", alpha = 0.75) +
  geom_vline(xintercept = 1, linetype = 2) +
  annotate(geom = "text", 
           label = expression(beta[treatment]),
           size = 3, 
           x = 1.05, y = 2,
           hjust = 0) + 
  coord_cartesian(xlim = c(-0.5, 2)) + 
  labs(
    x = expression(paste(hat(beta)[treatment], " [median, 89% credible interval]")),
    y = NULL,
    subtitle = "Model: H1 ~ 1 + H0 + Treatment + Fungus"
  ) + 
  theme_classic() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9),
        text = element_text(family = "Gill Sans MT"))

plt_2 <-
  posterior_samples(m2) %>%
  as_tibble() %>%
  ggplot(aes(x = b_treatment, y = 0)) + 
  geom_halfeyeh(point_interval = median_qi, .width = 0.89,
                fill = "#8E1900", alpha = 0.75) +
  geom_vline(xintercept = 1, linetype = 2) +
  annotate(geom = "text", 
           label = expression(beta[treatment]),
           size = 3, 
           x = 1.03, y = 2,
           hjust = 0) + 
  coord_cartesian(xlim = c(-0.5, 2)) + 
  labs(
    x = expression(paste(hat(beta)[treatment], " [median, 89% credible interval]")),
    y = NULL,
    subtitle = "Model: H1 ~ 1 + H0 + Treatment",
    caption = "Graphic by Ben Andrew | @BenYAndrew"
  ) +
  theme_classic() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9),
        text = element_text(family = "Gill Sans MT"),
        plot.caption = element_text(size = 6))

grid_1 <- plot_grid(dag_1, plt_1, plt_2, ncol = 1, align = "hv")

ggsave("figures/post_treatment_bias.jpeg", grid_1,
       height = 8, width = 5, device = "jpeg")

## Example 2: conditioning on a collider
dag_2 <- 
  dagify(C ~ P + G + U,
         P ~ G + U
  ) %>% 
  ggdag(text_size = 3,
        node = FALSE,
        text_col = "black") + 
  labs(x = NULL, y = NULL) + 
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(family = "Gill Sans MT"))

# Generate simulated data
n <- 200  # number of trials
b_GP <- 1 # direct effect of G on P
b_GC <- 0 # direct effect of G on C
b_PC <- 1 # direct effect of P on C
b_U <- 2 # direct effect of U on P and C

set.seed(137) 

df <-
  tibble(
    U = 2 * rbernoulli(n, 0.5) - 1,
    G = rnorm(n),
    P = rnorm(n, b_GP * G + b_U * U),
    C = rnorm(n, b_PC * P + b_GC * G + b_U * U)
  )

m3 <-
  brm(
    data = df,
    family = gaussian,
    C ~ 1 + G + P,
    prior = c(prior(normal(0, 2), class = Intercept),
              prior(normal(0, 2), class = b),
              prior(exponential(1), class = sigma)),
    iter = 2000, warmup = 500, chains = 4, cores = 4
  )

m4 <- update(m3, C ~ 1 + G)
m5 <- update(m3, C ~ 1 + G + P + U, newdata = df)

# Plots
plt_3 <-
  posterior_samples(m3) %>%
  as_tibble() %>%
  ggplot(aes(x = b_G, y = 0)) + 
  geom_halfeyeh(point_interval = median_qi, .width = 0.89,
                fill = "#06759A", alpha = 0.75) +
  geom_vline(xintercept = 0, linetype = 2) +
  annotate(geom = "text", 
           label = expression(beta[G]),
           size = 3, 
           x = 0.05, y = 1,
           hjust = 0) + 
  coord_cartesian(xlim = c(-1.5, 2.5)) + 
  labs(
    x = expression(paste(hat(beta)[G], " [median, 89% credible interval]")),
    y = NULL,
    subtitle = "Model: C ~ G + P"
  ) + 
  theme_classic() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9),
        text = element_text(family = "Gill Sans MT"))

plt_4 <-
  posterior_samples(m4) %>%
  as_tibble() %>%
  ggplot(aes(x = b_G, y = 0)) + 
  geom_halfeyeh(point_interval = median_qi, .width = 0.89,
                fill = "#06759A", alpha = 0.75) +
  geom_vline(xintercept = 0, linetype = 2) +
  annotate(geom = "text", 
           label = expression(beta[G]),
           size = 3, 
           x = 0.05, y = 1,
           hjust = 0) + 
  coord_cartesian(xlim = c(-1.5, 2.5)) + 
  labs(
    x = expression(paste(hat(beta)[G], " [median, 89% credible interval]")),
    y = NULL,
    subtitle = "Model: C ~ G"
  ) + 
  theme_classic() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9),
        text = element_text(family = "Gill Sans MT"))

plt_5 <-
  posterior_samples(m5) %>%
  as_tibble() %>%
  ggplot(aes(x = b_G, y = 0)) + 
  geom_halfeyeh(point_interval = median_qi, .width = 0.89,
                fill = "#06759A", alpha = 0.75) +
  geom_vline(xintercept = 0, linetype = 2) +
  annotate(geom = "text", 
           label = expression(beta[G]),
           size = 3, 
           x = 0.05, y = 1,
           hjust = 0) + 
  coord_cartesian(xlim = c(-1.5, 2.5)) + 
  labs(
    x = expression(paste(hat(beta)[G], " [median, 89% credible interval]")),
    y = NULL,
    subtitle = "Model: C ~ G + P + U",
    caption = "Graphic by Ben Andrew | @BenYAndrew"
  ) + 
  theme_classic() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9),
        text = element_text(family = "Gill Sans MT"),
        plot.caption = element_text(size = 6))

grid_2 <- plot_grid(dag_2, plt_3, plt_4, plt_5, ncol = 2, align = "hv")

ggsave("figures/collider_bias.jpeg", grid_2,
       height = 7, width = 10, device = "jpeg")

## Example 3: conditioning on the descendant of a fork
dag_3 <- 
  dagify(Y ~ X + U,
         X ~ U,
         Z ~ U
  ) %>% 
  ggdag(text_size = 3,
        node = FALSE,
        text_col = "black") + 
  labs(x = NULL, y = NULL) + 
  theme_classic() + 
  theme(axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        text = element_text(family = "Gill Sans MT"))

# Generate simulated data
n <- 200  # number of trials
b_XY <- 0 # direct effect of X on Y
b_UY <- 2 # direct effect of U on Y
b_UX <- 1 # direct effect of U on X
b_UZ <- 2 # direct effect of U on M

set.seed(562) 

df <-
  tibble(
    U = rnorm(n, 2),
    X = rnorm(n, b_UX * U),
    Y = rnorm(n, b_XY * X + b_UY * U),
    Z = rnorm(n, b_UZ * U)
  )

# Models
m6 <-
  brm(
    data = df,
    family = gaussian,
    Y ~ X,
    prior = c(prior(normal(0, 2), class = Intercept),
              prior(normal(0, 2), class = b),
              prior(exponential(1), class = sigma)),
    iter = 2000, warmup = 500, chains = 4, cores = 4
  )

m7 <- update(m6, Y ~ X + Z, newdata = df)
m8 <- update(m6, Y ~ X + U, newdata = df)

# Plots
plt_6 <-
  posterior_samples(m6) %>%
  as_tibble() %>%
  ggplot(aes(x = b_X, y = 0)) + 
  geom_halfeyeh(point_interval = median_qi, .width = 0.89,
                fill = "#007126", alpha = 0.75) +
  geom_vline(xintercept = 0, linetype = 2) +
  annotate(geom = "text", 
           label = expression(beta[X]),
           size = 3, 
           x = 0.05, y = 1,
           hjust = 0) + 
  coord_cartesian(xlim = c(-1, 1.5)) + 
  labs(
    x = expression(paste(hat(beta)[X], " [median, 89% credible interval]")),
    y = NULL,
    subtitle = "Model: Y ~ X"
  ) + 
  theme_classic() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9),
        text = element_text(family = "Gill Sans MT"))

plt_7 <-
  posterior_samples(m7) %>%
  as_tibble() %>%
  ggplot(aes(x = b_X, y = 0)) + 
  geom_halfeyeh(point_interval = median_qi, .width = 0.89,
                fill = "#007126", alpha = 0.75) +
  geom_vline(xintercept = 0, linetype = 2) +
  annotate(geom = "text", 
           label = expression(beta[X]),
           size = 3, 
           x = 0.05, y = 1,
           hjust = 0) + 
  coord_cartesian(xlim = c(-1, 1.5)) + 
  labs(
    x = expression(paste(hat(beta)[X], " [median, 89% credible interval]")),
    y = NULL,
    subtitle = "Model: Y ~ X + Z"
  ) + 
  theme_classic() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9),
        text = element_text(family = "Gill Sans MT"))

plt_8 <-
  posterior_samples(m8) %>%
  as_tibble() %>%
  ggplot(aes(x = b_X, y = 0)) + 
  geom_halfeyeh(point_interval = median_qi, .width = 0.89,
                fill = "#007126", alpha = 0.75) +
  geom_vline(xintercept = 0, linetype = 2) +
  annotate(geom = "text", 
           label = expression(beta[X]),
           size = 3, 
           x = 0.05, y = 1,
           hjust = 0) + 
  coord_cartesian(xlim = c(-1, 1.5)) + 
  labs(
    x = expression(paste(hat(beta)[X], " [median, 89% credible interval]")),
    y = NULL,
    subtitle = "Model: Y ~ X + U",
    caption = "Graphic by Ben Andrew | @BenYAndrew"
  ) + 
  theme_classic() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9),
        text = element_text(family = "Gill Sans MT"),
        plot.caption = element_text(size = 6))

grid_3 <- plot_grid(dag_3, plt_6, plt_7, plt_8, ncol = 2, align = "hv")

ggsave("figures/descendant_adjustment.jpeg", grid_3,
       height = 7, width = 10, device = "jpeg")

