library(tidyverse)
library(brms)
library(ggdag)
library(tidybayes)
library(cowplot)

## Example 1: post-treatment bias (i.e., conditioning on a mediator)

# Generate DAG
dag_1 <- 
  dagify(Y ~ Z + S,
         Z ~ X) %>% 
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
b_XZ <- 1 # direct effect of X on mediator Z
b_ZY <- 1 # direct effect of mediator Z on outcome Y
b_SY <- 2 # direct effect of S on Y

set.seed(13)

df <-
  tibble(
    S = rnorm(n),
    X = rnorm(n),
    Z = rnorm(n, b_XZ * X),
    Y = rnorm(n, b_ZY * Z + b_SY * S)
  )


# Models
m1 <- 
  brm(
    data = df,
    family = gaussian,
    Y ~ 1 + X + Z,
    prior = c(prior(normal(0, 5), class = Intercept),
              prior(normal(0, 5), class = b),
              prior(exponential(1), class = sigma)),
    iter = 2000, warmup = 500, chains = 4, cores = 4)

m2 <- update(m1, Y ~ 1 + X)
m3 <- update(m1, Y ~ 1 + X + S, newdata = df)

# Plots
plt_1 <-
  posterior_samples(m1) %>%
  as_tibble() %>%
  ggplot(aes(x = b_X, y = 0)) + 
  geom_halfeyeh(point_interval = median_qi, .width = 0.89,
                fill = "#8E1900", alpha = 0.75) +
  geom_vline(xintercept = 1, linetype = 2) +
  annotate(geom = "text", 
           label = expression(beta[X]),
           size = 3, 
           x = 1.05, y = 2,
           hjust = 0) + 
  coord_cartesian(xlim = c(-0.5, 2)) + 
  labs(
    x = expression(paste(hat(beta)[X], " [median, 89% credible interval]")),
    y = NULL,
    subtitle = "Model: Y ~ 1 + X + Z"
  ) + 
  theme_classic() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9),
        text = element_text(family = "Gill Sans MT"))

plt_2 <-
  posterior_samples(m2) %>%
  as_tibble() %>%
  ggplot(aes(x = b_X, y = 0)) + 
  geom_halfeyeh(point_interval = median_qi, .width = 0.89,
                fill = "#8E1900", alpha = 0.75) +
  geom_vline(xintercept = 1, linetype = 2) +
  annotate(geom = "text", 
           label = expression(beta[X]),
           size = 3, 
           x = 1.03, y = 2,
           hjust = 0) + 
  coord_cartesian(xlim = c(-0.5, 2)) + 
  labs(
    x = expression(paste(hat(beta)[X], " [median, 89% credible interval]")),
    y = NULL,
    subtitle = "Model: Y ~ 1 + X"
  ) +
  theme_classic() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9),
        text = element_text(family = "Gill Sans MT"))

plt_3 <-
  posterior_samples(m3) %>%
  as_tibble() %>%
  ggplot(aes(x = b_X, y = 0)) + 
  geom_halfeyeh(point_interval = median_qi, .width = 0.89,
                fill = "#8E1900", alpha = 0.75) +
  geom_vline(xintercept = 1, linetype = 2) +
  annotate(geom = "text", 
           label = expression(beta[X]),
           size = 3, 
           x = 1.03, y = 2,
           hjust = 0) + 
  coord_cartesian(xlim = c(-0.5, 2)) + 
  labs(
    x = expression(paste(hat(beta)[X], " [median, 89% credible interval]")),
    y = NULL,
    subtitle = "Model: Y ~ 1 + X + S",
    caption = "Graphic by Ben Andrew | @BenYAndrew"
  ) +
  theme_classic() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9),
        text = element_text(family = "Gill Sans MT"),
        plot.caption = element_text(size = 6))

grid_1 <- plot_grid(dag_1, plt_1, plt_2, plt_3, ncol = 2, align = "hv")

title <- ggdraw() + 
  draw_label("Example #1: 'The Clogged Pipe'",
             fontface = 'bold',
             fontfamily = "Gill Sans MT")

grid_1b <- plot_grid(title, grid_1, ncol = 1, rel_heights = c(0.1, 1))

ggsave("figures/post_treatment_bias.jpeg", grid_1b,
       height = 6, width = 7, device = "jpeg")

## Example 2: conditioning on a collider
dag_2 <- 
  dagify(Y ~ Z + X + U,
         Z ~ X + U
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
b_XZ <- 1 # direct effect of G on P
b_XY <- 0 # direct effect of G on C
b_ZY <- 1 # direct effect of P on C
b_U <- 2 # direct effect of U on P and C

set.seed(137) 

df <-
  tibble(
    U = rnorm(n),
    X = rnorm(n),
    Z = rnorm(n, b_XZ * X + b_U * U),
    Y = rnorm(n, b_ZY * Z + b_XY * X + b_U * U)
  )

m4 <-
  brm(
    data = df,
    family = gaussian,
    Y ~ 1 + X + Z,
    prior = c(prior(normal(0, 5), class = Intercept),
              prior(normal(0, 5), class = b),
              prior(exponential(1), class = sigma)),
    iter = 2000, warmup = 500, chains = 4, cores = 4
  )

m5 <- update(m4, Y ~ 1 + X)
m6 <- update(m4, Y ~ 1 + X + Z + U, newdata = df)

# Plots
plt_4 <-
  posterior_samples(m4) %>%
  as_tibble() %>%
  ggplot(aes(x = b_X, y = 0)) + 
  geom_halfeyeh(point_interval = median_qi, .width = 0.89,
                fill = "#06759A", alpha = 0.75) +
  geom_vline(xintercept = 0, linetype = 2) +
  annotate(geom = "text", 
           label = expression(beta[X]),
           size = 3, 
           x = 0.05, y = 1,
           hjust = 0) + 
  coord_cartesian(xlim = c(-1.5, 2.5)) + 
  labs(
    x = expression(paste(hat(beta)[X], " [median, 89% credible interval]")),
    y = NULL,
    subtitle = "Model: Y ~ 1 + X + Z"
  ) + 
  theme_classic() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9),
        text = element_text(family = "Gill Sans MT"))

plt_5 <-
  posterior_samples(m5) %>%
  as_tibble() %>%
  ggplot(aes(x = b_X, y = 0)) + 
  geom_halfeyeh(point_interval = median_qi, .width = 0.89,
                fill = "#06759A", alpha = 0.75) +
  geom_vline(xintercept = 0, linetype = 2) +
  annotate(geom = "text", 
           label = expression(beta[X]),
           size = 3, 
           x = 0.05, y = 1,
           hjust = 0) + 
  coord_cartesian(xlim = c(-1.5, 2.5)) + 
  labs(
    x = expression(paste(hat(beta)[X], " [median, 89% credible interval]")),
    y = NULL,
    subtitle = "Model: Y ~ 1 + X"
  ) + 
  theme_classic() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9),
        text = element_text(family = "Gill Sans MT"))

plt_6 <-
  posterior_samples(m6) %>%
  as_tibble() %>%
  ggplot(aes(x = b_X, y = 0)) + 
  geom_halfeyeh(point_interval = median_qi, .width = 0.89,
                fill = "#06759A", alpha = 0.75) +
  geom_vline(xintercept = 0, linetype = 2) +
  annotate(geom = "text", 
           label = expression(beta[X]),
           size = 3, 
           x = 0.05, y = 1,
           hjust = 0) + 
  coord_cartesian(xlim = c(-1.5, 2.5)) + 
  labs(
    x = expression(paste(hat(beta)[X], " [median, 89% credible interval]")),
    y = NULL,
    subtitle = "Model: Y ~ 1 + X + Z + U",
    caption = "Graphic by Ben Andrew | @BenYAndrew"
  ) + 
  theme_classic() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9),
        text = element_text(family = "Gill Sans MT"),
        plot.caption = element_text(size = 6))

grid_2 <- plot_grid(dag_2, plt_4, plt_5, plt_6, ncol = 2, align = "hv")

title <- ggdraw() + 
  draw_label("Example #2: 'The Hidden Collider'",
             fontface = 'bold',
             fontfamily = "Gill Sans MT")

grid_2b <- plot_grid(title, grid_2, ncol = 1, rel_heights = c(0.1, 1))

ggsave("figures/collider_bias.jpeg", grid_2b,
       height = 6, width = 7, device = "jpeg")

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
b_UY <- 1 # direct effect of U on Y
b_UX <- 1 # direct effect of U on X
b_UZ <- 2 # direct effect of U on M

set.seed(52) 

df <-
  tibble(
    U = rnorm(n),
    X = rnorm(n, b_UX * U),
    Y = rnorm(n, b_XY * X + b_UY * U),
    Z = rnorm(n, b_UZ * U)
  )

# Models
m7 <-
  brm(
    data = df,
    family = gaussian,
    Y ~ X,
    prior = c(prior(normal(0, 5), class = Intercept),
              prior(normal(0, 5), class = b),
              prior(exponential(1), class = sigma)),
    iter = 2000, warmup = 500, chains = 4, cores = 4
  )

m8 <- update(m7, Y ~ X + Z, newdata = df)
m9 <- update(m7, Y ~ X + U, newdata = df)

# Plots
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
    subtitle = "Model: Y ~ 1 + X"
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
    subtitle = "Model: Y ~ 1 + X + Z"
  ) + 
  theme_classic() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9),
        text = element_text(family = "Gill Sans MT"))

plt_9 <-
  posterior_samples(m9) %>%
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
    subtitle = "Model: Y ~ 1 + X + U",
    caption = "Graphic by Ben Andrew | @BenYAndrew"
  ) + 
  theme_classic() + 
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 9),
        text = element_text(family = "Gill Sans MT"),
        plot.caption = element_text(size = 6))

grid_3 <- plot_grid(dag_3, plt_7, plt_8, plt_9, ncol = 2, align = "hv")

title <- ggdraw() + 
  draw_label("Example #3: 'The Fork's Descendant'",
             fontface = 'bold',
             fontfamily = "Gill Sans MT")

grid_3b <- plot_grid(title, grid_3, ncol = 1, rel_heights = c(0.1, 1))

ggsave("figures/descendant_adjustment.jpeg", grid_3b,
       height = 6, width = 7, device = "jpeg")

