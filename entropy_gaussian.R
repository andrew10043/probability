library(tidyverse)
library(RColorBrewer)
library(cowplot)
extrafont::loadfonts(quiet = TRUE)
extrafont::font_import()

alpha_per_beta <- function(variance, beta){
  sqrt((variance * gamma(1 / beta)) / gamma(3 / beta))
}

df_1 <-
  tibble(
    beta = seq(from = 1, to = 4, by = 0.01),
    mu = 0,
    variance = 1
  ) %>%
  mutate(
    alpha = map2_dbl(variance, beta, alpha_per_beta) 
  ) %>%
  expand(
    nesting(mu, beta, alpha),
    value = seq(from = -5, to = 5, by = 0.01)
  ) %>%
  mutate(
    density = (beta / (2 * alpha * gamma(1 / beta))) * 
      exp(1) ^ (-1 * (abs(value - mu) / alpha) ^ beta)
  ) 

df_2 <- 
  df_1 %>%
  group_by(beta) %>%
  summarise(
    entropy = -sum(density * log(density))
  )

plt_1 <-
  ggplot(
    data = df_1,
    aes(x = value,
        y = density,
        group = beta)
  ) + 
  geom_line(aes(color = beta),
            size = 1) + 
  scale_color_viridis_c(name = expression(paste(paste("Shape (", beta), ")"))) + 
  labs(
    x = "x",
    y = "Density",
    caption = "Figure by Ben Andrew | @BenYAndrew"
  ) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = c(0.9, 0.75)
  )

plt_2 <-
  ggplot(
    data = df_1 %>% filter(beta %in% c(1, 2, 4)),
    aes(x = value,
        y = density,
        group = beta)
  ) + 
  geom_line(aes(color = as.factor(beta)),
            size = 1, alpha = 1) + 
  scale_color_brewer(type = "qual", palette = "Dark2") + 
  labs(
    x = "x",
    y = "Density"
  ) + 
  annotate(geom = "text", label = expression(paste(beta, " = 1")),
           x = 0.5, y = 0.55, 
           hjust = 0, size = 4,
           color = brewer.pal(n = 3, name = "Dark2")[1],
           fontface = "bold") + 
  annotate(geom = "text", label = expression(paste(beta, " = 2 (Gaussian)")),
           x = 0.9, y = 0.34, 
           hjust = 0, size = 4,
           color = brewer.pal(n = 3, name = "Dark2")[2],
           fontface = "bold") + 
  annotate(geom = "text", label = expression(paste(beta, " = 4")),
           x = 1.7, y = 0.2, 
           hjust = 0, size = 4,
           color = brewer.pal(n = 3, name = "Dark2")[3],
           fontface = "bold") + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )

plt_3 <- 
  ggplot(
    data = df_2,
    aes(x = beta, y = entropy)
    ) +
  geom_line(size = 1) + 
  geom_vline(xintercept = 2,
             linetype = "dashed",
             color = "grey80",
             alpha = 0.7) + 
  labs(
    x = expression(paste(paste("Shape (", beta), ")")),
    y = "Entropy (H)"
  ) + 
  annotate(geom = "text", label = expression(paste(paste("Gaussian (",
                                                   beta), " = 2)")),
           x = 2.05, y = 142.5,
           hjust = 0, size = 3.5) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

grid <- plot_grid(plt_3, plt_2, plt_1, align = "hv",
                  ncol = 3)

title <- ggdraw() + 
  draw_label("The Gaussian Distribution as a Maximum Entropy Distribution",
             fontface = 'bold',
             fontfamily = "Gill Sans MT")

grid_b <- plot_grid(title, grid, ncol = 1, rel_heights = c(0.1, 1))

ggsave("figures/entropy_gaussian.jpeg", grid_b,
       height = 5, width = 12, device = "jpeg")

