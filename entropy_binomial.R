library(tidyverse)
library(RColorBrewer)
library(cowplot)

sim_p <- function(exp_val) {
  
  all_but <- runif(3)
  sub <- function(x, exp_val) {
    out <- sum(x) * exp_val
    for (i in 2:length(x)) {
      out <- out - all_but[i]
    }
    return(out)
  }
  
  out <- sub(x = all_but, exp_val = exp_val)
  last <- (out / (2 - exp_val))
  z <- sum(c(all_but, last))
  p <- c(all_but, last) / z
  
  return(list(H = -sum(p * log(p)), p = p))
  
}

H <- replicate(1e6, sim_p(exp_val = 1.4))

df <-
  tibble(
    entropy = rep(unlist(H[1, ]), each = 4),
    prob = unlist(H[2, ]),
    pos = rep(c(1, 2, 3, 4), times = 4000000 / 4)
  )

binom_prob <- c((1 - 0.7)^2,
                0.7 * (1 - 0.7),
                (1 - 0.7) * 0.7 , 
                0.7^2)

binom_df <-
  tibble(
    pos = 1:4,
    prob = binom_prob
  )

plt_1 <-
  ggplot(
    data = df %>% filter(pos == 1),
    aes(x = entropy)
  ) + 
  geom_density(fill = "grey50", bw = 0.0001) +
  geom_vline(xintercept = max(df$entropy), 
             color = brewer.pal(n = 4, name = "Dark2")[4],
             size = 1) + 
  geom_vline(xintercept = 1.1,
             color = brewer.pal(n = 3, name = "Dark2")[2],
             size = 1) + 
  geom_vline(xintercept = 0.8,
             color = brewer.pal(n = 3, name = "Dark2")[1],
             size = 1) + 
  annotate(geom = "text", label = "H = 0.8",
           x = 0.79, y = 2,
           hjust = 1,
           color = brewer.pal(n = 3, name = "Dark2")[1],
           fontface = "bold") +
  annotate(geom = "text", label = "H = 1.1",
           x = 1.09, y = 18,
           hjust = 1,
           color = brewer.pal(n = 3, name = "Dark2")[2],
           fontface = "bold") +
  annotate(geom = "text", label = "H = 1.22",
           x = 1.215, y = 38,
           hjust = 1,
           color = brewer.pal(n = 4, name = "Dark2")[4],
           fontface = "bold") +
  coord_cartesian(xlim = c(0.6, 1.25),
                  ylim = c(0, 43),
                  expand = FALSE) + 
  labs(
    x = "Entropy (H)",
    y = "Density"
  ) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    axis.title = element_text(size = 12),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

plt_2 <-
  ggplot(
    data = df %>% filter(entropy %in% c(max(df$entropy))),
    aes(x = pos, y = prob,
        group = factor(entropy))
  ) + 
  geom_line(color = brewer.pal(n = 4, name = "Dark2")[4],
            size = 1) +
  geom_point(color = brewer.pal(n = 4, name = "Dark2")[4]) +  
  labs(
    y = "Probability",
    x = "Draw Result"
  ) + 
  coord_cartesian(ylim = c(0, 0.7)) + 
  scale_x_continuous(labels = c("ww", "bw", "wb", "bb")) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

plt_3 <-
  ggplot(
    data = df %>% filter(entropy %in% c(max(df$entropy[df$entropy < 1.1001]))),
    aes(x = pos, y = prob,
        group = factor(entropy))
  ) +
  geom_line(color = brewer.pal(n = 3, name = "Dark2")[2],
            size = 1) +
  geom_point(color = brewer.pal(n = 3, name = "Dark2")[2]) +   labs(
    y = "Probability",
    x = "Draw Result"
  ) + 
  coord_cartesian(ylim = c(0, 0.7)) + 
  scale_x_continuous(labels = c("ww", "bw", "wb", "bb")) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

plt_4 <-
  ggplot(
    data = df %>% filter(entropy %in% c(max(df$entropy[df$entropy < 0.80001]))),
    aes(x = pos, y = prob,
        group = factor(entropy))
  ) + 
  geom_line(color = brewer.pal(n = 3, name = "Dark2")[1],
            size = 1) +
  geom_point(color = brewer.pal(n = 3, name = "Dark2")[1]) + 
  coord_cartesian(ylim = c(0, 0.7)) + 
  scale_x_continuous(labels = c("ww", "bw", "wb", "bb")) + 
  labs(
    y = "Probability",
    x = "Draw Result",
    caption = "Graphic by Ben Andrew | @BenYAndrew"
  ) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

grid <- plot_grid(plt_1, plt_2, plt_3, plt_4,
             align = "hv", ncol = 2)

title <- ggdraw() + 
  draw_label("The Binomial Distribution as a Maximum Entropy Distribution",
             fontface = 'bold',
             fontfamily = "Gill Sans MT")

grid_b <- plot_grid(title, grid, ncol = 1, rel_heights = c(0.1, 1))

ggsave("figures/entropy_binomial.jpeg", grid_b,
       height = 9, width = 9, device = "jpeg")

