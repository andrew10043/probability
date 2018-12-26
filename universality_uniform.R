## Universality of the Uniform ##
devtools::install_github('thomasp85/gganimate')
library(ggplot2)
library(tidyverse)
library(gganimate)
library(magick)

# Plot Theme
universality_theme <- theme(
  plot.margin = margin(t = 0.75, r = 0.75, b = 0.5, l = 0.75, unit = "cm"),
  plot.title = element_text(size = 14, hjust = 0.5,
                            margin= margin(b = 0.5, unit = "cm")),
  plot.subtitle = element_text(size = 12, hjust = 0.5,
                               margin= margin(b = 0.5, unit = "cm")),
  plot.caption = element_text(size = 9, hjust = 1,
                              margin = margin(t = 0.6, unit = "cm")),
  axis.text = element_text(size = 10),
  axis.title.x = element_text(size = 13,
                              margin = margin(t = 0.4, unit = "cm")),
  axis.title.y = element_text(size = 13,
                              margin = margin(r = 0.4, unit = "cm")),
  text = element_text(family = "Gill Sans MT"),
  plot.background = element_rect(fill = "#f5f5f2", color = NA),
  legend.position = "none")

# Plot Colors
org_col <- "#E59900"
pur_col <- "#5B0699"
green_col <- "#12BB00"
blue_col <- "#019AA8"
red_col <- "#CD0A00"

# 10 Iterations of Random Sampling
sample_size <- 100
df <- NULL
set.seed(1991)

for (i in 1:10){
  unif_r <- runif(n = sample_size, min = 0, max = 1) # U(0,1) r.s.
  norm_cdf_r <- qnorm(unif_r, mean = 0, sd = 1) # Mapped to N(0,1) CDF inverse
  norm_r <- rnorm(sample_size, mean = 0, sd = 1) # N(0,1) r.s. 
  unif_pdf_r <- pnorm(norm_r, mean = 0, sd = 1) # Mapped to N(0,1) CDF
  pre_df <- cbind(unif_r, norm_cdf_r, norm_r, unif_pdf_r, 
                  rep(i, sample_size))
  df <- rbind(df, pre_df)
}

dist_data <- as.tibble(df)
colnames(dist_data) <- c("samp_unif","samp_cdf", "samp_norm", 
                         "samp_pdf", "sample")

# Normal CDF Generation
x <- seq(-4, 4, by = 0.001)
norm_data <- data.frame(x = x, norm_cdf = pnorm(x, mean = 0, sd = 1),
                        norm_pdf = dnorm(x, mean = 0, sd = 1))

# Plot 1A
plot_1a <- ggplot() + 
  geom_segment(aes(x = 0, xend = 0, y = 0, yend = 1),
               color = blue_col, linetype = "dashed") + 
  geom_segment(aes(x = 1, xend = 1, y = 0, yend = 1),
               color = blue_col, linetype = "dashed") + 
  geom_segment(aes(x = 0, xend = 1, y = 1, yend = 1),
               color = blue_col) + 
  geom_point(data = dist_data, aes(x = samp_unif, y = 1),
             color = org_col, size = 0.75) +
  geom_rug(data = dist_data, aes(x = samp_unif),
           color = org_col) + 
  transition_states(states = sample, transition_length = 1,
                    state_length = 1) +
  annotate(geom = "text", label = "U(0,1) PDF", color = blue_col,
           size = 3, fontface = "bold", x = 0.15, y = 1.1,
           hjust = 1) + 
  geom_segment(aes(x = 0.15, xend = 0.2, y = 1.08, yend = 1.02),
               color = blue_col, 
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.03, "inches"))) +
  annotate(geom = "text", label = "U(0,1) random\nsample (n = 100)",
           size = 3, fontface = "bold", x = 0.65, y = 0.1, hjust = 0,
           color = org_col) + 
  geom_segment(aes(x = 0.64, xend = 0.6, y = 0.06, yend = 0.01),
               color = org_col,
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.03, "inches"))) + 
  scale_y_continuous(limits = c(0, 1.1), 
                     breaks = c(0, 0.25, 0.50, 0.75, 1.0)) + 
  ylab("Probability Density") + 
  xlab("x") + 
  labs(title = "Universality of the Uniform - Step 1:",
       subtitle = "Random Sample from U(0,1)",
       caption = "") + 
  theme_classic() + 
  universality_theme

# Plot 1B
plot_1b <- ggplot() + 
  geom_line(data = norm_data, aes(x = x, y = norm_cdf), 
            color = pur_col) + 
  geom_point(data = dist_data, aes(x = samp_cdf, y = samp_unif), 
             color = org_col, size = 0.75) +
  geom_density(data = dist_data, aes(x = samp_cdf), 
               fill = green_col, alpha = 0.75) + 
  geom_rug(data = dist_data, aes(y = samp_unif),
           color = org_col) + 
  geom_rug(data = dist_data, aes(x = samp_cdf),
           color = pur_col) +
  transition_states(states = sample, transition_length = 1,
                    state_length = 1) +
  annotate(geom = "text", label = "N(0,1) CDF", color = pur_col,
           size = 3, fontface = "bold", x = 0.6, y = 0.95,
           hjust = 1) + 
  geom_segment(aes(x = 0.65, xend = 1.1, y = 0.94, yend = 0.9),
               color = pur_col, 
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.03, "inches"))) + 
  annotate(geom = "text", label = "U(0,1) random\nsample (n = 100)",
           size = 3, fontface = "bold", x = -3.4, y = 1, hjust = 0,
           color = org_col) + 
  geom_segment(aes(x = -3.48, xend = -3.9, y = 0.97, yend = 0.93),
               color = org_col,
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.03, "inches"))) +
  annotate(geom = "text", label = "X ~ N(0,1)", color = green_col,
           size = 3, fontface = "bold", x = 2.9, y = 0.1, hjust = 0) + 
  geom_segment(aes(x = 2.8, xend = 2.4, y = 0.1, yend = 0.075),
               color = green_col,
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.03, "inches"))) + 
  xlab("x") + 
  ylab("F(x) or Probability Density") + 
  labs(title = "Universality of the Uniform - Step 2:",
       subtitle = "Map U(0,1) to Inverse of N(0,1) CDF",
       caption = "Graphic by Ben Andrew | @BenYAndrew") + 
  theme_classic() + 
  universality_theme 

# Convert to GIF and combine
gif_1a <- animate(plot_1a, device = "png", width = 6, 
                  height = 6, units = "in", res = 300)

gif_1b <- animate(gif_1b, device = "png", width = 6,
                  height = 6, units = "in", res = 300)

mgif_1a <- image_read(gif_1a)
mgif_1b <- image_read(gif_1b)

part_1 <- image_append(c(mgif_1a[1], mgif_1b[1]))

for(i in 2:100){
  combined <- image_append(c(mgif_1a[i], mgif_1b[i]))
  part_1 <- c(part_1, combined)
}

# Plot 2A
plot_2a <- ggplot() + 
  geom_line(data = norm_data, aes(x = x, y = norm_pdf), 
            color = red_col) +
  geom_point(data = dist_data, aes(x = samp_norm, 
                                   y = dnorm(samp_norm, mean = 0, sd = 1)),
             color = green_col, size = 0.75) +
  geom_rug(data = dist_data, aes(x = samp_norm),
           color = green_col) + 
  transition_states(states = sample, transition_length = 1,
                    state_length = 1) +
  annotate(geom = "text", label = "N(0,1) PDF", color = red_col,
           size = 3, fontface = "bold", x = -3, y = 0.05,
           hjust = 1) + 
  geom_segment(aes(x = -2.95, xend = -2.5, y = 0.047, yend = 0.03),
               color = red_col, 
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.03, "inches"))) +
  annotate(geom = "text", label = "N(0,1) random\nsample (n = 100)",
           size = 3, fontface = "bold", x = 0.4, y = 0.03, hjust = 0,
           color = green_col) + 
  geom_segment(aes(x = 0.31, xend = 0.1, y = 0.014, yend = 0),
               color = green_col,
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.03, "inches"))) + 
  scale_y_continuous(limits = c(0, 0.4), 
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5)) + 
  ylab("Probability Density") + 
  xlab("x") + 
  labs(title = "Universality of the Uniform - Step 1:",
       subtitle = "Random Sample from N(0,1)",
       caption = "") + 
  theme_classic() + 
  universality_theme

# Plot 2B
plot_2b <- ggplot() + 
  geom_line(data = norm_data, aes(x = norm_cdf, y = x), 
            color = pur_col) + 
  geom_point(data = dist_data, aes(x = pnorm(samp_norm, mean = 0, sd = 1), 
                                   y = samp_norm), 
             color = green_col, size = 0.75) +
  geom_density(data = dist_data, aes(x = samp_pdf), 
               fill = org_col, alpha = 0.75) + 
  geom_rug(data = dist_data, aes(y = samp_norm),
           color = green_col) + 
  geom_rug(data = dist_data, aes(x = samp_pdf),
           color = pur_col) +
  transition_states(states = sample, transition_length = 1,
                    state_length = 1) +
  annotate(geom = "text", label = "N(0,1) CDF", color = pur_col,
           size = 3, fontface = "bold", x = 0.15, y = -1.8,
           hjust = 0) + 
  geom_segment(aes(x = 0.14, xend = 0.1, y = -1.72, yend = -1.5),
               color = pur_col, 
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.03, "inches"))) + 
  annotate(geom = "text", label = "N(0,1) random\nsample (n = 100)",
           size = 3, fontface = "bold", x = 0.05, y = 2, hjust = 0,
           color = green_col) + 
  geom_segment(aes(x = 0.04, xend = 0, y = 1.8, yend = 1.5),
               color = green_col,
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.03, "inches"))) +
  annotate(geom = "text", label = "X ~ U(0,1)", color = org_col,
           size = 3, fontface = "bold", x = 0.625, y = 1.8, hjust = 1) + 
  geom_segment(aes(x = 0.63, xend = 0.65, y = 1.65, yend = 1.4),
               color = org_col,
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.03, "inches"))) + 
  xlab("x or F(x)") + 
  ylab("x or Probability Density") + 
  labs(title = "Universality of the Uniform - Step 2:",
       subtitle = "Map N(0,1) to N(0,1) CDF",
       caption = "Graphic by Ben Andrew | @BenYAndrew") + 
  theme_classic() + 
  universality_theme 

# Convert to GIF and combine
gif_2a <- animate(plot_2a, device = "png", 
                    width = 6, height = 6, 
                    units = "in", res = 100)

gif_2b <- animate(plot_2b, device = "png", 
                   width = 6, height = 6, 
                   units = "in", res = 100)

mgif_2a <- image_read(gif_2a)
mgif_2b <- image_read(gif_2b)

part_2 <- image_append(c(mgif_2a[1], mgif_2b[1]))

for(i in 2:100){
  combined <- image_append(c(norm_mgif[i], new_mgif[i]))
  part_2 <- c(part_2, combined)
}

# Save Files
anim_save("/Users/student/Desktop/universality_1.gif", animation = part_1,
          device = "png", width = 15, height = 6, units = "in", res = 300)

anim_save("/Users/student/Desktop/universality_2.gif", animation = part_2,
          device = "png", width = 15, height = 6, units = "in", res = 300)

