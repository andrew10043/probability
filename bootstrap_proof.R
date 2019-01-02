## Bootstrap Resampling ##
library(ggplot2)
library(latex2exp)

## Plot Theme ##
bootstrap_theme <- theme(
  plot.margin = margin(t = 0.75, r = 0.75, b = 0.5, l = 0.75, unit = "cm"),
  plot.title = element_text(size = 11, hjust = 0.5,
                            margin= margin(b = 0.5, unit = "cm")),
  plot.subtitle = element_text(size = 10, hjust = 0.5,
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

bootstrap_labels <- labs(
  title = "What Proportion of Observations will Occur in a Given Bootstrap Sample?",
  caption = "Graphic by Ben Andrew | @BenYAndrew")

val <- "$1 - \\,\\frac{1}{\\e}$"

## Probability Plot ##
n <- 1:55
prob_not_chosen <- (1 - (1/n))^n
prob_chosen <- 1 - prob_not_chosen
limit_data <- data.frame(x = n, y = prob_chosen)

limit_plot <- 
  ggplot(limit_data, aes(x = x, y = y)) + 
  geom_hline(yintercept = 0.632, linetype = "dashed", color = "black") + 
  geom_line(color = "#8A0057", lwd = 1) + 
  geom_point(color = "#8A0057", size = 1.5) + 
  scale_y_continuous(limits = c(0.56, 1)) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50), limits = c(0, 60)) + 
  geom_rect(xmin = 42, xmax = 54, ymin = 0.55, ymax = 0.62,
            linetype = 1, color = "black", fill = "grey") + 
  geom_segment(x = 59, xend = 59, y = 0.585, yend = 0.625, 
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.05, "inches"))) + 
  geom_segment(x = 54, xend = 59, y = 0.585, yend = 0.585) + 
  annotate(geom = "text", label = TeX(val, output = "character"),
           parse = TRUE, size = 2.8, x = 45, y = 0.585) + 
  annotate(geom = "text", label = " = 0.632", size = 2.8, x = 50, y = 0.585) + 
  xlab("Sample Size") + 
  ylab("Probability of Inclusion in Bootstrap Sample") + 
  labs(subtitle = "Theoretical Probability Solution") + 
  theme_classic() + 
  bootstrap_theme + 
  bootstrap_labels

## Sample Size Simulation Plot ##
set.seed(1991)
x <- 1:5000
simulation_data <- data.frame(sample_size = x,
                              lower_ci = numeric(length = length(x)),
                              estimate = numeric(length = length(x)),
                              upper_ci = numeric(length = length(x)))
for (i in x){
  test_sample <- 1:i
  test_prop <- numeric(length = 1000)
  for (iter in 1:1000){
    set.seed(11 + iter + i)
    bs_sample <- sample(test_sample, size = length(test_sample),
                        replace = TRUE)
    bs_prop <- length(unique(bs_sample)) / length(test_sample)
    test_prop[iter] <- bs_prop
  }
  simulation_data[i, 2:4] <- 
    as.numeric(quantile(test_prop, probs = c(0.025, 0.5, 0.975),
                        type = 3))
}

simulation_plot <- 
  ggplot(simulation_data, aes(x = sample_size, y = estimate)) + 
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci),
              fill = "darkred", alpha = 0.65) + 
  geom_line(lwd = 1) + 
  annotate(geom = "text", label = "Bootstrap 95% CI",
           fontface = "bold", x = 195, y = 0.5, size = 2.8, hjust = 0) +
  geom_segment(x = 185, xend = 75, y = 0.5, yend = 0.55,
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.02, "inches"))) + 
  annotate(geom = "text", label = "Bootstrap Estimate",
           fontface = "bold", x = 345, y = 0.71, size = 2.8, hjust = 0) +
  geom_segment(x = 340, xend = 300, y = 0.7, yend = 0.64,
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.02, "inches"))) + 
  annotate(geom = "text", label = "Stablizes at 0.632",
           fontface = "bold", x = 900, y = 0.57, size = 2.8, hjust = 1) + 
  geom_segment(x = 910, xend = 990, y = 0.57, yend = 0.625,
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.02, "inches"))) + 
  annotate(geom = "text", 
           label = "Higher probability and more\nvariability in small samples",
           fontface = "bold", x = 100, y = 0.93, size = 2.8, hjust = 0) + 
  geom_segment(x = 100, xend = 25, y = 0.9, yend = 0.8,
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.02, "inches"))) + 
  scale_x_continuous(limits = c(0, 1000)) + 
  xlab("Sample Size") + 
  ylab("Proportion of Observations in Bootstrap Sample") + 
  labs(subtitle = "1,000 Bootstrap Samples for Datasets of Increasing Size (n = 1 to n = 1,000)") + 
  theme_classic() + 
  bootstrap_theme + 
  bootstrap_labels

## Distribution Plots ##
g <- 1:10000
j <- 50000
h <- numeric(length = j)

for (i in 1:j){
  set.seed(1991+i)
  bs_sample <- sample(g, size = length(g), replace = TRUE)
  prop_sample <- length(unique(bs_sample)) / length(g)
  h[i] <- prop_sample
}

distribution_data <- data.frame(g = g, h = h, k = "group")

histogram_plot <-
  ggplot(distribution_data, aes(x = h)) + 
  geom_vline(xintercept = 0.632, linetype = "dashed", color = "black") +
  geom_histogram(fill = "navyblue", color = "black", binwidth = 0.0005, 
                 alpha = 0.75) + 
  scale_x_continuous(breaks = c(0.622, 0.627, 0.632, 0.637, 0.642)) + 
  scale_y_continuous(limits = c(0, 3600)) + 
  geom_rect(xmin = 0.637, xmax = 0.643, ymin = 3200, ymax = 3600,
            linetype = 1, color = "black", fill = "grey") + 
  geom_segment(x = 0.637, xend = 0.6325, y = 3400, yend = 3400, 
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.05, "inches"))) + 
  annotate(geom = "text", label = TeX(val, output = "character"),
           parse = TRUE, size = 2.8, x = 0.6385, y = 3400, fontface = "bold") + 
  annotate(geom = "text", label = " = 0.632", size = 2.8, x = 0.641, y = 3400) + 
  xlab("Proportion of Observations in Bootstrap Sample") + 
  ylab("Frequency") + 
  theme_classic() + 
  bootstrap_theme + 
  labs(subtitle = "50,000 Bootstrap Samples of a Large Dataset (n = 10,000)") + 
  bootstrap_labels

violin_plot <-
  ggplot(distribution_data, aes(x = k, y = h)) + 
  geom_violin(fill = "#74A600", color = "black", alpha = 0.85, width = 0.5) + 
  geom_hline(yintercept = 0.632, linetype = "dashed") + 
  geom_boxplot(width = 0.05, outlier.size = 0.7,
               fill = "lightgrey", color = "black", lwd = 0.7) + 
  geom_rect(xmin = 0.55, xmax = 0.75, ymin = 0.626, ymax = 0.629,
            linetype = 1, color = "black", fill = "grey") + 
  geom_segment(x = 0.65, xend = 0.65, y = 0.629, yend = 0.6315, 
               arrow = arrow(type = "closed", angle = 30, 
                             length = unit(0.05, "inches"))) + 
  annotate(geom = "text", label = TeX(val, output = "character"),
           parse = TRUE, size = 2.8, x = 0.6, y = 0.6275, fontface = "bold") + 
  annotate(geom = "text", label = " = 0.632", size = 2.8, 
           x = 0.68, y = 0.6275) + 
  scale_y_continuous(breaks = c(0.617, 0.622, 0.627, 0.632, 
                                0.637, 0.642, 0.647)) + 
  ylab("Proportion of Observations in Boostrap Sample") + 
  xlab("") + 
  theme_classic() + 
  bootstrap_theme + 
  theme(
        axis.ticks.x = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_blank()) + 
  labs(subtitle = "50,000 Bootstrap Samples of a Large Dataset (n = 10,000)") + 
  bootstrap_labels

ggsave("/Users/student/Desktop/bootstrap_1.jpeg", plot = limit_plot,
       device = "jpeg", width = 6, height = 6, units = "in", dpi = 500)

ggsave("/Users/student/Desktop/bootstrap_2.jpeg", plot = histogram_plot,
       device = "jpeg", width = 6, height = 6, units = "in", dpi = 500)

ggsave("/Users/student/Desktop/bootstrap_3.jpeg", plot = violin_plot,
       device = "jpeg", width = 6, height = 6, units = "in", dpi = 500)

ggsave("/Users/student/Desktop/bootstrap_4.jpeg", plot = simulation_plot,
       device = "jpeg", width = 6, height = 6, units = "in", dpi = 500)
