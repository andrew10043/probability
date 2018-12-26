## The Birthday Problem ##
library(latex2exp)
library(ggplot2)

k <- 0:100
n <- 365

probNoMatch = numeric(length = length(k))

for (iter in 1:length(k)){
  if (k[iter] %in% c(0, 1)){
    probNoMatch[iter] = 1
  } else {
    f <- k[iter] - 1
    r <- 365 - (0:f)
    probNoMatch[iter] = prod(r) / (365^k[iter])
  }
}

plotData = data.frame(k = k, probMatch = 1-probNoMatch)

eqn <- "$P(\\geq 1\\,match) = 1 - \\,\\frac{365 \\cdot 364 \\,\\cdots\\, (365-k+1)}{365^{k}}$"

birthday_plot <- 
  ggplot(data = plotData, aes(x = k, y = probMatch)) + 
    geom_point() + 
    xlab("Number of People (k)") + 
    ylab("Probability of at Least One Match") + 
    labs(
      title = "The Birthday Problem",
      subtitle = "There are k people in a room. What is the probability that two or more people have the same birthday?",
      caption = "Graphic by Ben Andrew | @BenYAndrew") + 
    geom_segment(aes(x = 23, xend = 23, y = -0.01, yend = 0.5), 
                 color = "blue") + 
    geom_segment(aes(x = -1, xend = 23, y = 0.5, yend = 0.5), 
                 color = "blue") +
    geom_segment(aes(x = 46, xend = 46, y = -0.01, yend = 0.95), 
                 color = "purple") + 
    geom_segment(aes(x = -1, xend = 46, y = 0.95, yend = 0.95), 
                 color = "purple") +
    geom_segment(aes(x = 57, xend = 57, y = -0.01, yend = 0.99), 
                 color = "red") + 
    geom_segment(aes(x = -1, xend = 57, y = 0.99, yend = 0.99), 
                 color = "red") +
    geom_rect(xmin = 61, xmax = 97, ymin = 0.03, ymax = 0.13,
              linetype = 0, fill = "lightgrey") + 
    geom_rect(xmin = 61, xmax = 97, ymin = 0.15, ymax = 0.36,
              linetype = 0, fill = "lightgrey") + 
    annotate(geom = "text", x = 2, y = 0.53, label = "0.50 at k = 23",
             hjust = 0, size = 3.5, color = "blue") + 
    annotate(geom = "text", x = 2, y = 0.92, label = "0.95 at k = 46",
             hjust = 0, size = 3.5, color = "purple") + 
    annotate(geom = "text", x = 2, y = 1.02, label = "0.99 at k = 57",
             hjust = 0, size = 3.5, color = "red") + 
    annotate(geom = "text", x = 63, y = 0.33, label = "Basic Assumptions:",
             hjust = 0, size = 3, fontface = "bold") + 
    annotate(geom = "text", x = 63, y = 0.28, 
             label = "- 365 days per year (i.e., 2/29 excluded)",
             hjust = 0, size = 3) + 
    annotate(geom = "text", x = 63, y = 0.23, 
             label = "- Independence (i.e., no twins)",
             hjust = 0, size = 3) + 
    annotate(geom = "text", x = 63, y = 0.18, 
             label = "- Equal probability of birth on any day",
             hjust = 0, size = 3) + 
    annotate(geom = "text", x = 63, y = 0.08,
             hjust = 0,
             label = TeX(eqn, output = "character"),
             parse = TRUE, size = 2.8) + 
    coord_cartesian(xlim = c(-1,101), 
                    ylim = c(-0.01, 1.05), 
                    expand = FALSE) + 
    scale_x_continuous(breaks = 0:100,
                       labels = c(0, rep("", 9), 10, rep("", 9),
                                  20, rep("", 9), 30, rep("", 9), 40,
                                  rep("", 9), 50, rep("", 9), 60, rep("", 9),
                                  70, rep("", 9), 80, rep("", 9), 90, rep("", 9),
                                  100)) + 
    scale_y_continuous(breaks = seq(from = 0, to = 1, by = 0.05), 
                       labels = c("0.00", rep("", 4), 0.25, rep("", 4), "0.50",
                                  rep("", 4), 0.75, rep("", 4), "1.00")) + 
    theme_classic() + 
    theme(
      plot.margin = margin(t = 0.75, r = 0.75, b = 0.5, l = 0.75, unit = "cm"),
      plot.title = element_text(size = 18, hjust = 0.5,
                                margin= margin(b = 0.5, unit = "cm")),
      plot.subtitle = element_text(size = 11, hjust = 0.5,
                                margin= margin(b = 0.5, unit = "cm")),
      plot.caption = element_text(size = 10, hjust = 1,
                                  margin = margin(t = 0.6, unit = "cm")),
      axis.text = element_text(size = 12),
      axis.title.x = element_text(size = 15,
                                  margin = margin(t = 0.4, unit = "cm")),
      axis.title.y = element_text(size = 15,
                                  margin = margin(r = 0.4, unit = "cm")),
      text = element_text(family = "Gill Sans MT"),
      plot.background = element_rect(fill = "#f5f5f2", color = NA))
 
ggsave("figures/birthday_plot.jpeg", plot = birthday_plot,
       device = "jpeg", width = 8, height = 8, units = "in")
