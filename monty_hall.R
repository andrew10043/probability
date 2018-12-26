## Monty Hall Problem ##
library(ggplot2)
library(latex2exp)

n <- 1000

switch <- logical(length = n)
stay <- logical(length = n)
switch_win <- numeric(length = n)
stay_win <- numeric(length = n)

for (iter in 1:n){
  set.seed(iter + 1991)
  car <- sample(x = c(1, 2, 3), size = 1)
  stay[iter] <- car == 1
  switch[iter] <- car != 1
  stay_win[iter] <- mean(stay[1:iter])
  switch_win[iter] <- mean(switch[1:iter])
}

plot_data <- data.frame(trial = 1:n, win_prob = c(stay_win, switch_win), 
                        strategy = c(rep("stay", 1000), rep("switch", 1000)))

monty_plot <- 
  ggplot(data = plot_data, aes(x = trial, y = win_prob, group = strategy)) +
  geom_hline(yintercept = 0.333, linetype = "dashed", alpha = 1, 
             color = "grey") + 
  geom_hline(yintercept = 0.666, linetype = "dashed", alpha = 1,
             color = "grey") + 
  geom_line(aes(color = strategy), lwd = 1) + 
  scale_color_manual(values = c("#74A600", "#8A0057")) + 
  coord_cartesian(xlim = c(0, 1010)) + 
  annotate(geom = "text", label = "P(win | stay) = 0.333", fontface = "bold", 
           x = 1000, y = 0.3, hjust = 1, size = 3.2) + 
  annotate(geom = "text", label = "P(win | switch) = 0.666", fontface = "bold",
           x = 1000, y = 0.70, hjust = 1, size = 3.2) + 
  annotate(geom = "text", label = "Stay with First Door",
           x = 300, y = 0.38, hjust = 0, color = "#74A600",
           fontface = "bold") + 
  annotate(geom = "text", label = "Switch Doors",
           x = 300, y = 0.62, hjust = 0, color = "#8A0057",
           fontface = "bold") + 
  xlab("Trial Number") + 
  ylab("Win Probability") + 
  labs(
    title = "The Monty Hall Problem",
    subtitle = "Monty reveals a goat, do you switch doors or stay with your first choice?",
    caption = "Graphic by Ben Andrew | @BenYAndrew") + 
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
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.position = "none")
  
ggsave("/Users/student/Desktop/monty_plot.jpeg", plot = monty_plot,
       device = "jpeg", width = 8, height = 8, units = "in")

