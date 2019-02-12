library(tidyverse)
library(cowplot)
library(scales)

num_samples <- 1e4
lambda_list <- seq(from = 1, to = 10, by = 0.1)

entropy_list <- numeric(length = length(lambda_list))
sample_list <- list()

# Calculate average entropy for various lambda

for (i in 1:length(lambda_list)) { 
  
  temp_entropy <- numeric()
  
  # Repeat 100 times and average
  for (iter in 1:100){
    
    set.seed(iter + 16)
    temp_sample <- rpois(n = num_samples, lambda = lambda_list[i])
    temp_p <- as.numeric(table(temp_sample) / num_samples)
    temp_entropy[iter] <- -sum(temp_p * log(temp_p))
    
    # Store last sample for KLD plots
    if (iter == 100) {
      
      sample_list[[i]] <- temp_sample
      
      }
    
    }
  
  entropy_list[[i]] <- mean(temp_entropy)
  
}

plot_data <- tibble(
  lambda = lambda_list,
  entropy = entropy_list
)

hist_data <- 
  tibble(
    x = c(sample_list[[1]], sample_list[[51]], sample_list[[91]]),
    group = rep(c("low", "mid", "high"), each = num_samples)
    )

# Entropy plot
plt_1 <- 
  ggplot(data = plot_data, aes(x = lambda, y = entropy)) + 
  geom_point(pch = 19) + 
  labs(
    x = expression(italic(lambda)),
    y = "Entropy",
    subtitle = expression(paste("Range of Entropy: ", H(italic(p)))),
    caption = ""
    ) +
  scale_x_continuous(breaks = 1:10) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    plot.caption = element_text(size = 7)
    )

# Representative distribution plot
plt_2 <-
  ggplot() + 
  geom_histogram(data = hist_data %>% filter(group == "low"),
                 binwidth = 1, color = "black",
                 aes(x = x), fill = "#06759A", alpha = 0.4,
                 center = 0) + 
  geom_histogram(data = hist_data %>% filter(group == "mid"),
                 binwidth = 1, color = "black",
                 aes(x = x), fill = "#8E1900", alpha = 0.4,
                 center = 0) + 
  geom_histogram(data = hist_data %>% filter(group == "high"),
                 binwidth = 1, color = "black",
                 aes(x = x), fill = "#007126", alpha = 0.4,
                 center = 0) + 
  coord_cartesian(xlim = c(0, 25)) + 
  labs(
    x = "Number of Events",
    y = "Count",
    subtitle = "Representative Distributions",
    caption = ""
    ) + 
  annotate(geom = "text", label = expression(paste(lambda, " = 1")),
           color = "#06759A", size = 4,
           x = 2.05, y = 2000, hjust = 0) + 
  annotate(geom = "text", label = expression(paste(lambda, " = 6")),
           color = "#8E1900", size = 4,
           x = 7.05, y = 1600, hjust = 0) + 
  annotate(geom = "text", label = expression(paste(lambda, " = 10")),
           color = "#007126", size = 4,
           x = 11.05, y = 1350, hjust = 0) + 
  theme_classic() +
  theme(
    text = element_text(family = "Gill Sans MT"),
    plot.caption = element_text(size = 7)
    )

# Loop over all "target distributions" and find KLD from others to target
kl_data <- tibble(initial = numeric(length = length(lambda_list)))

for (s in 1:length(sample_list)){
  
  target <- sample_list[[s]]
  target_p <- numeric(length = 25)
  
  for (i in 1:25){
    
    # Calculate probabilities for counts
    # Ensure no zero probabilities by rescaling for sake of KLD calculations
    target_p[i] <- sum(target == i) / num_samples
    target_p <- rescale(target_p, to = c(1e-10, 1)) 
    
    }
  
  # For all distributions, calculate the KL divergence to this target
  temp_list <- numeric(length = length(lambda_list))
  
  for (i in 1:length(lambda_list)) { 
    
    sample_p <- numeric(length = 25)
    
    for (j in 1:25){
      
      sample_p[j] <- sum(sample_list[[i]] == j) / num_samples
      
      }
    
    # Again rescale to ensure no zero probabilities for sake of KDL calculation
    sample_p <- rescale(sample_p, to = c(1e-10, 1))
    temp_list[i] <- sum(target_p * (log(target_p) - log(sample_p)))
    
    }
  
  kl_data <- bind_cols(kl_data, tibble(temp_list))
  
  }

kl_data <-
  kl_data %>%
  select(-initial) %>%
  mutate(lambda = seq(from = 1, to = 10, by = 0.1))

colnames(kl_data) <- c(seq(from = 1, to = 10, by = 0.1), "lambda")

heat_data <- 
  kl_data %>%
  gather(key = "target", value = "kld", -lambda) %>%
  mutate(target = as.numeric(target))

# KLD plot
heat_1 <-
  ggplot(heat_data,
         aes(x = lambda, y = target)) + 
  geom_tile(aes(fill = kld)) + 
  labs(
    x = expression(paste("Model ", lambda)),
    y = expression(paste("Target ", lambda)),
    subtitle = expression(paste("KL Divergence: ", D[KL](italic(p), italic(q)))),
    caption = ""
    ) + 
  scale_fill_viridis_c(name = NULL) + 
  theme_classic() + 
  theme(
    legend.position = "bottom",
    text = element_text(family = "Gill Sans MT"),
    plot.caption = element_text(size = 7)
    )

# Look at differences based on direction
diff_data <- heat_data
diff_data$diff <- NA

for (i in 1:nrow(diff_data)) {
  
  # Find matching row (where lambda = target and target = lambda)
  match_row = which(near(diff_data$target, diff_data$lambda[i]) & 
                      near(diff_data$lambda, diff_data$target[i]))
  
  if (match_row == i) {
    
    diff_data$diff[i] <- 0
    
    } else {
      
      diff_data$diff[i] <- diff_data$kld[i] - diff_data$kld[match_row]
      
    }
  
  }

# KLD difference plot
heat_2 <- 
  ggplot(diff_data,
         aes(x = lambda, y = target)) + 
  geom_tile(aes(fill = diff)) + 
  labs(
    x = expression(paste("Model ", lambda)),
    y = expression(paste("Target ", lambda)),
    subtitle = expression(paste("Difference in KL Divergence: ", D[KL](italic(p), italic(q)) - D[KL](italic(q), italic(p)))),
    caption = "Graphic by Ben Andrew | @BenYAndrew" 
    ) + 
  scale_fill_viridis_c(option = "B", name = NULL) + 
  theme_classic() + 
  theme(
    legend.position = "bottom",
    text = element_text(family = "Gill Sans MT"),
    plot.caption = element_text(size = 9)
    )

grid <- plot_grid(plt_1, plt_2, heat_1, heat_2, ncol = 2, align = "hv",
                  labels = "AUTO",
                  label_fontfamily = "Gill Sans MT",
                  rel_heights = c(0.75, 1))

title <- ggdraw() + 
  draw_label("Entropy & KL Divergence in Poisson Distributions",
             fontface = 'bold',
             fontfamily = "Gill Sans MT")

grid_b <- plot_grid(title, grid, ncol = 1, rel_heights = c(0.1, 1))

ggsave("figures/entropy_kld.jpeg", grid_b, device = "jpeg",
       width = 8, height = 9, dpi = 300)

