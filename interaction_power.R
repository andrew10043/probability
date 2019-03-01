library(tidyverse)
library(RColorBrewer)

iter <- seq(from = 100, to = 5000, by = 50)

int_sim <- function(beta_trt = -0.4, beta_x = 0, ratio = 1) {
  
  beta_int <- beta_trt * ratio
  
  p_function <- function(trt, x) {
    
    exp((beta_trt * trt) + (beta_x * x) + (beta_int * trt * x)) / 
      (1 + exp((beta_trt * trt) + (beta_x * x) + (beta_int * trt * x)))
    
  }
  
  final_main <- numeric()
  final_int <- numeric()
  
  for (n in iter){
    
    out_main <- logical()
    out_int <- logical()
    
    for (j in 1:2000){
      
      set.seed(0 + n*j)
      
      trt <- rep(c(0, 1), each = n/2)
      x <- rep(c(0, 1), times = n/2)
      p <- p_function(trt = trt, x = x)
      y <- rbinom(n, size = 1, prob = p)
      
      model <- summary(glm(y ~ trt * x, family = "binomial"))

      out_main[j] <- model$coef["trt", "Pr(>|z|)"] < 0.05
      
      out_int[j] <- model$coef["trt:x", "Pr(>|z|)"] < 0.05
      
    }
    
    final_main[which(iter == n)] <- mean(out_main)
    final_int[which(iter == n)] <- mean(out_int)
    
  }
  
  return(list(main = final_main, int = final_int))
  
}

arg_list <- list(
  beta_trt = 0.4,
  beta_x = 0,
  ratio = list(1, 1.5, 2, 0.5)
)

sim <- pmap(arg_list,
            ~ int_sim(beta_trt = ..1,
                      beta_x = ..2,
                      ratio = ..3))

plot_data <-
  tibble(
    iter = rep(iter, 5),
    prob = c(sim[[1]]$main, sim[[1]]$int,
             sim[[2]]$int, sim[[3]]$int,
             sim[[4]]$int),
    group = rep(c("main", "int_1", "int_1.5", "int_2", "int_0.5"), 
                each = length(sim[[1]]$int))
  )

plot_1 <- 
ggplot(data = plot_data,
       aes(x = iter,
           y = prob)) + 
  geom_smooth(aes(group = group, color = group),
              se = FALSE, method = "loess",
              span = 0.25, size = 1) +
  geom_point(aes(group = group,
                color = group),
             alpha = 1) +
  scale_color_brewer(type = "qual",
                     palette = "Dark2") + 
  annotate(geom = "text", 
           label = "Main Effect",
           x = 600, y = 0.95, hjust = 1,
           color = brewer.pal(n = 5, name = "Dark2")[5],
           fontface = "bold", size = 3.5) +
  annotate(geom = "text", 
           label = "Interaction\nRatio: 0.5",
           x = 4200, y = 0.3, hjust = 0,
           color = brewer.pal(n = 5, name = "Dark2")[1],
           fontface = "bold", size = 3.5) +
  annotate(geom = "text", 
           label = "Interaction\nRatio: 1",
           x = 3500, y = 0.75, hjust = 0,
           color = brewer.pal(n = 5, name = "Dark2")[2],
           fontface = "bold", size = 3.5) +
  annotate(geom = "text", 
           label = "Interaction\nRatio: 1.5",
           x = 2500, y = 0.9, hjust = 0,
           color = brewer.pal(n = 5, name = "Dark2")[3],
           fontface = "bold", size = 3.5) +
  annotate(geom = "text", 
           label = "Interaction\nRatio: 2",
           x = 1425, y = 0.9, hjust = 0,
           color = brewer.pal(n = 5, name = "Dark2")[4],
           fontface = "bold", size = 3.5) +
  labs(
    x = "Sample Size",
    y = "Proportion of Significant Terms in Simulation",
    subtitle = expression(paste(logit(p[i]), paste(" = ", 
                          paste(alpha, paste(" + ",
                          paste(beta[1], paste(t[i],
                          paste(" + ", paste(beta[2],
                          paste(x[i], paste(" + ",
                          paste(beta[3], paste(t[i],
                          paste(x[i])))))))))))))),
    title = expression(paste(y[i], " ~ ",
                       paste("Binomial(", 
                       paste("1",
                       paste(", ",
                       paste(p[i],
                       paste(")")))))))
  ) + 
  theme_classic() + 
  theme(
    text = element_text(family = "Gill Sans MT"),
    legend.position = "none",
    plot.title = element_text(size = 12),
    plot.subtitle = element_text(size = 12),
    axis.title = element_text(size = 12, face = "bold"),
    axis.text = element_text(size = 11, face = "bold"))

ggsave("figures/interaction_plot.jpeg", plot_1, device = "jpeg",
       dpi = 400, width = 9, height = 9, units = "in")
