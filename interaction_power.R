library(tidyverse)
library(RColorBrewer)

iter <- seq(from = 100, to = 3000, by = 100)
num_sim <- 2000

int_sim <- function(beta_trt = -0.6, beta_x = -0.6, ratio = 1) {
  
  beta_int <- beta_trt * ratio
  
  p_function <- function(trt, x) {
    
    exp((beta_trt * trt) + (beta_x * x) + (beta_int * trt * x)) / 
      (1 + exp((beta_trt * trt) + (beta_x * x) + (beta_int * trt * x)))
    
  }
  
  trt <- rep(c(0, 1), times = iter[length(iter)]/2)
  x <- rep(c(1, 1, 0, 0), times = iter[length(iter)]/4)
  p <- p_function(trt = trt, x = x)
  
  # Draw samples up front (columns = unique samples)
  samples <- sapply(1:num_sim, function(x){
    
    set.seed(x)
    rbinom(iter[length(iter)], size = 1, prob = p)
  
  })
  
  out <- lapply(iter, function(i){
    
    sapply(1:num_sim, function(z){
      
      summary(
        glm(samples[1:i, z] ~ trt[1:i] * x[1:i], 
                          family = "binomial"))$coefficients[c("trt[1:i]", 
                                                               "trt[1:i]:x[1:i]"), 
                                                             "Pr(>|z|)"]
      
    })
    
  })
  
  prop <- sapply(1:length(iter), function(k){
    
    list(mean(unlist(out[[k]][1, ]) < 0.05),
         mean(unlist(out[[k]][2, ]) < 0.05))
    
  })
  
  df <-
    tibble(
      n = iter,
      main = unlist(prop[1, ]),
      int = unlist(prop[2, ])
    )
  
  
}

arg_list <- list(
  beta_trt = -0.6,
  beta_x = -0.6,
  ratio = list(0.5, 0.75, 1, 1.25)
)

sim <- pmap(arg_list,
            ~ int_sim(beta_trt = ..1,
                      beta_x = ..2,
                      ratio = ..3))

plot_data <-
  tibble(
    iter = rep(iter, 5),
    prob = c(sim[[1]]$main, sim[[1]]$int,
             sim[[2]]$int, sim[[3]]$int, sim[[4]]$int),
    group = rep(c("main", "int_0.5", "int_0.75", "int_1",
                  "int_1.25"), 
                each = length(sim[[1]]$int))
  )

plot_1 <- 
ggplot(data = plot_data,
       aes(x = iter,
           y = prob)) + 
  geom_smooth(aes(group = group, color = group),
              se = FALSE, method = "loess",
              span = 0.5, size = 1) +
  geom_point(aes(group = group,
                color = group),
             alpha = 1) +
  scale_color_brewer(type = "qual",
                     palette = "Dark2") + 
  annotate(geom = "text",
           label = "Main Effect",
           x = 1000, y = 0.95, hjust = 1,
           color = brewer.pal(n = 5, name = "Dark2")[5],
           fontface = "bold", size = 3.5) +
  annotate(geom = "text",
           label = "Interaction\nRatio: 0.5",
           x = 2200, y = 0.3, hjust = 0,
           color = brewer.pal(n = 5, name = "Dark2")[1],
           fontface = "bold", size = 3.5) +
  annotate(geom = "text",
           label = "Interaction\nRatio: 0.75",
           x = 2500, y = 0.64, hjust = 0,
           color = brewer.pal(n = 5, name = "Dark2")[2],
           fontface = "bold", size = 3.5) +
  annotate(geom = "text",
           label = "Interaction\nRatio: 1",
           x = 2400, y = 0.85, hjust = 0,
           color = brewer.pal(n = 5, name = "Dark2")[3],
           fontface = "bold", size = 3.5) +
  annotate(geom = "text",
           label = "Interaction\nRatio: 1.25",
           x = 1425, y = 0.83, hjust = 0,
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
                          paste(x[i], paste(" [",
                          paste(alpha, paste(" = 0, ",
                          paste(beta[1], paste(" = -0.6, ",
                          paste(beta[2], paste(" = -0.6, ",
                          paste(beta[3], paste(" = ",
                          paste(beta[1], paste("*ratio",
                          paste("]")))))))))))))))))))))))))),
                          
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
       dpi = 400, width = 7, height = 7, units = "in")
