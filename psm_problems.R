## PSM vs. MDM
library(ggplot2)
library(dplyr)
library(StatMatch)
library(purrr)
library(cowplot)

n_df <- 1000     # Number of data sets to generate
n_mp <- 50       # Sample size for matched pair exp.
n_cr <- 50       # Sample size for completely randomized exp.
n_obs <- 50      # Sample size for observational exp. additions
mp_jitter <- 0.1 # Jitter for matched pair generation

iter_list <- 1:n_df

# Function to generate a data frame with a simulated sample
generate_simulation <- 
  function(mp_size,
           cr_size,
           obs_size,
           mp_jitt,
           iter) {
    
    # Iterative seed
    set.seed(11 + iter) 
    
    # Matched pair 'treated' values
    mp_t_x1 <- runif(   
      n = mp_size / 2,
      min = -2,
      max = 2
    )
    
    mp_t_x2 <- runif(   
      n = mp_size / 2,
      min = -2,
      max = 2
    )
    
    # Jitter matched pair 'treated' values to get 'control' values
    mp_c_x1 <- jitter(  
      mp_t_x1,
      amount = mp_jitt
    )
    
    mp_c_x2 <- jitter(
      mp_t_x2,
      amount = mp_jitt
    )
    
    # Completely randomized values
    cr_x1 <- runif(
      n = cr_size,
      min = -2,
      max = 2
    )
    
    cr_x2 <- runif(
      n = cr_size,
      min = -8,
      max = -4
    )
    
    # Observational values
    obs_x1 <- runif(
      n = obs_size,
      min = -8,
      max = -4
    )
    
    obs_x2 <- runif(
      n = obs_size,
      min = -8,
      max = 2
    )
    
    # Sample data frame for a given simulation
    df <- data.frame(
      x1 = c(mp_t_x1, mp_c_x1, cr_x1, obs_x1),
      x2 = c(mp_t_x2, mp_c_x2, cr_x2, obs_x2),
      set = c(
        rep("mp", times = mp_size),
        rep("cr", times = cr_size),
        rep("obs", times = obs_size)),
      treat = c(
        rep(1, times = mp_size / 2),
        rep(0, times = mp_size / 2),
        rep(1, times = cr_size / 2),
        rep(0, times = cr_size / 2),
        rep(0, times = obs_size)),
      id = 1:150)
    
  }

# Map over iter_list to generate a list of df's, each with a simulated sample
sim <- pmap(
  .l = list(iter_list),
  .id = "n_iter",
  ~ generate_simulation(
    iter = ..1,
    mp_size = n_mp,
    cr_size = n_cr,
    obs_size = n_obs,
    mp_jitt = mp_jitter))

# Generate plotting data frame
plot_sim <- sim %>%
  bind_rows(.id = "n_iter") %>%
  mutate(
    plt_group = paste(set, treat),
    plt_group = factor(plt_group)
  )

# Function to iteratively prune observations from data frame using
# PS and MH matching methods
prune_order <-
  function(df) {

    df <- data.frame(df)
    
    ps_model <- glm(
      treat ~ x1 + x2,
      family = "binomial",
      data = df
    )

    ps_pred <- predict(ps_model, newdata = df)

    ps_dist <- as.matrix(dist(ps_pred))

    mh_dist <- mahalanobis.dist(df[, c('x1', 'x2')])
    
    mh_list <- c()
    ps_list <- c()

    for (k in 1:150) {

      mh_min <- numeric(length = 150)
      ps_min <- numeric(length = 150)

      for (j in 1:150) {
        
        # Iterate over each column of the distance matrix.
        # Look for minimum distance for each column.
        # Restrict to observations with opposite treat/control value.
        
        if (j %in% c(1:25, 51:75)) {

          mh_min[j] <-
            min(mh_dist[c(26:60, 76:50), j][mh_dist[c(26:60, 76:50), j] > 0])

          ps_min[j] <-
            min(ps_dist[c(26:60, 76:50), j][ps_dist[c(26:60, 76:50), j] > 0])

        } else {

          mh_min[j] <-
            min(mh_dist[c(1:25, 51:75), j][mh_dist[c(1:25, 51:75), j] > 0])

          ps_min[j] <-
            min(ps_dist[c(1:25, 51:75), j][ps_dist[c(1:25, 51:75), j] > 0])

        }

      }
      
      # Order the minimum distances
      mh_order <- order(mh_min, decreasing = TRUE)
      ps_order <- order(ps_min, decreasing = TRUE)
      
      # Identify the highest minimum distance for pruning
      mh_out <- mh_order[1]
      ps_out <- ps_order[1]
      
      # Set this distance in the distance matrix to effectively '0'
      # This step ensures that the next iteration will not id the same point
      mh_dist[, mh_out] <- 1e-10
      ps_dist[, ps_out] <- 1e-10
      
      # Iteratively generate list of pruned points
      mh_list <- c(mh_list, mh_out)
      ps_list <- c(ps_list, ps_out)
      
    }
    
    mh_temp <-
      data.frame(
        order = 1:150,
        method = "mh",
        set = df$set[mh_list],
        stringsAsFactors = FALSE) %>%
      mutate(set = as.character(set))

    ps_temp <-
      data.frame(
        order = 1:150,
        method = "ps",
        set = df$set[ps_list],
        stringsAsFactors = FALSE) %>%
      mutate(set = as.character(set))

    random_temp <-
      data.frame(
        order = 1:150,
        method = "rnd",
        set = c(as.character(df$set[ps_list[1:50]]),
                sample(c("mp", "cr"),
                       size = 100,
                       replace = TRUE)),
        stringsAsFactors = FALSE
      )

    prune_data <-
      mh_temp %>%
      bind_rows(ps_temp) %>%
      mutate(set = as.character(set)) %>%
      bind_rows(random_temp)

  }

# Map over simulated df's and prune observations
prune_df <- pmap_df(
  .l = list(sim),
  .id = "n_iter",
  ~ prune_order(
    df = ..1
  )
) %>%
  mutate(n_iter = as.numeric(n_iter))

# Plotting colors
obs_color <- '#FF7A00'
cr_color <- '#0A32A9'
mp_color <- '#00C021'

# Representative sample plot
df_plot <-
  ggplot(
    data = filter(plot_sim,
                  n_iter == 1),
    aes(x = x1, y = x2)) + 
  geom_point(
    aes(color = plt_group,
        shape = plt_group),
    alpha = 0.5,
    stroke = 1,
    size = 4) +
  facet_wrap(~ n_iter) + 
  scale_color_manual(
    values = c(cr_color, cr_color, mp_color, mp_color, obs_color),
    breaks = c("cr 0", "mp 0", "obs 0", "cr 1","mp 1"),
    labels = c("Completely Randomized (Control)", 
               "Matched Pair (Control)",
               "Observational (Control)",
               "Completely Randomized (Treated)",
               "Matched Pair (Treated)")) +
  scale_shape_manual(
    values = c(19, 4, 19, 4, 19),
    breaks = c("cr 0", "mp 0", "obs 0", "cr 1","mp 1"),
    labels = c("Completely Randomized (Control)", 
               "Matched Pair (Control)",
               "Observational (Control)",
               "Completely Randomized (Treated)",
               "Matched Pair (Treated)")) +
  scale_x_continuous(
    breaks = c(-8, -6, -4, -2, 0, 2)) + 
  scale_y_continuous(
    breaks = c(-8, -6, -4, -2, 0, 2)) + 
  xlab("X1") + 
  ylab("X2") + 
  labs(
    title = "Representative Sample",
    caption = ""
  ) + 
  guides(
    color = guide_legend(nrow = 2, byrow = TRUE)) + 
  theme_classic() + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    strip.text = element_blank(),
    legend.position = "bottom",
    legend.text = element_text(size = 11),
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    text = element_text(family = "Gill Sans MT"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    strip.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    plot.caption = element_text(size = 9, hjust = 1,
                                margin = margin(t = 0.6, unit = "cm"))
  )

# Prune plot
facet_labels <- c(mh = "Mahalanobis", 
                  ps = "Propensity Score", 
                  rnd = "PS & Random")

heat_plot <- 
  ggplot(
    data = filter(prune_df, n_iter <= 1000),
    aes(x = order,
        y = n_iter)) + 
  geom_point(
    aes(color = set,
        fill = set),
    shape = 15,
    size = 0.5) + 
  geom_segment(x = 100, xend = 100,
               y = 0, yend = 1000,
               linetype = "dashed",
               color = "black",
               lwd = 0.75) + 
  facet_wrap(~ method, labeller = labeller(method = facet_labels)) +
  xlab("Prune Order") + 
  ylab("Simulation Number") + 
  labs(
    title = "Prune Order by Matching Method",
    caption = 
         "Adapted from: gking.harvard.edu | Graphic by Ben Andrew") + 
  scale_color_manual(
    values = c(cr_color, mp_color, obs_color),
    breaks = c("obs", "cr", "mp"),
    labels = c("Observational", "Completely Randomized", 
               "Matched Pair")) +
  scale_fill_manual(
    values = c(cr_color, mp_color, obs_color),
    breaks = c("obs", "cr", "mp"),
    labels = c("Observational", "Completely Randomized",
               "Matched Pair")) +
  guides(
    color = guide_legend(override.aes = list(size = 3))) + 
  theme_classic() + 
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    legend.position = "bottom",
    strip.text = element_text(size = 12),
    legend.text = element_text(size = 13),
    legend.title = element_blank(),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    text = element_text(family = "Gill Sans MT"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA), 
    panel.background = element_rect(fill = "#f5f5f2", color = NA), 
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    strip.background = element_rect(fill = "#f5f5f2", color = NA),
    plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
    plot.caption = element_text(size = 9, hjust = 1,
                                margin = margin(t = 0.6, unit = "cm"))
  )

grid <- plot_grid(df_plot, heat_plot,
                  ncol = 2, align = "hv",
                  axis = "tb")

ggsave("figures/ps_match.jpeg", plot = grid,
       device = "jpeg", width = 14, height = 9, units = "in", dpi = 300)
