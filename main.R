library(tidymodels)
library(DALEX)
library(ingredients)

library(distributions3)
library(colorblindr)

set.seed(42)


# visualization -----------------------------------------------------------


theme_scatter = function() {
  theme_minimal(base_size = 12) %+replace%
    theme(panel.grid.minor = element_blank(),
          panel.grid.major = element_line(color = "gray", size = 0.1),
          legend.position = "top",
          axis.title = element_text(size = 15, color = "black"),
          axis.title.x = element_text(margin = margin(10, 0, 0, 0), hjust = 1),
          axis.title.y = element_text(margin = margin(0, 10, 0, 0), angle = 90, hjust = 1),
          axis.text = element_text(size = 12, color = "black"),
          strip.text = element_text(size = 15, color = "black", margin = margin(5, 5, 5, 5)),
          plot.title = element_text(size = 15, color = "black", margin = margin(0, 0, 18, 0)))
}


save_plot = function(fname, width = 8, height = 4) {
  ggsave(fname, width = width, height = height, dpi = "retina")
}

# Simulation1 -------------------------------------------------------------



N = 1000

U = Uniform(-1, 1)
Z = Normal(mu = 0, sigma = 0.1)

X1 = random(U, N)
X2 = random(U, N)
X3 = random(U, N)
E = random(Z, N)

Y = X1 - 5*X2 + E

df = tibble(Y, X1, X2, X3)

df %>% 
  sample_n(200) %>% 
  pivot_longer(cols = contains("X"), values_to = "X") %>% 
  ggplot(aes(X, Y)) +
  geom_point(size = 3, 
             shape = 21,
             color = "white", fill = palette_OkabeIto[5], 
             alpha = 0.7) +
  facet_wrap(~name) + 
  theme_scatter()

save_plot("figure/sim1_scatter.png")

fitted = rand_forest(mode = "regression", 
                     trees = 1000,
                     mtry = 3,
                     min_n = 1) %>% 
  set_engine(engine = "ranger", 
             num.threads = parallel::detectCores(), 
             seed = 42) %>% 
  fit(Y ~ ., data = df)


explainer = explain(fitted,
                    data = df %>% select(-Y),
                    y = df %>% pull(Y),
                    label = "Random Forest")


fi = feature_importance(explainer, 
                        loss_function = loss_root_mean_square,
                        type = "raw")
fi

plot(fi)

save_plot("figure/sim1_fi.png")

pdp = partial_dependency(explainer)

pdp %>% 
  plot() + 
  scale_y_continuous(breaks = seq(-6, 6, 2)) + 
  theme_scatter() +
  theme(legend.position = "none")

save_plot("figure/sim1_pdp.png")


# Simulation2 -------------------------------------------------------------


N = 1000

U = Uniform(-1, 1)
B = Bernoulli(p = 0.5)
Z = Normal(mu = 0, sigma = 0.1)

X1 = random(U, N)
X2 = random(U, N)
X3 = random(B, N)
E = random(Z, N)

Y = X1 - 5*X2 + 10*X2*X3 + E

df = tibble(Y, X1, X2, X3)


df %>% 
  sample_n(200) %>% 
  pivot_longer(cols = contains("X"), values_to = "X") %>% 
  ggplot(aes(X, Y)) +
  geom_point(size = 3, 
             shape = 21,
             color = "white", fill = palette_OkabeIto[5], 
             alpha = 0.7) +
  facet_wrap(~name) + 
  theme_scatter()

save_plot("figure/sim2_scatter.png")

fitted = rand_forest(mode = "regression", 
                     trees = 1000,
                     mtry = 3,
                     min_n = 1) %>% 
  set_engine(engine = "ranger", 
             num.threads = parallel::detectCores(), 
             seed = 42) %>% 
  fit(Y ~ ., data = df)


explainer = DALEX::explain(fitted,
                           data = df %>% select(-Y),
                           y = df %>% pull(Y),
                           label = "Random Forest")



pdp = partial_dependency(explainer, variables = "X2")

pdp %>% 
  plot() + 
  geom_abline(slope = -5, color = "gray70", size = 1) + 
  geom_abline(slope = 5, color = "gray70", size = 1) + 
  scale_y_continuous(breaks = seq(-6, 6, 2),
                     limits = c(-6, 6)) + 
  theme_scatter() +
  theme(legend.position = "none")


save_plot("figure/sim2_pdp.png", height = 6)


ice = ceteris_paribus(explainer, 
                      variables = "X2",
                      new_observation = df %>% sample_n(100) %>% as.data.frame())


ice %>% 
  plot(alpha = 0.5, size = 0.5, color = colors_discrete_drwhy(1)) + 
  geom_abline(slope = -5, color = "gray70", size = 1) + 
  geom_abline(slope = 5, color = "gray70", size = 1) +
  scale_y_continuous(breaks = seq(-6, 6, 2),
                     limits = c(-6, 6)) + 
  theme_scatter() +
  theme(legend.position = "none")

save_plot("figure/sim2_ice.png", height = 6)


conditional_pdp = aggregate_profiles(ice, groups = "X3")

conditional_pdp %>% 
  plot() + 
  geom_abline(slope = -5, color = "gray70", size = 1) + 
  geom_abline(slope = 5, color = "gray70", size = 1) +
  scale_y_continuous(breaks = seq(-6, 6, 2),
                     limits = c(-6, 6)) + 
  theme_scatter() +
  theme(legend.position = "none")

save_plot("figure/sim2_comnditional_pdp.png", height = 6)


clustered_ice = cluster_profiles(ice, k = 2)

clustered_ice %>% 
  plot() + 
  geom_abline(slope = -5, color = "gray70", size = 1) + 
  geom_abline(slope = 5, color = "gray70", size = 1) +
  scale_y_continuous(breaks = seq(-6, 6, 2),
                     limits = c(-6, 6)) + 
  theme_scatter() +
  theme(legend.position = "none")

save_plot("figure/sim2_clustered_ice.png", height = 6)







