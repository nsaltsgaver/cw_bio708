# two-group comparison: t-test

pacman::p_load(tidyverse)
rm(list = ls())

# read fish lengths in
df_fl <- read_csv("data_src/data_fish_length.csv")

unique(df_fl$lake)

distinct(df_fl, lake)

df_fl_mu <- df_fl %>%
  group_by(lake) %>%
  summarise(
    mu_l = mean(length),
    sd_l = sd(length)
  )

# figure

df_fl %>%
  ggplot(
    aes(x = lake,
        y = length)
    
  ) + 
  geom_jitter(
    width = 0.1,
    height = 0,
    alpha = 0.25
  ) +
  geom_segment(
    data = df_fl_mu,
    aes(
      x = lake,
      xend = lake,
      y=mu_l - sd_l,
      yend = mu_l + sd_l
    )
  )

#t-test

x <- df_fl %>%
  filter(lake == "a") %>%
  pull(length)

y <- df_fl %>%
  filter(lake == "b") %>%
  pull(length)

t.test(x, y, var.equal = TRUE)

# get t-value

v_mu <- df_fl_mu %>%
  pull(mu_l)

v_mu[1] - v_mu[2]

df_t <- df_fl %>%
  group_by(lake) %>%
  summarise(
    mu_l = mean(length),
    var_l = var(length),
    n = n()
  )

# Mean vector

v_mu <- pull(df_t, mu_l)

# Variance vector

v_var <- pull(df_t, var_l)

# sample size vector

v_n <- pull(df_t, n)

var_p <- ((v_n[1] - 1) / (sum(v_n)-2)) * v_var[1] +
  ((v_n[2] - 1) / (sum(v_n) -2)) * v_var[2]

t_value <- (v_mu[1] - v_mu[2]) / sqrt(var_p * ((1 / v_n[1]) + (1 / v_n[2])))

# get p-value

x <- seq(-5,5, length=500)

# prob. density of t-statistics with df = 98

y <- dt(x, df=sum(v_n) - 2)

tibble(x, y) %>%
  ggplot(
    aes(
      x=x,
      y=y
    )
  ) +
  geom_line() +
  geom_vline(xintercept = t_value,
             color = "salmon") + 
  geom_vline(xintercept = abs(t_value),
             color = "salmon") +
  labs(y="Probability Density",
       x="t-statistics")

# t-test under unequal variance

x <- df_fl %>%
  filter(lake == "a") %>%
  pull(length)

y <- df_fl %>%
  filter(lake == "b") %>%
  pull(length)

t.test(x, y, var.equal = FALSE)
