# Sampling

rm(list = ls())
pacman::p_load(tidyverse)

h <- c(16.9, 20.9, 15.8, 28, 21.6, 15.9, 22.4, 23.7, 22.9, 18.5)
df_h <- tibble(plant_id = 1:length(h),
       height = h,
       unit = "cm")

df_h1 <- df_h %>%
  mutate(mu_height = mean(h),
         var_height = sum((h - mean(h))^2) / nrow(.))

h <- c(27.6, 21.9, 16.9, 8.9, 25.6, 19.8, 19.9, 24.7, 24.1, 23)

df_h2 <- tibble(plant_id = 11:20,
                height = h,
                unit = "cm") %>% 
  mutate(mu_height = mean(height),
         var_height = sum((height - mu_height)^2) / nrow(.))

## read csv file from source

df_h0 <- read_csv("data_src/data_plant_height.csv")

## true_mean

mu <- mean(df_h0$height)

## true variance

sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)

## random sampling

df_i <- df_h0 %>%
  sample_n(size = 10)

## for loop

mu_i <- var_i <- NULL

for (i in 1:1000) {
  ## randomly sample 10 individuals
  
  df_i <- df_h0 %>%
    sample_n(size = 10)
  
  # mean for subset
  
  mu_i[i] <- mean(df_i$height)
  
  # variance for subset
  
  var_i[i] <- sum((df_i$height - mu_i[i])^2) / nrow(df_i)
}

## randomly sample 10 individuals

df_i <- df_h0 %>%
  sample_n(size = 10)

# mean for subset

mu_i <- mean(df_i$height)

# variance for subset

var_i <- sum((df_i$height - mu_i)^2) / nrow(df_i)

library(patchwork)

## visualization
df_sample <- tibble(mu_hat = mu_i,
                    var_hat = var_i)

g_mu <- df_sample %>%
  ggplot(aes(x = mu_i)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

g_var <- df_sample %>%
  ggplot(aes(x=var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

## how patchwork works
g_hor <- g_mu + g_var
g_ver <- g_mu / g_var


# bias-corrected version --------------------------------------------------

## for loop

var_ub_i <- NULL

for (i in 1:1000) {
  ## randomly sample 10 individuals
  
  df_i <- df_h0 %>%
    sample_n(size = 10)

  ## variance for a subset
  ## var()'s denominator is N-1, not N
  var_ub_i[i] <- var(df_i$height)
}

#adding a new column to df_sample
df_sample <- df_sample %>%
  mutate(var_ub_hat = var_ub_i)

g_var_ub <- df_sample %>%
  ggplot(aes(x=var_ub_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

## combined figure


g_mu / g_var / g_var_ub
