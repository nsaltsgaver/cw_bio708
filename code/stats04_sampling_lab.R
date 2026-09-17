# setup
rm(list = ls())
pacman::p_load(tidyverse)

# loading csv

df_h0 <- read_csv("data_src/data_plant_height.csv")

## true_mean

mu <- mean(df_h0$height)

## true variance

sigma2 <- sum((df_h0$height - mu)^2) / nrow(df_h0)

## for loop with a sample size of 100

mu_i <- var_i <- NULL

for (i in 1:100) {
  ## randomly sample 100 individuals
  
  df_i <- df_h0 %>%
    sample_n(size = 100)
  
  # mean for subset
  
  mu_i[i] <- mean(df_i$height)
  
  # variance for subset
  
  var_i[i] <- sum((df_i$height - mu_i[i])^2) / nrow(df_i)
}

library(patchwork)

## visualization for 100 sample
df_h0_sample <- tibble(mu_hat = mu_i,
                    var_hat = var_i)

g_mu <- df_h0_sample %>%
  ggplot(aes(x = mu_i)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

g_var <- df_h0_sample %>%
  ggplot(aes(x=var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

## patchwork
g_hor <- g_mu + g_var
g_ver <- g_mu / g_var

## for loop 50 sample

mu_5i <- var_5i <- NULL

for (i in 1:100) {
  ## randomly sample 5 individuals
  
  df_5i <- df_h0 %>%
    sample_n(size = 50)
  
  # mean for subset
  
  mu_5i[i] <- mean(df_5i$height)
  
  # variance for subset
  
  var_5i[i] <- sum((df_5i$height - mu_5i[i])^2) / nrow(df_5i)
}

## visualization
df_h50_sample <- tibble(mu_hat = mu_5i,
                        var_hat = var_5i)

g_5_mu <- df_h50_sample %>%
  ggplot(aes(x = mu_5i)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

g_5_var <- df_h50_sample %>%
  ggplot(aes(x=var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

## how patchwork works
g_5_hor <- g_5_mu + g_5_var
g_5_ver <- g_5_mu / g_5_var

# filtering height to be over 10 cm

df_h10 <- df_h0 %>% 
  filter(height >= 10)

## for loop

mu_1i <- var_1i <- NULL

for (i in 1:100) {
  ## randomly sample 100 individuals
  
  df_1i <- df_h10 %>%
    sample_n(size = 100)
  
  # mean for subset
  
  mu_1i[i] <- mean(df_1i$height)
  
  # variance for subset
  
  var_1i[i] <- sum((df_1i$height - mu_1i[i])^2) / nrow(df_1i)
}

## visualization
df_h10_sample <- tibble(mu_hat = mu_1i,
                       var_hat = var_1i)

g_1_mu <- df_h10_sample %>%
  ggplot(aes(x = mu_1i)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

g_1_var <- df_h10_sample %>%
  ggplot(aes(x=var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

## patchwork
g_1_hor <- g_1_mu + g_1_var
g_1_ver <- g_1_mu / g_1_var

## for loop

mu_15i <- var_15i <- NULL

for (i in 1:100) {
  ## randomly sample 50 individuals
  
  df_15i <- df_h0 %>%
    sample_n(size = 50)
  
  # mean for subset
  
  mu_15i[i] <- mean(df_15i$height)
  
  # variance for subset
  
  var_15i[i] <- sum((df_15i$height - mu_15i[i])^2) / nrow(df_15i)
}

## visualization
df_h150_sample <- tibble(mu_hat = mu_15i,
                        var_hat = var_15i)

g_15_mu <- df_h150_sample %>%
  ggplot(aes(x = mu_15i)) +
  geom_histogram() +
  geom_vline(xintercept = mu)

g_15_var <- df_h150_sample %>%
  ggplot(aes(x=var_hat)) +
  geom_histogram() +
  geom_vline(xintercept = sigma2)

## patchwork
g_15_hor <- g_15_mu + g_15_var
g_15_ver <- g_15_mu / g_15_var


