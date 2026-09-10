# Descriptive Statistics - Lab

library(tidyverse)

# central tendency --------------------------------------------------------

# Z calculations for central tendency & histogram

z <- exp(rnorm(n = 100, mean = 0, sd = 0.1))

mu_z <- sum(z) / length(z)

mu_z_ge <- prod(z)^(1/length(z))

med_z <- median(z)

t_z <- tibble(z)

t_z %>%
  ggplot(
    mapping = aes(x=z)
  ) + geom_histogram() +
  geom_vline(xintercept = med_z, color = "green") +
  geom_vline(xintercept = mu_z, color = "blue") +
  geom_vline(xintercept = mu_z_ge, color = "red")

# Z_Rev calculations for central tendency

z_rev <- -z + max(z) + 0.5

mu_z_rev <- sum(z_rev) / length(z_rev)

mu_z_rev_ge <- prod(z_rev)^(1/length(z_rev))

med_z_rev <- median(z_rev)

t_z_rev <- tibble(z_rev)

t_z_rev %>%
  ggplot(
    mapping = aes(x=z_rev)
  ) + geom_histogram() +
  geom_vline(xintercept = med_z_rev, color = "green") +
  geom_vline(xintercept = mu_z_rev, color = "blue") +
  geom_vline(xintercept = mu_z_rev_ge, color = "red")

# variation ---------------------------------------------------------------

w <- rnorm(100, mean = 10, sd = 1)

m <- w*1000

#SD & MAD for W & M

var_w <- (sum((w - mean(w))^2)) / length(w)
sd_w <- sqrt(var_w)

var_m <- (sum((m - mean(m))^2)) / length(m)
sd_m <- sqrt(var_m)

ad_w <- abs(w - median(w))
mad_w <- median(ad_w)

ad_m <- abs(m - median(m))
mad_m <- median(ad_m)

#Coefficient of variance

mu_w <- sum(w) / length(w)
cv_w <- sd_w / mu_w

mu_m <- sum(m) / length(m)
cv_m <- sd_m / mu_m

