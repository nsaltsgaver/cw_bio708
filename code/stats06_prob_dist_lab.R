# Probability Distribution Lab

rm(list = ls())
pacman::p_load(tidyverse,
               patchwork)

x <- rnorm(50,mean=48.3,sd=1.6)

df_x <- data.frame(x = x)

p_x <- seq(min(df_x$x),
         max(df_x$x),
         length = 50)

mu <- mean(df_x$x)
sigma <- sd(df_x$x)
pd <- dnorm(p_x, mean = mu, sd = sigma)

tibble(y = pd, x = p_x) %>% 
  ggplot(aes(x = p_x, y = y)) +
  geom_line() + 
  labs(y = "Probability density")

p10 <- pnorm (q = 10, mean = mu, sd = sigma)
p20 <- pnorm (q = 20, mean = mu, sd = sigma)

p20 - p10

x_min <- floor(min(df_x$x))
x_max <- ceiling(max(df_x$x))
bin <- seq(x_min, x_max, by = 1)

p <- NULL
for (i in 1:(length(bin) - 1)) {
  p_up <- pnorm(bin[i + 1], mean = mu, sd = sigma)
  p_low <- pnorm(bin[i], mean = mu, sd = sigma)
  p[i] <- p_up - p_low
}

df_prob <- tibble(p, bin = bin[-length(bin)] + 0.5) %>% 
  mutate(freq = p * nrow(df_x))

# Overlayed histogram

df_x %>% 
  ggplot(aes(x = x)) + 
  geom_histogram(binwidth = 1, 
                 center = 0.5) + 
  geom_point(data = df_prob,
             aes(y = freq,
                 x = bin),
             color = "salmon") +
  geom_line(data = df_prob,
            aes(y = freq,
                x = bin),
            color = "salmon")


# Poisson Dist. -----------------------------------------------------------


# generate poisson dist.
pois_x <- rpois(1000, 48)

df_pois_x <- data.frame(pois_x = pois_x)

x_p <- seq(0, 100, by = 1)

lambda_hat <- mean(df_pois_x$pois_x)
pm <- dpois(x_p, lambda = lambda_hat)

df_prob_pois <- tibble(x = x_p, y = pm) %>% 
  mutate(freq = y * nrow(df_pois_x)) 

# generate figure for poisson dist.

df_pois_x %>% 
  ggplot(aes(x = pois_x)) +
  geom_histogram(binwidth = 0.5, # must be divisible number of one; e.g., 0.1, 0.25, 0.5...
                 center = 0) +
  geom_line(data = df_prob_pois,
            aes(x = x_p,
                y = freq),
            linetype = "dashed") +
  geom_point(data = df_prob_pois,
             aes(x = x_p,
                 y = freq))

