pacman::p_load(tidyverse)
rm(list = ls())

df_algae <- read_csv("data_src/data_algae.csv")

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point()

m <- lm(biomass ~ conductivity,
        data = df_algae)

summary(m)

alpha <- coef(m)[1]
beta <- coef(m)[2]

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) 

v_x <- df_algae %>% pull(conductivity)
X <- cbind(1, v_x)

Y <- df_algae %>% pull(biomass)

theta <- solve(t(X) %*% X) %*% t(X) %*% Y

coef(m)

theta <- coef(m)

se <- sqrt(diag(vcov(m)))

t_value <- theta / se
print(t_value)

p_alpha <- (1 - pt(t_value[1], df = 48)) + pt(-t_value[1], df = 48)

p_beta <- (1 - pt(t_value[2], df = 48)) + pt(-t_value[2], df = 48)

print(p_alpha)
print(p_beta)

eps <- resid(m)
head(eps)

v_x <- df_algae %>% pull(conductivity)
v_y <- df_algae %>% pull(biomass)

error <- v_y - (theta[1] + theta[2] * v_x)

head(cbind(eps, error))

eps <- round(eps, 5)
error <- round(error, 5)

all(eps == error)

df_algae <- df_algae %>% 
  mutate(eps = eps)

df_algae %>% 
  ggplot(aes(x = conductivity,
             y = biomass)) +
  geom_point() +
  geom_abline(intercept = alpha,
              slope = beta) + 
  geom_segment(aes(x = conductivity, # start-coord x
                   xend = conductivity, # end-coord x
                   y = biomass, # start-coord y
                   yend = biomass - eps), # end-coord y
               linetype = "dashed")

ss <- sum(resid(m)^2)

ss_0 <- sum((v_y - mean(v_y))^2)

r2 <- 1 - ss / ss_0

print(r2)

