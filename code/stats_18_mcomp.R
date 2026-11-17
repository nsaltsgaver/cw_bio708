# Model Comparison

pacman::p_load(tidyverse,
               patchwork)


# model fit ---------------------------------------------------------------


set.seed(1)

# sample size

n <- 100

# True intercept and slope

b <- c(0.1,0.5)

# hypothetical predictors

x1 <- rnorm(n=n,mean=0,sd=1)

# design matrix

X <- model.matrix(~x1)

#simulated y

y_hat <- X %*% b
y <- rnorm(n=n,mean=y_hat,sd=0.5)

df0 <- tibble(y=y,x1=x1)

df0 %>%
  ggplot(
    aes(x=x1,
        y=y)
  ) +
  geom_point()

# lm fit ------------------------------------------------------------------

m1 <- lm(y ~ x1, df0)
summary(m1)

df0 <- df0 %>% 
  mutate(x2 = rnorm(n = n))

m2 <- lm(y ~ x1 + x2, data = df0)
summary(m2)

## extract R^2 values

summary(m1)$adj.r.squared
summary(m2)$adj.r.squared


# non normal case ---------------------------------------------------------

df_count <- read_csv("data_src/data_garden_count.csv") %>%
  mutate(moisture = runif(nrow(.), 0, 100))


m_pois0 <- glm(count ~ nitrate,
    data = df_count,
    family = "poisson")

s_pois0 <- summary(m_pois0)

m_pois1 <- glm(count ~ nitrate + moisture,
               data = df_count,
               family = "poisson")

s_pois1 <- summary(m_pois1)

## for model0 with nitrate only

pseudoR0 <- 1 - (s_pois0$deviance.resid / s_pois0$null.deviance)
adj.pseudoR0 <- 1 - ((s_pois0$deviance - 1) / s_pois0$null.deviance)

## for model1 with nitrate + moisture

pseudoR1 <- 1 - (s_pois1$deviance.resid / s_pois1$null.deviance)
adj.pseudoR1 <- 1 - ((s_pois1$deviance - 2) / s_pois1$null.deviance)



# likelihood ratio test ---------------------------------------------------

## normal distribution case

anova(m1, m2, test = "Chisq")

## poisson case

anova(m_pois0, m_pois1, test = "Chisq")


# AIC ---------------------------------------------------------------------

## out-of-sample prediction
## AIC = 2k - 2 ln L

## get AIC value from a model
AIC(m1)
AIC(m2)

AIC(m_pois0)
AIC(m_pois1)
