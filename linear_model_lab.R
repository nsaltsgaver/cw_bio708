pacman::p_load(tidyverse)
rm(list=ls())

head(ToothGrowth)

# set up basic data frame

df_tooth <- as.tibble(ToothGrowth)

m_tooth <- lm(len ~ supp * dose,
   data = df_tooth)

m_tooth

# use residuals to test if normal distribution is being followed

shapiro.test(residuals(m_tooth))

df_pred <- ToothGrowth %>%
  group_by(supp) %>%
  reframe(dose = seq(min(dose),
                     max(dose),
                     length = 100))

y_pred <- predict(m_tooth,
                  newdata = df_pred)

df_pred <- df_pred %>% 
  mutate(y_pred = y_pred)

#figure for plot vs. pred

m_tooth %>%
  ggplot(aes(x=dose,
         y=len,
         color=supp)) +
  geom_point(alpha = 0.5) +
  geom_line(data=df_pred,
            aes(y=y_pred))

## variance-covariance matrix
mv <- rbind(c(1, 0.9),
            c(0.9, 1))

## true regression coefficients
b <- c(0.05, 1.00)

## produce simulated data
set.seed(523)
X <- MASS::mvrnorm(100,
                   mu = c(0, 0),
                   Sigma = mv)

df_y <- tibble(x1 = X[,1],
               x2 = X[,2]) %>% 
  mutate(y = rnorm(nrow(.),
                   mean = 1 + b[1] * x1 + b[2] * x2))

df_y %>%
  ggplot(aes(x=x1,
             y=y)) +
  geom_point(alpha=0.5)


df_y %>%
  ggplot(aes(x=x2,
             y=y)) +
  geom_point(alpha=0.5)

## It would seem both x1 & x2 have a positive correlation / effect on y based
## on our scatter plots.

m_df_y <- lm(y ~ x1 + x2,
   data = df_y)

df_y %>%
  ggplot(aes(x=x1,
             y=x2)) +
  geom_point(alpha=0.5)

cor(df_y$x1, df_y$x2, method = "pearson")

## So there seems to be a high correlation between x1 & x2, with an r=0.88.

### 13.4.4 Experimental Design Question

# I would believe B is the most appropriate statement. Because the predator 
# effect can only be analyzed at the high nutrient level & the nutrient effect
# can only be analyzed at high nutrient levels.

