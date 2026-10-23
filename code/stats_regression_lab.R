pacman::p_load(tidyverse)
rm(list = ls())

head(iris)

setosa <- subset(iris, Species == "setosa")
versicolor <- subset(iris, Species == "versicolor")
virginica <- subset(iris, Species == "virginica")

m_se <- lm(Sepal.Width ~ Petal.Width,
        data = setosa)

summary(m_se)

alpha_se <- coef(m_se)[1]
beta_se <- coef(m_se)[2]

setosa %>% 
  ggplot(aes(x = Petal.Width,
             y = Sepal.Width)) +
  geom_point() +
  geom_abline(intercept = alpha_se,
              slope = beta_se) 

## Although a positive trendline appears, this also does not appear to be a 
## significant correlation by any meaning with an R^2 of ~0.05 & a p-value of
## 0.10. 

m_ve <- lm(Sepal.Width ~ Petal.Width,
           data = versicolor)

summary(m_ve)

alpha_ve <- coef(m_ve)[1]
beta_ve <- coef(m_ve)[2]

versicolor %>% 
  ggplot(aes(x = Petal.Width,
             y = Sepal.Width)) +
  geom_point() +
  geom_abline(intercept = alpha_ve,
              slope = beta_ve) 

## Here there is once again a positive trend with Sepal Width increasing as 
## Petal Width increases. A higher R^2 value is obtained (~0.44) & a significant
## p-value of 1.467e-07 is found 


m_vi <- lm(Sepal.Width ~ Petal.Width,
           data = virginica)

summary(m_vi)

alpha_vi <- coef(m_vi)[1]
beta_vi <- coef(m_vi)[2]

virginica %>% 
  ggplot(aes(x = Petal.Width,
             y = Sepal.Width)) +
  geom_point() +
  geom_abline(intercept = alpha_vi,
              slope = beta_vi) 

## Once again a correlation between Sepal Width and Petal Width is found, 
## stronger than that found among Setosa but weaker than that found among 
## Versicolor with an R^2 value of ~0.29 & a still significant p-value of 
## 5.648e-05 is found.

# Multiple explanatory variables

m2_se <- lm(Sepal.Width ~ Petal.Width + Petal.Length, data = setosa)
summary(m2_se)

m2_ve <- lm(Sepal.Width ~ Petal.Width + Petal.Length, data = versicolor)
summary(m2_ve)

m2_vi <- lm(Sepal.Width ~ Petal.Width + Petal.Length, data = virginica)
summary(m2_vi)

## In all three the addition of Petal Length as an explanatory variable did little
## to change the coefficient of determination. At most providing a slightly higher
## R^2 than the initial single variable values. Across all three the p-values 
## which had prior significance retained their significant and for setosa the 
## p-value remained insignificant.
