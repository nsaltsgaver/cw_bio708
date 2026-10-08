

pacman::p_load(tidyverse)
rm(list=ls())
install.packages("pwr")
library(pwr)

distinct(PlantGrowth, group)

PlantGrowth %>%
  ggplot(
    aes(x = group,
        y = weight)
  ) +
  geom_violin(
    draw_quantiles = 0.5,
    alpha = 0.2
  ) +
  geom_jitter(
    height = 0,
    width = 0.1,
    alpha = 0.2
  )

m <- aov(formula = weight ~ group,
         data = PlantGrowth)

summary(m)

# The p-value would be rather obviously reported, along with the group means and
# the variance between them.

pwr::pwr.anova.test(
  k = 3,
  f = 0.5,
  sig.level = 0.05,
  power = 0.8
)

pwr::pwr.anova.test(
  k = 3,
  n = 3,
  f = 0.5,
  sig.level = 0.05
)

pwr::pwr.anova.test(
  k = 10,
  n = 3,
  f = 0.5,
  sig.level = 0.05
)

## comparing how the number of samples per group affects power

pwr::pwr.anova.test(
  k = 4,
  n = 50,
  f = 0.5,
  sig.level = 0.05
)


pwr::pwr.anova.test(
  k = 2,
  n = 20,
  f = 0.70,
  sig.level = 0.05
)

