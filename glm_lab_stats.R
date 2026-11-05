# GLM Lab

pacman::p_load(tidyverse)
rm(list=ls())

# glm exercise ------------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/public-proj_fish-richness-shubuto/master/data/data_vpart.csv"
df_fish <- read_csv(url)

m_pois_fish <- glm(n_sp ~ distance + cat_area + hull_area,
              data = df_fish,
              family = "poisson")
summary(m_pois_fish)

m_gaus_cars <- glm(am ~ mpg + hp + wt,
                   data = mtcars,
                   family = "gaussian")
summary(m_gaus_cars)

# effect size -------------------------------------------------------------

df_fish <- df_fish %>%
  mutate(
    std_dist = c(scale(distance)),
    std_cat = c(scale(cat_area)),
    std_hull = c(scale(hull_area))
  )

m_pois_fish_std <- glm(n_sp ~ std_dist + std_cat + std_hull,
                   data = df_fish,
                   family = "poisson")

# comparison
summary(m_pois_fish)
summary(m_pois_fish_std)

# offset term -------------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_src/data_offset.csv"
df_offset <- read_csv(url)

ggplot(df_offset, aes(x = nitrate, y = count)) +
  geom_point()

ggplot(df_offset, aes(x = area, y = count)) +
  geom_point()

ggplot(df_offset, aes(x = nitrate, y = count / area)) +
  geom_point() +
  labs(y = "count / area")

df_offset <- df_offset %>% 
  mutate(density = count / area)

glm(density ~ nitrate,
    data = df_offset,
    family = "poisson")

m_pois_count <- glm(count ~ nitrate,
    data = df_offset,
    family = "poisson")

summary(m_pois_count)

m_pois_count_offset <- glm(count ~ nitrate + offset(log(area)),
                     data = df_offset,
                     family = "poisson")

summary(m_pois_count_offset)


# overdispersion ----------------------------------------------------------

url <- "https://raw.githubusercontent.com/aterui/biostats/master/data_src/data_tadpole.csv"
df_tadpole <- read_csv(url)


ggplot(df_tadpole, aes(x = aqveg, y = tadpole)) +
  geom_point()

ggplot(df_tadpole, aes(x = permanence, y = tadpole)) +
  geom_point()

mean(df_tadpole$tadpole)
sd(df_tadpole$tadpole)
var(df_tadpole$tadpole)

MASS::glm.nb(tadpole ~ aqveg + permanence,
            data = df_tadpole) %>%
  summary()

