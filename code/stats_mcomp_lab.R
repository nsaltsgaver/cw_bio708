# setup

pacman::p_load(tidyverse,
               patchwork,
               palmerpenguins,
               janitor)

rm(list = ls())


# Cleaning the dataset ----------------------------------------------------

penguins_clean <- clean_names(penguins_raw)

penguins_clean <- penguins_clean %>%
  mutate(clutch_completion = ifelse(clutch_completion == "Yes", 1, 0))

penguins_clean <- penguins_clean %>%
  mutate(species = case_when(
    str_detect(species, regex("adelie", ignore_case = TRUE))     ~ "adelie",
    str_detect(species, regex("chinstrap", ignore_case = TRUE))  ~ "chinstrap",
    str_detect(species, regex("gentoo", ignore_case = TRUE))     ~ "gentoo",
    TRUE ~ NA_character_
  ))

penguins_clean <- penguins_clean %>%
  drop_na(culmen_length_mm, culmen_depth_mm, flipper_length_mm, body_mass_g, sex)

# Modeling ----------------------------------------------------------------

## full model for model selection with dredge

m <- glm(clutch_completion ~ species + culmen_length_mm + 
           culmen_depth_mm + flipper_length_mm + body_mass_g + sex,
         data=penguins_clean,
         family="binomial")

## model selection 

options(na.action = "na.fail")
m_set <- dredge(m, rank = "AIC")
m_sub <- subset(m_set, delta < 2)

list_m <- get.models(object = m_set, subset = delta < 2)
