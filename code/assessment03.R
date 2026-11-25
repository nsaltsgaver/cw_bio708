# Submitting this assessment by the due date is worth 50 points.
# Each question is worth 5 points.
# Call suitable packages as needed.

library(tidyverse)
library(janitor)

# CO2 dataset -------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# The CO2 dataset in R records how grass plants from two origins, Quebec and
# Mississippi, respond to varying CO₂ concentrations and temperature treatments.

head(CO2)

# Each plant is identified by the `Plant` factor, with `Type` indicating its origin
# and `Treatment` showing whether it was chilled or nonchilled. `conc` gives the
# ambient CO₂ concentration (mL/L), and `uptake` measures the rate of CO₂
# assimilation (µmol/m²/sec). 

## DATA DESCRIPTION END ###################################################

# Q1
# CO2 dataframe is a base dataframe. Convert this to a class `tibble`
# then assign to `df_co2`

df_co2 <- as.tibble(CO2)


# Q2
# Convert column names to lowercase and reassign to `df_co2`

df_co2 <- clean_names(df_co2)

# Q3
# Create scatter plots of CO₂ uptake versus ambient CO₂ concentration using `df_co2`.
# The figure should satify the following:
# - The x-axis should represent ambient CO₂ concentration
# - The y-axis should represent CO₂ assimilation rate
# - Color the points by treatment.
# - Create separate panels for each plant type (Quebec vs Mississippi) and combine the plots.

ggplot(df_co2, aes(x = conc, y = uptake, color = treatment)) +
  geom_point(size = 2, alpha = 0.8) +
  facet_wrap(~ type) +
  labs(
    x = expression("Ambient CO2 concentration"),
    y = expression("CO2 assimilation rate"),
    color = "Treatment"
  ) +
  theme_bw()

# Q4
# The df_co2 dataset contains the following variables:
# - CO₂ assimilation rate
# - ambient CO₂ concentration
# - treatment: chilled vs nonchilled
# - type: plant origin (Quebec vs Mississippi)
# 
# Develop a statistical model that examines the following effects simultaneously:
#   
# - The main effect of ambient CO₂ concentration (conc)
# - The main effect of treatment
# - The interaction between concentration and treatment
# 
# Fit these models separately for each plant origin.

model_quebec      <- lm(uptake ~ conc * treatment, data = subset(df_co2, type == "Quebec"))
model_mississippi <- lm(uptake ~ conc * treatment, data = subset(df_co2, type == "Mississippi"))

summary(model_quebec)
summary(model_mississippi)

anova(model_quebec)
anova(model_mississippi)

# Q5
# Based on the models fitted in Q4 for Quebec and Mississippi plants,
# describe how CO2 assimilation rate responded to ambient CO2
# concentration under different treatments (chilled vs non-chilled) 
# for each plant origin. Highlight the differences between Quebec and 
# Mississippi plants, and use the model results to support your answers.

# ENTER YOUR ANSWER HERE as COMMENT:
# (no coding required for this question)

## It would seem for the Quebec plants, while higher ambient concentration 
## resulted in higher assimilation rates, the treatment's effects were 
## non-significant. Whereas for the Mississippi plants there is a significant
## effect when considering the treatment's effects among Mississippi plants
## there is a significant effect for the treatment on assimilation rate, and
## with a p of 0.048 for the interaction between ambient concentration &
## treatment among Mississippi plants.


# BCI data ----------------------------------------------------------------

## DATA DESCRIPTION #######################################################

# BCI dataset:
# run the following code to get data.
if(!require(vegan)) install.packages("vegan")
library(vegan)
data("BCI")
data("BCI.env")

# BCI dataset:
# The BCI dataset contains tree species abundance data from 50 1-hectare plots 
# on Barro Colorado Island (Panama). Each row represents a plot, and each 
# column represents a tree species. Entries are counts of individuals of each 
# species in that plot. This dataset is often used to study species richness, 
# community composition, and diversity patterns in tropical forests.

print(BCI)

# The following code transforms the BCI dataset from wide to long format. 
# Originally, each plot was a row and each species a column. 
# After the transformation, each row represents a single species in a 
# single plot, with columns indicating the plot (plot), species, and 
# the corresponding count.

cnm <- colnames(BCI)

df_bci <- BCI %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  pivot_longer(cols = cnm[1]:cnm[length(cnm)], 
               names_to = "species", 
               values_to = "count")

# BCI.env dataset:
# This dataset contains environmental variables for the 50 plots in the BCI dataset.
# Key columns include:
# - UTM.EW, UTM.NS: spatial coordinates of each plot
# - Precipitation: mean annual rainfall (mm)
# - Elevation: plot elevation (m)
# - Age.cat: categorical forest age class
# - Geology: underlying geological formation type
# - Habitat: dominant habitat type in the plot
# - Stream: indicates presence of streamside (riparian) habitat
# - EnvHet: environmental heterogeneity (Simpson diversity of habitat subcells)
# These variables help explain variation in species composition and abundance 
# across plots and allow exploration of species–environment relationships.

print(BCI.env)

# The following code adds a new "plot" column.

df_env <- BCI.env %>% 
  mutate(plot = paste0("p", str_pad(row_number(),
                                    width = 2, 
                                    pad = 0))) %>% 
  relocate(plot)

## DATA DESCRIPTION END ####################################################

# Q6
# Convert column names of `df_env` to lowercase and reassign to `df_env`

df_env <- clean_names(df_env)

# Q7
# In `df_env`, some environmental variables have no variation between plots
# (i.e., the same value for all plots). Identify these columns and remove them
# from the dataframe. Assign the resulting dataframe to `df_env_sub`.

df_env_sub <- df_env %>%
  dplyr::select(where(~ n_distinct(.x) > 1))

# Q8
# Calculate summary statistics for each plot using `df_bci`.
# Compute the following quantities by plot as columns:
# - n_sum: total count of all individuals across species
# - n1: count of the most abundant species (maximum count among species for each plot)
# - p: proportion of the most abundant species (n1 / n_sum)
#
# Ensure that each plot has one row representing a data point of the dominant species.
#
# Assign the resulting dataframe to `df_n`.

df_n <- df_bci %>%
  group_by(plot) %>%
  mutate(n_sum = sum(count)) %>%
  slice_max(count, n = 1, with_ties = FALSE) %>%
  transmute(plot, species, n_sum, n1 = count, p = n1 / n_sum) %>%
  ungroup()

# Q9
# Combine the summary data (`df_n`) with the environmental variables
# (`df_env_sub`) for each plot. Assign the resulting dataframe to `df_m`.

df_m <- df_n %>%
  left_join(df_env_sub, by = "plot")

# Q10
# Develop a statistical model to explain variation in the proportion of the dominant
# species with a suitable error distribution. 
# - Use `EnvHet`, `Stream`, and `Habitat` as predictors.
# - Use model selection based on predictability (i.e., out-of-sample prediction) 
# rather than the goodness of fit, and report which variables are included in 
# the best predictive model as a comment.

library(MuMIn)

m <- glm(cbind(n1, n_sum - n1) ~ env_het + stream + habitat,
         data = df_m,
         family = binomial)

options(na.action = "na.fail")
m_set <- dredge(m, rank = "AIC")
m_set

m_sub  <- subset(m_set, delta < 2)
list_m <- get.models(m_set, subset = delta < 2)




