#Cleaning Environment & Setup

rm(list = ls())

library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lme4)
library(glmmTMB)
library(MASS)
library(emmeans)
library(patchwork)

path <- "C:/Users/nsalt/Downloads/Larval_Experiment_Data_2_Clean.xlsx"

excel_sheets(path)

First_Data <- read_excel(path, sheet = "1st_Time_To_2nd") |>   
  pivot_longer(everything(), names_to = "cup", values_to = "days") |>
  drop_na(days) |>
  mutate(
    treatment = factor(ifelse(grepl("C-", cup), "Control", "Treatment")),
    cup = factor(cup)
  )

Second_Data <- read_excel(path, sheet = "2nd_Time_To_4th") |>   
  pivot_longer(everything(), names_to = "cup", values_to = "days") |>
  drop_na(days) |>
  mutate(
    treatment = factor(ifelse(grepl("CL", cup), "Control", "Treatment")),
    cup = factor(cup)
  )

Pupae_Data <- read_excel(path, sheet = "Pupae_Time_To_Adult") |>   
  pivot_longer(everything(), names_to = "cup", values_to = "days") |>
  drop_na(days) |>
  mutate(
    treatment = factor(ifelse(grepl("PC", cup), "Control", "Treatment")),
    cup = factor(cup)
  )

cmp_first <- glmmTMB(days ~ treatment + (1 | cup), family = compois, data = First_Data)
summary(cmp_first)

cmp_second <- glmmTMB(days ~ treatment + (1 | cup), family = compois, data = Second_Data)
summary(cmp_second)

cmp_pupae <- glmmTMB(days ~ treatment + (1 | cup), family = compois, data = Pupae_Data)
summary(cmp_pupae)

plot_days <- function(dat, title) {
  cup_means <- dat |>
    group_by(treatment, cup) |>
    summarise(days = mean(days), n = n(), .groups = "drop")
  
  grp_means <- cup_means |>
    group_by(treatment) |>
    summarise(days = mean(days), .groups = "drop")
  
  # x labels that show the real replication
  lab <- dat |>
    group_by(treatment) |>
    summarise(cups = n_distinct(cup), larvae = n(), .groups = "drop") |>
    mutate(lab = paste0(treatment, "\n(", cups, " cups, ", larvae, " larvae)"))
  labs_vec <- setNames(lab$lab, lab$treatment)
  
  ggplot(dat, aes(treatment, days, color = treatment)) +
    geom_jitter(width = 0.18, height = 0.12, alpha = 0.35, size = 1.5) +
    geom_point(data = cup_means, aes(size = n),
               position = position_jitter(width = 0.12, height = 0, seed = 1),
               shape = 21, fill = NA, stroke = 1.1, color = "black") +
    geom_point(data = cup_means, aes(size = n, fill = treatment),
               position = position_jitter(width = 0.12, height = 0, seed = 1),
               shape = 21, color = "black", show.legend = FALSE) +
    geom_crossbar(data = grp_means, aes(ymin = days, ymax = days),
                  width = 0.7, color = "black", linewidth = 0.6) +
    scale_x_discrete(labels = labs_vec) +
    scale_color_manual(values = c(Control = "#4C78A8", Treatment = "#E07B39")) +
    scale_fill_manual(values = c(Control = "#4C78A8", Treatment = "#E07B39")) +
    scale_size_continuous(range = c(2, 6), name = "Larvae per cup") +
    guides(color = "none") +
    labs(title = title, x = NULL, y = "Days") +
    theme_classic()
}

p <- plot_days(First_Data,  "1st instar to 2nd instar") +
  plot_days(Second_Data, "2nd/3rd instar to 4th instar") +
  plot_days(Pupae_Data,  "Pupa to adult") +
  plot_layout(guides = "collect") +
  plot_annotation(caption = "Faint dots = individual larvae. Large circles = cup means. Black bar = mean of cup means.")

ggsave("larval_plots.png", p, width = 13, height = 4.8, dpi = 200)
