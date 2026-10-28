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
    treatment = factor(ifelse(grepl("C-", cup), "26°C", "30°C")),
    cup = factor(cup)
  )

Second_Data <- read_excel(path, sheet = "2nd_Time_To_4th") |>   
  pivot_longer(everything(), names_to = "cup", values_to = "days") |>
  drop_na(days) |>
  mutate(
    treatment = factor(ifelse(grepl("CL", cup), "26°C", "30°C")),
    cup = factor(cup)
  )

Pupae_Data <- read_excel(path, sheet = "Pupae_Time_To_Adult") |>   
  pivot_longer(everything(), names_to = "cup", values_to = "days") |>
  drop_na(days) |>
  mutate(
    treatment = factor(ifelse(grepl("PC", cup), "26°C", "30°C")),
    cup = factor(cup)
  )

cmp_first <- glmmTMB(days ~ treatment + (1 | cup), family = compois, data = First_Data)
summary(cmp_first)

cmp_second <- glmmTMB(days ~ treatment + (1 | cup), family = compois, data = Second_Data)
summary(cmp_second)

cmp_pupae <- glmmTMB(days ~ treatment + (1 | cup), family = compois, data = Pupae_Data)
summary(cmp_pupae)

# Treatment contrast p-value from each model
get_p <- function(model) {
  as.data.frame(pairs(emmeans(model, ~ treatment)))$p.value
}

p_raw  <- c(get_p(cmp_first), get_p(cmp_second), get_p(cmp_pupae))
p_holm <- p.adjust(p_raw, method = "holm")   # correct across the 3 stages

stars <- function(p) {
  dplyr::case_when(p < 0.001 ~ "***",
                   p < 0.01  ~ "**",
                   p < 0.05  ~ "*",
                   TRUE      ~ "ns")
}

n_range <- range(c(
  count(First_Data,  cup)$n,
  count(Second_Data, cup)$n,
  count(Pupae_Data,  cup)$n
))
n_breaks <- unique(round(seq(n_range[1], n_range[2], length.out = 4)))

trt_cols <- c("26°C" = "#4C78A8", "30°C" = "#E07B39")

plot_days <- function(dat, title, p) {
  cup_means <- dat |>
    group_by(treatment, cup) |>
    summarise(days = mean(days), n = n(), .groups = "drop")
  
  grp_means <- cup_means |>
    group_by(treatment) |>
    summarise(days = mean(days), .groups = "drop")
  
  lab <- dat |>
    group_by(treatment) |>
    summarise(cups = n_distinct(cup), larvae = n(), .groups = "drop") |>
    mutate(lab = paste0(treatment, "\n(", cups, " cups, ", larvae, " larvae)"))
  labs_vec <- setNames(lab$lab, lab$treatment)
  
  # bracket position
  y_max   <- max(dat$days)
  y_range <- diff(range(dat$days))
  y_brack <- y_max + 0.08 * y_range
  tick    <- 0.02 * y_range
  p_text  <- ifelse(p < 0.001, "p < 0.001", paste0("p = ", signif(p, 2)))
  
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
    # significance bracket
    annotate("segment", x = 1, xend = 2, y = y_brack, yend = y_brack) +
    annotate("segment", x = 1, xend = 1, y = y_brack - tick, yend = y_brack) +
    annotate("segment", x = 2, xend = 2, y = y_brack - tick, yend = y_brack) +
    annotate("text", x = 1.5, y = y_brack + 0.01 * y_range,
             label = stars(p), vjust = 0, size = 5) +
    scale_x_discrete(labels = labs_vec) +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(mult = c(0, 0.2))) +
    scale_color_manual(values = trt_cols) +
    scale_fill_manual(values = trt_cols) +
    scale_size_continuous(range = c(2, 6), limits = n_range,
                          breaks = n_breaks, name = "Larvae per cup") +
    guides(color = "none") +
    labs(title = title, x = NULL, y = "Days") +
    theme_classic()
}

p <- plot_days(First_Data,  "Time from 1st to 2nd instar",     p_holm[1]) +
  plot_days(Second_Data, "Time from 2nd instar to 4th instar", p_holm[2]) +
  plot_days(Pupae_Data,  "Time from pupation to adulthood",                p_holm[3]) +
  plot_layout(guides = "collect") +
  plot_annotation(caption = paste0(
    "Translucent dots describe individual larvae. Large circles describe cup means. Black bars describes mean of cup means.\n",
    "Brackets: Conway-Maxwell-Poisson GLMM; Holm-adjusted across the three stages. ",
    "*** p<0.001, ** p<0.01, * p<0.05."))

ggsave("larval_plots.png", p, width = 13, height = 4.8, dpi = 200)
