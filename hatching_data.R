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

path <- "C:/Users/nsalt/Downloads/Hatching_Time_Data_Clean.xlsx"

excel_sheets(path)

Hatch_Data <- read_excel(path, sheet = "Hatching_Time") |>   
  pivot_longer(everything(), names_to = "cup", values_to = "days") |>
  drop_na(days) |>
  mutate(
    treatment = factor(ifelse(grepl("C-", cup), "26C", "30C")),
    cup = factor(cup)
  )

cmp_hatch <- glmmTMB(days ~ treatment + (1 | cup), family = compois, data = Hatch_Data)
summary(cmp_hatch)

# Treatment contrast p-value from each model
get_p <- function(model) {
  as.data.frame(pairs(emmeans(model, ~ treatment)))$p.value
}

p_raw  <- get_p(cmp_hatch)
p_holm <- p.adjust(p_raw, method = "holm")   # correct across the 3 stages

stars <- function(p) {
  dplyr::case_when(p < 0.001 ~ "***",
                   p < 0.01  ~ "**",
                   p < 0.05  ~ "*",
                   TRUE      ~ "ns")
}

n_range <- range(
  count(Hatch_Data,  cup)$n)

n_breaks <- unique(round(seq(n_range[1], n_range[2], length.out = 4)))

trt_cols <- c("26C" = "#4C78A8", "30C" = "#E07B39")

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
    mutate(lab = paste0(treatment, "\n(", cups, " cups, ", larvae, " eggs)"))
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
                          breaks = n_breaks, name = "Eggs per cup") +
    guides(color = "none") +
    labs(title = title, x = NULL, y = "Days") +
    theme_classic()
}

p <- plot_days(Hatch_Data,  "Time from collection to hatching",     p_holm[1]) +
  plot_layout(guides = "collect") +
  plot_annotation(caption = paste0(
    "Translucent dots describe individual eggs. Large circles describe cup means. Black bars describes mean of cup means.\n",
    "Brackets: Conway-Maxwell-Poisson GLMM; Holm-adjusted. ",
    "*** p<0.001, ** p<0.01, * p<0.05."))

ggsave("egg_plots.png", p, width = 13, height = 4.8, dpi = 200)

# ---- Hatch rate (survival) add-on ------------------------------------------
# Run AFTER your existing script (uses path, trt_cols, stars, get_p, plot_days,
# Hatch_Data, p_holm from it).

# 1. Read the Hatching_Rate sheet (one column per cup, one row per egg, 1/0)
excel_sheets(path)
Surv_Data <- read_excel(path, sheet = "Hatching_Rate") |>
  pivot_longer(everything(), names_to = "cup", values_to = "hatched") |>
  drop_na(hatched) |>
  mutate(
    treatment = factor(ifelse(grepl("C-", cup), "26C", "30C")),
    cup = factor(cup),
    hatched = as.integer(hatched)
  )

stopifnot(all(Surv_Data$hatched %in% c(0, 1)))

# 2. Hatch rate per cup
cup_rates <- Surv_Data |>
  group_by(treatment, cup) |>
  summarise(n = n(), hatched = sum(hatched), rate = hatched / n, .groups = "drop")

cup_rates |> group_by(treatment) |>
  summarise(cups = n(), eggs = sum(n), mean_rate = mean(rate), sd = sd(rate))

# 3. Tests
# (a) Primary: binomial GLMM on individual eggs, cup as random effect.
#     Weights cups by number of eggs and handles 0/1 data properly.
bin_hatch <- glmmTMB(hatched ~ treatment + (1 | cup),
                     family = binomial, data = Surv_Data)
summary(bin_hatch)
p_hatch <- get_p(bin_hatch)

# (b) Simple version: Welch t-test on the cup hatch rates (cup = replicate).
#     Fine if cups have similar egg counts; rates are bounded 0-1, so treat
#     as a sanity check. Wilcoxon is the safer non-parametric alternative.
t.test(rate ~ treatment, data = cup_rates)
wilcox.test(rate ~ treatment, data = cup_rates)

# 4. Plot
# One shared size scale for BOTH panels: same limits, breaks and name, so circle
# size means the same thing everywhere and patchwork can merge them into one key.
# (Overwrites n_range / n_breaks from the original script; plot_days() reads these.)
n_range  <- range(c(count(Hatch_Data, cup)$n, count(Surv_Data, cup)$n))
n_breaks <- unique(round(seq(n_range[1], n_range[2], length.out = 4)))

plot_rate <- function(dat, title, p) {
  cup_rates <- dat |>
    group_by(treatment, cup) |>
    summarise(n = n(), rate = mean(hatched) * 100, .groups = "drop")
  grp_means <- cup_rates |>
    group_by(treatment) |>
    summarise(rate = mean(rate), .groups = "drop")
  
  lab <- dat |>
    group_by(treatment) |>
    summarise(cups = n_distinct(cup), eggs = n(), .groups = "drop") |>
    mutate(lab = paste0(treatment, "\n(", cups, " cups, ", eggs, " eggs)"))
  labs_vec <- setNames(lab$lab, lab$treatment)
  
  y_brack <- 108
  tick    <- 2
  
  ggplot(cup_rates, aes(treatment, rate)) +
    geom_point(aes(size = n, fill = treatment),
               position = position_jitter(width = 0.12, height = 0, seed = 1),
               shape = 21, color = "black", stroke = 1.1) +
    geom_crossbar(data = grp_means, aes(ymin = rate, ymax = rate),
                  width = 0.7, color = "black", linewidth = 0.6) +
    annotate("segment", x = 1, xend = 2, y = y_brack, yend = y_brack) +
    annotate("segment", x = 1, xend = 1, y = y_brack - tick, yend = y_brack) +
    annotate("segment", x = 2, xend = 2, y = y_brack - tick, yend = y_brack) +
    annotate("text", x = 1.5, y = y_brack + 1, label = stars(p),
             vjust = 0, size = 5) +
    scale_x_discrete(labels = labs_vec) +
    scale_y_continuous(limits = c(0, 118), breaks = seq(0, 100, 25)) +
    scale_fill_manual(values = trt_cols) +
    scale_size_continuous(range = c(2, 6), limits = n_range,
                          breaks = n_breaks, name = "Eggs per cup") +
    guides(fill = "none") +
    labs(title = title, x = NULL, y = "Hatch rate (%)") +
    theme_classic()
}

p_time_plot <- plot_days(Hatch_Data, "Time from collection to hatching", p_holm[1])
p_rate_plot <- plot_rate(Surv_Data,  "Hatch rate",                       p_hatch[1])

# 5. Combine side by side and save
p_both <- (p_time_plot | p_rate_plot) +
  plot_layout(guides = "collect") +
  plot_annotation(caption = paste0(
    "Translucent dots (left) are individual eggs. Large circles are cup means (point size = eggs per cup). ",
    "Black bars are the mean of cup means.\n",
    "Brackets: hatching time, Conway-Maxwell-Poisson GLMM; hatch rate, binomial GLMM (cup random effect). ",
    "*** p<0.001, ** p<0.01, * p<0.05."))

ggsave("egg_plots_combined.png", p_both, width = 13, height = 4.8, dpi = 200)