rm(list = ls())

install.packages("tidyverse")
install.packages("dplyr")
install.packages("janitor")
install.packages("lubridate")
install.packages("ggplot")
library(tidyverse)
library(dplyr)
library(janitor)
library(lubridate)
library(ggplot2)
library(patchwork)

Egg_Count_Control <- read.csv("C:/Users/nsalt/Documents/DataWrangling/UNCG_DataWrangling/Gonotrophic_Results_Data(Egg_Approximate_Count_Control).csv") 

Egg_Count_Control_Transform <- as.data.frame(t(Egg_Count_Control))

Egg_Count_Control_Clean <- Egg_Count_Control_Transform %>%
  subset(select = -c(V26:V46)) %>%
  subset(select = -c(V4,V18)) %>%
  slice(-(2:5)) %>%
  slice(-(22:61)) %>%
  mutate(across(everything(), ~ if_else(row_number() == 1, str_c(., "CG"), as.character(.)))) %>%
  janitor::row_to_names(row_number=1)

Egg_Count_Treatment <- read.csv("C:/Users/nsalt/Documents/DataWrangling/UNCG_DataWrangling/Gonotrophic_Results_Data(Egg_Approximate_Count_Treatment).csv")

Egg_Count_Treatment_Transform <- as.data.frame(t(Egg_Count_Treatment))

Egg_Count_Treatment_Clean <- Egg_Count_Treatment_Transform %>%
  subset(select = -c(V26:V41)) %>%
  subset(select = -c(V8,V11,V16,V21,V25)) %>%
  slice(-(2:5)) %>%
  slice(-(34:61)) %>%
  mutate(across(everything(), ~ if_else(row_number() == 1, str_c(., "TG"), as.character(.)))) %>%
  janitor::row_to_names(row_number=1)

Egg_Count_Merged <- merge(Egg_Count_Control_Clean, Egg_Count_Treatment_Clean, by=0, all=TRUE)

Egg_Count_Merged_Clean <- Egg_Count_Merged %>%
  mutate(
    across(
      .cols = Row.names,
      .fns = ~ str_remove(.x, "^X\\.?") %>%
        str_replace_all("[._]", "-") %>%
        mdy(quiet = TRUE)
    )
  ) %>%
  arrange(Row.names) %>%
  fill(2:24,.direction="down") %>%
  rename(Date=Row.names) %>%
  mutate(across(2:44, as.numeric))

Egg_Timing_Control_Clean <- Egg_Count_Control_Transform %>%
  subset(select = -c(V26:V46)) %>%
  subset(select = -c(V2,V4,V18,V20)) %>%
  slice(-(6:66)) %>%
  mutate(across(everything(), ~ if_else(row_number() == 1, str_c(., "CG"), as.character(.)))) %>%
  janitor::row_to_names(row_number=1)

Egg_Timing_Treatment_Clean <- Egg_Count_Treatment_Transform %>%
  subset(select = -c(V26:V41)) %>%
  subset(select = -c(V4,V8,V11,V16,V18,V21,V25)) %>%
  slice(-(6:66)) %>%
  mutate(across(everything(), ~ if_else(row_number() == 1, str_c(., "TG"), as.character(.)))) %>%
  janitor::row_to_names(row_number=1)

Egg_Timing_Merged <- merge(Egg_Timing_Control_Clean, Egg_Timing_Treatment_Clean, by=0, all=TRUE)

Egg_Timing_Merged$Row.names <- gsub("\\.", "_", Egg_Timing_Merged$Row.names)

Egg_Timing_Merged_Clean <- Egg_Timing_Merged %>%
  rename(Statistic = Row.names) %>%
  mutate(across(2:40, as.numeric))

# Same colors/levels as the larval figure
trt_cols <- c("26°C" = "#4C78A8", "30°C" = "#E07B39")

# One row of the cleaned data -> long format, one value per female
to_long <- function(row_df) {
  row_df |>
    pivot_longer(everything(), names_to = "pot", values_to = "value") |>
    drop_na(value) |>
    mutate(treatment = factor(ifelse(grepl("CG$", pot), "26°C", "30°C"),
                              levels = c("26°C", "30°C")))
}

Eggs_Long  <- Egg_Count_Merged_Clean |> slice(32) |> dplyr::select(-Date) |> to_long()
Death_Long <- Egg_Timing_Merged_Clean |> slice(1) |> dplyr::select(-Statistic) |> to_long()
Start_Long <- Egg_Timing_Merged_Clean |> slice(4) |> dplyr::select(-Statistic) |> to_long()
Median_Long <- Egg_Timing_Merged_Clean |> slice(2) |> dplyr::select(-Statistic) |> to_long()
End_Long   <- Egg_Timing_Merged_Clean |> slice(3) |> dplyr::select(-Statistic) |> to_long()

# Integer check: compois needs whole-number counts
datasets <- list(Eggs = Eggs_Long, Start = Start_Long, Median = Median_Long,
                 End = End_Long, Death = Death_Long)
sapply(datasets, function(d) all(d$value %% 1 == 0))   # want all TRUE

fit_cmp <- function(dat) {
  glmmTMB(value ~ treatment, family = compois, data = dat)
}

models <- lapply(datasets, fit_cmp)
lapply(models, summary)

get_p <- function(model) {
  as.data.frame(pairs(emmeans(model, ~ treatment)))$p.value
}

p_raw  <- sapply(models, get_p)
p_holm <- p.adjust(p_raw, method = "holm")
p_use  <- p_raw          # swap to p_holm if you treat these as one family
rbind(p_raw, p_holm)     # compare the two

stars <- function(p) {
  dplyr::case_when(p < 0.001 ~ "***", p < 0.01 ~ "**", p < 0.05 ~ "*", TRUE ~ "ns")
}

plot_box <- function(dat, title, ylab, p, y_max = max(dat$value)) {
  lab <- dat |>
    group_by(treatment) |>
    summarise(n = n(), .groups = "drop") |>
    mutate(lab = paste0(treatment, "\n(n = ", n, ")"))
  labs_vec <- setNames(lab$lab, lab$treatment)
  
  y_brack <- y_max * 1.08
  tick    <- y_max * 0.02
  p_text  <- ifelse(p < 0.001, "p < 0.001", paste0("p = ", signif(p, 2)))
  
  ggplot(dat, aes(treatment, value, fill = treatment, color = treatment)) +
    geom_boxplot(alpha = 0.35, width = 0.5, outlier.shape = NA,
                 color = "black", linewidth = 0.6) +
    geom_jitter(width = 0.15, height = 0, alpha = 0.6, size = 1.8) +
    stat_summary(fun = mean, geom = "point", shape = 23, size = 3,
                 fill = "white", color = "black") +
    annotate("segment", x = 1, xend = 2, y = y_brack, yend = y_brack, color = "black") +
    annotate("segment", x = 1, xend = 1, y = y_brack - tick, yend = y_brack, color = "black") +
    annotate("segment", x = 2, xend = 2, y = y_brack - tick, yend = y_brack, color = "black") +
    annotate("text", x = 1.5, y = y_brack + 0.01 * y_max,
             label = stars(p), vjust = 0, size = 5, color = "black") +
    annotate("text", x = 1.5, y = y_brack + 0.09 * y_max,
             label = p_text, vjust = 0, size = 3, color = "black") +
    scale_x_discrete(labels = labs_vec) +
    scale_y_continuous(limits = c(0, y_max * 1.25),
                       expand = expansion(mult = c(0, 0))) +
    scale_fill_manual(values = trt_cols) +
    scale_color_manual(values = trt_cols) +
    labs(title = title, x = "Treatment Group", y = ylab) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      axis.title.x = element_text(face = "bold"),
      axis.title.y = element_text(face = "bold"),
      panel.grid.major.x = element_blank(),
      legend.position = "none"
    )
}

p_eggs   <- plot_box(Eggs_Long,   "Eggs Laid per Female",   "Number of Eggs Laid", p_use[["Eggs"]])
p_start  <- plot_box(Start_Long,  "First Egg Laid", "Days", p_use[["Start"]])
p_median <- plot_box(Median_Long, "Median Egg Laid","Days", p_use[["Median"]])
p_end    <- plot_box(End_Long,    "Last Egg Laid",  "Days", p_use[["End"]])
p_death  <- plot_box(Death_Long,  "Time to Death",          "Days", p_use[["Death"]])

# Panel A: Egg Timing
p_timing <- (p_start + p_median + p_end) +
  plot_layout(axes = "collect") +
  plot_annotation(
    title = "Oviposition Timing by Temperature",
    tag_levels = "A",
    caption = paste0(
      "Boxes describe median and IQR; white diamonds describe means; dots describe individual females.\n",
      "Brackets describe Conway-Maxwell-Poisson GLM, unadjusted p-values. ",
      "*** p<0.001, ** p<0.01, * p<0.05."
    ),
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
  )

# Panel B: fecundity and survival
p_fecund <- (p_eggs + p_death) +
  plot_annotation(
    title = "Fecundity and Longevity by Temperature",
    tag_levels = "A",
    caption = paste0(
      "Boxes describe median and IQR; white diamonds describe means; dots describe individual females.\n",
      "Brackets describe Conway-Maxwell-Poisson GLM, unadjusted p-values. ",
      "*** p<0.001, ** p<0.01, * p<0.05."
    ),
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 16))
  )

ggsave("Oviposition_Timing.png",    p_timing, width = 15, height = 5.5, dpi = 200)
ggsave("Fecundity_and_Longevity.png", p_fecund, width = 10, height = 5.5, dpi = 200)
