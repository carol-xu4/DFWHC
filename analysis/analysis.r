## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, knitr, ggthemes, stringr, data.table, gdata, readr, tidyr, arrow, scales, purrr, tinytex)

# Set working directory
setwd("C:/Users/xucar/Desktop/DFWHC")

## ANALYSIS ----------------------------------------------------------------
dfwhc1 = read_csv("data/output/DFWHC1.csv")

# sex distribution
gender_counts = dfwhc1 %>%
    count(gender) %>%
    mutate(prop = n / sum(n),
    label = paste0(gender, "\n", n, "(", round(prop*100, 1), "%)"))

ggplot(gender_counts, aes(x = "", y = n, fill = factor(gender))) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar(theta = "y") +
    geom_text(aes(label = label),
        position = position_stack(vjust = 0.5),
        color = "white", size = 8, fontface = "bold") +
    scale_fill_manual(values = c("Male" = "royalblue2", "Female" = "violetred2", "Blinded Sex" = "azure4", "Unknown" = "ghostwhite")) +
    labs(title = "DFWHC Outpatient Substance Use Sex Distribution", x = NULL, y = NULL) +
    theme_stata() +
    theme(
        plot.title = element_text(size = 35, face = "bold"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank(),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "white"),
        legend.position = "none") +
    scale_y_continuous(expand = c(0,0), labels = NULL, breaks = NULL)
ggsave("results/gender.png",
    width = 12, height = 8)

# substances over time
substance_counts = dfwhc1 %>%
    count(month_year, substance) %>%
    arrange(month_year, desc(n))
write_csv(substance_counts, "results/substance_counts.csv")

substance_counts = substance_counts %>%
  mutate(
    month = as.Date(paste0(month_year, "01"), format = "%Y%m%d"))

opioids = c("opium","heroin","other opioids","methadone","synthetic narcotics","other narcotics")
stimulants = c("psychostimulants","cocaine")
sedatives = c("benzodiazepines","barbiturates","antidepressants","antipsychotics/neuroleptics","other psychotropics")
other_drugs = c("cannabis","hallucinogens","other psychodysleptics","inhalants/solvents")

# opioids
ggplot(substance_counts %>%
    filter(substance %in% opioids),
    aes(x = month, y = n, color = substance)) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 2) +
    labs(title = "Opioid-Related Encounters (DFW, 2023–2025)",
         x = "Year",
         y = "Count") +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    theme_stata() +
    theme(
      plot.title = element_text(size = 35, face = "bold"),
      axis.title  = element_text(size = 25, face = "bold"),
      axis.text   = element_text(size = 20),
      plot.background = element_rect(fill = "white"),
      legend.title = element_text(size = 18),
      legend.text  = element_text(size = 16)
    )
ggsave("results/opioid_trends.png",
    width = 12, height = 8)

# stimulants
ggplot(substance_counts %>%
    filter(substance %in% stimulants),
    aes(x = month, y = n, color = substance)) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 2) +
    labs(title = "Stimulant-Related Encounters (DFW, 2023–2025)",
         x = "Year",
         y = "Count") +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    theme_stata() +
    theme(
      plot.title = element_text(size = 35, face = "bold"),
      axis.title  = element_text(size = 25, face = "bold"),
      axis.text   = element_text(size = 20),
      plot.background = element_rect(fill = "white"),
      legend.title = element_text(size = 18),
      legend.text  = element_text(size = 16)
    )
ggsave("results/stimulant_trends.png",
    width = 12, height = 8)

# sedatives
ggplot(substance_counts %>%
    filter(substance %in% sedatives),
    aes(x = month, y = n, color = substance)) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 2) +
    labs(title = "Sedative-Related Encounters (DFW, 2023–2025)",
         x = "Year",
         y = "Count") +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    theme_stata() +
    theme(
      plot.title = element_text(size = 35, face = "bold"),
      axis.title  = element_text(size = 25, face = "bold"),
      axis.text   = element_text(size = 20),
      plot.background = element_rect(fill = "white"),
      legend.title = element_text(size = 18),
      legend.text  = element_text(size = 16)
    )
ggsave("results/sedative_trends.png",
    width = 12, height = 8)

# other drugs
ggplot(substance_counts %>%
    filter(substance %in% other_drugs),
    aes(x = month, y = n, color = substance)) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 2) +
    labs(title = "Other Drug-Related Encounters (DFW, 2023–2025)",
         x = "Year",
         y = "Count") +
    scale_x_date(
      date_breaks = "1 year",
      date_labels = "%Y"
    ) +
    theme_stata() +
    theme(
      plot.title = element_text(size = 35, face = "bold"),
      axis.title  = element_text(size = 25, face = "bold"),
      axis.text   = element_text(size = 20),
      plot.background = element_rect(fill = "white"),
      legend.title = element_text(size = 18),
      legend.text  = element_text(size = 16)
    )
ggsave("results/other_drug_trends.png",
    width = 12, height = 8)

# aggregated groups