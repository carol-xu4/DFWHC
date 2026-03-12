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

# alcohol
ggplot(substance_counts %>%
    filter(substance == "alcohol"),
    aes(x = month, y = n, color = substance)) +
    geom_line(linewidth = 1.3) +
    geom_point(size = 2) +
    labs(title = "Alcohol-Related Encounters (DFW, 2023–2025)",
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
ggsave("results/alcohol_trends.png",
    width = 12, height = 8)

# age distribution
age_counts = dfwhc1 %>%
    count(age_group) %>%
    mutate(prop = n / sum(n))
view(age_counts)
write_csv(age_counts, "results/age_counts.csv")

age_levels = c("0–17", "18–44", "45–64", "65–74", "75+", "Unknown")

dfwhc1 = dfwhc1 %>%
  mutate(
    age_raw = str_remove_all(age_group, "\\*"),
    age_clean = case_when(
      str_detect(age_raw, "UNKNOWN") ~ "Unknown",

      age_raw %in% c("1-28 Days", "29-365 Days", "1-4 Years",
                     "5-9 Years", "10-14 Years", "15-17 Years") ~ "0–17",

      age_raw == "0-17 Years"  ~ "0–17",
      age_raw == "18-44 Years" ~ "18–44",
      age_raw == "45-64 Years" ~ "45–64",
      age_raw == "65-74 Years" ~ "65–74",
      age_raw == "75+ Years"   ~ "75+",

      age_raw %in% c("18-19 Years", "20-24 Years", "25-29 Years",
                     "30-34 Years", "35-39 Years", "40-44 Years") ~ "18–44",
      age_raw %in% c("45-49 Years", "50-54 Years", "55-59 Years", "60-64 Years") ~ "45–64",
      age_raw %in% c("65-69 Years", "70-74 Years") ~ "65–74",
      age_raw %in% c("75-79 Years", "80-84 Years", "85-89 Years", "90+ Years") ~ "75+",
      TRUE ~ NA_character_),
    age_clean = factor(age_clean, levels = age_levels)) %>%
  select(-age_raw)

ggplot(dfwhc1 %>%
    filter(!is.na(age_clean)),
    aes(x = age_clean)) +
    geom_bar(fill = "royalblue2", color = "black") +
    labs(title = "DFWHC Substance Use Encounters Age Distribution",
         x = "Age group", y = "Count (in thousands)") +
    scale_y_continuous(
      labels = label_number(scale = 1e-3)) +
    theme_stata() +
    theme(
      plot.title = element_text(size = 35, face = "bold"),
      axis.title  = element_text(size = 25, face = "bold"),
      axis.text   = element_text(size = 20),
      axis.text.y = element_text(angle = 0),
      axis.text.x = element_text(angle = 0),
      plot.background = element_rect(fill = "white"))
ggsave("results/age_groups.png",
    width = 12, height = 8)

# hospitals
hospital_counts = dfwhc1 %>%
 distinct(hospital, hosp_county) %>%
 right_join(
 dfwhc1 %>% count(hospital), 
 by = "hospital") %>% arrange(desc(n))
write_csv(hospital_counts, "results/hospital_counts.csv")

# JPS Hospital substance counts
jps_substances = dfwhc1 %>%
    filter(hospital == "JPS Hosp") %>%
    count(substance, sort = TRUE) 
write_csv(jps_substances, "results/jps_substances")

jps_substances_year = dfwhc1 %>%
    filter(hospital == "JPS Hosp") %>%
    count(substance, year, sort = TRUE) %>%
    arrange(substance, year)
write_csv(jps_substances_year, "results/jps_substances_year")

jps_monthly = dfwhc1 %>%
    mutate(month = as.Date(paste0(month_year, "01"), format = "%Y%m%d")) %>%
    filter(hospital == "JPS Hosp") %>%
    count(year, month, substance)
write_csv(jps_monthly, "results/jps_monthly")

ggplot(jps_monthly,
       aes(x = month, y = n, color = substance)) +
  geom_line(linewidth = 1.3) +
  geom_point(size = 2) +
  labs(title = "JPS Hospital: Substance-Related Encounters by Month",
       x = "Year",
       y = "Count") +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  theme_stata() +
  theme(
    plot.title = element_text(size = 30, face = "bold"),
    axis.title  = element_text(size = 22, face = "bold"),
    axis.text   = element_text(size = 18),
    plot.background = element_rect(fill = "white"),
    legend.title = element_text(size = 16),
    legend.text  = element_text(size = 14)
  )
ggsave("results/jps_substances.png", width = 14, height = 9)

# Payer trends
payer_group_counts = dfwhc1 %>%
    count(payer_group) %>%
    mutate(prop = n / sum(n),
    pct_label = paste0(round(prop*100, 1), "%"))    

ggplot(payer_group_counts, aes(x = "", y = n, fill = payer_group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = pct_label),
    position = position_stack(vjust = 0.5),
    color = "white", size = 8, fontface = "bold") +
  scale_fill_manual(
    values = c(
      "Insured" = "dodgerblue2",
      "MC Advantage" = "magenta",
      "Medicaid" = "paleturquoise4",
      "Medicare" = "tomato",
      "Uninsured" = "lightpink"),
    name = "Payer type" ) +
  labs(title = "Payer Type Distribution (DFW 2023-2025)",
    x = NULL, y = NULL) +
  theme_stata() +
  theme(
    plot.title = element_text(size = 35, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)) +
  scale_y_continuous(expand = c(0, 0), labels = NULL, breaks = NULL)
ggsave("results/payer_group.png",
    width = 12, height = 8)

# payer types by year
payers2023 = dfwhc1 %>%
    filter(year==2023) %>%
    count(payer_group) %>%
    mutate(prop = n / sum(n),
    pct_label = paste0(round(prop*100, 1), "%"))

ggplot(payers2023, aes(x = "", y = n, fill = payer_group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = pct_label),
    position = position_stack(vjust = 0.5),
    color = "white", size = 8, fontface = "bold") +
  scale_fill_manual(
    values = c(
      "Insured" = "dodgerblue2",
      "MC Advantage" = "magenta",
      "Medicaid" = "paleturquoise4",
      "Medicare" = "tomato",
      "Uninsured" = "lightpink"),
    name = "Payer type" ) +
  labs(title = "2023 Payer Type Distribution",
    x = NULL, y = NULL) +
  theme_stata() +
  theme(
    plot.title = element_text(size = 35, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)) +
  scale_y_continuous(expand = c(0, 0), labels = NULL, breaks = NULL)
ggsave("results/payers2023.png",
    width = 12, height = 8)

payers2024 = dfwhc1 %>%
    filter(year==2024) %>%
    count(payer_group) %>%
    mutate(prop = n / sum(n),
    pct_label = paste0(round(prop*100, 1), "%"))

ggplot(payers2024, aes(x = "", y = n, fill = payer_group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = pct_label),
    position = position_stack(vjust = 0.5),
    color = "white", size = 8, fontface = "bold") +
  scale_fill_manual(
    values = c(
      "Insured" = "dodgerblue2",
      "MC Advantage" = "magenta",
      "Medicaid" = "paleturquoise4",
      "Medicare" = "tomato",
      "Uninsured" = "lightpink"),
    name = "Payer type" ) +
  labs(title = "2024 Payer Type Distribution",
    x = NULL, y = NULL) +
  theme_stata() +
  theme(
    plot.title = element_text(size = 35, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)) +
  scale_y_continuous(expand = c(0, 0), labels = NULL, breaks = NULL)
ggsave("results/payers2024.png",
    width = 12, height = 8)

payers2025 = dfwhc1 %>%
    filter(year==2025) %>%
    count(payer_group) %>%
    mutate(prop = n / sum(n),
    pct_label = paste0(round(prop*100, 1), "%"))

ggplot(payers2025, aes(x = "", y = n, fill = payer_group)) +
  geom_bar(stat = "identity", width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(
    aes(label = pct_label),
    position = position_stack(vjust = 0.5),
    color = "white", size = 8, fontface = "bold") +
  scale_fill_manual(
    values = c(
      "Insured" = "dodgerblue2",
      "MC Advantage" = "magenta",
      "Medicaid" = "paleturquoise4",
      "Medicare" = "tomato",
      "Uninsured" = "lightpink"),
    name = "Payer type" ) +
  labs(title = "2025 Payer Type Distribution",
    x = NULL, y = NULL) +
  theme_stata() +
  theme(
    plot.title = element_text(size = 35, face = "bold"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14)) +
  scale_y_continuous(expand = c(0, 0), labels = NULL, breaks = NULL)
ggsave("results/payers2025.png",
    width = 12, height = 8)

# payer by substance
dfwhc1 = dfwhc1 %>%
  mutate(substance_type = case_when(
      substance %in% opioids ~ "Opioids",
      substance %in% stimulants ~ "Stimulants",
      substance %in% sedatives ~ "Sedatives",
      substance %in% other_drugs ~ "Other drugs",
      substance == "alcohol" ~ "Alcohol",
      TRUE ~ NA_character_))

payer_substance_counts = dfwhc1 %>%
  filter(!is.na(substance_type)) %>%
  count(substance_type, payer_group) %>%
  group_by(substance_type) %>%
  mutate(
    prop = n / sum(n),
    pct_label = paste0(round(prop * 100, 1), "%")) %>%
  ungroup()

ggplot(payer_substance_counts, aes(x = substance_type, y = n, fill = payer_group)) +
  geom_bar(stat = "identity", color = "white") +
  geom_text(
    aes(label = pct_label),
    position = position_stack(vjust = 0.5),
    color = "white", size = 8, fontface = "bold") +
  scale_fill_manual(
    values = c(
      "Insured" = "dodgerblue2",
      "MC Advantage" = "magenta",
      "Medicaid" = "paleturquoise4",
      "Medicare" = "tomato",
      "Uninsured" = "lightpink"),
    name = "Payer type" ) +
  labs(
    title = "Payer Type by Substance Category (DFW 2023-2025)",
    x = "Substance Category",
    y = "Number of Encounters") +
  theme_stata() +
  theme(
    plot.title = element_text(size = 35, face = "bold"),
    axis.title = element_text(size = 20),
    axis.text = element_text(size = 16),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "white"),
    legend.title = element_text(size = 16, face = "bold"),
    legend.text = element_text(size = 14))
ggsave("results/payer_by_substance_category.png",
    width = 14, height = 15) 
