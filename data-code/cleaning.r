## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, readr)

# Set working directory
setwd("C:/Users/xucar/Desktop/DFWHC")

## Read in data ------------------------------------------------------------
dfwhc1 = read_excel("data/input/DFWHC1-26.xlsx")

## Cleaning ----------------------------------------------------------------

# rename columns
dfwhc1 = dfwhc1 %>%
    rename(
        icd10 = Prn_Diag_Desc,
        month_year = D_Mnth_Yr,
        year = D_Year,
        hospital = Hosp_Name_Short,
        hosp_county = Hosp_County,
        payer = Stat_Payer_Primary,
        payer_group = Stat_Payer_Group,
        race = Pt_Race,
        gender = Pt_Gender,
        ethnicity = Pt_Ethnicity,
        age_group = Pt_AgeGroup
    )

# extract ICD-10 codes
dfwhc1 = dfwhc1 %>%
  mutate(
    icd10 = str_extract(icd10, "^[A-Z0-9\\.]+"))

# create new substance column, grouped by ICD-10 codes (18 substance categories)
dfwhc1 = dfwhc1 %>%
  mutate(
    substance = case_when(
      # alcohol
      icd10 %in% c(
        "Z71.41","Y90.2","T51.94XA","T51.91XA","T51.8X1A","R78.0","Q86.0","P04.3","G31.2",
        "F10.988","F10.982","F10.288","F10.282","F10.280","F10.188","F10.182","F10.180",
        "F10.99","F10.94","F10.29","F10.24","F10.19","F10.14"
      ) ~ "alcohol",

      # opium
      icd10 %in% c("T40.0X4A","T40.0X1A") ~ "opium",

      # heroin
      icd10 %in% c("T40.1X4A","T40.1X2D","T40.1X2A","T40.1X1A") ~ "heroin",

      # other opioids
      icd10 %in% c("T40.2X5A","T40.2X4A","T40.2X3A","T40.2X2A","T40.2X1S","T40.2X1D","T40.2X1A") ~ "other opioids",

      # methadone
      icd10 %in% c("T40.3X6A","T40.3X5A","T40.3X4A","T40.3X2A","T40.3X1A") ~ "methadone",

      # synthetic narcotics
      icd10 %in% c(
        "T40.415A","T40.414A","T40.412A","T40.411D","T40.411A",
        "T40.425A","T40.424A","T40.422A","T40.421A",
        "T40.495A","T40.492A","T40.491A"
      ) ~ "synthetic narcotics",

      # cocaine
      icd10 %in% c(
        "T40.5X5A","T40.5X4D","T40.5X4A","T40.5X2A","T40.5X1A","R78.2",
        "F14.988","F14.982","F14.288","F14.259","F14.251","F14.250","F14.188","F14.182",
        "F14.180","F14.151","F14.150","F14.99","F14.94","F14.29","F14.24","F14.19","F14.14"
      ) ~ "cocaine",

      # other narcotics
      icd10 %in% c(
        "T40.695A","T40.694A","T40.692A","T40.691A",
        "T40.605A","T40.604S","T40.604A","T40.602A","T40.601S","T40.601A"
      ) ~ "other narcotics",

      # cannabis
      icd10 %in% c(
        "T40.715A","T40.714A","T40.712A","T40.711S","T40.711D","T40.711A",
        "T40.725A","T40.724A","T40.723A","T40.722D","T40.722A","T40.721S","T40.721A",
        "F12.988","F12.288","F12.188","F12.180","F12.99","F12.29","F12.19"
      ) ~ "cannabis",

      # hallucinogens
      icd10 %in% c("T40.8X4A","T40.8X2A","T40.8X1A") ~ "hallucinogens",

      # other psychodysleptics
      icd10 %in% c("T40.994A","T40.993A","T40.992A","T40.991A","T40.902A","T40.901A") ~ "other psychodysleptics",

      # psychostimulants
      icd10 %in% c(
        "T43.695A","T43.694A","T43.692A","T43.691A",
        "T43.655A","T43.654A","T43.652A","T43.651A",
        "T43.644A","T43.642A","T43.641A",
        "T43.636A","T43.634A","T43.632A","T43.631A",
        "T43.625A","T43.624A","T43.623A","T43.622A","T43.621D","T43.621A",
        "T43.615A","T43.614A","T43.612A","T43.611A",
        "T43.602A","T43.601A"
      ) ~ "psychostimulants",

      # antidepressants
      icd10 %in% c(
        "T43.296A","T43.295A","T43.294A","T43.293A","T43.292A","T43.291A",
        "T43.226A","T43.225A","T43.224A","T43.222A","T43.221A",
        "T43.216A","T43.215A","T43.214A","T43.212A","T43.211A",
        "T43.206A","T43.205A","T43.204A","T43.202A","T43.201A",
        "T43.025A","T43.022A","T43.021A",
        "T43.016A","T43.015A","T43.014A","T43.012A","T43.011A"
      ) ~ "antidepressants",

      # antipsychotics / neuroleptics 
      icd10 %in% c(
        "T43.3X5A","T43.3X2A","T43.3X1A",
        "T43.4X5A","T43.4X2A","T43.4X1A",
        "T43.506A","T43.505A","T43.504A","T43.502A","T43.501A","T43.592A",
        "T43.591A", "T43.594A", "T43.595A", "T43.596A", "T43.592S"
      ) ~ "antipsychotics/neuroleptics",

      # other psychotropics
      icd10 %in% c("T43.96XA","T43.95XA","T43.92XA","T43.91XA","T43.8X2A","T43.8X1A") ~ "other psychotropics",

      # barbiturates
      icd10 %in% c("T42.3X5A","T42.3X4A","T42.3X2A","T42.3X1A") ~ "barbiturates",

      # benzodiazepines
      icd10 %in% c("T42.4X5A","T42.4X4A","T42.4X3A","T42.4X2A","T42.4X1A") ~ "benzodiazepines",

      # inhalants / solvents
      icd10 %in% c(
        "T52.94XA","T52.92XA","T52.91XA","T52.8X4A","T52.8X2A","T52.8X1D","T52.8X1A",
        "T52.4X2A","T52.4X1A","T52.3X1A","T52.2X1A","T52.1X1A","T52.0X4A","T52.0X3A","T52.0X2A","T52.0X1A"
      ) ~ "inhalants/solvents",

      TRUE ~ NA_character_
    )
  )

# type casting
dfwhc1 = dfwhc1 %>%
    mutate(
        icd10 = as.factor(icd10),
        substance = as.factor(substance),
        hospital = as.factor(hospital),
        hosp_county= as.factor (hosp_county),
        payer = as.factor(payer),
        payer_group = as.factor(payer_group),
        race = as.factor(race),
        gender = as.factor(gender),
        ethnicity = as.factor(ethnicity),
        age_group = as.factor(age_group))

# deleting 27 rows with NA for year and month
sum(is.na(dfwhc1$year))
sum(is.na(dfwhc1$month_year))

dfwhc1 = dfwhc1 %>%
    filter(!is.na(year), !is.na(month_year))

dfwhc1 = dfwhc1 %>%
    mutate(
        year = as.integer(year), 
        month_year = as.integer(month_year))

# Rewrite final dataset
write_csv(dfwhc1, "data/output/dfwhc1.csv")