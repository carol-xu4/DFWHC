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
dfwhc1 <- dfwhc1 %>%
  mutate(
    substance = case_when(

      # alcohol
      str_detect(icd10, "^(T51|F10|R78\\.0|Y90|Z71\\.41|G31\\.2|P04\\.3|Q86\\.0)") ~ "alcohol",

      # --- Opioids ---
      str_detect(icd10, "^T40\\.0") ~ "opium",
      str_detect(icd10, "^T40\\.1") ~ "heroin",
      str_detect(icd10, "^T40\\.2") ~ "other opioids",
      str_detect(icd10, "^T40\\.3") ~ "methadone",
      str_detect(icd10, "^T40\\.4") ~ "synthetic narcotics",

      # --- Cocaine ---
      str_detect(icd10, "^(T40\\.5|F14|R78\\.2)") ~ "cocaine",

      # --- Other narcotics ---
      str_detect(icd10, "^T40\\.6|^T40\\.69|^T40\\.695") ~ "other narcotics",

      # --- Cannabis ---
      str_detect(icd10, "^(T40\\.7|F12)") ~ "cannabis",

      # --- Hallucinogens ---
      str_detect(icd10, "^T40\\.8") ~ "hallucinogens",

      # --- Other psychodysleptics ---
      str_detect(icd10, "^T40\\.9") ~ "other psychodysleptics",

      # --- Psychostimulants ---
      str_detect(icd10, "^T43\\.6") ~ "psychostimulants",

      # --- Antidepressants ---
      str_detect(icd10, "^T43\\.2|^T43\\.0") ~ "antidepressants",

      # --- Antipsychotics ---
      str_detect(icd10, "^T43\\.3") ~ "antipsychotics",

      # --- Neuroleptics ---
      str_detect(icd10, "^T43\\.4") ~ "neuroleptics",

      # --- Other antipsychotics ---
      str_detect(icd10, "^T43\\.5") ~ "other antipsychotics",

      # --- Other psychotropics ---
      str_detect(icd10, "^T43\\.(8|9)") ~ "other psychotropics",

      # --- Sedatives ---
      str_detect(icd10, "^T42\\.3") ~ "barbiturates",
      str_detect(icd10, "^T42\\.4") ~ "benzodiazepines",

      # --- Inhalants / Solvents ---
      str_detect(icd10, "^T52") ~ "inhalants/solvents",

      TRUE ~ NA_character_
    )
  )
