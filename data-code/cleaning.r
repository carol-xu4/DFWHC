## Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata, readr)

## Set working directory
setwd("C:/Users/xucar/Desktop/DFWHC")

## Read in data ------------------------------------------------------------
dfwhc1 = read_excel("data/input/DFWHC1-26.xlsx")

dfwhc1 = dfwhc1 %>%
    rename(
        dx = Prn_Diag_Desc,
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
