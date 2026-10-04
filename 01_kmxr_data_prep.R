# =============================================================
# 01. Data preprocessing
# KMXR_Data.csv
# 1.Read data
# 2.Data Preprocessing
# 3.Cleaned Data table
#
# Output: 
# 01_prep_kmxr_data.csv
# =============================================================

# ================================================
# 0. Set Up
# ================================================
#Package
library(tidyverse)
library(lubridate)
library(rstatix)
library(readr)
# working dir
setwd("D:/Capstone/CapstoneSp26")
# read data
df_raw <- read_csv("D:/CDS_SYST_Prj/KMXR_Data.csv")
nrow(df_raw) #[1] 11371245


## Box
df_raw %>%
  summarise(
    lon_min = min(lon, na.rm = TRUE),
    lon_max = max(lon, na.rm = TRUE),
    lat_min = min(lat, na.rm = TRUE),
    lat_max = max(lat, na.rm = TRUE)
  )


# ================================================
# 1. Filter Data
# ================================================
# Data Setting
df <- df_raw
# TimeZone: UTC
# Date Range: 2025-10-01 : 2025-12-31
# AI note: combined all data preprocessing code

## Time (UTC) -------------------------------
df <- df %>%
  mutate(
    datetime = ymd_hms(time, tz = "UTC"),
    date = as_date(datetime),
    year = year(datetime),
    month = month(datetime),
    day = day(datetime),
    hour = hour(datetime),
    minute = minute(datetime),
    second = second(datetime),
    day_of_week = wday(datetime, label = TRUE, abbr = TRUE, week_start = 1)
  ) %>%
  filter(
    date >= as.Date("2025-10-01"),
    date <= as.Date("2025-12-31")
  ) 

#%>%filter(minute %in% c(0, 10, 20, 30, 40, 50),second == 0)


## Altitude, Velocity ------------------------------------
df <- df %>%
  filter(
    !is.na(baroaltitude),
    !is.na(velocity),
    baroaltitude > 20000,       # Filter above FL200
    !is.na(heading)
  ) %>%
  mutate(
    altitude_band = case_when(
      baroaltitude >= 20000 & baroaltitude < 25000 ~ "FL200-250",
      baroaltitude >= 25000 & baroaltitude < 30000 ~ "FL250-300",
      baroaltitude >= 30000 & baroaltitude < 35000 ~ "FL300-350",
      baroaltitude >= 35000 & baroaltitude <= 40000 ~ "FL350-400",
      baroaltitude > 40000 ~ "Above FL400",
      TRUE ~ "Other")
  )


## Heading ------------------------------------
df <- df %>%
  mutate(
    direction = case_when(
      heading < 45 | heading > 315 ~ "North",
      heading > 135 & heading < 225 ~ "South",
      TRUE ~ "Other")
  ) %>%
  filter(!is.na(direction)
  )

## Quick Overview ----------------------------
#cat("Start date:", min(df$date), "\n")
#cat("End date:", max(df$date), "\n")
#cat("Unique dates in raw data:", n_distinct(df_raw$date), "\n")
nrow(df) #[1] 10964122


## Save Filtered data ------------------------
write_csv(df, "01_prep_kmxr_data.csv")

