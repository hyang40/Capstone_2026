# =============================================================
# 02. Prep Data EDA
# Read 01_prep_kmxr_data.csv
# 1. Address RQ1, RQ2
# 2.
# 3.
#
# Output: 
# Preprocessed Data EDA Plots
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
df_prep <- read_csv("01_prep_kmxr_data.csv")
df_prep <- df #from 01.R
nrow(df_prep) #[1] 10964122

df <- df_prep
# output dir -------------------------------------
plot_dir <- file.path("02_All_Data_EDA")
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}


# ================================================
# 1. Day of Week and Hour - 날짜별 × 시간대별 집계
# ================================================

df_WeekHour <- df %>%
  group_by(date, year, month, day_of_week, hour) %>%
  summarise(
    flight_count  = n_distinct(callsign),
    mean_altitude = mean(baroaltitude, na.rm = TRUE),
    mean_speed    = mean(velocity, na.rm = TRUE),
    .groups = "drop"
  )

head(df_WeekHour)

# ================================================
# 2. Full date of hour grid - Date Coverage Verify 
# ================================================
# fill zeros in NA values

all_dates <- seq.Date(min(df_WeekHour$date), max(df_WeekHour$date), by = "day")

df_full <- expand_grid(date = all_dates, hour = 0:23) %>%
  left_join(df_WeekHour, by = c("date", "hour")) %>%
  mutate(
    year         = ifelse(is.na(year),  year(date),  year),
    month        = ifelse(is.na(month), month(date), month),
    day_of_week  = wday(date, label = TRUE, abbr = TRUE, week_start = 1),
    has_data     = !is.na(flight_count),
    flight_count = ifelse(is.na(flight_count), 0, flight_count)
  )

cat("Full grid rows:", nrow(df_full), "\n")


# ================================================
# 3. All Date summary
# ================================================
coverage_by_date <- df_full %>%
  group_by(date) %>%
  summarise(
    hours_with_data = sum(has_data),
    hours_missing   = 24 - sum(has_data),
    .groups = "drop"
  )

coverage_by_hour <- df_full %>%
  group_by(hour) %>%
  summarise(
    days_with_data = sum(has_data),
    days_missing   = n() - sum(has_data),
    .groups = "drop"
  )

print(coverage_by_date, n = 20)
print(coverage_by_hour, n = 24)


# ================================================
# ***3. Statistic *** - EDA 1
# ================================================


## 1) Summary: Weekday * Hour
summary_weekday_hour <- df_WeekHour %>%
  group_by(day_of_week, hour) %>%
  summarise(
    n_blocks     = n(),
    
    count_mean   = mean(flight_count,  na.rm = TRUE),
    count_median = median(flight_count, na.rm = TRUE),
    count_sd     = sd(flight_count,    na.rm = TRUE),
    count_min    = min(flight_count,   na.rm = TRUE),
    count_q25    = quantile(flight_count, 0.25, na.rm = TRUE),
    count_q75    = quantile(flight_count, 0.75, na.rm = TRUE),
    count_max    = max(flight_count,   na.rm = TRUE),
    
    alt_mean     = mean(mean_altitude,  na.rm = TRUE),
    alt_median   = median(mean_altitude, na.rm = TRUE),
    alt_sd       = sd(mean_altitude,    na.rm = TRUE),
    alt_min      = min(mean_altitude,   na.rm = TRUE),
    alt_q25      = quantile(mean_altitude, 0.25, na.rm = TRUE),
    alt_q75      = quantile(mean_altitude, 0.75, na.rm = TRUE),
    alt_max      = max(mean_altitude,   na.rm = TRUE),
    
    speed_mean   = mean(mean_speed,  na.rm = TRUE),
    speed_median = median(mean_speed, na.rm = TRUE),
    speed_sd     = sd(mean_speed,    na.rm = TRUE),
    speed_min    = min(mean_speed,   na.rm = TRUE),
    speed_q25    = quantile(mean_speed, 0.25, na.rm = TRUE),
    speed_q75    = quantile(mean_speed, 0.75, na.rm = TRUE),
    speed_max    = max(mean_speed,   na.rm = TRUE),
    
    .groups = "drop"
  )
print(summary_weekday_hour, n = 50)

## 2) Summary: Hours
summary_hour <- df_WeekHour %>%
  group_by(hour) %>%
  summarise(
    n_blocks     = n(),
    
    count_mean   = mean(flight_count,  na.rm = TRUE),
    count_median = median(flight_count, na.rm = TRUE),
    count_sd     = sd(flight_count,    na.rm = TRUE),
    count_q25    = quantile(flight_count, 0.25, na.rm = TRUE),
    count_q75    = quantile(flight_count, 0.75, na.rm = TRUE),
    
    alt_mean     = mean(mean_altitude,  na.rm = TRUE),
    alt_median   = median(mean_altitude, na.rm = TRUE),
    alt_sd       = sd(mean_altitude,    na.rm = TRUE),
    alt_q25      = quantile(mean_altitude, 0.25, na.rm = TRUE),
    alt_q75      = quantile(mean_altitude, 0.75, na.rm = TRUE),
    
    speed_mean   = mean(mean_speed,  na.rm = TRUE),
    speed_median = median(mean_speed, na.rm = TRUE),
    speed_sd     = sd(mean_speed,    na.rm = TRUE),
    speed_q25    = quantile(mean_speed, 0.25, na.rm = TRUE),
    speed_q75    = quantile(mean_speed, 0.75, na.rm = TRUE),
    
    .groups = "drop"
  )
print(summary_hour, n = 24)


# ================================================
# 4. Plotting - EDA 2
# ================================================

## 1) p1: Data availability heatmap ***
p1 <- ggplot(df_full, aes(x = hour, y = date, fill = has_data)) +
  geom_tile(color = "white") +
  scale_x_continuous(breaks = 0:23) +
  scale_fill_manual(values = c("TRUE" = "#00BFC4", "FALSE" = "#F8766D")) +
  labs(
    title = "Data Availability by Date and Hour",
    x     = "Hour",
    y     = "Date",
    fill  = "has_data"
  ) +
  theme_gray(base_size = 12)

## 2) p2: Flight count heatmap
p2 <- ggplot(df_full, aes(x = hour, y = date, fill = flight_count)) +
  geom_tile(color = "white") +
  scale_x_continuous(breaks = 0:23) +
  scale_fill_gradient(low = "white", high = "navy") +
  labs(
    title = "Flight Count by Date and Hour",
    x     = "Hour",
    y     = "Date",
    fill  = "flight_count"
  ) +
  theme_gray(base_size = 12)


## 3) p3: Flight count boxplot by hour
p3 <- ggplot(df_WeekHour, aes(x = factor(hour), y = flight_count)) +
  geom_boxplot() +
  labs(
    title = "Flight Count Distribution by Hour",
    x     = "Hour",
    y     = "Flight Count"
  ) +
  theme_gray(base_size = 12)

## 4) p4: Mean altitude boxplot by hour
p4 <- ggplot(df_WeekHour, aes(x = factor(hour), y = mean_altitude)) +
  geom_boxplot() +
  labs(
    title = "Mean Altitude Distribution by Hour",
    x     = "Hour",
    y     = "Mean Altitude"
  ) +
  theme_gray(base_size = 12)

## 5) p5: Mean speed boxplot by hour
p5 <- ggplot(df_WeekHour, aes(x = factor(hour), y = mean_speed)) +
  geom_boxplot() +
  labs(
    title = "Mean Speed Distribution by Hour",
    x     = "Hour",
    y     = "Mean Speed"
  ) +
  theme_gray(base_size = 12)


## 6) p6: Flight count boxplot by weekday
p6 <- ggplot(df_WeekHour, aes(x = day_of_week, y = flight_count)) +
  geom_boxplot() +
  labs(
    title = "Flight Count Distribution by Weekday",
    x     = "Weekday",
    y     = "Flight Count"
  ) +
  theme_gray(base_size = 12)

## 7) p7: Mean altitude boxplot by weekday
p7 <- ggplot(df_WeekHour, aes(x = day_of_week, y = mean_altitude)) +
  geom_boxplot() +
  labs(
    title = "Mean Altitude Distribution by Weekday",
    x     = "Weekday",
    y     = "Mean Altitude"
  ) +
  theme_gray(base_size = 12)

## 8) p8: Mean speed boxplot by weekday
p8 <- ggplot(df_WeekHour, aes(x = day_of_week, y = mean_speed)) +
  geom_boxplot() +
  labs(
    title = "Mean Speed Distribution by Weekday",
    x     = "Weekday",
    y     = "Mean Speed"
  ) +
  theme_gray(base_size = 12)

## 9) p9: Histogram of flight count
p9 <- ggplot(df_WeekHour, aes(x = flight_count)) +
  geom_histogram(binwidth = 1, color = "black", fill = "skyblue") +
  labs(title = "Histogram of Flight Count",
       x     = "mean_altitude [feet]") +
  theme_gray(base_size = 12)

## 10) p10: Histogram of mean altitude
p10 <- ggplot(df_WeekHour, aes(x = mean_altitude)) +
  geom_histogram(bins = 30, color = "black", fill = "lightgreen") +
  labs(title = "Histogram of Mean Altitude",
       x     = "mean_altitude [feet]") +
  theme_gray(base_size = 12)

## 11) p11: Histogram of mean speed
p11 <- ggplot(df_WeekHour, aes(x = mean_speed)) +
  geom_histogram(bins = 30, color = "black", fill = "orange") +
  labs(title = "Histogram of Mean Speed") +
  theme_gray(base_size = 12)


# ================================================
# 5. Save all plots - Claude AI
# ================================================
plot_list <- list(
  availability_heatmap                = p1,
  flight_count_heatmap                = p2,
  flight_count_by_hour                = p3,
  mean_altitude_by_hour               = p4,
  mean_speed_by_hour                  = p5,
  Flight_Count_Distribution_by_Weekday  = p6,
  Mean_Altitude_Distribution_by_Weekday = p7,
  Mean_Speed_Distribution_by_Weekday    = p8,
  Histogram_of_Flight_Count            = p9,
  Histogram_of_Mean_Altitude           = p10,
  Histogram_of_Mean_Speed              = p11
)

for (nm in names(plot_list)) {
  ggsave(
    filename = file.path(plot_dir, paste0(nm, ".png")),
    plot     = plot_list[[nm]],
    width    = 10,
    height   = 6,
    dpi      = 300
  )
  cat("Saved:", nm, ".png\n")
}

