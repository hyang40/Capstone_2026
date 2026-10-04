# =============================================================
# 03. Baseline Table
# =============================================================

# ================================================
# 0. Set Up
# ================================================
##Package
library(tidyverse)
library(lubridate)
library(rstatix)
library(readr)
# working dir
setwd("D:/Capstone/CapstoneSp26")
# read data
df_prep <- read_csv("01_prep_kmxr_data.csv")
#df_prep <- df #from 02b.R [1] 9718865
nrow(df_prep) #[1] 10964122


cat("Date Validation\n")
cat(" Start Date:", as.character(min(df$date)), "\n")
cat(" End Datae:", as.character(max(df$date)), "\n")
cat(" Total Date Count:", n_distinct(df$date), "Days\n\n")


# output dir -------------------------------------
plot_dir <- file.path("03_Baseline_EDA")
if (!dir.exists(plot_dir)) {
  dir.create(plot_dir, recursive = TRUE)
}


# ================================================
# 1. Significant Time Filter (UTC 07 - 18)
# ================================================

## 1) Time Filter
df_day <- df_prep %>%
  filter(hour >= 7, hour <= 18)
cat("Post Time Filter:", nrow(df_day), "\n\n")

## 2) Day of Week ReOrder
dow_order <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")


# ================================================
# 2. ## Flight Count (Daily / Hourly)
# ================================================
## 1) Daily Flight Count
daily_counts <- df_day %>%
  group_by(date, day_of_week) %>%
  summarise(n_flights = n_distinct(flight_id), .groups = "drop") %>%
  mutate(day_of_week = factor(as.character(day_of_week), levels = dow_order))
## Total day of Week count
daily_counts %>% count(day_of_week) %>% print()

## 2) Hourly Flight Count
hourly_counts <- df_day %>%
  group_by(date, day_of_week, hour) %>%
  summarise(n_flights = n_distinct(flight_id), .groups = "drop") %>%
  mutate(day_of_week = factor(as.character(day_of_week), levels = dow_order))


## 3) Summary
dow_summary <- daily_counts %>%
  group_by(day_of_week) %>%
  summarise(
    n      = n(),
    mean   = round(mean(n_flights), 1),
    median = round(median(n_flights), 1),
    sd     = round(sd(n_flights), 1),
    iqr    = round(IQR(n_flights), 1),
    min    = min(n_flights),
    max    = max(n_flights),
    cv     = round(sd(n_flights) / mean(n_flights), 3),
    .groups = "drop"
  )

hourly_summary <- hourly_counts %>%
  group_by(day_of_week, hour) %>%
  summarise(
    mean_flights   = round(mean(n_flights), 1),
    median_flights = round(median(n_flights), 1),
    sd_flights     = round(sd(n_flights), 1),
    .groups = "drop"
  )


## 4) Save as CSV
write_csv(dow_summary, file.path(plot_dir, "table1_dow_summary.csv"))
write_csv(hourly_summary, file.path(plot_dir, "table2_hourly_summary.csv"))


# ================================================
# 3. Visualization - Plotting (Daily / Hourly)
# ================================================
## 1) p1: Boxplot — daily count by DOW
p1 <- ggplot(daily_counts,
             aes(x = day_of_week, y = n_flights, fill = day_of_week)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 2, show.legend = FALSE) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.5) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title    = "ZJX Corridor: Daily Overflight Count by Day of Week",
    subtitle = "Oct 1 – Dec 31, 2025. Above FL200. UTC 7–18.",
    x = "Day of Week", y = "Unique Flights per Day"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

## Save
ggsave(file.path(plot_dir, "plot1_daily_count_by_dow.png"), p1, width = 9, height = 5, dpi = 150)
cat("Saved: plot1_daily_count_by_dow.png\n")



## 2) p2: Hourly line chart
p2 <- ggplot(hourly_summary,
             aes(x = hour, y = mean_flights,
                 color = day_of_week, group = day_of_week)) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.2) +
  #scale_x_continuous(breaks = 0 :23) +
  scale_x_continuous(breaks = 7:18) +
  scale_color_brewer(palette = "Dark2", name = "Day") +
  labs(
    title    = "ZJX Corridor: Mean Hourly Flight Count by Day of Week",
    subtitle = "Oct 1 – Dec 31, 2025. UTC hours.",
    x = "Hour (UTC)", y = "Mean Unique Flights"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

## Save
ggsave(file.path(plot_dir, "plot2_hourly_by_dow.png"), p2, width = 12, height = 5, dpi = 150)
cat("Saved: plot2_hourly_by_dow.png\n")


## p3: Heatmap with numbers
p3 <- ggplot(hourly_summary,
             aes(x = hour, y = day_of_week, fill = mean_flights)) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(aes(label = round(mean_flights, 0)),
            size = 3.2, color = "white", fontface = "bold") +
  scale_fill_gradient(low = "#cfe2f3", high = "#1a4f7a",
                      name = "Mean\nFlights") +
  scale_x_continuous(breaks = 7:18) +
  labs(
    title    = "ZJX Corridor: Baseline Heatmap — Day × Hour",
    subtitle = "Oct 1 – Dec 31, 2025. UTC hours.",
    x = "Hour (UTC)", y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
## Save
ggsave(file.path(plot_dir, "plot3_heatmap.png"), p3, width = 12, height = 5, dpi = 150)
cat("Saved: plot3_heatmap.png\n")

