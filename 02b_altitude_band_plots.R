# =============================================================
# 02b_altitude_band_plots.R
#
# MUST RUN AFTER : 01_kmxr_data_prep.R 
####altitude_band
#   FL200-250  : 20,000 – 24,999 ft
#   FL250-300  : 25,000 – 29,999 ft
#   FL300-350  : 30,000 – 34,999 ft
#   FL350-400  : 35,000 – 40,000 ft
#   Above FL400: 40,000 over
#
#   altitude_band_by_hour.png
#   altitude_band_by_weekday.png
#   altitude_band_proportion_by_hour.png
#   altitude_band_by_direction.png
# =============================================================


library(tidyverse)
library(lubridate)
library(scales)

setwd("D:/Capstone/CapstoneSp26")

df <- read_csv("D:/Capstone/CapstoneSp26/01_prep_kmxr_data.csv")

plot_dir <- "02_All_Data_EDA"
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)


# Step1. Altitude preprocessing
band_levels <- c(
  "FL200-250",
  "FL250-300",
  "FL300-350",
  "FL350-400",
  "Above FL400"
)

# Altitude Band
band_colors <- c(
  "FL200-250"   = "#c6dbef",
  "FL250-300"   = "#6baed6",
  "FL300-350"   = "#2171b5",
  "FL350-400"   = "#084594",
  "Above FL400" = "#08306b"
)

df <- df %>%
  filter(direction != "Other") %>%
  mutate(altitude_band = factor(altitude_band, levels = band_levels))


# Step 2: date × hour × altitude_band
df_block_alt <- df %>%
  group_by(date, day_of_week, hour, altitude_band) %>%
  summarise(
    flight_count = n_distinct(callsign),
    .groups = "drop"
  )


#Plot 1
p1 <- df_block_alt %>%
  group_by(hour, altitude_band) %>%
  summarise(total = sum(flight_count), .groups = "drop") %>%
  ggplot(aes(x = factor(hour), y = total, fill = altitude_band)) +
  geom_col(position = "stack", width = 0.8) +
  scale_fill_manual(values = band_colors, name = "Altitude Band") +
  labs(
    title    = "ZJX Corridor: Flight Count by Hour and Altitude Band",
    subtitle = "Above FL200. Color = altitude in 5,000 ft increments.",
    x        = "Hour (UTC)",
    y        = "Flight Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

ggsave(file.path(plot_dir, "altitude_band_by_hour.png"),
       p1, width = 12, height = 5, dpi = 300)
cat("Saved: altitude_band_by_hour.png\n")


#Plot 2
p2 <- df_block_alt %>%
  group_by(day_of_week, altitude_band) %>%
  summarise(total = sum(flight_count), .groups = "drop") %>%
  ggplot(aes(x = day_of_week, y = total, fill = altitude_band)) +
  geom_col(position = "stack", width = 0.7) +
  scale_fill_manual(values = band_colors, name = "Altitude Band") +
  labs(
    title    = "ZJX Corridor: Flight Count by Weekday and Altitude Band",
    subtitle = "Above FL200. Color = altitude in 5,000 ft increments.",
    x        = "Day of Week",
    y        = "Flight Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

ggsave(file.path(plot_dir, "altitude_band_by_weekday.png"),
       p2, width = 10, height = 5, dpi = 300)
cat("Saved: altitude_band_by_weekday.png\n")


#Plot 3: proportion
p3 <- df_block_alt %>%
  group_by(hour, altitude_band) %>%
  summarise(total = sum(flight_count), .groups = "drop") %>%
  ggplot(aes(x = factor(hour), y = total, fill = altitude_band)) +
  geom_col(position = "fill", width = 0.8) +
  scale_y_continuous(labels = percent) +
  scale_fill_manual(values = band_colors, name = "Altitude Band") +
  labs(
    title    = "ZJX Corridor: Altitude Band Proportion by Hour",
    subtitle = "Proportion of flights in each 5,000 ft increment per hour.",
    x        = "Hour (UTC)",
    y        = "Proportion"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title = element_text(face = "bold"),
        legend.position = "right")

ggsave(file.path(plot_dir, "altitude_band_proportion_by_hour.png"),
       p3, width = 12, height = 5, dpi = 300)
cat("Saved: altitude_band_proportion_by_hour.png\n")


# Plot 4: North vs South
p4 <- df %>%
  count(direction, altitude_band) %>%
  ggplot(aes(x = altitude_band, y = n, fill = altitude_band)) +
  geom_col(show.legend = FALSE, width = 0.7) +
  scale_fill_manual(values = band_colors) +
  scale_y_continuous(labels = comma) +
  facet_wrap(~ direction, nrow = 1) +
  labs(
    title    = "ZJX Corridor: Altitude Band Distribution by Direction",
    subtitle = "North vs South. Raw ping count per altitude band.",
    x        = "Altitude Band",
    y        = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(plot.title  = element_text(face = "bold"),
        axis.text.x = element_text(angle = 30, hjust = 1),
        strip.text  = element_text(face = "bold", size = 13))

ggsave(file.path(plot_dir, "altitude_band_by_direction.png"),
       p4, width = 11, height = 5, dpi = 300)
cat("Saved: altitude_band_by_direction.png\n")

df %>%
  count(altitude_band) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  arrange(altitude_band) %>%
  print()