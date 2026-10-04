# =============================================================
# 05_rq3_holiday_comparison.R
# RQ2-2 / RQ3: Normal week vs Holiday week comparison
#
# 전제: 01_pipeline_baseline.R 실행 완료
# 필요 객체: hourly_counts, daily_counts, dow_order
#
# 생성 파일:
#   plot_holiday_daily_overlay.png  — 논문 Fig.3 스타일 histogram
#   plot_holiday_hourly_overlay.png — 시간대별 overlay
#   table_holiday_comparison.csv
# =============================================================

library(tidyverse)
library(rstatix)

# ── 1. Holiday dates 정의 ─────────────────────────────────────
# Oct–Dec 2025 미국 major holiday + 전후일
holiday_dates <- as.Date(c(
  "2025-11-11",   # Veterans Day
  "2025-11-26",   # Thanksgiving eve (travel peak)
  "2025-11-27",   # Thanksgiving
  "2025-11-28",   # Black Friday
  "2025-12-24",   # Christmas Eve
  "2025-12-25",   # Christmas
  "2025-12-26"    # Day after Christmas
))

# ── 2. 날짜 분류 ──────────────────────────────────────────────
daily_labeled <- daily_counts %>%
  mutate(
    period = if_else(date %in% holiday_dates, "Holiday week", "Normal week"),
    period = factor(period, levels = c("Normal week", "Holiday week"))
  )

cat("=== 날짜 분류 결과 ===\n")
daily_labeled %>% count(period) %>% print()

# ── 3. 기술통계 비교 ──────────────────────────────────────────
comparison_stats <- daily_labeled %>%
  group_by(period) %>%
  summarise(
    n      = n(),
    mean   = round(mean(n_flights), 1),
    median = round(median(n_flights), 1),
    sd     = round(sd(n_flights), 1),
    min    = min(n_flights),
    max    = max(n_flights),
    .groups = "drop"
  )

cat("\n=== 기술통계 비교 ===\n")
print(comparison_stats)
write_csv(comparison_stats, "table_holiday_comparison.csv")

# ── 4. Wilcoxon test ──────────────────────────────────────────
wx_result <- wilcox.test(
  n_flights ~ period,
  data      = daily_labeled,
  exact     = FALSE
)
cat("\n=== Wilcoxon rank-sum test ===\n")
print(wx_result)

# ── 5. Plot: Daily count overlay histogram (논문 Fig.3 스타일) ─
# A = Normal week (파랑), B = Holiday week (주황)
p_hist <- ggplot(daily_labeled,
                 aes(x = n_flights, fill = period, color = period)) +
  geom_histogram(
    aes(y = after_stat(count)),
    binwidth   = 20,
    alpha      = 0.55,
    position   = "identity",
    linewidth  = 0.3
  ) +
  scale_fill_manual(
    values = c("Normal week" = "#2C7BB6", "Holiday week" = "#E8793C"),
    name   = NULL
  ) +
  scale_color_manual(
    values = c("Normal week" = "#1a5a8a", "Holiday week" = "#b55a20"),
    name   = NULL
  ) +
  # mean lines
  geom_vline(
    data = comparison_stats,
    aes(xintercept = mean, color = period),
    linetype  = "dashed",
    linewidth = 0.9
  ) +
  labs(
    title    = "ZJX Corridor: Daily Flight Count Distribution",
    subtitle = "Normal weeks vs holiday weeks. Dashed lines = group means.",
    x        = "Unique Flights per Day",
    y        = "Frequency"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "top"
  )

ggsave("plot_holiday_daily_overlay.png", p_hist,
       width = 9, height = 5, dpi = 150)
cat("Saved: plot_holiday_daily_overlay.png\n")

# ── 6. Plot: Hourly profile overlay ───────────────────────────
# 시간대별로 Normal vs Holiday mean 비교선
hourly_labeled <- hourly_counts %>%
  mutate(
    period = if_else(date %in% holiday_dates, "Holiday week", "Normal week"),
    period = factor(period, levels = c("Normal week", "Holiday week"))
  )

hourly_period_summary <- hourly_labeled %>%
  group_by(period, hour) %>%
  summarise(
    mean_flights = round(mean(n_flights), 1),
    sd_flights   = round(sd(n_flights), 1),
    .groups = "drop"
  )

p_hourly <- ggplot(hourly_period_summary,
                   aes(x = hour, y = mean_flights,
                       color = period, group = period)) +
  geom_ribbon(
    aes(ymin = mean_flights - sd_flights,
        ymax = mean_flights + sd_flights,
        fill = period),
    alpha = 0.12, color = NA
  ) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = 7:18) +
  scale_color_manual(
    values = c("Normal week" = "#2C7BB6", "Holiday week" = "#E8793C"),
    name   = NULL
  ) +
  scale_fill_manual(
    values = c("Normal week" = "#2C7BB6", "Holiday week" = "#E8793C"),
    name   = NULL
  ) +
  labs(
    title    = "ZJX Corridor: Hourly Flight Count — Normal vs Holiday Weeks",
    subtitle = "Mean ± 1 SD. UTC 07–18.",
    x        = "Hour (UTC)",
    y        = "Mean Unique Flights"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "top"
  )

ggsave("plot_holiday_hourly_overlay.png", p_hourly,
       width = 11, height = 5, dpi = 150)
cat("Saved: plot_holiday_hourly_overlay.png\n")

# ── 7. 결과 요약 ──────────────────────────────────────────────
cat("\n=== 논문용 결과 요약 ===\n")
normal_mean   <- comparison_stats %>% filter(period == "Normal week")  %>% pull(mean)
holiday_mean  <- comparison_stats %>% filter(period == "Holiday week") %>% pull(mean)
cat("Normal week mean: ", normal_mean, "\n")
cat("Holiday week mean:", holiday_mean, "\n")
cat("Difference:       ", round(normal_mean - holiday_mean, 1), "\n")
cat("Wilcoxon p-value: ", round(wx_result$p.value, 3), "\n")

cat("\n=== 05_rq3_holiday_comparison.R 완료 ===\n")
cat("다음 실행: 06_rq4_rq5_launch_analysis.R\n")
