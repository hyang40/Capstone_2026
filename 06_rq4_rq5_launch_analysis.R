# =============================================================
# 06_rq4_rq5_launch_analysis.R
# RQ4: Launch day EDA
# RQ5: Launch day vs stratified baseline comparison
#
# 전제: 01_pipeline_baseline.R 실행 완료
# 필요 파일: launch_dates_daytime_only.csv
#
# 생성 파일:
#   plot_launch_vs_baseline_overlay.png
#   plot_launch_zscore.png
#   plot_launch_hourly_comparison.png
#   table_launch_zscore.csv
# =============================================================

library(tidyverse)
library(rstatix)

# ── 1. Launch dates 로드 ──────────────────────────────────────
launch_df <- read_csv("launch_dates_daytime_only.csv") %>%
  mutate(launch_date = as.Date(launch_date))

cat("=== Daytime 발사 목록 ===\n")
print(launch_df %>% select(launch_date, time_utc, rocket, site))

launch_dates_vec <- launch_df$launch_date

# ── 2. RQ4: Launch day EDA ───────────────────────────────────
# 발사일의 daily count 추출
launch_daily <- daily_counts %>%
  filter(date %in% launch_dates_vec) %>%
  left_join(launch_df %>% select(launch_date, rocket, site),
            by = c("date" = "launch_date"))

cat("\n=== 발사일 daily flight count ===\n")
print(launch_daily %>% select(date, day_of_week, n_flights, rocket))

# 발사일 기술통계
cat("\n발사일 평균:", round(mean(launch_daily$n_flights), 1), "\n")
cat("전체 평균:  ", round(mean(daily_counts$n_flights), 1), "\n")
cat("차이:       ", round(mean(launch_daily$n_flights) - mean(daily_counts$n_flights), 1), "\n")

# ── 3. Plot: Launch days overlay on boxplot ───────────────────
p_overlay <- ggplot(daily_counts,
                    aes(x = day_of_week, y = n_flights)) +
  geom_boxplot(aes(fill = day_of_week),
               outlier.shape = NA, show.legend = FALSE,
               alpha = 0.4, linewidth = 0.6) +
  # 발사일 포인트
  geom_point(
    data = launch_daily,
    aes(x = day_of_week, y = n_flights),
    color = "#D7191C", size = 4, shape = 18,
    show.legend = TRUE
  ) +
  geom_text(
    data = launch_daily,
    aes(x = day_of_week, y = n_flights,
        label = format(date, "%m/%d")),
    color = "#D7191C", size = 3,
    vjust = -1, hjust = 0.5
  ) +
  scale_fill_brewer(palette = "Blues") +
  labs(
    title    = "ZJX Corridor: Launch Days vs Baseline Distribution",
    subtitle = "Gray boxes = baseline distribution. Red diamonds = daytime launch days.",
    x        = "Day of Week",
    y        = "Unique Flights per Day"
  ) +
  theme_minimal(base_size = 13) +
  theme(plot.title = element_text(face = "bold"))

ggsave("plot_launch_vs_baseline_overlay.png", p_overlay,
       width = 10, height = 5, dpi = 150)
cat("Saved: plot_launch_vs_baseline_overlay.png\n")

# ── 4. RQ5: Z-score calculation ───────────────────────────────
# 각 발사일을 해당 요일의 baseline(mean, sd)과 비교
dow_stats <- daily_counts %>%
  group_by(day_of_week) %>%
  summarise(
    baseline_mean = mean(n_flights),
    baseline_sd   = sd(n_flights),
    .groups = "drop"
  )

launch_zscore <- launch_daily %>%
  left_join(dow_stats, by = "day_of_week") %>%
  mutate(
    z_score      = round((n_flights - baseline_mean) / baseline_sd, 2),
    pct_change   = round((n_flights - baseline_mean) / baseline_mean * 100, 1),
    anomalous    = abs(z_score) > 1.5
  ) %>%
  select(date, day_of_week, rocket, n_flights,
         baseline_mean, baseline_sd, z_score, pct_change, anomalous) %>%
  arrange(z_score)

cat("\n=== 발사일 z-score 결과 ===\n")
print(launch_zscore)
write_csv(launch_zscore, "table_launch_zscore.csv")

# ── 5. Plot: Z-score bar chart ────────────────────────────────
p_zscore <- ggplot(launch_zscore,
                   aes(x = reorder(as.character(date), z_score),
                       y = z_score,
                       fill = z_score < 0)) +
  geom_col(width = 0.65, show.legend = FALSE) +
  geom_hline(yintercept =  1.5, linetype = "dashed",
             color = "gray40", linewidth = 0.7) +
  geom_hline(yintercept = -1.5, linetype = "dashed",
             color = "gray40", linewidth = 0.7) +
  geom_text(aes(label = paste0(z_score, "\n(", pct_change, "%)")),
            vjust = ifelse(launch_zscore$z_score >= 0, -0.3, 1.2),
            size = 3.2, color = "gray20") +
  scale_fill_manual(values = c("FALSE" = "#2C7BB6", "TRUE" = "#D7191C")) +
  annotate("text", x = 0.5, y = 1.65,
           label = "±1.5 SD threshold", hjust = 0,
           size = 3, color = "gray40") +
  labs(
    title    = "ZJX Corridor: Launch Day Deviation from Baseline",
    subtitle = "Z-score relative to same weekday baseline. Red = below average.",
    x        = "Launch Date",
    y        = "Z-score"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title  = element_text(face = "bold"),
    axis.text.x = element_text(angle = 30, hjust = 1)
  )

ggsave("plot_launch_zscore.png", p_zscore,
       width = 10, height = 5, dpi = 150)
cat("Saved: plot_launch_zscore.png\n")

# ── 6. Plot: Hourly profile — launch day vs baseline ─────────
# 각 발사일의 시간대별 count vs 해당 요일 baseline
launch_hourly <- hourly_counts %>%
  filter(date %in% launch_dates_vec) %>%
  mutate(type = paste0(format(date, "%m/%d"), " (launch)"))

baseline_hourly_summary <- hourly_counts %>%
  filter(!date %in% launch_dates_vec) %>%
  group_by(day_of_week, hour) %>%
  summarise(
    mean_flights = mean(n_flights),
    sd_flights   = sd(n_flights),
    .groups = "drop"
  )

# 각 발사일 요일의 baseline line을 함께 그림
p_hourly_launch <- ggplot() +
  # baseline ribbon per DOW
  geom_ribbon(
    data = baseline_hourly_summary %>%
      filter(day_of_week %in% unique(launch_daily$day_of_week)),
    aes(x = hour,
        ymin = mean_flights - sd_flights,
        ymax = mean_flights + sd_flights,
        group = day_of_week),
    fill = "gray70", alpha = 0.25
  ) +
  geom_line(
    data = baseline_hourly_summary %>%
      filter(day_of_week %in% unique(launch_daily$day_of_week)),
    aes(x = hour, y = mean_flights, group = day_of_week),
    color = "gray50", linewidth = 0.8, linetype = "dashed"
  ) +
  # launch day lines
  geom_line(
    data = launch_hourly,
    aes(x = hour, y = n_flights,
        group = date, color = type),
    linewidth = 1
  ) +
  geom_point(
    data = launch_hourly,
    aes(x = hour, y = n_flights, color = type),
    size = 2
  ) +
  scale_x_continuous(breaks = 7:18) +
  scale_color_brewer(palette = "Set1", name = "Launch day") +
  labs(
    title    = "ZJX Corridor: Launch Day Hourly Profile vs Baseline",
    subtitle = "Colored lines = launch days. Dashed gray = weekday baseline mean ± 1 SD.",
    x        = "Hour (UTC)",
    y        = "Unique Flights"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold"),
    legend.position = "bottom",
    legend.text     = element_text(size = 10)
  )

ggsave("plot_launch_hourly_comparison.png", p_hourly_launch,
       width = 12, height = 6, dpi = 150)
cat("Saved: plot_launch_hourly_comparison.png\n")

# ── 7. RQ5: Wilcoxon signed-rank test ────────────────────────
# 각 발사일의 n_flights를 해당 요일 baseline mean과 비교
# n=7이므로 결과 해석 시 한계 명시 필요

cat("\n=== RQ5: Wilcoxon signed-rank test ===\n")
cat("H0: Launch day count = weekday baseline mean\n")
cat("H1: Launch day count < weekday baseline mean (단측)\n\n")

wx_launch <- wilcox.test(
  x         = launch_zscore$n_flights,
  mu        = mean(launch_zscore$baseline_mean),
  alternative = "less",
  exact     = FALSE
)
print(wx_launch)

cat("\n=== 논문용 결과 요약 ===\n")
cat("분석 발사 수(daytime only):", nrow(launch_zscore), "\n")
cat("발사일 mean flight count: ",
    round(mean(launch_zscore$n_flights), 1), "\n")
cat("해당 요일 baseline mean:  ",
    round(mean(launch_zscore$baseline_mean), 1), "\n")
cat("평균 z-score:             ",
    round(mean(launch_zscore$z_score), 2), "\n")
cat("Wilcoxon p-value:         ",
    round(wx_launch$p.value, 3), "\n")
cat("\n주의: n=7 소표본 — 결과 해석 시 통계 검정력 한계 명시 필요\n")

cat("\n=== 06_rq4_rq5_launch_analysis.R 완료 ===\n")
